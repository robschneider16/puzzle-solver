#lang racket

(require (planet gcr/riot))
;(require (planet soegaard/gzip:2:2))
;(require file/gzip)
;(require file/gunzip)
(require rnrs/sorting-6)
(require data/heap)
(require srfi/1)
(require racket/fixnum)
(require racket/set)
(require mzlib/string) ;; supposedly depricated but seems to need the require for 5.3.5


(require "stp-init.rkt")
(require "stp-solve-base.rkt")
(require "stp-fringefilerep.rkt")
;(require profile)
;(instrumenting-enabled #t)
;(profiling-enabled #t)

(provide (all-defined-out))

(define *depth-start-time* "the time from current-seconds at the start of a given depth")
(define *master-name* "the name of the host where the master process is running")
(define *local-store* "the root portion of path to where workers can store temporary fringe files")
;#|
(set! *master-name* "localhost")
(set! *local-store* "/space/fringefiles/")
;(set! *local-store* "/state/partition1/fringefiles/")
(define *n-processors* 1)
;|#
#|
(set! *master-name* "wcp")
(set! *local-store* "/state/partition1/fringefiles/")
(define *n-processors* 32)
|#
(define *expand-multiplier* 1)
(define *merge-multiplier* 1)
(define *n-expanders* (* *n-processors* *expand-multiplier*))
(define *n-mergers* (* *n-processors* *merge-multiplier*))

(define *diy-threshold* 1000) ;;**** this must be significantly less than EXPAND-SPACE-SIZE 


(define *most-positive-fixnum* 0)
(define *most-negative-fixnum* 0)
(cond [(fixnum? (expt 2 61))
       (set! *most-positive-fixnum* (fx+ (expt 2 61) (fx- (expt 2 61) 1)))  ;; ****** only on 64-bit architectures *****
       (set! *most-negative-fixnum* (fx+ (fx* -1 (expt 2 61)) (fx* -1 (expt 2 61))))];; ***** likewise *****
      [else 
       ;; For the cluster platform use the following:
       (set! *most-positive-fixnum* (fx+ (expt 2 29) (fx- (expt 2 29) 1)))  ;; ****** only on our cluster, wcp *****
       (set! *most-negative-fixnum* (fx+ (fx* -1 (expt 2 29)) (fx* -1 (expt 2 29))))]);; ***** likewise *****

(define *found-goal* #f)

;;------------------------------------------
;; proto-fringe slicing

(define *num-proto-fringe-slices* *n-mergers*)
;; define the fixed hash-code bounds to be used for repsonsibility ranges and proto-fringe slicing
(define *proto-fringe-slice-bounds*
  (let* ([slice-width (floor (/ (- *most-positive-fixnum* *most-negative-fixnum*) *num-proto-fringe-slices*))]
         [slices (for/vector #:length (add1 *num-proto-fringe-slices*)
                   ([i *num-proto-fringe-slices*])
                   (+ *most-negative-fixnum* (* i slice-width)))])
    (vector-set! slices *num-proto-fringe-slices* (add1 *most-positive-fixnum*))
    slices))

;; get-slice-num: fixnum [low number] [hi number] -> number
;; use binary search to find index for given hash-code within ranges defined by *proto-fringe-slice-bounds*
(define (get-slice-num phc (low 0) (hi *num-proto-fringe-slices*))
  (let ([mid (floor (/ (+ low hi) 2))])
    (cond [(= low mid)
           (when (>= phc (vector-ref *proto-fringe-slice-bounds* (add1 mid)))
             (error (format "get-slice-num: missed the mark with index ~a for hc=~a~%" mid phc)))
           mid]
          [(< phc (vector-ref *proto-fringe-slice-bounds* mid))
           (get-slice-num phc low mid)]
          [else (get-slice-num phc mid hi)])))


;;----------------------------------------------------------------------------------------

;; expand-fringe-self: fringe fringe int -> fringe
;; expand just the current-fringe and remove duplicates in the expansion and repeats from prev-fringe
;; returning the new fringe
(define (expand-fringe-self pf cf depth)
  (let* ([prev-fringe-set (for/fold ([the-fringe (set)])
                            ([sgmnt (fringe-segments pf)])
                            (set-union the-fringe
                                       (list->set (read-fringe-from-file (filespec-fullpathname sgmnt)))))] ; pf- and cf-spec's in expand-fringe-self should have empty fbase
         [current-fringe-set
          (for/fold ([the-fringe (set)])
            ([sgmnt (fringe-segments cf)])
            (set-union the-fringe
                       (list->set (read-fringe-from-file (filespec-fullpathname sgmnt)))))]
         [new-cf-name (format "fringe-d~a" depth)]
         ;[prntmsg (printf "finished reading the fringes~%")]
         [exp-ptr 0]
         [expand-them (for ([p-to-expand current-fringe-set])
                        (set! exp-ptr (expand p-to-expand exp-ptr)))]
         [res (set->list (for/set ([i exp-ptr]
                                   #:unless (or (set-member? prev-fringe-set (vector-ref *expansion-space* i))
                                                (set-member? current-fringe-set (vector-ref *expansion-space* i))))
                           (when (is-goal? (vector-ref *expansion-space* i)) (set! *found-goal* (vector-ref *expansion-space* i)))
                           (vector-ref *expansion-space* i)))]
         )
    #|(printf "Finished the work packet generating a set of ~a positions~%" (set-count res))
    (for ([p res])
      (printf "pos: ~a~%~a~%" (stringify p) p))|#
    (for ([sgmnt (fringe-segments pf)]) (delete-file (filespec-fullpathname sgmnt)))
    (write-fringe-to-disk (sort res hcposition<?) new-cf-name)
    (make-fringe "" (list (make-filespec new-cf-name (length res) (file-size new-cf-name) "")) (length res))))



;;----------------------------------------------------------------------------------------
;; DISTRIBUTED EXPANSION AND MERGING OF FRINGES

;; a sampling-stat is a (vector int fixnum fixnum (listofof fixnum) boolean string int)
;; where the elements are:s
;; 0. total number of duplicate-free positions summed over slices
;; 1. number of positions discarded because duplicate with current or prev-fringes
;; 2. number of positions discarded because duplicate with other partial-expansion file
;; 3. vector of numbers counting duplicate-free positions in each respective slice (provide pcount if needed for fspec)
;; 4. boolean goal-found if found when expanding the assigned positions
;; 5. output file name prefix, e.g., proto-fringe-dXX-NN, without slice id which is assumed to be added when needed
;; 6. vector of slice file sizes
;; 7. total number of positions processed to give rise to duplicate free positions in index 0
;; 8. number of duplicate positions eliminated while generating the partial-expansions

;; ---------------------------------------------------------------------------------------
;; EXPANSION .....

;; make-vector-ranges: int -> (listof (list int int)
;; create the pairs of indices into the current-fringe-vector that will specify the part of the fringe each worker tackles
(define (make-vector-ranges vlength)
  (if (< vlength 10)
      (list (list 0 vlength))
      (let ((start-list (build-list *n-expanders*
                                    (lambda (i) (floor (* i (/ vlength *n-expanders*)))))))
        (foldr (lambda (x r) (cons (list x (first (first r))) r)) 
               (list (list (last start-list) vlength)) 
               (drop-right start-list 1)))))

;; simple-ranges: (listof filespec) -> (listof (list int int)
;; just use the fringe-segments
(define (make-simple-ranges lofspec)
  (let ([start-range 0])
    (for/list ([fs lofspec])
      (set! start-range (+ start-range (filespec-pcount fs)))
      (list (- start-range (filespec-pcount fs)) start-range))))

;; remove-dupes: fringe fringe (listof filespec) string int int -> sampling-stat
;; Running in distributed worker processes:
;; Remove duplicate positions from the list of expand-fspec files (i.e., partial-expansion...),
;; for any positions that also appear in the prev- or current-fringes, or within multiple partial-expansion... files.
;; All of the partial-expansion files are sorted, so we can write the merged result to slices as we go.
;; Write the non-duplicate positions to a "proto-fringe-dXX-NN" file -- previously we wrote this in a *local-store* folder
;; and later delivered a copy to the working directory to share with all the other compute-nodes;
;; Try writing directly to the shared NFS drive as a way to spread out network traffic.  This will clean up file name in the return sampling-stat...
;; Accordingly, the sampling-stat return value has a filename pointing to the working directory.
(define (remove-dupes pf cf lo-expand-fspec ofile-name depth partial-exp-dupes)
  ;; the ofile-name is just the file-name -- no *local-store* path where needed
  #|(printf "EXPAND PHASE 2 (REMOVE DUPLICATES) pf: ~a~%cf: ~a~%all of lo-expand-fspec: ~a~%ofile-name: ~a~%depth: ~a~%"
          pf cf lo-expand-fspec ofile-name depth);|#
  ;; EXPAND PHASE 2 (REMOVE DUPLICATES)
  (let* ([pffh (fh-from-fringe pf)]
         [cffh (fh-from-fringe cf)]
         [n-pos-to-process (for/sum ([an-fspec lo-expand-fspec]) (filespec-pcount an-fspec))]
         [lo-effh (for/list ([an-fspec lo-expand-fspec]) (fh-from-filespec an-fspec))]
         [heap-o-fheads (let ([lheap (make-heap (lambda (fh1 fh2) (hcposition<? (fringehead-next fh1) (fringehead-next fh2))))])
                          (heap-add-all! lheap lo-effh)
                          lheap)]
         ; writing directly to NFS doesn't seem any slower than *local-store* and then copy -- that claim still needs verification
         ; write to one slice-ofile at a time, since everything is sorted
         [proto-slice-num 0]
         [slice-upper-bound (vector-ref *proto-fringe-slice-bounds* (add1 proto-slice-num))]
         [proto-slice-ofile (open-output-file (string-append ofile-name "-" (~a proto-slice-num #:left-pad-string "0" #:width 3 #:align 'right)))]
         [unique-expansions 0]
         [slice-counts (make-vector *num-proto-fringe-slices* 0)]
         [sample-stats 
          (vector 0 ; number of positions preserved for further merging
                  0 ; number of positions discarded because duplicate with prev- or current-fringes
                  0 ; number of positions discarded because duplicate with another partial-expansion
                  slice-counts
                  #f 
                  ofile-name ;; here, use the stem of the shared ofile-name 
                  0
                  n-pos-to-process
                  partial-exp-dupes)]
         [last-pos-bs #"~~~~~~~~NoLastPos"]
         )
    ;; locally merge the pre-proto-fringes, removing duplicates from prev- and current-fringes
    (for ([an-fhead (in-heap/consume! heap-o-fheads)])
      (let ([efpos (fringehead-next an-fhead)])
        (unless ;; efpos is a duplicate
            (or (and (bytes=? (hc-position-bs efpos) last-pos-bs) ; duplicate from most recently written 
                     (vector-set! sample-stats 2 (add1 (vector-ref sample-stats 2))))
                (and (position-in-fhead? efpos pffh) (vector-set! sample-stats 1 (add1 (vector-ref sample-stats 1))))
                (and (position-in-fhead? efpos cffh) (vector-set! sample-stats 1 (add1 (vector-ref sample-stats 1)))))
          (set! last-pos-bs (hc-position-bs efpos))
          (do ([efpos-hc (hc-position-hc efpos)])
            ;; if efpos-hc is >= to the slice-upper-bound, advance the proto-slice-num/ofile/upper-bound until it is not
            ((< efpos-hc slice-upper-bound))
            (close-output-port proto-slice-ofile)
            (set! proto-slice-num (add1 proto-slice-num))
            (set! proto-slice-ofile (open-output-file (string-append ofile-name "-" (~a proto-slice-num #:left-pad-string "0" #:width 3 #:align 'right))))
            (set! slice-upper-bound (vector-ref *proto-fringe-slice-bounds* (add1 proto-slice-num))))
          (fprintf proto-slice-ofile "~a~%" (hc-position-bs efpos))
          (when (is-goal? efpos) (vector-set! sample-stats 4 (hc-position-bs efpos)))
          (vector-set! slice-counts proto-slice-num (add1 (vector-ref slice-counts proto-slice-num))))
        (advance-fhead! an-fhead)
        (unless (fhdone? an-fhead) ;;(eof-object? (peek-byte (fringehead-iprt an-fhead) 1))
          (heap-add! heap-o-fheads an-fhead))))
    ;(printf "remote-expand-part-fringe: HAVE EXPANSIONS:~%")
    ;; close input and output ports
    (for ([fh (cons pffh (cons cffh lo-effh))]) (close-input-port (fringehead-iprt fh)))
    (close-output-port proto-slice-ofile)
    (for ([i (in-range (add1 proto-slice-num) *num-proto-fringe-slices*)])
      (touch (string-append ofile-name "-" (~a i #:left-pad-string "0" #:width 3 #:align 'right))))
    ;; complete the sampling-stat
    (vector-set! sample-stats 0 (for/sum ([i (vector-ref sample-stats 3)]) i))
    (vector-set! sample-stats 6 (for/vector ([i *num-proto-fringe-slices*]) 
                                  (file-size (format "~a-~a" ofile-name (~a i #:left-pad-string "0" #:width 3 #:align 'right)))))
    ;; delete files that are no longer needed
    (for ([efspec lo-expand-fspec]) (delete-file (filespec-fullpathname efspec)))
    ;(unless (string=? *master-name* "localhost") (delete-fringe pf))
    ;(delete-file use-ofilename)
    ;;**** THIS STRIKES ME AS DANGEROUS: IF ONE PROCESS ON MULTI-CORE MACHINE FINISHES FIRST ....
    ;(when (file-exists? (string-append *local-store* (fspec-fname pfspec))) (delete-file (string-append *local-store* (fspec-fname pfspec))))
    #|(printf "remove-dupes: starting w/ ~a positions, expansion has ~a/~a positions~%"
            (fspec-pcount expand-fspec) unique-expansions (position-count-in-file ofile-name))|#
    #|(unless (check-sorted-fringe? ofile-name)
      (error 'remove-dupes "phase 2: failed to generate a sorted fringe file ~a" ofile-name))|#
    sample-stats))

;; dump-partial-expansion: int string int (listof fspec) -> (values (listof fspec) int)
;; given the count of pending expanded positions to write, the out-file template, the out-file counter, and the previously written filespecs,
;; sort and write the specified number of positions to the appropriately opened new file,
;; returning two values: the list with the new filespec added and the number of duplicates eliminated at this phase
(define (dump-partial-expansion pcount ofile-template ofile-counter ofiles prev-dupes)
  (let* ([hc-to-scrub 'hcpos-to-scrub]
         [f (format "~a~a" ofile-template ofile-counter)]
         [fullpath (string-append *local-store* f)]
         [this-batch 0])
    ;; scrub the last part of the vector with bogus positions
    (for ([i (in-range pcount (vector-length *expansion-space*))])
      (set! hc-to-scrub (vector-ref *expansion-space* i))
      (set-hc-position-hc! hc-to-scrub *most-positive-fixnum*)    ;; make vector-sort! put these at the very end, but if a positions has *most-positive-fixnum* ...
      (bytes-copy! (hc-position-bs hc-to-scrub) 0 #"~~~~~~~~IgnoreMe~~" 0 *num-pieces*)) ;; #\~ (ASCII character 126) is greater than any of our positions
    ;; sort the vector
    (vector-sort! hcposition<? *expansion-space*)
    ;; write the first pcount positions to the file
    (set! this-batch (write-fringe-to-disk *expansion-space* fullpath pcount #t))
    ;; return the two values: augmented list of filespecs, and the incremented number of duplicates eliminated during writing
    (values (cons (make-filespec f this-batch (file-size fullpath) *local-store*)
                  ofiles)
            (+ prev-dupes (- pcount this-batch)))))

;; remote-expand-part-fringe: (list int int) int fringe fringe int -> {whatever returned by remove-dupes}
;; given a pair of indices into the current-fringe that should be expanded by this process, a process-id,
;; and the prev- and current-fringes, and the depth ...
;; expand the positions in the indices range, ignoring duplicates other than within the new fringe being constructed.
(define (remote-expand-part-fringe ipair process-id pf cf depth)
  ;; prev-fringe spec points to default shared directory; current-fringe spec points to *local-store* folder
  ;(printf "remote-expand-part-fringe: starting with pf: ~a, and cf: ~a~%" pf cf)
  ;; EXPAND PHASE 1
  (let* ([pre-ofile-template-fname (format "partial-expansion~a" (~a process-id #:left-pad-string "0" #:width 2 #:align 'right))]
         [pre-ofile-counter 0]
         [pre-ofiles empty]
         ;; *** Dynamically choose the size of the pre-proto-fringes to keep the number of files below 500 ***
         [start (first ipair)]
         [end (second ipair)]
         [assignment-count (- end start)]
         [expanded-phase1 1];; technically, not yet, but during initialization in pre-resv do loop
         ;; make the fringehead advanced to the right place
         [cffh (fh-from-fringe cf start)]
         [dupes-caught-here 0]
         )
    ;; do the actual expansions
    (do ([i 1 (add1 i)]
         [expansion-ptr (expand (fringehead-next cffh) 0)
                        (expand (fringehead-next cffh) expansion-ptr)])
      ((>= i assignment-count)
       (set!-values (pre-ofiles dupes-caught-here)
                    (dump-partial-expansion expansion-ptr pre-ofile-template-fname pre-ofile-counter pre-ofiles dupes-caught-here)))
      ;; When we have collected the max number of expansions, create another pre-proto-fringe file
      (when (>= expansion-ptr EXPAND-SPACE-SIZE)
        (set!-values (pre-ofiles dupes-caught-here)
                     (dump-partial-expansion expansion-ptr pre-ofile-template-fname pre-ofile-counter pre-ofiles dupes-caught-here))
        (set! pre-ofile-counter (add1 pre-ofile-counter))
        (set! expansion-ptr 0))
      (advance-fhead! cffh)
      (set! expanded-phase1 (add1 expanded-phase1)))
    #|(printf "remote-exp-part-fringe: PHASE 1: expanding ~a positions of assigned ~a~%" 
            expanded-phase1 assignment-count);|#
    (when (< expanded-phase1 assignment-count)
      (error 'remote-expand-part-fringe
             (format "only expanded ~a of the assigned ~a (~a-~a) positions" expanded-phase1 assignment-count start end)))
    ;(close-input-port (fringehead-iprt cffh))
    ;; PHASE 2: now pass through the proto-fringe expansion file(s) as well as prev-fringe and current-fringe to remove duplicates
    (remove-dupes pf cf pre-ofiles 
                  (format "proto-fringe-d~a-~a" depth (~a process-id #:left-pad-string "0" #:width 2 #:align 'right)) ;; ofile-name
                  depth
                  dupes-caught-here)))


;; remote-expand-fringe: (listof (list fixnum fixnum)) fring fringe int -> (listof sampling-stat)
;; trigger the distributed expansion according to the given ranges
;; In theory, it shouldn't matter where the files pointed to by the fringe are located,
;; but we expect they will point to a *local-store* copy of the current-fringe,
;; where the copy is arranged-for by the master also
(define (remote-expand-fringe ranges pf cf depth)
  ;;(printf "remote-expand-fringe: current-fringe of ~a split as: ~a~%" cur-fringe-size (map (lambda (pr) (- (second pr) (first pr))) ranges))
  (let* ([distrib-results (for/work ([range-pair ranges]
                                     [i (in-range (length ranges))])
                                    (when (> depth *max-depth*) (error 'distributed-expand-fringe "ran off end")) ;;prevent riot cache-failure
                                    ;; need alternate version of wait-for-files that just checks on the assigned range
                                    ;; but for now, just append the fspecs
                                    #| push the wait into where we're trying to access positions 
                                    (wait-for-files (append (map (lambda (seg) (segment-fspec seg)) pf-findex)
                                                            (map (lambda (seg) (segment-fspec seg)) cf-findex)) #t)|#
                                    (remote-expand-part-fringe range-pair i pf cf depth)
                                    )])
    ;(printf "remote-expand-fringe: respective expansion counts: ~a~%" (map (lambda (ssv) (vector-ref ssv 0)) distrib-results))
    distrib-results))


;; ------------------------------------------------------------------------------------------
;; MERGING .....

;; bring-local-partial-expansions: (listof fspec) -> void
;; copy the partial expansions from the shared disk to our local /tmp, 
(define (bring-local-partial-expansions lo-expand-specs)
  (for ([fs lo-expand-specs] #:unless (zero? (filespec-pcount fs)))
    (let* ([base-fname (filespec-fname fs)] 
           [tmp-partexp-name (string-append *local-store* base-fname)])
      (unless (file-exists? tmp-partexp-name) ; unless this process id is here from expansion
        (copy-file base-fname tmp-partexp-name)))))
                                

;; distributed-merge-proto-fringe-slices: (vectorof fspec) int string -> (list string number)
;; given a list of filespecs pointing to the slices assigend to this worker and needing to be merged, copy the slices
;; and merge into a single segment that will participate in the new fringe, removing duplicates among slices.
;; Note: we have already removed from slices any duplicates found in prev- and current-fringes
(define (distributed-merge-proto-fringe-slices slice-fspecs depth ofile-name)
  ;(define (remote-merge-proto-fringes my-range expand-files-specs depth ofile-name)
  ;; expand-files-specs are of pattern: "proto-fringe-dXX-NN" for depth XX and proc-id NN, pointing to working (shared) directory 
  ;; WAS: ofile-name is of pattern: "fringe-segment-dX-NN", where the X is the depth and the NN is a process identifier
  ;; NEW: ofile-name is of pattern: "fringe-segment-dX-NNN", where the X is the depth and the NN is a slice identifier
  (let* ([mrg-segment-oport (open-output-file ofile-name)] ; try writing directly to NFS
         ;[local-protofringe-fspecs (for/list ([fs slice-fspecs] #:unless (zero? (filespec-pcount fs))) (rebase-filespec fs *local-store*))]
         [local-protofringe-fspecs (for/list ([fs slice-fspecs] #:unless (zero? (filespec-pcount fs))) fs)]
         ;[pmsg1 (printf "distmerge-debug1: ~a fspecs in ~a~%distmerge-debug1: or localfspecs=~a~%" (vector-length slice-fspecs) slice-fspecs local-protofringe-fspecs)]
         ;******
         ;****** move the fringehead creation inside the heap-o-fhead construction in order to avoid the short-lived list allocation *******
         [to-merge-fheads 
          (for/list ([exp-fspec local-protofringe-fspecs])
            (fh-from-filespec exp-fspec))]
         ;[pmsg2 (printf "distmerge-debug2: made 'to-merge-fheads'~%")]
         [heap-o-fheads (let ([lheap (make-heap (lambda (fh1 fh2) (hcposition<? (fringehead-next fh1) (fringehead-next fh2))))])
                          (heap-add-all! lheap 
                                         (filter-not (lambda (fh) (eof-object? (fringehead-next fh))) to-merge-fheads))
                          lheap)]
         ;[pmsg3 (printf "distmerge-debug3: made the heap with ~a frigeheads in it~%" (heap-count heap-o-fheads))]
         ;****** log duplicate eliminations here
         [segment-size (let ([last-pos (make-hcpos #"~~~~~~~~NoLastPos")]
                             [keep-pos (void)]
                             [num-written 0])
                         (for ([an-fhead (in-heap/consume! heap-o-fheads)])
                           (set! keep-pos (fringehead-next an-fhead))
                           (advance-fhead! an-fhead)
                           (unless (fhdone? an-fhead) ;;(eof-object? (peek-byte (fringehead-iprt an-fhead) 1))
                             (heap-add! heap-o-fheads an-fhead))
                           (unless (bytes=? (hc-position-bs keep-pos) (hc-position-bs last-pos)) ;; don't write duplicates
                             (fprintf mrg-segment-oport "~a~%" (hc-position-bs keep-pos))
                             (set! num-written (add1 num-written))
                             (set! last-pos keep-pos)))
                         num-written)])
    (close-output-port mrg-segment-oport)
    (for ([fhead to-merge-fheads]) (close-input-port (fringehead-iprt fhead)))
    (unless (or #t 
                (string=? *master-name* "localhost"))
      (for ([fspc local-protofringe-fspecs]) 
        (delete-file (filespec-fullpathname fspc)))) ;remove the local expansions *** but revisit when we reduce work packet size for load balancing
    (list ofile-name segment-size)))

;; remote-merge: (vectorof (vectorof fspec)) int -> (listof string int)
;; expand-files-specs (proto-fringe-specs) is vector of vector of filespecs, the top-level has one for each slice,
;; each one containing as many proto-fringes as expanders, all of which need to be merged
(define (remote-merge expand-files-specs depth)
  ;;**** RETHINK THIS -- MAYBE FORCE THE WORKER TO GRAB THE SLICE IT NEEDS??????
  #|(when (string=? *master-name* "localhost")
    (for ([efs expand-files-specs]) (bring-local-partial-expansions efs)))|#
  ;(printf "remote-merge: n-protof-slices=~a, and length expand-files-specs=~a~%" *num-proto-fringe-slices* (vector-length expand-files-specs))
  (let ([merge-results
         (for/work ([i *num-proto-fringe-slices*]
                    [expand-fspecs-slice expand-files-specs])
                   (when (> depth *max-depth*) (error 'distributed-expand-fringe "ran off end")) ;finesse Riot caching
                   (let* ([ofile-name (format "fringe-segment-d~a-~a" depth (~a i #:left-pad-string "0" #:width 3 #:align 'right))]
                          [merged-fname-and-resp-rng-size (distributed-merge-proto-fringe-slices expand-fspecs-slice depth ofile-name)]
                          )
             ;;(printf "distributed-expand-fringe: merge-range = ~a~%~a~%" merge-range merged-responsibility-range)
             merged-fname-and-resp-rng-size))])
    ;(printf "remote-merge: merged segment names and lengths ~a~%" merge-results)
    #|(when (string=? *master-name* "localhost")
      (for ([fs expand-files-specs])
        (for ([f fs] #:unless (zero? (filespec-pcount f)))
          (delete-file (string-append *local-store* (filespec-fname f))))))|#
    merge-results))


;; ------------------------------------------------------------------------------------------
;; DISTRIBUTED EXPAND AND MERGE

;; distributed-expand-fringe: fringe fringe int -> (list string int int)
;; Distributed version of expand-fringe
;; given prev and current-fringes and the present depth, expand and merge the current fringe,
;; returning the fringe-spec of the newly expanded and merged fringe.
(define (distributed-expand-fringe pf cf depth)
  #|(printf "distributed-expand-fringe: at depth ~a, pf-spec: ~a; cf-spec: ~a~%" 
          depth pf-spec cf-spec)|#
  (let* (;; EXPAND
         [start-expand (current-seconds)]
         ;[ranges (make-vector-ranges (fringe-pcount cf))]
         [ranges (make-simple-ranges (fringe-segments cf))]
         ;; --- Distribute the actual expansion work ------------------------
         ;[sampling-stats (remote-expand-fringe ranges pf rcf depth)]
         [sampling-stats (remote-expand-fringe ranges pf cf depth)]
         [end-expand (current-seconds)]
         ;; -----------------------------------------------------------------
         [check-for-goal (for/first ([ss sampling-stats]
                                     #:when (vector-ref ss 4))
                           (set! *found-goal* (vector-ref ss 4)))]
         ;; make filespecs for proto-fringe-dXX-NN slices the relevant data in the sampling-stats
         [proto-fringe-fspecs (for/vector ([i *num-proto-fringe-slices*]);; for each index to a slice...
                                ;; pull out the info from each sampling-stat
                                (for/vector ([ss sampling-stats]) 
                                  (make-filespec (string-append (vector-ref ss 5) "-" (~a i #:left-pad-string "0" #:width 3 #:align 'right)) ;; fname
                                                 (vector-ref (vector-ref ss 3) i) ;pcount
                                                 (vector-ref (vector-ref ss 6) i) ;file-size
                                                 "")))]
         ;; need to wait for write to complete -- i.e., all data to appear on master
         ;; push this wait into the place we're trying to access the file ... [wait-for-partial-expansions (wait-for-files expand-files-specs)]
         ;; MERGE
         ;; --- Distribute the merging work ----------
         [sorted-segment-fspecs (remote-merge proto-fringe-fspecs depth)]
         [merge-end (current-seconds)]
         ;; -------------------------------------------
         [sorted-expansion-files (map first sorted-segment-fspecs)]
         [sef-lengths (map second sorted-segment-fspecs)]
         #|[error-check2 (for/first ([f sorted-expansion-files]
                                   [len sef-lengths]
                                   #:unless (= len (position-count-in-file f)))
                         (error 'distributed-expand-fringe (format "err-chk2: partial-merges do not match up for ~a which should be ~a" f len)))]|#
         [new-cf-name (format "fringe-d~a" depth)]
         )
    ;; create the _new_ current-fringe
    #|
    (for ([f sorted-expansion-files])
      ;(printf "distributed-expand-fringe: concatenating ~a~%" f)
      (system (format "cat ~a >> fringe-d~a" f depth)))|#
    ;;--- delete files we don't need anymore ---------
    ;; delete previous fringe
    (delete-fringe pf)
    (when (string=? *master-name* "localhost") ; delete the *local-store* prev-fringe
      (delete-fringe pf *local-store*))
    (for ([fspecs proto-fringe-fspecs])
      (for ([fspec fspecs]) (delete-file (filespec-fullpathname fspec))))
    ;(system "rm partial-expansion* partial-merge*")
    ;(unless (string=? *master-name* "localhost") (delete-file (fspec-fname cf-spec)))
    ;; file-copy, expansion, merge, total
    (printf "expand-merge-times: ~a\t~a\t~a\t~a~%"
            depth
            (- end-expand start-expand)         ;expansion
            (- merge-end end-expand)            ;merge
            (- (current-seconds) *depth-start-time*)) ;total
    ;; report the duplicate elimination data
    (let ([counts (vector 0 0 0 0)])
      (for ([ss sampling-stats]) 
        (vector-set! counts 0 (+ (vector-ref ss 0) (vector-ref counts 0)))
        (vector-set! counts 1 (+ (vector-ref ss 1) (vector-ref counts 1)))
        (vector-set! counts 2 (+ (vector-ref ss 2) (vector-ref counts 2)))
        (vector-set! counts 3 (+ (vector-ref ss 8) (vector-ref counts 3))))
      (printf "duplicate-elimination-data: ~a\t~a\t~a\t~a\t~a\t~a\t~a~%"
              depth
              (vector-ref counts 0) ; sum of duplicate-free positions written to proto-fringes -- pre-merge
              (vector-ref counts 1) ; duplicates eliminated because prev- or current-fringe
              (vector-ref counts 2) ; duplicates eliminated because other partial-expansion at current depth
              (vector-ref counts 3) ; duplicates eliminated before first writing to partial-expansion
              (for/sum ([n counts]) n) ; total number of expanded positions handled at this level
              (for/sum ([n sef-lengths]) n))) ; number of positions in the new fringe
    ;; make the new fringe to return
    (make-fringe ""
                 (for/list ([segmentfile sorted-expansion-files]
                            [length sef-lengths])
                   (make-filespec segmentfile length (file-size segmentfile) ""))
                 (for/sum ([len sef-lengths]) len))
    ))


;;----------------------------------------------------------------------------------------

;; expand-fringe: fringe fringe int -> fringe
;; Given the prev- and current-fringes, and the current depth of search,
;; do the expansions and merges as appropriate, returning the new fringe
(define (expand-fringe prev-fringe current-fringe depth)
  (if (< (fringe-pcount current-fringe) *diy-threshold*)
      ;; do it myself
      (expand-fringe-self prev-fringe current-fringe depth)
      ;; else call distributed-expand, which will farm out to workers
      (distributed-expand-fringe prev-fringe current-fringe depth)))


(define *max-depth* 10)(set! *max-depth* 75)

;; cfs-file: fringe fringe int -> position
;; perform a file-based cluster-fringe-search at given depth
;; using given previous and current fringes
(define (cfs-file prev-fringe current-fringe depth)
  (set! *depth-start-time* (current-seconds))
  (cond [(or (zero? (fringe-pcount current-fringe)) (> depth *max-depth*)) #f]
        [*found-goal*
         (print "found goal")
         *found-goal*]
        [else (let ([new-fringe (expand-fringe prev-fringe current-fringe depth)])
                (printf "At depth ~a: current-fringe has ~a positions (and new-fringe ~a) in ~a (~a)~%" 
                        depth (fringe-pcount current-fringe) (fringe-pcount new-fringe)
                        (- (current-seconds) *depth-start-time*) (seconds->time (- (current-seconds) *depth-start-time*)))
                (flush-output)
                ;;(for ([p current-fringe]) (displayln p))
                (cfs-file current-fringe ;; use current-fringe as prev-fringe at next level
                          new-fringe
                          (add1 depth)))]))

;; start-cluster-fringe-search: hc-position -> ...
(define (start-cluster-fringe-search start-position)
  ;; initialization of fringe files
  (write-fringe-to-disk empty "fringe-d-1")
  (write-fringe-to-disk (list start-position) "fringe-d0")
  (cfs-file (make-fringe "" (list (make-filespec "fringe-d-1" 0 (file-size "fringe-d-1") "")) 0)
            (make-fringe "" (list (make-filespec "fringe-d0" 1 (file-size "fringe-d0") "")) 1)
            1))

;; make-fringe-from-files: string int -> fringe
;; given a base-string and number of processors (actually, segments), create the fringe
(define (make-fringe-from-files base-string n-seg)
  (let ([pcount 0])
    (make-fringe 
     "" 
     (for/list ([i n-seg])
       (let* ([f (format "~a~a" base-string (~a i #:left-pad-string "0" #:width 3 #:align 'right))]
              [lpcount (read-from-string (with-output-to-string (lambda () (system (format "wc -l ~a" f)))))])
         (set! pcount (+ pcount lpcount))
         (make-filespec f lpcount (file-size f) "")))
     pcount)))

(define (make-fringe-from-file file)
  (let ([filepcount (read-from-string (with-output-to-string (lambda () (system (format "wc -l ~a" file)))))])
    (make-fringe ""
                 (list (make-filespec file filepcount (file-size file) ""))
                 filepcount)))

;(block10-init)
(climb12-init)
;(climb15-init)
;(climbpro24-init)
(compile-ms-array! *piece-types* *bh* *bw*)

;; THIS IS JUST FOR TESTING THE DUPLICATE-DISCREPANCY
(set! *most-negative-fixnum* 0)
(set! *most-positive-fixnum* (expt *bsz* 10))

;#|
(module+ main
  ;; Switch between these according to if using the cluster or testing on multi-core single machine
  (connect-to-riot-server! *master-name*)
  (define search-result (time (start-cluster-fringe-search *start*)))
  #|
  (define search-result (time (cfs-file (make-fringe-from-files "fringe-segment-dX-" n)
                                        (make-fringe-from-files "fringe-segment-dX-" n)
                                        X)))
  |#
  #|
  (define search-result (time (cfs-file (make-fringe-from-file "c12d59fringe")
                                        (make-fringe-from-file "c12d58fringe")
                                        1)))
  |#
  (print search-result)
  )
;|#

;(time (start-cluster-fringe-search *start*))
