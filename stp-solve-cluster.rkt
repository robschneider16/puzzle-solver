#lang racket/base
;hashddd
(require (planet gcr/riot))
;(require (planet soegaard/gzip:2:2))
;(require file/gzip)
;(require file/gunzip)
;(require rnrs/sorting-6)
(require racket/list
         racket/format
         racket/vector
         )
(require data/heap)
(require racket/fixnum)
(require racket/set)

(require "stpconfigs/configenv.rkt")
(require "stp-init.rkt"
         "stp-solve-base.rkt"
         "stp-fringefilerep.rkt"
         "stp-spaceindex.rkt"
         "myvectorsort.rkt"
         )
;(require profile)
;(instrumenting-enabled #t)
;(profiling-enabled #t)

(provide (all-defined-out))

(define *depth-start-time* "the time from current-seconds at the start of a given depth")

(define *expand-multiplier* 1)
(define *merge-multiplier* 1)
(define *n-expanders* (* *n-processors* *expand-multiplier*))
(define *n-mergers* (* *n-processors* *merge-multiplier*))

(define *diy-threshold* 22000000) ;;**** this must be significantly less than EXPAND-SPACE-SIZE 

(define *most-positive-fixnum* 0)
(define *most-negative-fixnum* 0)
(cond [(fixnum? (expt 2 61))
       (set! *most-positive-fixnum* (fx+ (expt 2 61) (fx- (expt 2 61) 1)))  ;; ****** only on 64-bit architectures *****
       (set! *most-negative-fixnum* (fx+ (fx* -1 (expt 2 61)) (fx* -1 (expt 2 61))))];; ***** likewise *****
      [else 
       ;; For the cluster platform use the following:
       (set! *most-positive-fixnum* (fx+ (expt 2 29) (fx- (expt 2 29) 1)))  ;; ****** only on our cluster, wcp *****
       (set! *most-negative-fixnum* (fx+ (fx* -1 (expt 2 29)) (fx* -1 (expt 2 29))))]);; ***** likewise *****



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
;; A* version of our search
;; expand just the current-fringe and remove duplicates in the expansion and repeats from prev-fringe
;; returning the new fringe
(define (expand-fringe-self pf cf depth)
  (let* (
        
         [prev-fringe-hash (read-fringe-from-file (filespec-fullpathname (car(fringe-segments pf))))];note, use fold for mutiple segments
         [current-fringe-hash (read-fringe-from-file (filespec-fullpathname (car(fringe-segments cf))))];assumes only one single filespec in list
        
         [new-cf-name (format "fringe-d~a" depth)]
         [new-cf-fullpath (format "~a~a" *share-store* new-cf-name)]
         ;[prntmsg (printf "finished reading the fringes~%")]
         
         
         [expand-them (for ([(key p-to-expand) (in-hash current-fringe-hash)])
                        (expand* p-to-expand))]
         [exp-ptr (hash-count *expansion-hash*)]
         
         
         [new-hash (for/hash ([(k v) (in-hash *expansion-hash*)] 
                              #:unless (or (hash-ref prev-fringe-hash k (lambda () #f ))
                                           (hash-ref current-fringe-hash k (lambda () #f ))))
                    (values k v) 
                     )]
         [new-pcount (hash-count new-hash)]
         )
    
    ;(printf "Finished the work packet generating a set of ~a positions~%" (set-count res))
    ;(printf "Num in exp-hash = ~a ~%" exp-ptr)
    ;(printf "Num after duplicates removed = ~a ~%" new-pcount)
    ;(for ([p (hash-values *expansion-hash*)])
     ;(printf "exp-hash-pos: ~a~%" (hc-position-bs p)))
    ;(for ([p (hash-values new-hash)])
     ;(printf "new-hash-pos: ~a~%" (hc-position-bs p)))
    
    ;(for ([sgmnt (fringe-segments pf)]) (delete-file (filespec-fullpathname sgmnt)))
    (write-fringe-to-disk new-hash new-cf-fullpath);Here and also above, we will save as hash tables.
    
    (hash-clear! *expansion-hash*)
    ;(hash-clear! new-hash)
    (make-fringe *share-store*
                 (list (make-filespec new-cf-name exp-ptr (file-size new-cf-fullpath) *share-store*)) new-pcount 
                 )))



;;----------------------------------------------------------------------------------------
;; DISTRIBUTED EXPANSION AND MERGING OF FRINGES

;; a sampling-stat is a (vector int fixnum fixnum (vectorof fixnum) boolean string (vectorof int) int int real real)
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
;; 9. cumulative sort-time from partial-expansion files phase1
;; 10. cumulative write-time from partial-expansion files phase1
;; 11. other-expand-time (i.e., non-sort and write, or basically the successor-generation)

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
;; create the pairs of indices into the current-fringe-vector that will specify the part of the fringe each worker tackles
;; just use the fringe-segments
(define (make-simple-ranges lofspec)
  (let ([start-range 0])
    (for/list ([fs lofspec])
      (set! start-range (+ start-range (filespec-pcount fs)))
      (list (- start-range (filespec-pcount fs)) start-range))))



;; remove-dupes: fringe fringe (listof filespec) string int int float float float -> sampling-stat
;; Running in distributed worker processes:
;; Remove duplicate positions from the list of expand-fspec files (i.e., partial-expansion...),
;; for any positions that appear in multiple partial expansion files.
;; All of the partial-expansion files are sorted, so we can write the merged result to slices as we go.
;; Write the non-duplicate positions to a "proto-fringe-dXX-NN" file -- previously we wrote this in a *local-store* folder
;; and later delivered a copy to the working directory to share with all the other compute-nodes;
;; Try writing directly to the shared NFS drive as a way to spread out network traffic.  This will clean up file name in the return sampling-stat...
;; Accordingly, the sampling-stat return value has a filename pointing to the working directory.
(define (remove-dupes pf cf lo-expand-fspec ofile-name depth partial-exp-dupes part-sort-time part-write-time other-expand-time)
  ;; the ofile-name is just the file-name -- no *local-store* path where needed
  #|(printf "EXPAND PHASE 2 (REMOVE DUPLICATES) pf: ~a~%cf: ~a~%all of lo-expand-fspec: ~a~%ofile-name: ~a~%depth: ~a~%"
          pf cf lo-expand-fspec ofile-name depth);|#
  ;; EXPAND PHASE 2 (REMOVE DUPLICATES)
  (let* ([pffh (fh-from-fringe pf)];previous fringe fringe head
         [cffh (fh-from-fringe cf)];current fringe fringe head
         [n-pos-to-process (for/sum ([an-fspec lo-expand-fspec]) (filespec-pcount an-fspec))]
         [lo-effh (for/list ([an-fspec lo-expand-fspec]) (fh-from-filespec an-fspec))]
         
         [heap-o-fheads (let ([lheap (make-heap (lambda (fh1 fh2) (hcposition<? (fringehead-next fh1) (fringehead-next fh2))))])
                          (heap-add-all! lheap lo-effh)
                          lheap)]
         ; writing directly to NFS doesn't seem any slower than *local-store* and then copy -- that claim still needs verification
         ; write to one slice-ofile at a time, since everything is sorted 
         ;slice-ofile changed to a hash table
         [proto-slice-num 0]
         [slice-upper-bound (vector-ref *proto-fringe-slice-bounds* (add1 proto-slice-num))]
         [proto-slice-ofile (open-output-file (string-append *share-store* ofile-name "-" (~a proto-slice-num #:left-pad-string "0" #:width 3 #:align 'right)))]
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
                  partial-exp-dupes 
                  part-sort-time 
                  part-write-time
                  other-expand-time)]
         [last-pos-bs #"NoLastPos"]
         )
    
    
    ;; locally merge the pre-proto-fringes, removing duplicates from prev- and current-fringes
    (for ([an-fhead (in-heap/consume! heap-o-fheads)])
      (let ([efpos (fringehead-next an-fhead)])
        (unless ;; efpos is a duplicate
            (or (and (bytes=? (hc-position-bs efpos) last-pos-bs) ; duplicate from most recently written 
                     (vector-set! sample-stats 2 (add1 (vector-ref sample-stats 2))))
                (and
                  (not *late-duplicate-removal*)
                  (or (and (position-in-fhead? efpos pffh) (vector-set! sample-stats 1 (add1 (vector-ref sample-stats 1))))
                      (and (position-in-fhead? efpos cffh) (vector-set! sample-stats 1 (add1 (vector-ref sample-stats 1)))))))
          (set! last-pos-bs (hc-position-bs efpos))
          (do ([efpos-hc (hc-position-hc efpos)])
            
            
            ;; if efpos-hc is >= to the slice-upper-bound, advance the proto-slice-num/ofile/upper-bound until it is not
            ((< efpos-hc slice-upper-bound))
            (close-output-port proto-slice-ofile)
            (set! proto-slice-num (add1 proto-slice-num))
            (set! proto-slice-ofile (open-output-file (string-append *share-store* ofile-name "-" (~a proto-slice-num #:left-pad-string "0" #:width 3 #:align 'right))))
            (set! slice-upper-bound (vector-ref *proto-fringe-slice-bounds* (add1 proto-slice-num))))
          (write-bytes  (hc-position-bs efpos) proto-slice-ofile)
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
      (touch (string-append *share-store* ofile-name "-" (~a i #:left-pad-string "0" #:width 3 #:align 'right))))
    ;; complete the sampling-stat
    (vector-set! sample-stats 0 (for/sum ([i (vector-ref sample-stats 3)]) i))
    (vector-set! sample-stats 6 (for/vector ([i *num-proto-fringe-slices*]) 
                                  (file-size (format "~a~a-~a" *share-store* ofile-name (~a i #:left-pad-string "0" #:width 3 #:align 'right)))))
    ;; delete files that are no longer needed
    (for ([efspec lo-expand-fspec]) (delete-file (filespec-fullpathname efspec)))
    ;(unless (string=? *master-name* "localhost") (delete-fringe pf))
    ;(delete-file use-ofilename)
    ;;**** THIS STRIKES ME AS DANGEROUS: IF ONE PROCESS ON MULTI-CORE MACHINE FINISHES FIRST ....
    ;(when (file-exists? (string-append *local-store* (fspec-fname pfspec))) (delete-file (string-append *local-store* (fspec-fname pfspec))))
    sample-stats))



;; dump-partial-expansion: int string int (listof fspec) float float -> (values (listof fspec) int float float)
;; given the count of pending expanded positions to write, the out-file template, the out-file counter, and the previously written filespecs,
;; sort and write the specified number of positions to the appropriately opened new file,
;; returning two values: the list with the new filespec added and the number of duplicates eliminated at this phase
(define (dump-partial-expansion ofile-template ofile-counter ofiles prev-dupes sort-time0 write-time0)
  (let* ([hc-to-scrub 'hcpos-to-scrub]
         [f (format "~a~a" ofile-template ofile-counter)]
         [fullpath (string-append *local-store* f)]
         [this-batch 0]
         [sort-time 0]
         [write-time 0]
         ;for testing the hash table implementation in place of the expansion-space/buffer, convert back to a vector of hash codes and sort. 
         ;[expansion-space (build-vector(hash-count *expansion-hash*) (lambda (N) (hash-iterate-val *expansion-hash* N)))];currently extracts length from count of expansion-hash, use pcount?
         [expansion-space *expansion-hash*];this sets it as a list, we want a vector.
         )
         ;; scrub the last part of the vector with bogus positions
    #|
    (for ([i (in-range pcount (vector-length *expansion-space*))])
      (set! hc-to-scrub (vector-ref *expansion-space* i))
      (set-hc-position-hc! hc-to-scrub *most-positive-fixnum*)    ;; make vector-sort! put these at the very end, but if a positions has *most-positive-fixnum* ...
      (bytes-copy! (hc-position-bs hc-to-scrub) 0 #"~~IgnoreMe")) ;; #\~ (ASCII character 126) is greater than any of our positions
    |#
    ;; sort the vector
    (set! sort-time (current-milliseconds))
    ;(vector-sort! hcposition<? *expansion-space*)
    ;(vector-sort! expansion-space hcposition<? 0 pcount);;dont need to sort because our hash merging will take care of this
    (set! sort-time (- (current-milliseconds) sort-time))
    ;; write the first pcount positions to the file
    (set! write-time (current-milliseconds))
    (set! this-batch (write-fringe-to-disk expansion-space fullpath pcount #t))
    (set! write-time (- (current-milliseconds) write-time))
    (hash-clear! *expansion-hash*)
    ;; return the two values: augmented list of filespecs, and the incremented number of duplicates eliminated during writing
    (values (cons (make-filespec f this-batch (file-size fullpath) *local-store*)
                  ofiles)
            (+ prev-dupes (- pcount this-batch))
            (+ sort-time sort-time0)
            (+ write-time write-time0))))

;; remote-expand-part-fringe: (list int int) int fringe fringe int -> {sampling stat?}
;; given a pair of indices into the current-fringe that should be expanded by this process, a process-id,
;; and the prev- and current-fringes, and the depth ...
;; and the ranges of all the other segments,
;; expand the positions in the indices range, ignoring duplicates other than within the new fringe being constructed.
;; when expanding positions, write them to the correct hash table that contains its position within the pair of indices.
(define (remote-expand-part-fringe ipair ranges process-id pf cf depth)
  ;; prev-fringe spec points to default shared directory; current-fringe spec points to *local-store* folder
  ;(printf "remote-expand-part-fringe: starting with pf: ~a, and cf: ~a~%" pf cf)
  ;; EXPAND PHASE 1
  (let* ([expand-part-time (current-milliseconds)]
         [pre-ofiles empty]
         ;; *** Dynamically choose the size of the pre-proto-fringes to keep the number of files below 500 ***
         [start (first ipair)]
         [end (second ipair)]
         [assignment-count (- end start)]
         [my-slice-num (get-slice-num (floor(/ assignment-count 2)))] ;;looks at a number in the middle of the slice to get its number? 
         [vector-of-slice-ofiles (for/vector (i *n-processors*) (open-output-file (format "fringe-segment-d~a-~a-~a" depth i my-slice-num)))];(~a i #:left-pad-string "0" #:width 3 #:align 'right )?
         
         [expanded-phase1 1];; technically, not yet, but during initialization in pre-resv do loop
         ;; make the fringehead advanced to the right place
         [cffh (fh-from-fringe cf start)];;does check the first?
         [pffh (fh-from-fringe pf start)]
         [dupes-caught-here 0]
         [sort-time  0.0]
         [write-time 0.0]
         [cf-and-pf-hashtable (hash)]
         [merged-files-expansion-hash (hash)]
         [sample-stats 
          (vector 0 ; number of positions preserved for further merging
                  0 ; number of positions discarded because duplicate with prev- or current-fringes
                  0 ; number of positions discarded because duplicate with another partial-expansion
                  slice-counts
                  #f 
                  ofile-name ;; here, use the stem of the shared ofile-name 
                  0
                  n-pos-to-process
                  partial-exp-dupes 
                  part-sort-time 
                  part-write-time
                  other-expand-time)]
         
         ;[This-Nodes-list-of-output-files-to-other-nodes] 
         
         )
    ;; do the actual expansions
    (for ([i assignment-count]
          [cfpos (fringehead-next cffh)]
          [pfpos (fringehead-next pffh)]
         (expand* cfpos);;why does it strt with next
         [expansion-ptr (hash-count *expansion-hash*)]
         [vect-of-expansion (list-vector (hash-values *expansion-hash*))])
      
      (hash-set cf-and-pf-hashtable (hc-position-hc cfpos) (hc-position-bs cfpos))
      (hash-set cf-and-pf-hashtable (hc-position-hc pfpos) (hc-position-bs pfpos))
         
         ;;loops through expansion hash, writing each position to the correct hash table.
         (for ([j expansion-ptr];;loops through each item in expansion-hash
               [efpos (vector-ref vect-of-expansion j)]
              [slice-index (get-slice-num efpos)]) ;;index for which bin to place position
           (write-bytes  (hc-position-bs efpos) (vector-ref vector-of-slice-ofiles slice-index)))
         
         (hash-clear *expansion-hash*);;delete the positions that we put into place.
          
      
      (advance-fhead! cffh)
      
      (set! expanded-phase1 (add1 expanded-phase1)))
    
    
    #|(printf "remote-exp-part-fringe: PHASE 1: expanding ~a positions of assigned ~a~%" 
            expanded-phase1 assignment-count);|#
    
    (when (< expanded-phase1 assignment-count)
      (error 'remote-expand-part-fringe
             (format "only expanded ~a of the assigned ~a (~a-~a) positions" expanded-phase1 assignment-count start end)))
    
    (close-input-port (fringehead-iprt cffh))
    
    
    
    (for (i *n-processors*) (close-output-port (vector-ref vector-of-flice-ofiles i)))
    ;;open files of expansions that belong to this fringe. TRANSPOSE Point. switch my-slice and index.
    (set vector-of-slice-ofiles (for/vector (i *n-processors*) (open-output-file (format "fringe-segment-d~a-~a-~a" depth my-slice-num i ))));(~a i #:left-pad-string "0" #:width 3 #:align 'right )?
        
    
    ;; PHASE 2: now pass through the proto-fringes generated from other files, and merge them into a hash table
    (for ([i *n-processors*]
          [open-file (vector-ref vector-of-slice-ofiles i)]
          [efpos 0]
          [cur-file-fh (fh-from-fringe open-file 0)]
          ;;pointer to front of open-file fh?
        )
       ;;for eachposition in the file. // do until fh pointer is null?
      (set efpos (fringehead-next cur-file-fh))
      (cond;;and late-duplicate-removal?
        [(false? (hash-ref cf-and-pf-hashtable (hc-position-hc efpos) #f)) (hash-set merged-files-expansion-hash (hc-position-hc efpos) (hc-position-bs efpos)) (vector-set! sample-stats 1 (add1 (vector-ref sample-stats 0)))]
        [else (vector-set! sample-stats 1 (add1 (vector-ref sample-stats 1)))])
      (when (is-goal? efpos) (vector-set! sample-stats 4 (hc-position-bs efpos))))
    
    sample-stats))
    
    ;;return sampling stat?
    
    
  



;; remote-expand-fringe: (listof (list fixnum fixnum)) fringe fringe int -> (listof sampling-stat)
;; trigger the distributed expansion according to the given ranges
;; In theory, it shouldn't matter where the files pointed to by the fringe are located,
;; but we expect they will point to a *local-store* copy of the current-fringe,
;; where the copy is arranged-for by the master also
(define (remote-expand-fringe ranges pf cf depth)
  ;;(printf "remote-expand-fringe: current-fringe of ~a split as: ~a~%" cur-fringe-size (map (lambda (pr) (- (second pr) (first pr))) ranges))
  (let* ([distrib-results (for/work ([range-pair ranges]
                                     [i (in-range (length ranges))])                                    (when (> depth *max-depth*) (error 'distributed-expand-fringe "ran off end")) ;;prevent riot cache-failure
                                    ;; need alternate version ofwait-for-files that just checks on the assigned range
                                    ;; but for now, just append the fspecs
                                    #| push the wait into where we're trying to access positions 
                                    (wait-for-files (append (map (lambda (seg) (segment-fspec seg)) pf-findex)
                                                            (map (lambda (seg) (segment-fspec seg)) cf-findex)) #t)|#
                                    (remote-expand-part-fringe range-pair ranges i pf cf depth)
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
                                

;; distributed-merge-proto-fringe-slices: (vectorof fspec) int string fringe fringe int -> (list string number)
;; given a list of filespecs pointing to the slices assigend to this worker and needing to be merged, copy the slices
;; and merge into a single segment that will participate in the new fringe, removing duplicates among slices.
;; Note: we have already removed from slices any duplicates found in prev- and current-fringes
(define (distributed-merge-proto-fringe-slices slice-fspecs depth ofile-name pf cf which-slice)
  ;(define (remote-merge-proto-fringes my-range expand-files-specs depth ofile-name)
  ;; expand-files-specs are of pattern: "proto-fringe-dXX-NN" for depth XX and proc-id NN, pointing to working (shared) directory 
  ;; ofile-name is of pattern: "fringe-segment-dX-NNN", where the X is the depth and the NN is a slice identifier
  (let* ([mrg-segment-oport (open-output-file (format "~a~a" *share-store* ofile-name) #:exists 'replace)] ; try writing directly to NFS
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
         [pffh (fh-from-fringe pf (if (= (length (fringe-segments pf)) 1)
                                      0
                                      (for/sum ([slice-num which-slice]
                                                [fspec (fringe-segments pf)])
                                        (filespec-pcount fspec))))]
         [cffh (fh-from-fringe cf (if (= (length (fringe-segments pf)) 1)
                                      0
                                      (for/sum ([slice-num which-slice]
                                                [fspec (fringe-segments cf)])
                                        (filespec-pcount fspec))))]
         ;[pmsg3 (printf "distmerge-debug3: made the heap with ~a frigeheads in it~%" (heap-count heap-o-fheads))]
         ;****** log duplicate eliminations here
         [segment-size (let ([last-pos (make-hcpos (bytes 49 49 49 49))]
                             [keep-pos (void)]
                             [num-written 0])
                         (for ([an-fhead (in-heap/consume! heap-o-fheads)])
                           (set! keep-pos (fringehead-next an-fhead))
                           (unless (or (and (bytes=? (hc-position-bs keep-pos) (hc-position-bs last-pos))) ;; don't write duplicates
                                       (and
                                         *late-duplicate-removal*
                                         (or (position-in-fhead? keep-pos pffh)
                                             (position-in-fhead? keep-pos cffh))))
                             (write-bytes (hc-position-bs keep-pos) mrg-segment-oport)
                             (set! num-written (add1 num-written))
                             (set! last-pos keep-pos))
                           (advance-fhead! an-fhead)
                           (unless (fhdone? an-fhead) ;;(eof-object? (peek-byte (fringehead-iprt an-fhead) 1))
                             (heap-add! heap-o-fheads an-fhead))
                           )
                         num-written)])
    (close-output-port mrg-segment-oport)
    (for ([fhead to-merge-fheads]) (close-input-port (fringehead-iprt fhead)))
    (unless (or #t 
                (string=? *master-name* "localhost"))
      (for ([fspc local-protofringe-fspecs]) 
        (delete-file (filespec-fullpathname fspc)))) ;remove the local expansions *** but revisit when we reduce work packet size for load balancing
    (list ofile-name segment-size)))

;; remote-merge: (vectorof (vectorof fspec)) int fringe fringe -> (listof string int)
;; merge the proto-fringes from the workes and remove duplicate positions appearing in either prev- or current-fringes at the same time.
;; expand-files-specs (proto-fringe-specs) is vector of vector of filespecs, the top-level has one for each slice,
;; each one containing as many proto-fringes as expanders, all of which need to be merged
(define (remote-merge expand-files-specs depth pf cf)
  ;;**** RETHINK THIS -- MAYBE FORCE THE WORKER TO GRAB THE SLICE IT NEEDS??????
  #|(when (string=? *master-name* "localhost")
    (for ([efs expand-files-specs]) (bring-local-partial-expansions efs)))|#
  ;(printf "remote-merge: n-protof-slices=~a, and length expand-files-specs=~a~%" *num-proto-fringe-slices* (vector-length expand-files-specs))
  (let ([merge-results
         (for/work ([i *num-proto-fringe-slices*]
                    [expand-fspecs-slice expand-files-specs])
                   (when (> depth *max-depth*) (error 'distributed-expand-fringe "ran off end")) ;finesse Riot caching
                   (let* ([ofile-name (format "fringe-segment-d~a-~a" depth (~a i #:left-pad-string "0" #:width 3 #:align 'right))]
                          [merged-fname-and-resp-rng-size (distributed-merge-proto-fringe-slices expand-fspecs-slice depth ofile-name pf cf i)]
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
         [sampling-stats (remote-expand-fringe ranges pf cf depth)]
         [end-expand (current-seconds)]
         ;; -----------------------------------------------------------------
         [check-for-goal (for/first ([ss sampling-stats]
                                     #:when (vector-ref ss 4))
                           (set-found-goal (vector-ref ss 4)))]
         ;; make filespecs for proto-fringe-dXX-NN slices the relevant data in the sampling-stats
         [proto-fringe-fspecs (for/vector ([i *num-proto-fringe-slices*]);; for each index to a slice...
                                ;; pull out the info from each sampling-stat
                                (for/vector ([ss sampling-stats]) 
                                  (make-filespec (string-append (vector-ref ss 5) "-" (~a i #:left-pad-string "0" #:width 3 #:align 'right)) ;; fname
                                                 (vector-ref (vector-ref ss 3) i) ;pcount
                                                 (vector-ref (vector-ref ss 6) i) ;file-size
                                                 *share-store*)))]
         ;; MERGE
         ;; --- Distribute the merging work ----------
         [sorted-segment-fspecs (remote-merge proto-fringe-fspecs depth pf cf)]
         [merge-end (current-seconds)]
         ;; -------------------------------------------
         ;; delete previous fringe now that duplicates have been removed
         [delete-previous-fringe (begin (delete-fringe pf)
                                        (when (string=? *master-name* "localhost") ; delete the *local-store* prev-fringe
                                          (delete-fringe pf *local-store*)))]
         [sorted-expansion-files (map first sorted-segment-fspecs)]
         [sef-lengths (map second sorted-segment-fspecs)]
         [new-cf-name (format "fringe-d~a" depth)]
         )
    ;; create the _new_ current-fringe
    #|
    (for ([f sorted-expansion-files])
      ;(printf "distributed-expand-fringe: concatenating ~a~%" f)
      (system (format "cat ~a >> fringe-d~a" f depth)))|#
    ;;--- delete files we don't need anymore ---------
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
    ;; report the cumulative node sort and write time during expansion phase1
    (printf "node-sort-write: ~a\t~a\t~a\t~a\t~a\t~a~%"
            depth
            (for/sum ([ss sampling-stats]) (vector-ref ss 9))  ; sum of worker sort-times
            (for/sum ([ss sampling-stats]) (vector-ref ss 10)) ; sum of worker write-times
            (length ranges)                                    ; number of workers for computing average
            (- end-expand start-expand)                        ; total elapsed expansion time for estimation of successor generation
            (for/sum ([ss sampling-stats]) (vector-ref ss 11)) ; time mainly for successor generation (non- sort and write)
            )
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
    (make-fringe *share-store*
                 (for/list ([segmentfile sorted-expansion-files]
                            [length sef-lengths])
                   (make-filespec segmentfile length (file-size (string-append *share-store* segmentfile)) *share-store*))
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


(define *max-depth* 10)(set! *max-depth* 110)

;; cfs-file: fringe fringe int -> position
;; perform a file-based cluster-fringe-search at given depth
;; using given previous and current fringes
(define (cfs-file prev-fringe current-fringe depth)
  (set! *depth-start-time* (current-seconds))
  (cond [(or (zero? (fringe-pcount current-fringe)) (> depth *max-depth*)) (printf "failed, nothing in current fringe")]
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
  (let ([d-1 (format "~afringe-d-1" *share-store*)]
        [d0 (format "~afringe-d0" *share-store*)])
    (for ([f (directory-list *share-store*)] #:unless (char=? #\. (string-ref (path->string f) 0))) 
      (delete-file (build-path *share-store* f))); actually should use pattern match to delete only fringe* or proto*
    (write-fringe-to-disk (hash) d-1)
    (write-fringe-to-disk (hash (hc-position-hc start-position) start-position) d0)
    (cfs-file (make-fringe *share-store* (list (make-filespec "fringe-d-1" 0 (file-size d-1) *share-store*)) 0)
              (make-fringe *share-store* (list (make-filespec "fringe-d0" 1 (file-size d0) *share-store*)) 1)
              1)))

;(block10-init)
(climb12-init)
;(climb15-init)
;(climbpro24-init)
;(compile-ms-array! *piece-types* *bh* *bw*)
(compile-spaceindex (format "~a~a-spaceindex.rkt" "stpconfigs/" *puzzle-name*))

;; canonicalize the *start* blank-configuration
(let* ([spacelist (bwrep->list (intify (hc-position-bs *start*) 0 4))]
       [cbref (rcpair->rcbyte (loc-to-cell (car spacelist)))]
       [canonical-spaces (apply canonize spacelist)])
  (bytes-set! (hc-position-bs *start*) 0 cbref)
  (bytes-copy! (hc-position-bs *start*) 1 canonical-spaces)
  (hc-position-bs *start*))

#|
(module+ main
  ;; Switch between these according to if using the cluster or testing on multi-core single machine
  (connect-to-riot-server! *master-name*)
  (define search-result (time (start-cluster-fringe-search *start*)))
  #|
  (define search-result (time (cfs-file (make-fringe-from-files "fringe-segment-d115-" 32 "fill-in-path-to-fringe-segments")
                                        (make-fringe-from-files "fringe-segment-d116-" 32 "fill-in-path-to-fringe-segments")
                                        117)))
  |#
  #|
  (define search-result (time (cfs-file (make-fringe-from-file "c12d59fringe" "fill-in-path-to-fringe-file")
                                        (make-fringe-from-file "c12d58fringe" "fill-in-path-to-fringe-file")
                                        1)))
  |#
  (print search-result)
  )
|#

(time (start-cluster-fringe-search *start*))
