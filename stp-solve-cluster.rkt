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
         racket/port
         racket/bool
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

(define *diy-threshold* 12000) ;;**** this must be significantly less than EXPAND-SPACE-SIZE 

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

;; remote-expand-part-fringe: (list int int) int fringe fringe int -> {sampling stat?}
;; given a pair of indices into the current-fringe that should be expanded by this process, a process-id,
;; and the prev- and current-fringes, and the depth ...
;; and the ranges of all the other segments,
;; expand the positions in the indices range, ignoring duplicates other than within the new fringe being constructed.
;; when expanding positions, write them to the correct hash table that contains its position within the pair of indices.
(define (remote-expand-part-fringe ipair process-id pf cf depth)
  ;; prev-fringe spec points to default shared directory; current-fringe spec points to *local-store* folder
  ;(printf "remote-expand-part-fringe: starting with pf: ~a, and cf: ~a~%" pf cf)
  ;; EXPAND PHASE 1
  (printf "Start ~a end ~a count ~a~%" (first ipair) (second ipair) (- (second ipair) (first ipair) ))  
  (let* ([expand-part-time (current-milliseconds)]
         [pre-ofiles empty]
         ;; *** Dynamically choose the size of the pre-proto-fringes to keep the number of files below 500 ***
         [start (first ipair)]
         [end (second ipair)]
         [assignment-count (- end start)]
         [my-slice-num process-id] ;;looks at a number in the middle of the slice to get its number? 
         [vector-of-slice-ofiles 
          (for/vector ([i *n-processors*]) 
            (open-output-file (string-append *share-store* "proto-fringe-d" (number->string depth) "-"  
                                                                   (~a my-slice-num #:left-pad-string "0" #:width 3 #:align 'right ) "-" 
                                                                   (~a i #:left-pad-string "0" #:width 3 #:align 'right))))]
         
         [expanded-phase1 1];; technically, not yet, but during initialization in pre-resv do loop
         ;; make the fringehead advanced to the right place
         [cffh (fh-from-fringe cf start)];;does check the first?
         [dupes-caught-here 0]
         [expansion-ptr 0]
         [expansion-time-end 0]
         [sample-stats 
          (vector ipair ;my ranges
                  0 ;number of positions written
                  0 ; 
                  *n-processors*
                  #f ;found goal?
                  (string-append "proto-fringe-d" (number->string depth) "-"   
                                                                  );; here, use the stem of the shared ofile-name 
                  0;expansion time
                  )]
         
         ;[This-Nodes-list-of-output-files-to-other-nodes] 
         
         )
    ;; do the actual expansions
    (do ([cfpos (fringehead-next cffh) (fringehead-next cffh)])
      ((>= expanded-phase1 assignment-count))
      (expand* cfpos)
      (when (>= expanded-phase1 (- assignment-count 10)) (printf "~a of ~a, size of temp-vector ~a ~%" expanded-phase1 assignment-count (hash-count *expansion-hash*)))
      (advance-fhead! cffh)
      (when (>= expanded-phase1 (- assignment-count 10)) (printf " after FHead! ~%"))
      
      ;;loops through expansion hash, writing each position to the correct hash table.
      (for ([efhcpos (hash-values *expansion-hash*)]);;change expand* to return a vector to save time here
        (write-bytes (hc-position-bs efhcpos) (vector-ref vector-of-slice-ofiles (get-slice-num (hc-position-hc efhcpos)))))
      (hash-clear! *expansion-hash*)
      (set! expanded-phase1 (add1 expanded-phase1)))
    
    
    #|(printf "remote-exp-part-fringe: PHASE 1: expanding ~a positions of assigned ~a~%" 
            expanded-phase1 assignment-count);|#
    
    (when (< expanded-phase1 assignment-count)
      (error 'remote-expand-part-fringe
             (format "only expanded ~a of the assigned ~a (~a-~a) positions" expanded-phase1 assignment-count start end)))
    (close-input-port (fringehead-iprt cffh))
    (for ([i *n-processors*]) (close-output-port (vector-ref vector-of-slice-ofiles i)))
    (set! expansion-time-end (current-milliseconds))
    (vector-set! sample-stats 6 (- expansion-time-end expand-part-time)); 
   
    sample-stats))
    

    
  



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
                                

;; distributed-merge-proto-fringe-slices: (vectorof fspec) int string fringe fringe int -> (list string number)
;; given a list of filespecs pointing to the slices assigend to this worker and needing to be merged, copy the slices
;; and merge into a single segment that will participate in the new fringe, removing duplicates among slices.
;; Note: we have already removed from slices any duplicates found in prev- and current-fringes
(define (distributed-merge-proto-fringe-slices slice-fspecs depth ofile-name pf cf which-slice slice-range)
  ;(define (remote-merge-proto-fringes my-range expand-files-specs depth ofile-name)
  ;; expand-files-specs are of pattern: "proto-fringe-dXX-MS-DS" for depth XX and proc-id NN, pointing to working (shared) directory 
  ;; ofile-name is of pattern: "fringe-segment-dX-NNN", where the X is the depth and the NN is a slice identifier
  (let* ([mrg-segment-file (format "~a~a" *share-store* ofile-name)] ;#:exists 'replace ; try writing directly to NFS
         [start (first slice-range)]
         [end (second slice-range)]
         [assignment-count (- end start)]
         ;[local-protofringe-fspecs (for/list ([fs slice-fspecs] #:unless (zero? (filespec-pcount fs))) (rebase-filespec fs *local-store*))]
         [input-file-names (for/vector ([k *n-processors*]) (string-append slice-fspecs "-" 
                                                                           (k #:left-pad-string "0" #:width 3 #:align 'right) "-" 
                                                                           (which-slice #:left-pad-string "0" #:width 3 #:align 'right)))]
         ;[pmsg1 (printf "distmerge-debug1: ~a fspecs in ~a~%distmerge-debug1: or localfspecs=~a~%" (vector-length slice-fspecs) slice-fspecs local-protofringe-fspecs)]
         ;******
         ;****** move the fringehead creation inside the heap-o-fhead construction in order to avoid the short-lived list allocation *******
         [pf-cf-hash  (hash)]
         [pffh (fh-from-fringe cf start)]
         [cffh (fh-from-fringe pf start)]
         [output-hash (hash)]
         [iport 0]
         [num-new-positions 0]
         [num-dupes 0])
         ;[pmsg3 (printf "distmerge-debug3: made the heap with ~a frigeheads in it~%" (heap-count heap-o-fheads))]
         ;****** log duplicate eliminations here
         
    
    ;;construct pf-cf-hash table to ref when merging.
    (do ([i 1 (add1 i)]
         [cfpos (fringehead-next cffh) (fringehead-next cffh)]
         [pfpos (fringehead-next pffh) (fringehead-next pffh)])
      ((>= i assignment-count))
      (hash-set! pf-cf-hash (hc-position-hc cfpos) cfpos)
      (hash-set! pf-cf-hash (hc-position-hc pfpos) pfpos)
      (advance-fhead! cffh)
      (advance-fhead! pffh))
    
    
    ;collapse files of the same destination(that being this slice) into a hash table
    (for ([i *n-processors*]) 
      (let* ([iport (open-input-file (vector-ref input-file-names i))]) 
      (for ([hcpos (port->list read-bs->hcpos iport)]) 
        (cond 
          [(false? (hash-ref pf-cf-hash (values (hc-position-hc hcpos)) #f)) (hash-set! output-hash (values (hc-position-hc hcpos) hcpos))]
          [else (set! num-dupes (add1 num-dupes))]))) ;;check for duplicates using hash-ref in here to keep track of how many are removed. 
      (close-input-port iport))
    
    ;;delete all the olf files
    (for ([i *n-processors*]) 
      (delete-file (vector-ref input-file-names i)))
      
    (set! num-new-positions (write-fringe-to-disk output-hash mrg-segment-file))
    (hash-clear output-hash)
    (list mrg-segment-file num-new-positions num-dupes)))

;; remote-merge: (vectorof (vectorof fspec)) int fringe fringe -> (listof string int)
;; merge the proto-fringes from the workes and remove duplicate positions appearing in either prev- or current-fringes at the same time.
;; expand-files-specs (proto-fringe-specs) is vector of vector of filespecs, the top-level has one for each slice,
;; each one containing as many proto-fringes as expanders, all of which need to be merged
(define (remote-merge expand-files-specs depth pf cf ranges)
  ;;**** RETHINK THIS -- MAYBE FORCE THE WORKER TO GRAB THE SLICE IT NEEDS??????
  #|(when (string=? *master-name* "localhost")
    (for ([efs expand-files-specs]) (bring-local-partial-expansions efs)))|#
  ;(printf "remote-merge: n-protof-slices=~a, and length expand-files-specs=~a~%" *num-proto-fringe-slices* (vector-length expand-files-specs))
  (let ([merge-results
         (for/work ([i *num-proto-fringe-slices*]
                    [my-slice-range ranges] 
                    [expand-fspecs-slice expand-files-specs])
                   (when (> depth *max-depth*) (error 'distributed-expand-fringe "ran off end")) ;finesse Riot caching
                   (let* ([ofile-name (format "fringe-segment-d~a-~a" depth (~a i #:left-pad-string "0" #:width 3 #:align 'right))]
                          [merged-fname-and-resp-rng-size (distributed-merge-proto-fringe-slices expand-fspecs-slice depth ofile-name pf cf i my-slice-range)]
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
  (printf "distributed-expand-fringe: at depth ~a, pf-spec: ~a; cf-spec: ~a~%" 
          depth pf-spec cf-spec)
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
         ;; make filespecs for proto-fringe-dXX-SN-DS slices the relevant data in the sampling-stats
         [proto-fringe-file-form (vector-ref sampling-stats 5)] ;;(~a i #:left-pad-string "0" #:width 3 #:align 'right) ;; fname
                                                 
         ;; MERGE
         ;; --- Distribute the merging work ----------
         [sorted-segment-fspecs (remote-merge proto-fringe-file-form depth pf cf ranges)]
         [merge-end (current-seconds)]
         ;; -------------------------------------------
         ;; delete previous fringe now that duplicates have been removed
         [delete-previous-fringe (begin (delete-fringe pf)
                                        (when (string=? *master-name* "localhost") ;delete the *local-store* prev-fringe
                                          (delete-fringe pf *local-store*)))]
         [expansion-files (map first sorted-segment-fspecs)]
         [sef-lengths (map second sorted-segment-fspecs)]
         ;[num-duplicates (map third sorted-segment-fspecs)]
         [new-cf-name (format "fringe-d~a" depth)]
         )
    ;; create the _new_ current-fringe
    #|
    (for ([f sorted-expansion-files])
      ;(printf "distributed-expand-fringe: concatenating ~a~%" f)
      (system (format "cat ~a >> fringe-d~a" f depth)))|#
    ;;--- delete files we don't need anymore ---------          DO THIS IN MERGING AFTER ITS ALL MERGED
   ;; (for ([fspecs proto-fringe-fspecs])
     ;; (for ([fspec fspecs]) (delete-file (filespec-fullpathname fspec))))
    ;(system "rm partial-expansion* partial-merge*")
    ;(unless (string=? *master-name* "localhost") (delete-file (fspec-fname cf-spec)))
    ;; file-copy, expansion, merge, total
    (printf "expand-merge-times: ~a\t~a\t~a\t~a~%"
            depth
            (- end-expand start-expand)         ;expansion
            (- merge-end end-expand)            ;merge
            (- (current-seconds) *depth-start-time*)) ;total
    ;; report the cumulative node sort and write time during expansion phase1
    (printf "node-expand-write: ~a\t~a\t~a\t~a~%"
            depth
            ;;(for/sum ([ss sampling-stats]) (vector-ref ss 9))  ; sum of worker sort-times no more sorting
            (for/sum ([ss sampling-stats]) (vector-ref ss 6)) ; sum of worker expand and write times
            (length ranges)                                    ; number of workers for computing average
            (- end-expand start-expand)                        ; total elapsed expansion time for estimation of successor generation
            ;(for/sum ([ss sampling-stats]) (vector-ref ss 11)) ; time mainly for successor generation (non- sort and write)
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
                 (for/list ([segmentfile expansion-files]
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


(define *max-depth* 10)(set! *max-depth* 24)

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

;#|
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
;|#

;;(time (start-cluster-fringe-search *start*))
