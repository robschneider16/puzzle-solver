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
(define *first-time-switch-to-distributed* #t)

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
         [exp-ptr 0]
         
         
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
    (for ([sgmnt (fringe-segments pf)]) (delete-file (filespec-fullpathname sgmnt)))
    (set! exp-ptr (write-fringe-to-disk new-hash new-cf-fullpath));Here and also above, we will save as hash tables.
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
  ;;(printf "Start ~a end ~a count ~a~%" (first ipair) (second ipair) (- (second ipair) (first ipair) ))  
  (let* ([expand-part-time (current-milliseconds)]
         ;; *** Dynamically choose the size of the pre-proto-fringes to keep the number of files below 500 ***
         [start (first ipair)]
         [end (second ipair)]
         [assignment-count (- end start)]
         [vect-of-files (for/vector ([i *n-processors*]) 
                          (string-append *share-store* "proto-fringe-d" (number->string depth) "-"  
                                         (~a process-id #:left-pad-string "0" #:width 3 #:align 'right ) "-" 
                                         (~a i #:left-pad-string "0" #:width 3 #:align 'right)))]
         [my-slice-num process-id] ;;looks at a number in the middle of the slice to get its number? 
         [vector-of-slice-ofiles (for/vector ([i vect-of-files])(open-output-file i))]
         
         [expanded-phase1 0];; technically, not yet, but during initialization in pre-resv do loop
         ;; make the fringehead advanced to the right place
         [cffh (fh-from-fringe cf start)];;does check the first?
         [num-new-positions (vector 0 0 0 0)]
         [expansion-ptr 0]
         [expansion-time-end 0]
         [sample-stats 
          (vector ipair ;my ranges
                  0 ;number of positions written
                  0 ; num duplicares, but there are none detected here
                  *n-processors*
                  #f ;found goal?
                  vect-of-files;; here, use the stem of the shared ofile-name 
                  0;expansion time
                  ;;(vector)
                  )]
         
         ;[This-Nodes-list-of-output-files-to-other-nodes] 
         
         )
    ;; do the actual expansions
    (do ([i 1 (add1 i)]
         [cfpos (fringehead-next cffh) (fringehead-next cffh)])
      ((> i assignment-count))
      (expand* cfpos)
      ;;(when (>= expanded-phase1 (- assignment-count 10)) (printf "~a of ~a, size of temp-vector ~a ~%" expanded-phase1 assignment-count (hash-count *expansion-hash*)))
      (advance-fhead! cffh)
      ;(when (>= expanded-phase1 (- assignment-count 10)) (printf " after FHead! ~%"))
      
      ;;loops through expansion hash, writing each position to the correct hash table.
      (for ([efhcpos (hash-values *expansion-hash*)])
        (cond [(is-goal? efhcpos) (vector-set! sample-stats 4 efhcpos)];;change expand* to return a vector to save time here
              )
        (write-bytes (hc-position-bs efhcpos) (vector-ref vector-of-slice-ofiles (get-slice-num (hc-position-hc efhcpos))))
        (vector-set! sample-stats 1 (add1 (vector-ref sample-stats 1 )))
        ;;(vector-set! num-new-positions (get-slice-num (hc-position-hc efhcpos)) (add1 (vector-ref num-new-positions (get-slice-num (hc-position-hc efhcpos)))))
        (hash-clear! *expansion-hash*)
        (set! expanded-phase1 (add1 expanded-phase1)))
      )
    
    
    #|(printf "remote-exp-part-fringe: PHASE 1: expanding ~a positions of assigned ~a~%" 
            expanded-phase1 assignment-count);|#
    
    (when (< expanded-phase1 assignment-count)
      (error 'remote-expand-part-fringe
             (format "only expanded ~a of the assigned ~a (~a-~a) positions" expanded-phase1 assignment-count start end)))
    (close-input-port (fringehead-iprt cffh))
    (for ([i *n-processors*]) (close-output-port (vector-ref vector-of-slice-ofiles i)))
    ;;(vector-set! sample-stats 2 (for/sum ([i num-new-positions]) i))
    (set! expansion-time-end (current-milliseconds))
    (vector-set! sample-stats 6 (- expansion-time-end expand-part-time));
    ;(printf "number of positions written from my node = ~a~%" (vector-ref sample-stats 1))
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
                                                                                                        (remote-expand-part-fringe range-pair i pf cf depth);;hope that these get generated in call order and not finish.
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
(define (distributed-merge-proto-fringe-slices slice-files depth ofile-name pf cf which-slice range)
  ;(define (remote-merge-proto-fringes my-range expand-files-specs depth ofile-name)
  ;; expand-files-specs are of pattern: "proto-fringe-dXX-MS-DS" for depth XX and proc-id NN, pointing to working (shared) directory 
  ;; ofile-name is of pattern: "fringe-segment-dX-NNN", where the X is the depth and the NN is a slice identifier
  ;(printf "cf: ~a~% pf: ~a~%" cf pf)
  (let* ([mrg-segment-file (format "~a~a" *share-store* ofile-name)] ;#:exists 'replace ; try writing directly to NFS
         ;[local-protofringe-fspecs (for/list ([fs slice-fspecs] #:unless (zero? (filespec-pcount fs))) (rebase-filespec fs *local-store*))]
         ;[input-file-names slice-files];(for/vector ([k (vector-length slice-files)]) (format "~aproto-fringe-d~a-~a-~a" *share-store* depth
         ;                                                (~a k #:left-pad-string "0" #:width 3 #:align 'right)
         ;                                                (~a which-slice #:left-pad-string "0" #:width 3 #:align 'right)))]
         [num-to-merge (vector-length slice-files)]
         ;[pmsg1 (printf "distmerge-debug1: ~a fspecs in ~a~%distmerge-debug1: or localfspecs=~a~%" (vector-length slice-fspecs) slice-fspecs local-protofringe-fspecs)]
         ;******
         ;****** move the fringehead creation inside the heap-o-fhead construction in order to avoid the short-lived list allocation *******
         
         [pffile (if (= (length (fringe-segments pf)) 1)
                     (open-input-file (filespec-fullpathname (car (fringe-segments pf))))
                     (open-input-file (filespec-fullpathname (list-ref (fringe-segments pf) which-slice))))]
         
         [cffile (if (= (length (fringe-segments cf)) 1)
                     (open-input-file (filespec-fullpathname (car (fringe-segments cf))))
                     (open-input-file (filespec-fullpathname (list-ref (fringe-segments cf) which-slice))))]
         
         ;[f (printf "ranges in DistMerge ~a~%" range)]
         [output-hash (make-hash)]
         [num-positions-before-merge 0]
         [num-positions-after-merge 0]
         [num-positions-after-ddd 0]
         ;[iport (open-input-file (vector-ref slice-files 0))]
         [num-ddd-dupes 0]
         [num-merge-dupes 0])
    
    
    ;[pmsg3 (printf "distmerge-debug3: made the heap with ~a frigeheads in it~%" (heap-count heap-o-fheads))]
    ;****** log duplicate eliminations here
    
    ;(close-input-port iport)
    ;(printf "my pf start ~a end ~a~%" pfstart end);;responsability range
    ;(printf "my cf start ~a end ~a~%" cfstart end);;responsability range
    ;collapse files of the same destination(that being this slice) into a hash table
    ;(printf "num of files im merging together is ~a~%" num-to-merge)
    (for ([infile slice-files])
      (for ([hcpos (in-port read-bs->hcpos (open-input-file infile))]) 
        (hash-set! output-hash (hc-position-hc hcpos) hcpos)
        (set! num-positions-before-merge (add1 num-positions-before-merge))
        ))
    
    ;;error here... gets eof.
    ;(printf "num-positions in files before merge = ~a~%" num-positions-before-merge)
    (set! num-positions-after-merge (hash-count output-hash))
    ;(printf "num-positions after merging into a hash table = ~a~%" num-positions-after-merge)
    ;;remove duplicates from previous and current
    (for ([cfpos (in-port read-bs->hcpos cffile)])
      (hash-remove! output-hash (hc-position-hc cfpos)))
    (close-input-port cffile)
    
    (for ([pfpos (in-port read-bs->hcpos pffile)])
      (hash-remove! output-hash (hc-position-hc pfpos)))
    (close-input-port pffile)
    
    
    ;;delete all the old files
    (for ([i slice-files]) 
      (delete-file i))
    
    
    (set! num-positions-after-ddd (write-fringe-to-disk output-hash mrg-segment-file))
    (set! num-merge-dupes (- num-positions-before-merge num-positions-after-merge))
    (set! num-ddd-dupes (- num-positions-after-merge num-positions-after-ddd))
    (hash-clear! output-hash)
    ;(printf "num-positions after remove duplicates from cf and pf = ~a~%" num-positions-after-ddd)
    (vector ofile-name num-positions-after-merge num-positions-after-ddd num-merge-dupes num-ddd-dupes)))

;; remote-merge: (listof (filenames)) int fringe fringe -> (listof string int)
;; merge the proto-fringes from the workes and remove duplicate positions appearing in either prev- or current-fringes at the same time.
;; expand-files-specs (proto-fringe-specs) is vector of vector of filespecs, the top-level has one for each slice,
;; each one containing as many proto-fringes as expanders, all of which need to be merged
(define (remote-merge expand-filenames depth pf cf ranges)
  ;;**** RETHINK THIS -- MAYBE FORCE THE WORKER TO GRAB THE SLICE IT NEEDS??????
  #|(when (string=? *master-name* "localhost")
    (for ([efs expand-files-specs]) (bring-local-partial-expansions efs)))|#
  ;(printf "remote-merge: n-protof-slices=~a, and length expand-files-specs=~a~%" *num-proto-fringe-slices* (vector-length expand-files-specs))
  (let* ([merge-results
          (for/work ([i *n-processors*]
                     [my-slice-range ranges])
                    (when (> depth *max-depth*) (error 'distributed-expand-fringe "ran off end")) ;finesse Riot caching
                    (let* ([ofile-name (format "fringe-segment-d~a-~a" depth (~a i #:left-pad-string "0" #:width 3 #:align 'right))]
                           [merged-fname-and-resp-rng-size (distributed-merge-proto-fringe-slices (vector-ref expand-filenames i) depth ofile-name pf cf i my-slice-range)]
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
  ;;(printf "distributed-expand-fringe: at depth ~a, pf-spec: ~a; cf-spec: ~a~%" 
  ;;      depth pf-spec cf-spec)
  (let* (;; EXPAND
         [start-expand (current-seconds)]
         ;[ranges (make-vector-ranges (for/sum ([c cf]) (fringe-pcount c)))]
         [ranges (make-simple-ranges (fringe-segments cf))]
         ;; --- Distribute the actual expansion work ------------------------
         [sampling-stats (remote-expand-fringe ranges pf cf depth)]
         [end-expand (current-seconds)]
         ;global expand stat
         [num-pos-written (for/sum ([ss sampling-stats]) (vector-ref ss 1))]
         ;; -----------------------------------------------------------------
         [check-for-goal (for ([ss sampling-stats])
                           (cond [(not(false? (vector-ref ss 4))) (set-found-goal (vector-ref ss 4))]))]
         ;; make filespecs for proto-fringe-dXX-SN-DS slices the relevant data in the sampling-stats
         ;;transpose the files
         [vector-of-proto-fringe-files (for/vector ([i *n-processors*])
                                         (for/vector ([ss  sampling-stats])
                                           (vector-ref (vector-ref ss 5) i)
                                           ))]
         ;; MERGE
         ;; --- Distribute the merging work ----------
         [segment-fspecs (remote-merge vector-of-proto-fringe-files depth pf cf 
                                       (if (= (length ranges) *n-processors*)
                                           
                                           ranges
                                           (for/list ([i *n-processors*]) (car ranges))))]
         [merge-end (current-seconds)]
         
         ;; -------------------------------------------
         ;; delete previous fringe now that duplicates have been removed
         [delete-previous-fringe  (begin (delete-fringe pf)
                                         (when (string=? *master-name* "localhost") ;delete the *local-store* prev-fringe
                                           (delete-fringe pf *local-store*)))]
         ;;merge stats
         [expansion-files (for/list ([fs segment-fspecs]) (vector-ref fs 0))]
         [exp-lengths (for/list ([fs segment-fspecs]) (vector-ref fs 2))]
         [num-pos-after-merge (for/sum ([seg-fs segment-fspecs]) (vector-ref seg-fs 1))]
         [num-pos-after-ddd (for/sum ([seg-fs segment-fspecs]) (vector-ref seg-fs 2))]
         [num-merge-dupes (for/sum ([seg-fs segment-fspecs]) (vector-ref seg-fs 3))]
         [num-ddd-dupes (for/sum ([seg-fs segment-fspecs]) (vector-ref seg-fs 4))]
         ;new file format
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
    ;;(print segment-fspecs)
    (printf "expand-merge-times:~a\t~a\t~a\t~a~%"
            depth
            (- end-expand start-expand)         ;expansion
            (- merge-end end-expand)            ;merge
            (- (current-seconds) *depth-start-time*)) ;total
    ;; report the cumulative node sort and write time during expansion phase1
    (printf "node-expand-write: ~a\t~a\t~a\t~a~%"
            depth
            ;;(for/sum ([ss sampling-stats]) (vector-ref ss 9))  ; sum of worker sort-times ;;no more sorting
            (for/sum ([ss sampling-stats]) (vector-ref ss 6)) ; sum of worker expand and write times
            (length ranges)                                    ; number of workers for computing average
            (- end-expand start-expand)                        ; total elapsed expansion time for estimation of successor generation
            ;(for/sum ([ss sampling-stats]) (vector-ref ss 11)) ; time mainly for successor generation (non- sort and write)
            )
    ;; report the duplicate elimination data
    (printf "duplicate-elimination-data: ~a\t#p-written ~a\t#p-merge ~a\t#merge-dupes ~a\t#p-ddd ~a\t#ddd-dupes~a~%"
            depth
            num-pos-written ;number of positions written to file from expand phase
            num-pos-after-merge ;number of positions after merged proto-fringes 
            num-merge-dupes ;number of duplicates from merge
            num-pos-after-ddd 
            num-ddd-dupes)
    ;; make the new fringe to return
    (make-fringe *share-store*
                 (for/list ([segmentfile expansion-files]
                            [length exp-lengths])
                   (make-filespec segmentfile length (file-size (string-append *share-store* segmentfile)) *share-store*))
                 num-pos-after-ddd)
    ))


;;----------------------------------------------------------------------------------------

;; expand-fringe: vectOf-fringe vectOf-fringe int -> vectorOf-fringe
;; Given the prev- and current-fringes, and the current depth of search,
;; do the expansions and merges as appropriate, returning the new fringe
(define (expand-fringe prev-fringe current-fringe depth)
  (if (< (fringe-pcount current-fringe) *diy-threshold*)
      ;; do it myself
      (expand-fringe-self prev-fringe current-fringe depth)
      ;; else call distributed-expand, which will farm out to workers
      (distributed-expand-fringe prev-fringe current-fringe depth)))


(define *max-depth* 10)(set! *max-depth* 109)

;; cfs-file: vector0f-fringes vectorOf-fringe int -> position
;; perform a file-based cluster-fringe-search at given depth
;; using given previous and current fringes
(define (cfs-file prev-fringes current-fringes depth)
  (set! *depth-start-time* (current-seconds))
  (cond [(or (zero? (fringe-pcount current-fringes )) (> depth *max-depth*)) (printf "failed, nothing in current fringe")]
        [*found-goal*
         (print "found goal")
         *found-goal*]
        [else (let ([new-fringes (expand-fringe prev-fringes current-fringes depth)]
                    )
                ;;(print new-fringes)
                (printf "At depth ~a: current-fringe has ~a positions (and new-fringe ~a) in ~a (~a)~%" 
                        depth (fringe-pcount current-fringes) (fringe-pcount new-fringes) 
                        (- (current-seconds) *depth-start-time*) (seconds->time (- (current-seconds) *depth-start-time*)))
                (flush-output)
                ;;(for ([p current-fringe]) (displayln p))
                (cfs-file current-fringes ;; use current-fringe as prev-fringe at next level
                          new-fringes
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
