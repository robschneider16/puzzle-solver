#lang racket

(require (planet gcr/riot))
(require (planet soegaard/gzip:2:2))
(require rnrs/sorting-6)
(require data/heap)
(require srfi/1)
(require racket/fixnum)
(require racket/set)
(require mzlib/string) ;; supposedly depricated but seems to need the require for 5.3.5
(require file/gzip)

(require "stp-init.rkt")
(require "stp-solve-base.rkt")
;(require profile)
;(instrumenting-enabled #t)
;(profiling-enabled #t)

(provide (all-defined-out))

(define *master-name* "the name of the host where the master process is running")
(set! *master-name* "localhost")
;(set! *master-name* "wcp")
;(define *n-processors* 31)
(define *n-processors* 4)
(define *expand-multiplier* 1)
(define *diy-threshold* 1000)
(define *pre-proto-fringe-size* 5000)

(define *most-positive-fixnum* 0)
(define *most-negative-fixnum* 0)
(cond [(fixnum? (expt 2 61))
       (set! *most-positive-fixnum* (fx+ (expt 2 61) (fx- (expt 2 61) 1)))  ;; ****** only on 64-bit architectures *****
       (set! *most-negative-fixnum* (fx+ (fx* -1 (expt 2 61)) (fx* -1 (expt 2 61))));; ***** likewise *****
       ]
      [else 
       ;; For the cluster platform use the following:
       (set! *most-positive-fixnum* (fx+ (expt 2 29) (fx- (expt 2 29) 1)))  ;; ****** only on our cluster, wcp *****
       (set! *most-negative-fixnum* (fx+ (fx* -1 (expt 2 29)) (fx* -1 (expt 2 29))));; ***** likewise *****
       ])

(define *found-goal* #f)


;; Cluster/multi-process specific code for the sliding-tile puzzle solver


;;----------------------------------------------------------------------------------------


;; expand-fringe-self: (setof position) (vectorof position) int -> fspec
;; expand just the portion of the sorted-fringe-vector specified by the indices in the given range-pair,
;; returning the number of positions in the newly expanded and merged fringe
;; ASSUME: current-fringe-vec is sorted.
(define (expand-fringe-self pf-spec cf-spec depth)
  (let* ([prev-fringe-set (list->set (read-fringe-from-gzfile (first pf-spec)))]
         [current-fringe-vec (vectorize-list (read-fringe-from-gzfile (first cf-spec)))]
         [new-cf-name (format "current-fringe-d~a.gz" depth)]
         [res (for/list ([p (for/fold ([expansions (set)])
                              ([p-to-expand current-fringe-vec])
                              (set-union expansions
                                         (expand p-to-expand)))]
                         #:unless (or (set-member? prev-fringe-set p)
                                      (position-in-vec? current-fringe-vec p)))
                (when (is-goal? p) (set! *found-goal* p))
                p)])
    #|(printf "Finished the work packet generating a set of ~a positions~%" (set-count res))
    (for ([p res])
      (printf "pos: ~a~%~a~%" (stringify p) p))|#
    (delete-file (first pf-spec))
    (rename-file-or-directory (first cf-spec) (format "prev-fringe-d~a.gz" depth) #t)
    (write-fringe-to-disk (sort res position<?) new-cf-name)
    (list new-cf-name (length res) (file-size new-cf-name))))

;;----------------------------------------------------------------------------------------
;; DISTRIBUTED EXPANSION AND MERGING OF FRINGES

;; a sampling-stat is a (vector int fixnum fixnum (listofof fixnum) boolean string int)
;; where the elements are:s
;; 0. total number of positions in the corresponding expansion
;; 1. minimum hash value of the positions
;; 2. maximum hash value of the positions
;; 3. [deprecated] list of sampled hash-codes
;; 4. boolean goal-found if found when expanding the assigned positions
;; 5. output file name, e.g., partial-expansionXX.gz
;; 6. compressed file size

;; -----------------------------------------------------------------------------------------------------
;; EXPANSION .....

;; make-vector-ranges: int -> (listof (list int int)
;; create the pairs of indices into the current-fringe-vector that will specify the part of the fringe each worker tackles
(define (make-vector-ranges vlength)
  (if (< vlength 10) ;;1000
      (list (list 0 vlength))
      (let ((start-list (build-list (* *expand-multiplier* *n-processors*)
                                    (lambda (i) (floor (* i (/ vlength (* *expand-multiplier* *n-processors*))))))))
        (foldr (lambda (x r) (cons (list x (first (first r))) r)) 
               (list (list (last start-list) vlength)) 
               (drop-right start-list 1)))))


;; remove-dupes: fspec fspec (listof fspec) string int -> sampling-stat
;; remove duplicate positions from the list of expand-fspec files, for any positions that also appear in the prev- or current-fringes.
;; write the non-duplicate positions to a file in the home directory
(define (remove-dupes pfspec cfspec lo-expand-fspec ofile-name depth)
  ;; EXPAND PHASE 2 (REMOVE DUPLICATES)
  (let* ([pffh (cond [(file-exists? (string-append "/tmp/" (car pfspec))) ;; moved to this from current-fringe during prev depth
                      (fh-from-fspec (cons (string-append "/tmp/" (car pfspec)) (cdr pfspec)))]
                     [else (copy-file (car pfspec) (string-append "/tmp/" (car pfspec)))
                           (fh-from-fspec (cons (string-append "/tmp/" (car pfspec)) (cdr pfspec)))])]
         [cffh (fh-from-fspec cfspec)]
         [lo-effh (for/list ([an-fspec lo-expand-fspec]) (fh-from-fspec an-fspec))]
         [heap-o-fheads (let ([lheap (make-heap (lambda (fh1 fh2) (position<? (fringehead-next fh1) (fringehead-next fh2))))])
                          (heap-add-all! lheap lo-effh)
                          lheap)]
         [expand-out (open-output-gz-file (string->path ofile-name) #:replace #t)]
         [unique-expansions 0]
         [sample-stats (vector 0 *most-positive-fixnum* *most-negative-fixnum* empty #f ofile-name 0)]
         )
    ;; locally merge the pre-proto-fringes, removing duplicates from prev- and current-fringes
    (for ([an-fhead (in-heap/consume! heap-o-fheads)])
      (let ([efpos (fringehead-next an-fhead)])
        (unless (or (position-in-fhead? efpos pffh)
                    (position-in-fhead? efpos cffh))
          (fprintf expand-out "~a~%" efpos)
          (when (is-goal? efpos) (vector-set! sample-stats 4 efpos))
          (vector-set! sample-stats 1 (fxmin (vector-ref sample-stats 1) (equal-hash-code efpos)))
          (vector-set! sample-stats 2 (fxmax (vector-ref sample-stats 2) (equal-hash-code efpos)))
          (set! unique-expansions (add1 unique-expansions))
          )
        (advance-fhead! an-fhead)
        (unless (fhdone? an-fhead) ;;(eof-object? (peek-byte (fringehead-iprt an-fhead) 1))
          (heap-add! heap-o-fheads an-fhead))))
    (vector-set! sample-stats 0 unique-expansions)
    (vector-set! sample-stats 6 (file-size ofile-name))
    ;(printf "remote-expand-part-fringe: HAVE EXPANSIONS:~%")
    ;(for ([p resv]) (displayln p))
    (for-each (lambda (fh) (close-input-port (fringehead-iprt fh))) (cons pffh (cons cffh lo-effh))) 
    (close-output-port expand-out)
    (for ([efspec lo-expand-fspec]) (delete-file (first efspec)))
    (when (file-exists? (string-append "/tmp/" (car pfspec))) (delete-file (string-append "/tmp/" (car pfspec))))
    (unless (file-exists? (format "/tmp/prev-fringe-d~a.gz" depth))
      (rename-file-or-directory (car cfspec) 
                                (format "/tmp/prev-fringe-d~a.gz" depth)))
    #|(printf "remove-dupes: starting w/ ~a positions, expansion has ~a/~a positions~%"
            (second expand-fspec) unique-expansions (position-count-in-file ofile-name))|#
    #|(unless (check-sorted-fringe? ofile-name)
      (error 'remove-dupes "phase 2: failed to generate a sorted fringe file ~a" ofile-name))|#
    sample-stats))


;; process-proto-fringe: (setof position) string string (listof fspec) -> (listof fspec)
;; convert set to vector, sort it and write the sorted vector to the given filename,
;; returning the file name to which the proto-fringe was written
(define (process-proto-fringe sop pre-ofile-template pre-ofile-counter pre-ofiles)
  (cons (let ([resv (for/vector #:length (set-count sop) ([p sop]) p)]
              [f (format "~a~a.gz" pre-ofile-template pre-ofile-counter)])
          (vector-sort! position<? resv)
          ;(set! resv (vector-sort position<? resv))
          (write-fringe-to-disk resv f)
          (list f (vector-length resv) (file-size f)))
        pre-ofiles))
                        

;; remote-expand-part-fringe: (list int int) int fspec fspec int -> sampling-stat
;; given a pair of indices into the current-fringe that should be expanded by this process, a process-id,
;; and the fspecs for the prev- and current-fringes, ...
;; expand the positions in the indices range, ignoring duplicates other than within the new fringe being constructed.
;; While building the expansion, maintain stats on hash-code values and a list of sampled hash-code values.
;; Return a pair containing the stats from this expansion and the SORTED expansion itself.
(define (remote-expand-part-fringe ipair process-id pf-spec cf-spec depth)
  ;; EXPAND PHASE 1
  (let* ([pre-ofile-template (format "/tmp/partial-expansion~a" (~a process-id #:left-pad-string "0" #:width 2 #:align 'right))]
         [pre-ofile-counter 0]
         [pre-ofiles empty]
         
         [start (first ipair)]
         [end (second ipair)]
         [assignment-count (- end start)]
         [expanded-phase1 1];; technically, not yet, but during initialization in pre-resv do loop
         [cffh (do ([i 0 (add1 i)]
                    [fh (fh-from-fspec cf-spec)])
                 ((>= i start) fh)
                 (advance-fhead! fh))])
    ;; do the actual expansions
    (do ([i 1 (add1 i)]
         [expansions (expand (fringehead-next cffh)) (set-union expansions (expand (fringehead-next cffh)))])
      ((>= i assignment-count)
       (set! pre-ofiles
             (process-proto-fringe expansions pre-ofile-template pre-ofile-counter pre-ofiles)))
      (when (fhdone? cffh) 
        (error 'remote-expand-part-fringe (format "hit end of cur-fringe after ~a of ~a expansions" expanded-phase1 assignment-count)))
      (when (> (set-count expansions) *pre-proto-fringe-size*)
        (set! pre-ofiles
              (process-proto-fringe expansions pre-ofile-template pre-ofile-counter pre-ofiles))
        (set! pre-ofile-counter (add1 pre-ofile-counter))
        (set! expansions (set)))
      (advance-fhead! cffh)
      (set! expanded-phase1 (add1 expanded-phase1)))
    #|(printf "remote-exp-part-fring: PHASE 1: expanding ~a positions yielding ~a/~a positions~%" 
            expanded-phase1 (vector-length pre-resv) (position-count-in-file pre-ofile-name))|#
    (when (< expanded-phase1 assignment-count)
      (error 'remote-expand-part-fringe
             (format "only expanded ~a of the assigned ~a (~a-~a) positions" expanded-phase1 assignment-count start end)))
    (close-input-port (fringehead-iprt cffh))
    ;; PHASE 2: now pass through the expansion file as well as prev-fringe and current-fringe to remove duplicates
    (remove-dupes pf-spec cf-spec pre-ofiles (string-append (substring pre-ofile-template 5) ".gz") depth)))


;; remote-expand-fringe: (listof (list fixnum fixnum)) fspec fspec int -> (listof sampling-stat)
;; trigger the distributed expansion according to the given ranges
;; The master should provide a cf-spec that points to a "/tmp" copy of the current-fringe, where the copy is provided by master also
(define (remote-expand-fringe ranges pf-spec cf-spec depth)
  ;;(printf "remote-expand-fringe: current-fringe of ~a split as: ~a~%" cur-fringe-size (map (lambda (pr) (- (second pr) (first pr))) ranges))
  (let* ([distrib-results (for/work ([range-pair ranges]
                                     [i (in-range (length ranges))])
                                    (when (> depth *max-depth*) (error 'distributed-expand-fringe "ran off end")) ;;prevent riot cache-failure
                                    (wait-for-files (list pf-spec cf-spec) #t)
                                    #|(cond [(file-exists? (format "/tmp/current-fringe-d~a.gz" (sub1 depth)))
                                           (rename-file-or-directory (format "/tmp/current-fringe-d~a.gz" (sub1 depth))
                                                                     (format "/tmp/prev-fringe-d~a.gz" (sub1 depth)) #t)]
                                          [else (copy-file (format "current-fringe-d~a.gz" (sub1 depth))
                                                           (format "/tmp/prev-fringe-d~a.gz" (sub1 depth)))])
                                    (copy-file (format "current-fringe-d~a.gz" (sub1 depth))
                                               (format "/tmp/current-fringe-d~a.gz" (sub1 depth)))|#
                                      
                                    #|(printf "rem-exp-frng_for/work: about to call remote-expand-part-fringe w/ pf=~a and cf=~a~%"
                                            (cons (string-append "/tmp/" (first pf-spec)) (cdr pf-spec))
                                            (cons (string-append "/tmp/" (first cf-spec)) (cdr cf-spec)))|#
                                    #|(remote-expand-part-fringe range-pair i 
                                                               (cons (string-append "/tmp/" (first pf-spec)) (cdr pf-spec))
                                                               (cons (string-append "/tmp/" (first cf-spec)) (cdr cf-spec)))|#
                                    (remote-expand-part-fringe range-pair i pf-spec cf-spec depth))])
    ;(printf "remote-expand-fringe: respective expansion counts: ~a~%" (map (lambda (ssv) (vector-ref ssv 0)) distrib-results))
    distrib-results))


;; ------------------------------------------------------------------------------------------
;; MERGING .....

;; merge-expansions: (list int int) (listof fspec) int string -> number
;; given a specification of a range of _position-hash-codes_ to consider,
;; a list of specs (filename, pos-count, file-size), and the counts on prev and current fringes,
;; go through all the partial-expansions and merge the positions (removing duplicates) in that range into a single collection
(define (remote-merge-expansions my-range expand-files-specs depth ofile-name)
  (let* (;[prev-fringe-fh (fh-from-fspec pf-spec)]
         ;[current-fringe-fh (fh-from-fspec cf-spec)]
         [mrg-segment-oport (open-output-gz-file ofile-name)]
         [to-merge-ports 
          (for/list ([i (in-range (length expand-files-specs))])
            (open-input-gz-file (string->path (format "partial-expansion~a.gz" (~a i #:left-pad-string "0" #:width 2 #:align 'right)))))]
         [fastforwarded-fheads
          (filter-not (lambda (fh) (eof-object? (fringehead-next fh)))
                      (for/list ([iprt to-merge-ports]
                                 [partial-expansion-size (map second expand-files-specs)])
                        (let* ([fhead (fringehead (read iprt) iprt 1 partial-expansion-size)]
                               [fhpos (fringehead-next fhead)])
                          (cond [(or (eof-object? fhpos)
                                     (fx>= (equal-hash-code fhpos) (first my-range)))
                                 fhead]
                                [else (do ([lfhpos fhpos (advance-fhead! fhead)])
                                        ((or (eof-object? lfhpos)
                                             (fx>= (equal-hash-code lfhpos) (first my-range)))
                                         fhead))]))))]
         [heap-o-fheads (let ([lheap (make-heap (lambda (fh1 fh2) (position<? (fringehead-next fh1) (fringehead-next fh2))))])
                          (heap-add-all! lheap fastforwarded-fheads)
                          lheap)]
         [segment-size
          (let ([last-pos (void)]
                [keep-pos (void)]
                [num-written 0])
           (for/list ([an-fhead (in-heap/consume! heap-o-fheads)]
                      #:break (fx>= (equal-hash-code (fringehead-next an-fhead)) (second my-range))
                      )
             (set! keep-pos (fringehead-next an-fhead))
             (advance-fhead! an-fhead)
             (unless (fhdone? an-fhead) ;;(eof-object? (peek-byte (fringehead-iprt an-fhead) 1))
               (heap-add! heap-o-fheads an-fhead))
             (unless (equal? keep-pos last-pos)
               (fprintf mrg-segment-oport "~a~%" keep-pos)
               (set! num-written (add1 num-written)))
             (set! last-pos keep-pos))
            num-written)])
    ;(printf "remote-merge-expansions: fw-lolop-lengths=~a [total ~a]~%" (map length fastforwarded-lolops) (for/sum ([l fastforwarded-lolops]) (length l)))
    ;(printf "remote-merge-expansions: merged-expns-length=~a~%" (length sorted-merged-expansions))
    ;;***error-check
    #|(unless (= (length sorted-merged-expansions) (set-count (list->set sorted-merged-expansions))) 
      (error 'remote-merge-expansions "list-merging vs. set mis-match"))|#
    ;(close-input-port (fringehead-iprt prev-fringe-fh))
    ;(close-input-port (fringehead-iprt current-fringe-fh))
    (close-output-port mrg-segment-oport)
    (for ([iprt to-merge-ports]) (close-input-port iprt))
    segment-size
    ))

;; remote-merge: (listof (list fixnum fixnum)) (listof int) int -> (listof string int)
(define (remote-merge merge-ranges expand-files-specs depth)
  (let ([merge-results
         (for/work ([merge-range merge-ranges] ;; merged results should come back in order of merge-ranges assignments
                    [i (in-range (length merge-ranges))])
                   (when (> depth *max-depth*) (error 'distributed-expand-fringe "ran off end"))
                   (let* ([ofile-name (format "partial-merge~a.gz" (~a i #:left-pad-string "0" #:width 2 #:align 'right))]
                          [merged-resp-rng-size (remote-merge-expansions merge-range expand-files-specs depth ofile-name)]
                          )
                     ;;(printf "distributed-expand-fringe: merge-range = ~a~%~a~%" merge-range merged-responsibility-range)
                     ;(write-fringe-to-disk merged-responsibility-range ofile-name)
                     (list ofile-name merged-resp-rng-size)))])
    ;(printf "remote-merge: merged segment names and lengths ~a~%" merge-results)
    merge-results))


;; simple-merge-ranges: (listof sampling-stats) -> (list merge-ranges)
;; just divide the span between the global min and max evenly among the processors
(define (simple-merge-ranges lo-sample-stat)
  (let* (;;[total-num-positions (foldl (lambda (ss sum) (+ (vector-ref ss 0) sum)) 0 lo-sample-stat)]
         [overall-min (foldl (lambda (ss tmin) (fxmin (vector-ref ss 1) tmin)) *most-positive-fixnum* lo-sample-stat)]
         [overall-max (foldl (lambda (ss tmax) (fxmax (vector-ref ss 2) tmax)) *most-negative-fixnum* lo-sample-stat)]
         [segment-width (fx+ (fxquotient overall-min (- *n-processors*)) (fxquotient overall-max *n-processors*))]
         [start-list (foldl (lambda (dinc res) (cons (list (fx- (car (car res)) dinc) (car (car res))) res))
                            (list (list (fx- overall-max segment-width)
                                        (if (fx= overall-max *most-positive-fixnum*)
                                            (error 'simple-merge-ranges "max hash-code is *most-positive-fixnum*")
                                            (fx+ 1 overall-max))))
                            (make-list (- *n-processors* 2) segment-width))]
         [final-list (cons (list overall-min (car (car start-list))) start-list)])
    ;(printf "simple-merge-ranges: generated ranges: ~a~%" final-list)
    final-list))

;; ------------------------------------------------------------------------------------------
;; DISTRIBUTED EXPAND AND MERGE

;; distributed-expand-fringe: fspec fspec int -> (list string int int)
;; Distributed version of expand-fringe
;; given prev and current fringe-specs and the present depth, expand and merge the current fringe,
;; returning the fringe-spec of the newly expanded and merged fringe.
(define (distributed-expand-fringe pf-spec cf-spec depth)
  #|(printf "distributed-expand-fringe: at depth ~a, expanding ~a (~a pos) w/ context ~a (~a pos)~%" 
          depth (first cf-spec) (second cf-spec) (first pf-spec) (second pf-spec))|#
  ;; push newest fringe (current-fringe) to all workers
  (if (string=? *master-name* "localhost")
      (copy-file (car cf-spec) (format "/tmp/~a" (car cf-spec)))
      (system (format "rocks run host compute 'scp wcp:puzzle-solver/~a /tmp'" (car cf-spec))))
  (let* (;; EXPAND
         [ranges (make-vector-ranges (second cf-spec))]
         [rcf-spec (cons (string-append "/tmp/" (car cf-spec)) (cdr cf-spec))]
         ;; --- Distribute the actual expansion work ------------------------
         [infomsg1 (printf "starting to distribute EXPAND work~%")]
         [expand-start (current-seconds)]
         [sampling-stats (remote-expand-fringe ranges pf-spec rcf-spec depth)]
         [expand-time-msg (printf "expand time: ~a~%" (~r (/ (- (current-seconds) expand-start) 60.0) #:precision 4))]
         ;; -----------------------------------------------------------------
         [check-for-goal (for/first ([ss sampling-stats]
                                     #:when (vector-ref ss 4))
                           (set! *found-goal* (vector-ref ss 4)))]
         ;; make list of triples: partial-expansion-name, number of positions, file-size
         [expand-files-specs (for/list ([ss sampling-stats])
                               #|(printf "exp-spec: ~a should have ~a postions: seeing ~a of expected ~a~%" 
                                       (vector-ref ss 5) (vector-ref ss 0) (file-size (vector-ref ss 5)) (vector-ref ss 6))|#
                               (list (vector-ref ss 5) (vector-ref ss 0) (vector-ref ss 6)))]
         ;; need to wait for write to complete -- i.e., all data to appear on master
         [wait-for-partial-expansions (wait-for-files expand-files-specs)]
         #|[error-check1 (for/first ([i (in-range (length sampling-stats))]
                                   [ss sampling-stats]
                                   #:unless (= (vector-ref ss 0) (position-count-in-file (format "partial-expansion~a" (~a i #:left-pad-string "0" #:width 2 #:align 'right)))))
                         (error 'distributed-expand-fringe (format "err-chk1: partial-expansion sizes do not match up for ~a which should be ~a" i (vector-ref ss 0))))]|#
         ;; MERGE
         ;;[merge-ranges (make-merge-ranges-from-expansions sampling-stats)]
         [merge-ranges (simple-merge-ranges sampling-stats)]
         ;; --- Distribute the merging work ----------
         [infomsg2 (printf "starting to distribute MERGE work~%")]
         [merge-start (current-seconds)]
         [sorted-expansion-files-lengths
          (let ([merged-expansion-files-lens (remote-merge merge-ranges expand-files-specs depth)]
                )
            #|(printf "distributed-expand-fringe: lengths = ~a [spread ~a or ~a%]~%" 
                    expan-lengths (- max-expan min-expan) variation-percent)|#
            ;;(error 'distributed-expand-fringe "stop to check partial files")
            merged-expansion-files-lens
            )]
         [merge-end (current-seconds)]
         [merge-time-msg (printf "merge time: ~a~%" (~r (/ (- merge-end merge-start) 60.0) #:precision 4))]
         ;; -------------------------------------------
         [sorted-expansion-files (map first sorted-expansion-files-lengths)]
         [sef-lengths (map second sorted-expansion-files-lengths)]
         #|[error-check2 (for/first ([f sorted-expansion-files]
                                   [len sef-lengths]
                                   #:unless (= len (position-count-in-file f)))
                         (error 'distributed-expand-fringe (format "err-chk2: partial-merges do not match up for ~a which should be ~a" f len)))]|#
         [new-cf-name (format "current-fringe-d~a.gz" depth)]
         )
    (rename-file-or-directory (first cf-spec)
                              (format "prev-fringe-d~a.gz" depth) #t) ;;(system "mv current-fringe prev-fringe")
    (for ([f sorted-expansion-files])
      ;(printf "distributed-expand-fringe: concatenating ~a~%" f)
      (system (format "zcat ~a >> current-fringe-d~a" f depth)))
    (gzip (format "current-fringe-d~a" depth))
    #|(unless (check-sorted-fringe? new-cf-name)
      (error 'distributed-expand-fringe "concatenated merge files do not make a sorted fringe"))|#
    (delete-file (first pf-spec))
    (delete-file (format "current-fringe-d~a" depth))
    (system "rm partial-expansion* partial-merge*")
    (printf "distributed-expand-fringe: file manipulation ~a, and total at depth ~a: ~a~%" 
            (~r (/ (- (current-seconds) merge-end) 60.0) #:precision 4) depth (~r (/ (- (current-seconds) expand-start) 60.0) #:precision 4))
    (list new-cf-name (foldl + 0 sef-lengths) (file-size new-cf-name))))


;;----------------------------------------------------------------------------------------

;; expand-fringe: int (list string int int) int -> (list string int int)
;; Given the size of the current fringe to expand, and the current depth of search,
;; do the expansions and merges as appropriate, returning the fringe-spec of the new fringe
(define (expand-fringe pf-spec cf-spec depth)
  (if (< (second cf-spec) *diy-threshold*)
      ;; do it myself
      (expand-fringe-self pf-spec cf-spec depth)
      
      ;; else call distributed-expand, which will farm out to workers
      (distributed-expand-fringe pf-spec cf-spec depth)))

;; vectorize-set: (setof position) -> (vectorof position)
;; convert a set of positions into a vector for easy/efficient partitioning
(define (vectorize-set f)
  (let ([new-vec (for/vector #:length (set-count f) ([p f]) p)])
    (vector-sort! position<? new-vec)
    new-vec))
(define (vectorize-list f)
  (let ([new-vec (list->vector f)])
    (vector-sort! position<? new-vec)
    new-vec))

;; fringe-merge: (listof position) (listof position) -> (listof X)
;; ASSUMES both lists are sorted and creates a new list with duplicates removed
(define (fringe-merge l1 l2)
  (cond [(empty? l1) l2]
        [(empty? l2) l1]
        [(position<? (first l1) (first l2)) (cons (first l1) (fringe-merge (rest l1) l2))]
        [(equal? (first l1) (first l2)) (fringe-merge l1 (rest l2))]
        [else (cons (first l2) (fringe-merge l1 (rest l2)))]))


(define *max-depth* 10)(set! *max-depth* 105)

;; cfs-file: fspec fspec int -> position
;; perform a file-based cluster-fringe-search at given depth
;; using previous and current fringes stored in corresponding fringe-specs 
(define (cfs-file pf-spec cf-spec depth)
  (cond [(or (zero? (second cf-spec)) (> depth *max-depth*)) #f]
        [*found-goal*
         (print "found goal")
         *found-goal*]
        [else (let ([new-fringe-spec (expand-fringe pf-spec cf-spec depth)])
                (printf "At depth ~a: current-fringe has ~a positions (and new-fringe ~a)~%" 
                        depth (second cf-spec) (second new-fringe-spec))
                ;;(for ([p current-fringe]) (displayln p))
                (cfs-file (cons (format "prev-fringe-d~a.gz" depth) (cdr cf-spec))
                          new-fringe-spec
                          (add1 depth)))]))

(define (start-cluster-fringe-search start-position)
  ;; initialization of fringe files
  (write-fringe-to-disk empty "prev-fringe-d0.gz")
  (write-fringe-to-disk (list start-position) "current-fringe-d0.gz")
  (cfs-file (list "prev-fringe-d0.gz" 0 (file-size "prev-fringe-d0.gz"))
            (list "current-fringe-d0.gz" 1 (file-size "current-fringe-d0.gz"))
            1))
  

;(block10-init)
(climb12-init)
;(climb15-init)
(compile-ms-array! *piece-types* *bh* *bw*)

;;#|
(module+ main
  ;; Switch between these according to if using the cluster or testing on multi-core single machine
  (connect-to-riot-server! *master-name*)
  (define search-result (time (start-cluster-fringe-search *start*)))
  (print search-result))
;;|#

;;(time (start-cluster-fringe-search *start*))
