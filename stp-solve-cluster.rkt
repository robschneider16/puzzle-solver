#lang racket

(require (planet gcr/riot))
(require (planet soegaard/gzip:2:2))
(require rnrs/sorting-6)
(require data/heap)
(require srfi/1)
(require racket/fixnum)
(require racket/set)
(require mzlib/string) ;; supposedly depricated but seems to need the require for 5.3.5

(require "stp-init.rkt")
(require "stp-solve-base.rkt")
;(require profile)
;(instrumenting-enabled #t)
;(profiling-enabled #t)

(provide (all-defined-out))

;(define *n-processors* 31)
(define *n-processors* 8)

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


;; expand-fringe-self: (setof position) (vectorof position) -> int
;; expand just the portion of the sorted-fringe-vector specified by the indices in the given range-pair,
;; returning the number of positions in the newly expanded and merged fringe
;; ASSUME: current-fringe-vec is sorted.
(define (expand-fringe-self prev-fringe-set current-fringe-vec)
  (let ((res (for/list ([p (for/fold ([expansions (set)])
                             ([p-to-expand current-fringe-vec])
                             (set-union expansions
                                        (expand p-to-expand)))]
                        #:unless (or (set-member? prev-fringe-set p)
                                     (position-in-vec? current-fringe-vec p)))
               (when (is-goal? p) (set! *found-goal* p))
               p)))
    #|(printf "Finished the work packet generating a set of ~a positions~%" (set-count res))
    (for ([p res])
      (printf "pos: ~a~%~a~%" (stringify p) p))|#
    (system "mv current-fringe.gz prev-fringe.gz")
    (write-fringe-to-disk (sort res position<?) "current-fringe.gz")
    (length res)))

;;----------------------------------------------------------------------------------------
;; DISTRIBUTED EXPANSION AND MERGING OF FRINGES

;; a sampling-stat is a (vector int int int (vectorof int)
;; where the elements are:s
;; 0. total number of positions in the corresponding expansion
;; 1. minimum hash value of the positions
;; 2. maximum hash value of the positions
;; 3. vector of bin-counts for the range min-to-max divided into *n-processors* ranges
;; 4. boolean goal-found if found when expanding the assigned positions
;; 5. output file name, e.g., partial-expansionXX.gz
;; 6. compressed file size

;; EXPANSION .....

;; make-vector-ranges: int -> (listof (list int int)
;; create the pairs of indices into the current-fringe-vector that will specify the part of the fringe each worker tackles
(define (make-vector-ranges vlength)
  (if (< vlength 10) ;;1000
      (list (list 0 vlength))
      (let ((start-list (build-list *n-processors* (lambda (i) (floor (* i (/ vlength *n-processors*)))))))
        (foldr (lambda (x r) (cons (list x (first (first r))) r)) 
               (list (list (last start-list) vlength)) 
               (drop-right start-list 1)))))

;; remote-expand-part-fringe: (list int int) int int -> sampling-stat
;; given a pair of indices into the current-fringe that should be expanded by this process, and a samping frequency,
;; expand the positions in the indices range, ignoring duplicates other than within the new fringe being constructed.
;; While building the expansion, maintain stats on hash-code values and a list of sampled hash-code values.
;; Return a pair containing the stats from this expansion and the SORTED expansion itself.
(define (remote-expand-part-fringe ipair sample-freq process-id current-fringe-size)
  (let* ([ofile-name (format "partial-expansion~a.gz" (~a process-id #:left-pad-string "0" #:width 2 #:align 'right))]
         [start (first ipair)]
         [end (second ipair)]
         [assignment-count (- end start)]
         [expanded 0]
         [sample-stats (vector 0 *most-positive-fixnum* *most-negative-fixnum* empty #f ofile-name 0)]
         [my-in (open-input-gz-file (string->path "current-fringe.gz"))]
         [my-in-seq (sequence-tail (in-port read my-in) start)]
         [resv (for/vector ([p (for/fold ([expansions (set)])
                                 ([pos-to-expand my-in-seq])
                                 ;; do the expansion of the indexed position, adding in any new positions found
                                 (set! expanded (add1 expanded))
                                 (set-union expansions (expand pos-to-expand))
                                 )])
                 (vector-set! sample-stats 0 (add1 (vector-ref sample-stats 0)))
                 (vector-set! sample-stats 1 (fxmin (vector-ref sample-stats 1) (equal-hash-code p)))
                 (vector-set! sample-stats 2 (fxmax (vector-ref sample-stats 2) (equal-hash-code p)))
                 (when (zero? (modulo (vector-ref sample-stats 0) sample-freq))
                   (vector-set! sample-stats 3 (cons (equal-hash-code p) (vector-ref sample-stats 3))))
                 (when (is-goal? p) (vector-set! sample-stats 4 p))
                 p)])
    (vector-sort! position<? resv)
    ;; resv should be sorted and have no duplicates w/in itself because it came from a set
    ;; write local-expansion to disk w/ naming convention "partial-expansion" + process-id + ".gz"
    (write-fringe-to-disk
     resv ;; this has just been sorted above
     ofile-name)
    #|
    (printf "remote-expand-part-fringe: starting w/ ~a positions, expansion has ~a/~a positions and sampled ~a hashcodes~%"
            assignment-count (vector-ref sample-stats 0) (vector-length resv) (length (vector-ref sample-stats 3)))
    (printf "remote-expand-part-fringe: HAVE EXPANSIONS:~%")
    (for ([p resv]) (displayln p))
    |#
    (when (< expanded assignment-count)
      (error 'remote-expand-part-fringe
             (format "only expanded ~a of the assigned ~a (~a-~a) positions" expanded assignment-count start end)))
    (close-input-port my-in)
    (vector-set! sample-stats 3 (sort (vector-ref sample-stats 3) fx<))
    (vector-set! sample-stats 6 (file-size (string->path ofile-name)))
    sample-stats))

;; remote-expand-fringe: (listof (list fixnum fixnum) int int -> (listof sampling-stat)
;; trigger the distributed expansion according to the given ranges
(define (remote-expand-fringe ranges cur-fringe-size depth)
  (printf "remote-expand-fringe: current-fringe of ~a split as: ~a~%" cur-fringe-size 
          (map (lambda (pr) (- (second pr) (first pr))) ranges))
  (for/work ([range-pair ranges]
             [i (in-range (length ranges))])
            (when (> depth *max-depth*) (error 'distributed-expand-fringe "ran off end"))
            (remote-expand-part-fringe range-pair 
                                       (max (floor (/ cur-fringe-size (* 100 *n-processors*))) 1)
                                       i cur-fringe-size)))


;; MERGING .....

;; merge-expansions: (list int int) (listof (list string int int) int int -> (listof position)
;; given a specification of a range of _position-hash-codes_ to consider,
;; a list of specs (filename, pos-count, file-size), and the counts on prev and current fringes,
;; go through all the partial-expansions and merge the positions (removing duplicates) in that range into a single collection
(define (remote-merge-expansions my-range expand-files-specs prev-fringe-count current-fringe-count)
  ;; as first step:
  ;; ...  need to read all the partial fringe-files int a local variable called lovo-positions
  (let* ([pf-port (open-input-gz-file (string->path "prev-fringe.gz"))]
         [cf-port (open-input-gz-file (string->path "current-fringe.gz"))]
         [prev-fringe-fh (fringehead (read pf-port) pf-port 0 prev-fringe-count)]
         [current-fringe-fh (fringehead (read cf-port) cf-port 0 current-fringe-count)]
         [to-merge-ports 
          (for/list ([i (in-range *n-processors*)])
            (open-input-gz-file (string->path (format "partial-expansion~a.gz" (~a i #:left-pad-string "0" #:width 2 #:align 'right)))))]
         [fastforwarded-fheads
          (filter-not (lambda (fh) (eof-object? (fringehead-next fh)))
                      (for/list ([iprt to-merge-ports]
                                 [partial-expansion-size (map second expand-files-specs)])
                        (let* ([fhead (fringehead (read iprt) iprt 0 partial-expansion-size)]
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
         [sorted-merged-expansions
          (sorted-remove-dups
           (for/list ([an-fhead (in-heap/consume! heap-o-fheads)]
                      #:break (fx>= (equal-hash-code (fringehead-next an-fhead)) (second my-range))
                      #:unless (and (or ;;(equal? (car lops) (car (heap-min heap-o-position-lists)))
                                     (position-in-fhead? (fringehead-next an-fhead) prev-fringe-fh)
                                     (position-in-fhead? (fringehead-next an-fhead) current-fringe-fh))
                                    (unless (eof-object? (peek-byte (fringehead-iprt an-fhead) 1))
                                      (advance-fhead! an-fhead)
                                      (heap-add! heap-o-fheads an-fhead))));;******???????
             (let ([keep-pos (fringehead-next an-fhead)])
               (unless (eof-object? (peek-byte (fringehead-iprt an-fhead) 1))
                 (advance-fhead! an-fhead)
                 (heap-add! heap-o-fheads an-fhead))
               keep-pos)))])
    ;(printf "remote-merge-expansions: fw-lolop-lengths=~a [total ~a]~%" (map length fastforwarded-lolops) (for/sum ([l fastforwarded-lolops]) (length l)))
    ;(printf "remote-merge-expansions: merged-expns-length=~a~%" (length sorted-merged-expansions))
    ;;***error-check
    #|(unless (= (length sorted-merged-expansions) (set-count (list->set sorted-merged-expansions))) 
      (error 'remote-merge-expansions "list-merging vs. set mis-match"))|#
    (close-input-port pf-port)
    (close-input-port cf-port)
    (for ([iprt to-merge-ports]) (close-input-port iprt))
    sorted-merged-expansions
    ))

;; remote-merge: (listof (list fixnum fixnum)) (listof int) int int int -> (listof string int)
(define (remote-merge merge-ranges expand-files-specs prev-fringe-size cur-fringe-size depth)
  (for/work ([merge-range merge-ranges] ;; merged results should come back in order of merge-ranges assignments
             [i (in-range (length merge-ranges))])
            (when (> depth *max-depth*) (error 'distributed-expand-fringe "ran off end"))
            (let* ([merged-responsibility-range (remote-merge-expansions merge-range expand-files-specs prev-fringe-size cur-fringe-size)]
                   [ofile-name (format "partial-merge~a.gz" (~a i #:left-pad-string "0" #:width 2 #:align 'right))] 
                   )
              ;;(printf "distributed-expand-fringe: merge-range = ~a~%~a~%" merge-range merged-responsibility-range)
              (write-fringe-to-disk merged-responsibility-range ofile-name)
              (list ofile-name (length merged-responsibility-range)))))

;; make-one-merge-range: int int (heapof (listof number)) -> (list int int)
;; consume the elements from the sorted lists in the heap until the target-num is reach, returning the range pair
(define (make-one-merge-range target-n start a-heap)
  (for ([i (in-range target-n)]
        #:break (and (= (heap-count a-heap) 1)
                     (= (length (heap-min a-heap)) 1)))
    (unless (empty? (cdr (heap-min a-heap))) (heap-add! a-heap (cdr (heap-min a-heap))))
    (heap-remove-min! a-heap))
  (list start (car (heap-min a-heap))))

;; make-merge-ranges-from-expansions: (listof sampling-stats) -> (list merge-ranges)
;; decide how to partition the space of positions as distributed over the expansions reflected in the sampling-stats
;;***** consider and compare this to simply dividing the hash-code range by *n-processors*
;;***** compare such an approach in terms of the uniformity of the distribution of positions assigned to each bin
(define (make-merge-ranges-from-expansions lo-sample-stat)
  ;;(printf "make-merge-ranges-from-expansions: sample-stats:~%~a" lo-sample-stat)
  (let* (;;[total-num-positions (foldl (lambda (ss sum) (+ (vector-ref ss 0) sum)) 0 lo-sample-stat)]
         [overall-min (foldl (lambda (ss tmin) (fxmin (vector-ref ss 1) tmin)) *most-positive-fixnum* lo-sample-stat)]
         [overall-max (foldl (lambda (ss tmax) (fxmax (vector-ref ss 2) tmax)) *most-negative-fixnum* lo-sample-stat)]
         [total-num-samples (foldl (lambda (ss sum) (+ (length (vector-ref ss 3)) sum)) 0 lo-sample-stat)]
         [heap-o-sample-hash-lists (let ([lheap (make-heap (lambda (l1 l2) (fx< (first l1) (first l2))))])
                                     (heap-add-all! lheap (map (lambda (ss) (sort (vector-ref ss 3) fx<)) lo-sample-stat))
                                     (heap-add! lheap (list overall-min))
                                     lheap)]
         [made-merge-ranges (append 
                             (for/list ([processor (in-range 1 *n-processors*)])
                               (make-one-merge-range (/ total-num-samples *n-processors*) (car (heap-min heap-o-sample-hash-lists)) heap-o-sample-hash-lists))
                             (list (list (car (heap-min heap-o-sample-hash-lists)) (add1 overall-max))))])
    ;;(printf "make-merge-ranges-from-expansions: returning ~a~%" made-merge-ranges)
    made-merge-ranges))


;; distributed-expand-fringe: int int int -> int
;; Distributed version of expand-fringe
;; given prev and current fringe sizes and present depth, expand and merge the current fringe,
;; returning the size of the newly expanded and merged fringe.
(define (distributed-expand-fringe prev-fringe-size cur-fringe-size depth)
  ;;(printf "distributed-expand-fringe: ~a nodes in prev and ~a in current fringes~%" (vector-length prev-fringe-vec) (vector-length current-fringe-vec))
  (let* (;; EXPAND
         [ranges (make-vector-ranges cur-fringe-size)]
         ;; --- Distribute the actual expansion work ------------------------
         [sampling-stats (remote-expand-fringe ranges cur-fringe-size depth)]
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
         [wait-for-partial-expansions (do ([exp-specs (filter fringe-file-not-ready? expand-files-specs)
                                                      (filter fringe-file-not-ready? exp-specs)]
                                           [sleep-time 0.1 (* sleep-time 2)])
                                        ((empty? exp-specs) 'ready)
                                        (printf "distributed-expand-fringe: waiting for partial expansions and sleeping ~a~%" sleep-time)
                                        (sleep sleep-time))]
         #|[error-check1 (for/first ([i (in-range (length sampling-stats))]
                                   [ss sampling-stats]
                                   #:unless (= (vector-ref ss 0) (position-count-in-file (format "partial-expansion~a" (~a i #:left-pad-string "0" #:width 2 #:align 'right)))))
                         (error 'distributed-expand-fringe (format "err-chk1: partial-expansion sizes do not match up for ~a which should be ~a" i (vector-ref ss 0))))]|#
         ;; MERGE
         [merge-ranges (make-merge-ranges-from-expansions sampling-stats)]
         ;; --- Distribute the merging work ----------
         [sorted-expansion-files-lengths
          (let* ([merged-expansion-files-lens (remote-merge merge-ranges expand-files-specs prev-fringe-size cur-fringe-size depth)]
                 #|[merged-expansion-files (map first merged-expansion-files-lens)]
                 [expan-lengths (for/list ([f merged-expansion-files])
                                  (do ([present #f])
                                    (present)
                                    (set! present (file-exists? f)))
                                  (position-count-in-file f))]
                 [min-expan (argmin identity expan-lengths)]
                 [max-expan (argmax identity expan-lengths)]
                 [variation-percent (/ (round (* 10000.0 (/ (- max-expan min-expan) (/ (for/sum ([i expan-lengths]) i) (length expan-lengths))))) 100.0)]|#
                 )
            #|(printf "distributed-expand-fringe: lengths = ~a [spread ~a or ~a%]~%" 
                    expan-lengths (- max-expan min-expan) variation-percent)|#
            ;;(error 'distributed-expand-fringe "stop to check partial files")
            merged-expansion-files-lens
            )]
         ;; -------------------------------------------
         [sorted-expansion-files (map first sorted-expansion-files-lengths)]
         [sef-lengths (map second sorted-expansion-files-lengths)]
         #|[error-check2 (for/first ([f sorted-expansion-files]
                                   [len sef-lengths]
                                   #:unless (= len (position-count-in-file f)))
                         (error 'distributed-expand-fringe (format "err-chk2: partial-merges do not match up for ~a which should be ~a" f len)))]|#
         )
    (rename-file-or-directory "current-fringe.gz" "prev-fringe.gz" #t) ;;(system "mv current-fringe prev-fringe")
    (for ([f sorted-expansion-files])
      (system (format "zcat ~a >> current-fringe" f)))
    (system "gzip current-fringe; rm partial-expansion* partial-merge*")
    (foldl + 0 sef-lengths)))


;;----------------------------------------------------------------------------------------

;; expand-fringe: int int int -> int
;; Given the size of the current fringe to expand, and the current depth of search,
;; do the expansions and merges as appropriate, returning the size of the new fringe
(define (expand-fringe prev-fringe-size current-fringe-size depth)
  (if (< current-fringe-size 1000)
      ;; do it myself
      (expand-fringe-self (list->set (read-fringe-from-gzfile "prev-fringe.gz"))
                          (vectorize-list (read-fringe-from-gzfile "current-fringe.gz")))
      ;; else call distributed-expand, which will farm out to workers
      (begin 
        #|(printf "calling distributed-exp... with ~a and ~a positions in prev and current fringes, respectively~%"
                  (set-count prev-fringe) (vector-length current-fringe-vec))|#
        (distributed-expand-fringe prev-fringe-size current-fringe-size depth))))

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


(define *max-depth* 10)(set! *max-depth* 61)

;; cluster-fringe-search: (setof position) (setof position) int -> position
;; perform a fringe BFS starting at the given state until depth is 0
(define (cluster-fringe-search prev-fringe-size current-fringe-size depth)
  (cond [(or (zero? current-fringe-size) (> depth *max-depth*)) #f]
        [*found-goal*
         (print "found goal")
         *found-goal*]
        [else (let ([new-fringe-size (expand-fringe prev-fringe-size current-fringe-size depth)])
                (printf "At depth ~a: current-fringe has ~a positions (and new-fringe ~a)~%" 
                        depth current-fringe-size new-fringe-size)
                ;;(for ([p current-fringe]) (displayln p))
                (cluster-fringe-search current-fringe-size new-fringe-size (add1 depth)))]))

(define (start-cluster-fringe-search start-position)
  ;; initialization of fringe files
  (write-fringe-to-disk empty "prev-fringe.gz")
  (write-fringe-to-disk (list start-position) "current-fringe.gz")
  (cluster-fringe-search 0 1 1))
  

(block10-init)
;(climb12-init)
;(climb15-init)
(compile-ms-array! *piece-types* *bh* *bw*)

;;#|
(module+ main
  ;; Switch between these according to if using the cluster or testing on multi-core single machine
  ;;(connect-to-riot-server! "wcp")
  (connect-to-riot-server! "localhost")
  (define search-result (time (start-cluster-fringe-search *start*)))
  (print search-result))
;;|#

;;(time (start-cluster-fringe-search *start*))
