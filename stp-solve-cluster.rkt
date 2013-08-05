#lang racket

(require (planet gcr/riot))
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
(define *n-processors* 4)

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


;; expand-fringe-self: (setof position) (vectorof position) -> void
;; expand just the portion of the sorted-fringe-vector specified by the indices in the given range-pair.  
;; ASSUME: current-fringe-vec is sorted
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
    (system "mv current-fringe prev-fringe")
    (write-fringe-to-disk (sort res position<?) "current-fringe")
    ))

;;----------------------------------------------------------------------------------------
;; DISTRIBUTED EXPANSION AND MERGING OF FRINGES

;; a sampling-stat is a (vector int int int (vectorof int)
;; where the elements are:s
;; 0. total number of positions in the corresponding expansion
;; 1. minimum hash value of the positions
;; 2. maximum hash value of the positions
;; 3. vector of bin-counts for the range min-to-max divided into *n-processors* ranges
;; 4. boolean goal-found if found when expanding the assigned positions

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
(define (remote-expand-part-fringe ipair sample-freq process-id)
  (let* ([start (first ipair)]
         [end (second ipair)]
         [sample-stats (vector 0 *most-positive-fixnum* *most-negative-fixnum* empty #f)]
         [resv (for/vector ([p (for*/fold ([expansions (set)])
                                 ([pos-to-expand (read-partial-fringe "current-fringe" start end)])
                                 ;; do the expansion of the indexed position, adding in any new positions found
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
    (vector-set! sample-stats 3 (sort (vector-ref sample-stats 3) fx<))
    ;; write local-expansion to disk w/ naming convention "partial-expansion" + beginning of expansion range
    (write-fringe-to-disk
     resv ;; this is already sorted in remote-expand-part-fringe
     (format "partial-expansion~a" (~a process-id #:left-pad-string "0" #:width 2 #:align 'right)))
    #|
    (printf "remote-expand-part-fringe: starting w/ ~a positions, expansion has ~a/~a positions and sampled ~a hashcodes~%"
            (- end start) (vector-ref sample-stats 0) (vector-length resv) (length (vector-ref sample-stats 3)))
    (printf "remote-expand-part-fringe: HAVE EXPANSIONS:~%")
    (for ([p resv]) (displayln p))
    |#
    sample-stats))


;; MERGING .....

;; merge-expansions: (list int int) (listof (vectorof position)) (vectorof position) (vectorof position) -> (listof position)
;; given a collection of (partial) fringe expansions and a specification of a range of _position-hash-codes_ to consider
;; go through all of them and merge the positions (removing duplicates) in that range into a single collection
(define (remote-merge-expansions my-range lovo-positions prev-fringe-vec current-fringe-vec)
  ;; as first step:
  ;; ...  need to read all the partial fringe-files int a local variable called lovo-positions
  (let* ([fastforwarded-lolops 
          (filter-not empty?
                      (map (lambda (vop)
                             #|(printf "remote-merge-expansions: fastforwarding vec of ~a positions to ~a positions (indices [~a-~a])~%" 
                                     (vector-length vop) (- (find-pos-index (second my-range) vop) (find-pos-index (first my-range) vop)) 
                                     (find-pos-index (first my-range) vop) (find-pos-index (second my-range)  vop))|#
                             (for/list ([i (in-range (find-pos-index (first my-range) vop)
                                                     (find-pos-index (second my-range) vop))])
                               (vector-ref vop i)))
                           lovo-positions))]
         ;; this will become a heap of file-handles
         [heap-o-position-lists (let ([lheap (make-heap (lambda (l1 l2) (position<? (car l1) (car l2))))])
                                  (heap-add-all! lheap fastforwarded-lolops)
                                  lheap)]
         ;; now remove duplicates
         [sorted-merged-expansions
          (sorted-remove-dups
           (for/list ([lops (in-heap/consume! heap-o-position-lists)]
                      #:break (fx>= (equal-hash-code (car lops)) (second my-range))
                      #:unless (and (or ;;(equal? (car lops) (car (heap-min heap-o-position-lists)))
                                     (position-in-vec? prev-fringe-vec (car lops))
                                     (position-in-vec? current-fringe-vec (car lops)))
                                    (unless (empty? (cdr lops)) (heap-add! heap-o-position-lists (cdr lops)))));;******???????
             (unless (empty? (cdr lops)) (heap-add! heap-o-position-lists (cdr lops)))
             (car lops)))])
    ;(printf "remote-merge-expansions: fw-lolop-lengths=~a [total ~a]~%" (map length fastforwarded-lolops) (for/sum ([l fastforwarded-lolops]) (length l)))
    ;(printf "remote-merge-expansions: merged-expns-length=~a~%" (length sorted-merged-expansions))
    ;;***error-check
    #|(unless (= (length sorted-merged-expansions) (set-count (list->set sorted-merged-expansions))) 
      (error 'remote-merge-expansions "list-merging vs. set mis-match"))|#
    sorted-merged-expansions
    ))


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


;; distributed-expand-fringe: int int -> void
;; Distributed version of expand-fringe
;; convert prev-fringe set to vector to pass riot-net
(define (distributed-expand-fringe cur-fringe-size depth)
  ;;(printf "distributed-expand-fringe: ~a nodes in prev and ~a in current fringes~%" (vector-length prev-fringe-vec) (vector-length current-fringe-vec))
  (let* (;; Distribute the expansion work
         [ranges (make-vector-ranges cur-fringe-size)]
         [sampling-stats (for/work ([range-pair ranges]
                                    [i (in-range (length ranges))])
                                   (when (> depth *max-depth*) (error 'distributed-expand-fringe "ran off end"))
                                   (remote-expand-part-fringe range-pair 
                                                              (max (floor (/ cur-fringe-size (* 100 *n-processors*))) 1)
                                                              i))]
         [check-for-goal (for/first ([ss sampling-stats]
                                     #:when (vector-ref ss 4))
                           (set! *found-goal* (vector-ref ss 4)))]
         [wait-for-partial-expansions (for ([i (in-range (length sampling-stats))])
                                        (do ([present #f]) (present)
                                          (set! present (file-exists? (format "partial-expansion~a" (~a i #:left-pad-string "0" #:width 2 #:align 'right))))))]
         #|[error-check1 (for/first ([i (in-range (length sampling-stats))]
                                   [ss sampling-stats]
                                   #:unless (= (vector-ref ss 0) (position-count-in-file (format "partial-expansion~a" (~a i #:left-pad-string "0" #:width 2 #:align 'right)))))
                         (error 'distributed-expand-fringe (format "err-chk1: partial-expansion sizes do not match up for ~a which should be ~a" i (vector-ref ss 0))))]|#
         ;; Distribute the merging work
         [merge-ranges (make-merge-ranges-from-expansions sampling-stats)]
         [sorted-expansion-files-lengths
          (let* ([merged-expansion-files-lens
                  (for/work ([merge-range merge-ranges] ;; merged results should come back in order of merge-ranges assignments
                             [i (in-range (length merge-ranges))])
                            (when (> depth *max-depth*) (error 'distributed-expand-fringe "ran off end"))
                            (let* ([prev-fringe-vec (list->vector (read-fringe-from-disk "prev-fringe"))] ;; bind prev-fringe-vec to read-fringe-from-disk
                                   [current-fringe-vec (list->vector (read-fringe-from-disk "current-fringe"))] ;; bind current-fringe-vec to read-fringe-from-disk
                                   [just-expansions (for/list ([f (string-split (with-output-to-string (lambda () (system "ls partial-expansion*"))))])
                                                      (list->vector (read-fringe-from-disk f)))]
                                   [merged-responsibility-range (remote-merge-expansions merge-range just-expansions prev-fringe-vec current-fringe-vec)]
                                   [ofile-name (format "partial-merge~a" (~a i #:left-pad-string "0" #:width 2 #:align 'right))] 
                                   )
                              ;;(printf "remote-expand-fringe: merge-range = ~a~%" merge-range)
                              (write-fringe-to-disk merged-responsibility-range ofile-name)
                              (list ofile-name (length merged-responsibility-range))))]
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
            merged-expansion-files-lens
            )]
         [sorted-expansion-files (map first sorted-expansion-files-lengths)]
         [sef-lengths (map second sorted-expansion-files-lengths)]
         #|[error-check2 (for/first ([f sorted-expansion-files]
                                   [len sef-lengths]
                                   #:unless (= len (position-count-in-file f)))
                         (error 'distributed-expand-fringe (format "err-chk2: partial-merges do not match up for ~a which should be ~a" f len)))]|#
         )
    (rename-file-or-directory "current-fringe" "prev-fringe" #t) ;;(system "mv current-fringe prev-fringe")
    (for ([f sorted-expansion-files])
      (system (format "cat ~a >> current-fringe" f)))
    (system "rm partial-expansion* partial-merge*")
    ))
;;----------------------------------------------------------------------------------------

;; expand-fringe: int int -> void
;; Given the size of the current fringe to expand, and the current depth of search,
;; do the expansions and merges as appropriate
(define (expand-fringe current-fringe-size depth)
  (if (< current-fringe-size 1000)
      ;; do it myself
      (expand-fringe-self (list->set (read-fringe-from-disk "prev-fringe"))
                          (vectorize-list (read-fringe-from-disk "current-fringe")))
      ;; else call distributed-expand, which will farm out to workers
      (begin 
        #|(printf "calling distributed-exp... with ~a and ~a positions in prev and current fringes, respectively~%"
                  (set-count prev-fringe) (vector-length current-fringe-vec))|#
        (distributed-expand-fringe current-fringe-size depth))))

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
(define (cluster-fringe-search depth)
  (let ([current-fringe-size (position-count-in-file "current-fringe")]
        )
    (cond [(or (zero? current-fringe-size) (> depth *max-depth*)) #f]
          [*found-goal*
           (print "found goal")
           *found-goal*]
          [else (expand-fringe current-fringe-size depth)
                (printf "At depth ~a: current-fringe has ~a positions (and new-fringe ~a)~%" 
                        depth (position-count-in-file "prev-fringe") (position-count-in-file "current-fringe"))
                ;;(for ([p current-fringe]) (displayln p))
                (cluster-fringe-search (add1 depth))])))

(define (start-cluster-fringe-search start-position)
  ;; initialization of fringe files
  (write-fringe-to-disk empty "prev-fringe")
  (write-fringe-to-disk (list start-position) "current-fringe")
  (cluster-fringe-search 1))
  

;(block10-init)
(climb12-init)
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
