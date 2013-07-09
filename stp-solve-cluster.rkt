#lang racket

(require (planet gcr/riot))
(require rnrs/sorting-6)
(require data/heap)
(require srfi/1)


(require "stp-init.rkt")
(require "stp-solve-base.rkt")
;(require profile)
;(instrumenting-enabled #t)
;(profiling-enabled #t)

(provide (all-defined-out))

(define *n-processors* 4)

;; Cluster/multi-process specific code for the sliding-tile puzzle solver
;; Currently assumes all in memory



;; expand-fringe-portion: (list int int) (setof position) (vectorof position) -> (setof position)
;; expand just the portion of the sorted-fringe-vector specified by the indices in the given range-pair.  
;; ASSUME: current-fringe-vec is sorted
(define (expand-fringe-portion range-pair prev-fringe-set current-fringe-vec)
  (let ((res (for/vector ([p (for/fold ([expansions (set)])
                               ([i (in-range (first range-pair) (second range-pair))])
                               (set-union expansions
                                          (expand (vector-ref current-fringe-vec i))))]
                          #:unless (or (set-member? prev-fringe-set p)
                                       (position-in-vec? current-fringe-vec p)))
               p)))
    #|(printf "Finished the work packet generating a set of ~a positions~%" (set-count res))
    (for ([p res])
      (printf "pos: ~a~%~a~%" (stringify p) p))|#
    ;;(vector-sort! position<? res) ; sort unneeded as resulting positions put into set
    res))

;;----------------------------------------------------------------------------------------
;; DISTRIBUTED EXPANSION AND MERGING OF FRINGES

;; a sampling-stat is a (vector int int int (vectorof int)
;; where the elements are:s
;; 1. total number of positions in the corresponding expansion
;; 2. minimum hash value of the positions
;; 3. maximum hash value of the positions
;; 4. vector of bin-counts for the range min-to-max divided into *n-processors* ranges

;; remote-expand-part-fringe: (listof position) int -> (list sampling-stat (vectorof position))
;; expand the given positions, ignoring duplicates other than in the new fringe being constructed.
;; Return a pair containing the stats from this expansion
(define (remote-expand-part-fringe list-of-pos sample-freq)
  (let* ((sample-stats (vector 0 +inf.0 -1 empty))
         (res (for/vector ([p (for/fold ([expansions (set)])
                                ([p list-of-pos])
                                (set-union expansions
                                           (expand p)))])
                          (vector-set! sample-stats 0 (add1 (vector-ref sample-stats 0)))
                          (vector-set! sample-stats 1 (min (vector-ref sample-stats 1) (equal-hash-code p)))
                          (vector-set! sample-stats 2 (max (vector-ref sample-stats 2) (equal-hash-code p)))
                          (when (zero? (modulo (vector-ref sample-stats 0) sample-freq))
                            (vector-set! sample-stats 3 (cons (equal-hash-code p) (vector-ref sample-stats 3))))
                          p)))
    (vector-sort! position<? res)
    (list sample-stats res)))

;; merge-expansions: (list int int) (listof (vectorof position)) (vectorof position) (vectorof position) -> (listof position)
;; given a collection of (partial) fringe expansions and a specification of a range of positions to consider
;; go through all of them and merge the positions (removing duplicates) in that range into a single collection
(define (merge-expansions my-range lovo-positions prev-fringe-vec current-fringe-vec)
  (let* ([fastforwarded-lolops 
          (filter-not empty?
                      (map (lambda (vop) 
                             (for/list ([i (in-range (find-pos-index (first my-range) vop)
                                                     (min (find-pos-index (second my-range) vop) (vector-length vop)))])
                               (vector-ref vop i)))
                           lovo-positions))]
         [heap-o-position-lists (let ([lheap (make-heap (lambda (l1 l2) (position<? (car l1) (car l2))))])
                                  (heap-add-all! lheap fastforwarded-lolops)
                                  lheap)])
    (printf "merge-expansions: fw-lolop-lengths=~a~%" (map length fastforwarded-lolops))
    (sorted-remove-dups
     (for/list ([lops (in-heap/consume! heap-o-position-lists)]
                #:break (>= (equal-hash-code (car lops)) (second my-range))
                #:unless (and (or ;;(equal? (car lops) (car (heap-min heap-o-position-lists)))
                                  (position-in-vec? (car lops) prev-fringe-vec)
                                  (position-in-vec? (car lops) current-fringe-vec))
                              (unless (empty? (cdr lops)) (heap-add! heap-o-position-lists (cdr lops)))))
       (unless (empty? (cdr lops)) (heap-add! heap-o-position-lists (cdr lops)))
       (car lops)))))
    

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
(define (make-merge-ranges-from-expansions lo-sample-stat)
  (let* ([total-num-positions (foldl (lambda (ss sum) (+ (vector-ref ss 0) sum)) 0 lo-sample-stat)]
         ;;[overall-min (argmin (lambda (ss) (vector-ref ss 1)) lo-sample-stat)]
         [overall-max (vector-ref (argmax (lambda (ss) (vector-ref ss 2)) lo-sample-stat) 2)]
         [total-num-samples (foldl (lambda (ss sum) (+ (length (vector-ref ss 3)) sum)) 0 lo-sample-stat)]
         [heap-o-position-lists (let ([lheap (make-heap (lambda (l1 l2) (< (first l1) (first l2))))])
                                  (heap-add-all! lheap (map (lambda (ss) (sort (vector-ref ss 3) <)) lo-sample-stat))
                                  lheap)]
         [made-merge-ranges (append 
                             (for/list ([processor (in-range 1 *n-processors*)])
                               (make-one-merge-range (/ total-num-samples *n-processors*) (car (heap-min heap-o-position-lists)) heap-o-position-lists))
                             (list (list (car (heap-min heap-o-position-lists)) (add1 overall-max))))])
    ;;(printf "make-merge-ranges-from-expansions: returning ~a~%" made-merge-ranges)
    made-merge-ranges))
                            

;; remote-expand-fringe: (vectorof position) (vectorof position) -> (vectorof position)
;; Distributed version of expand-fringe
;; convert prev-fringe set to vector to pass riot-net
(define (remote-expand-fringe prev-fringe-vec current-fringe-vec)
  ;; distribute expansion work
  (let* ((samp-freq (floor (/ (vector-length current-fringe-vec) (* 100 *n-processors*))))
         (stats+expansions (for/list #|work|# ([range-pair (make-vector-ranges (vector-length current-fringe-vec))])
                                     ;;(expand-fringe-portion range-pair prev-fringe current-fringe-vec)
                                     (remote-expand-part-fringe (for/list ([i (in-range (first range-pair) (second range-pair))])
                                                                  (vector-ref current-fringe-vec i))
                                                                samp-freq)
                                     ))
         (sampling-stats (map first stats+expansions))
         (just-expansions (map second stats+expansions)))
    ;; distribute merging work
    (apply append ;; GAK! -- do something else here
           (let ([expansion-parts (for/list #|work|# ([merge-range (make-merge-ranges-from-expansions sampling-stats)])
                                    ;;(printf "remote-expand-fringe: merge-range = ~a~%" merge-range)
                                    (merge-expansions merge-range just-expansions prev-fringe-vec current-fringe-vec))])
             (printf "remote-expand-fringe: lengths = ~a~%" (map length expansion-parts))
             (sort expansion-parts
                   < #:key (lambda (x) (equal-hash-code (car x))))))
    ))
;;----------------------------------------------------------------------------------------

;; expand-fringe: (setof position) (setof position) -> (setof position)
;; Given a current fringe to expand, and the immediately previous fringe, 
;; return the new fringe by breaking it up into ranges
(define (expand-fringe prev-fringe current-fringe)
  (let* ((current-fringe-vec  (vectorize-fringe current-fringe)))
    (vector-sort! position<? current-fringe-vec)
    (for/set ([p (if (< (vector-length current-fringe-vec) 1000)
                     ;; do it myself
                     (expand-fringe-portion (list 0 (vector-length current-fringe-vec)) prev-fringe current-fringe-vec)
                     ;; else call remote-expand, which will farm out to workers
                     (remote-expand-fringe (let ((prev-vec (vectorize-fringe prev-fringe)))
                                             (vector-sort! position<? prev-vec)
                                             prev-vec)
                                           current-fringe-vec))])
             p)))

;; vectorize-fringe: (setof position) -> (vectorof position)
;; convert a set of positions into a vector for easy/efficient partitioning
(define (vectorize-fringe f)
  (for/vector #:length (set-count f) ([p f]) p))

;; fringe-merge: (listof position) (listof position) -> (listof X)
;; ASSUMES both lists are sorted and creates a new list with duplicates removed
(define (fringe-merge l1 l2)
  (cond [(empty? l1) l2]
        [(empty? l2) l1]
        [(position<? (first l1) (first l2)) (cons (first l1) (fringe-merge (rest l1) l2))]
        [(equal? (first l1) (first l2)) (fringe-merge l1 (rest l2))]
        [else (cons (first l2) (fringe-merge l1 (rest l2)))]))

;; make-vector-ranges: int -> (listof (list int int)
;; create the streams that will give rise to the 
(define (make-vector-ranges vlength)
  (if (< vlength 1000)
      (list (list 0 vlength))
      (let ((start-list (build-list *n-processors* (lambda (i) (floor (* i (/ vlength *n-processors*)))))))
        (foldr (lambda (x r) (cons (list x (first (first r))) r)) 
               (list (list (last start-list) vlength)) 
               (drop-right start-list 1)))))

(define *max-depth* 10)(set! *max-depth* 31)

;; cluster-fringe-search: (setof position) (setof position) int -> ...
;; perform a fringe BFS starting at the given state until depth is 0
(define (cluster-fringe-search prev-fringe current-fringe depth)
  (cond [(or (set-empty? current-fringe) (> depth *max-depth*)) #f]
        [else
         (let ((maybe-goal (goal-in-fringe? current-fringe)))
           (cond [maybe-goal
                  (print "found goal")
                  maybe-goal]
                 [else (let ((new-fringe (expand-fringe prev-fringe current-fringe)))
                         (printf "At depth ~a fringe has ~a positions~%" depth (set-count current-fringe))
                         ;;(for ([p current-fringe]) (displayln p))
                         (cluster-fringe-search current-fringe new-fringe (add1 depth)))]))]))


(block10-init)
;(climb12-init)
(compile-ms-array! *piece-types* *bh* *bw*)

;;#|
(module+ main
  (connect-to-riot-server! "localhost")
  (define search-result (time (cluster-fringe-search (set) (set *start*) 1)))
  (print search-result))
;;|#
;;(time (cluster-fringe-search (set) (set *start*) 1))