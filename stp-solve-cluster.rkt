#lang racket

(require (planet gcr/riot))
(require rnrs/sorting-6)

(require "stp-init.rkt")
(require "stp-solve-base.rkt")
;(require profile)
;(instrumenting-enabled #t)
;(profiling-enabled #t)

(provide (all-defined-out))

(define *n-processors* 7)

;; Cluster/multi-process specific code for the sliding-tile puzzle solver
;; Currently assumes all in memory



;; expand-fringe-portion: (list int int) (setof position) (vectorof position) -> (setof position)
;; expand just the specified in the given range.  
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
    (vector-sort! position<? res)
    res))

;; a sampling-stat is a (vector int int int (vectorof int)
;; where the elements are:
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
                          (when (zero? (vector-ref sample-stats 0) sample-freq)
                            (vector-set! sample-stats 3 (cons (equal-hash-code p) (vector-ref sample-stats 3))))
                          p)))
    (vector-sort! position<? res)
    (list sample-stats res)))

;; remote-expand-fringe: (setof position) (vectorof position) -> (vectorof position)
;; Distributed version of expand-fringe
(define (remote-expand-fringe prev current-fringe-vec)
  ;; distribute expansion work
  (let* ((samp-freq (/ (vector-length current-fringe-vec) (* 1000 *n-processors*)))
         (expansions (for/work ([range-pair (make-vector-ranges (vector-length current-fringe-vec))])
                               ;;(expand-fringe-portion range-pair prev-fringe current-fringe-vec)
                               (remote-expand-part-fringe (for/list ([i (in-range (first range-pair) (second range-pair))])
                                                            (vector-ref current-fringe-vec i))
                                                          samp-freq)
                               ))
         (sampling-stats (map first expansions))
         (just-expansions (map rest expansions)))
    ;; distribute merging work
    (apply append ;; GAK! -- do something else here
           (for/work ([merge-range {make-merge-ranges-from-expansions sampling-stats}])
                     (merge-expansions merge-range just-expansions)))
    ))

;; merge-expansions: (list int int) (listof (listof position)) -> (listof position)
;; given a collection of (partial) fringe expansions and a specification of a range of positions to consider
;; go through all of them and merge the positions (removing duplicates) in that range into a single collection

;; make-merge-ranges-from-expansions: (listof sampling-stats) -> (list merge-ranges)
;; decide how to partition the space of positions as distributed over the expansions reflected in the sampling-stats

;; expand-fringe: (setof position) (setof position) -> (setof position)
;; Given a current fringe to expand, and the immediately previous fringe, 
;; return the new fringe by breaking it up into ranges
(define (expand-fringe prev-fringe current-fringe)
  (let* ((current-fringe-vec  (vectorize-fringe current-fringe)))
    (vector-sort! position<? current-fringe-vec)
    (for/set ([p (for/fold ([expansions '()])
                   ([partial-new-fringe (if (< (vector-length current-fringe-vec) 1000)
                                            ;; do it myself
                                            (list (expand-fringe-portion (list 0 (vector-length current-fringe-vec)) prev-fringe current-fringe-vec))
                                            ;; else farm out to workers with (de)serialization overhead
                                            (for/work ([range-pair (make-vector-ranges (vector-length current-fringe-vec))])
                                                      ;;(expand-fringe-portion range-pair prev-fringe current-fringe-vec)
                                                      (remote-expand-part-fringe (for/list ([i (in-range (first range-pair) (second range-pair))])
                                                                                   (vector-ref current-fringe-vec i)))
                                                      ))])
                   (fringe-merge expansions 
                                 (vector->list partial-new-fringe)))]
              #:unless (or (set-member? prev-fringe p)
                           (set-member? current-fringe p)))
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

(define *max-depth* 10)(set! *max-depth* 61)

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