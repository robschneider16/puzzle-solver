#lang racket

(require (planet gcr/riot))
(require rnrs/sorting-6)
(require racket/serialize)

;(require srfi/25) ;; multi-dimensional arrays
(require "stp-solve-base.rkt")

(provide (all-defined-out))

(define *n-processors* 32)

;; Cluster/multi-process specific code for the sliding-tile puzzle solver
;; Currently assumes all in memory


(define (my-vec-member? v p)
  (do ([found #f]
       [low 0]
       [high (vector-length v)]
       [mid (floor (/ (vector-length v) 2)) (floor (/ (+ low high) 2))])
    ((or found (>= low high)) found)
    (cond [(equal? p (vector-ref v mid)) (set! found #t)]
          [(compare-positions p (vector-ref v mid)) (set! high mid)]
          [else (set! low (add1 mid))])))


;; expand-fringe-portion: (list int int) (setof position) (vectorof position) -> (setof position)
;; expand just the specified in the given range.  
;; ASSUME: current-fringe-vec is sorted
(define (expand-fringe-portion range-pair prev-fringe-set current-fringe-vec)
  (let ((res (for/set ([p (for/fold ([expansions (set)])
                 ([i (in-range (first range-pair) (second range-pair))])
                 (set-union expansions
                            (expand (vector-ref current-fringe-vec i))))]
                       #:unless (or (set-member? prev-fringe-set p)
                                    (my-vec-member? current-fringe-vec p)))
                      p)))
    #|(printf "Finished the work packet generating a set of ~a positions~%" (set-count res))
    (for ([p res])
      (printf "pos: ~a~%~a~%" (stringify p) p))|#
    res ;; (serialize res)
    ))

(define (expand-fringe-portion2 list-of-pos)
  (let ((res (for/set ([p (for/fold ([expansions (set)])
                            ([p list-of-pos])
                            (set-union expansions
                                       (expand p)))])
                      p)))
    (serialize res)))

;; expand-fringe: (setof position) (setof position) -> (setof position)
;; Given a current fringe to expand, and the immediately previous fringe, 
;; return the new fringe by breaking it up into ranges
(define (expand-fringe prev-fringe current-fringe)
  (let* ([current-fringe-vec  (vectorize-fringe current-fringe)]
         [ignore (vector-sort! compare-positions current-fringe-vec)]
         [new-fringe (for/set ([p (expand-fringe-portion (list 0 (vector-length current-fringe-vec)) prev-fringe current-fringe-vec)]
                               #:unless (or (set-member? prev-fringe p)
                                            (set-member? current-fringe p)))
                              p)])
    ;;write-to-disk-here!!
    ))

;; vectorize-fringe: (setof position) -> (vectorof position)
;; convert a set of positions into a vector for easy/efficient partitioning
(define (vectorize-fringe f)
  (for/vector #:length (set-count f) ([p f]) p))

;; compare-positions: position position -> boolean
(define (compare-positions p1 p2)
  (string<? (stringify p1) (stringify p2)))

;; make-vector-ranges: int -> (listof (list int int)
;; create the streams that will give rise to the 
(define (make-vector-ranges vlength)
  (if (< vlength 1000)
      (list (list 0 vlength))
      (let ((start-list (build-list *n-processors* (lambda (i) (floor (* i (/ vlength *n-processors*)))))))
        (foldr (lambda (x r) (cons (list x (first (first r))) r)) 
               (list (list (last start-list) vlength)) 
               (drop-right start-list 1)))))

(define *max-depth* 10)(set! *max-depth* 32)

;; cluster-fringe-search: (setof position) (setof position) int -> ...
;; perform a fringe BFS starting at the given state until depth is 0
(define (cluster-fringe-search prev-fringe current-fringe depth)
  (cond [(or (set-empty? current-fringe) (> depth *max-depth*)) #f]
        [else
         (let ([maybe-goal (goal-in-fringe? current-fringe)])
           (cond [maybe-goal
                  (print "found goal")
                  maybe-goal]
                 [else (expand-fringe prev-fringe current-fringe) 
                  (let ([new-fringe ;;read from file!!
                         ])
                         (printf "At depth ~a fringe has ~a positions~%" depth (set-count current-fringe))
                         (cluster-fringe-search current-fringe new-fringe (add1 depth)))]))]))


(block10-init)
;(climb12-init)

(module+ main
  (connect-to-riot-server! "wcp")
  (define search-result (time (cluster-fringe-search (set) (set *start*) 1)))
  (print search-result))
