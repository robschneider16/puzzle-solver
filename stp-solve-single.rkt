#lang racket

(require srfi/25) ;; multi-dimensional arrays
(require "stp-solve-base.rkt")


(define *max-depth* 10)(set! *max-depth* 8)

;; fringe-search: (setof position) (setof position) int -> ...
;; perform a fringe BFS starting at the given state until depth is 0
(define (fringe-search prev-fringe current-fringe depth)
  (cond [(or (set-empty? current-fringe) (> depth *max-depth*)) #f]
        [else
         (let ((maybe-goal (goal-in-fringe? current-fringe)))
           (cond [maybe-goal
                  (print "found goal")
                  maybe-goal]
                 [else (let ((new-fringe
                              (for/set ([p (for/fold ([expansions (set)])
                                             ([p current-fringe])
                                             (set-union expansions (expand p)))]
                                        #:unless (or (set-member? prev-fringe p)
                                                     (set-member? current-fringe p)))
                                       p)))
                         (printf "At depth ~a fringe has ~a positions~%" depth (set-count current-fringe))
                         (when (= depth 6)
                           (for ([p current-fringe])
                             (printf "~a~%" p)))
                         (fringe-search current-fringe
                                        new-fringe
                                        (add1 depth)))]))]))

