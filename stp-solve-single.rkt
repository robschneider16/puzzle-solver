#lang racket

(require "stp-init.rkt")
(require "stp-solve-base.rkt")


(define *max-depth* 10)(set! *max-depth* 32)



;; fringe-search: [file(listof position)] [file(listof position)] int -> ...
;; perform a fringe BFS starting at the given state until depth is 0
(define (fringe-search depth)
  (let ([prev-fringe (list->set (read-fringe-from-disk "prev-fringe"))]
        [current-fringe (list->set (read-fringe-from-disk "current-fringe"))]) ; current fringe is a FILE
    ;;(printf "current-fringe: ~a~%" current-fringe)
    (cond [(or (empty? current-fringe) (> depth *max-depth*)) #f]
          [else
           (let ([maybe-goal (goal-in-fringe? current-fringe)])
             (cond [maybe-goal
                    (print "found goal")
                    maybe-goal]
                   [else (let* ([new-fringe (for/set ([p (for/fold ([expansions (set)])
                                                           ([p current-fringe])
                                                           (set-union expansions (expand p)))]
                                                      #:unless (or (set-member? prev-fringe p)
                                                                   (set-member? current-fringe p)))
                                                     p)])
                           (printf "At depth ~s fringe has ~a positions~%" depth (set-count current-fringe))
                           
                           #|(when (member depth '(5 6))
                           (for ([s (sort (for/list ([p current-fringe])
                                            (stringify p))
                                          string<?)])
                             (printf "~a~%" s)))|#
                           
                           (rename-file-or-directory "current-fringe" "prev-fringe" #t)
                           (write-fringe-to-disk (set->list new-fringe) "current-fringe")
                           (fringe-search (add1 depth)))]))])))



(block10-init)
;(climb12-init)
(compile-ms-array! *piece-types* *bh* *bw*)

(write-fringe-to-disk empty "prev-fringe")
(write-fringe-to-disk (list *start*) "current-fringe")


(time (fringe-search 1))
