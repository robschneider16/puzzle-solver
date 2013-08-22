#lang racket

(require "stp-init.rkt")
(require "stp-solve-base.rkt")


(define *max-depth* 10)(set! *max-depth* 61)



;; fringe-file-search: [file(listof position)] [file(listof position)] int -> ...
;; using file for storing fringes, perform a fringe BFS starting at the given state until depth is 0
(define (fringe-file-search depth [found-goal? #f] [npos 1])
  (let ([prev-fringe (list->set (read-fringe-from-file "prev-fringe"))]
        [current-fringe (list->set (read-fringe-from-file "current-fringe"))]) ; current fringe is a FILE
    ;;(printf "current-fringe: ~a~%" current-fringe)
    (cond [(or (set-empty? current-fringe) (> depth *max-depth*)) 
           (printf "exhausted the space after ~a positions~%" npos) #f]
          [else
           (cond [found-goal? (printf "found goal after encountering ~a positions~%" npos)
                              found-goal?]
                 [else (let* ([new-fringe (for/set ([p (for/fold ([expansions (set)])
                                                         ([p current-fringe])
                                                        (set-union expansions (expand p)))]
                                                    #:unless (or (set-member? prev-fringe p)
                                                                 (set-member? current-fringe p)))
                                            (when (is-goal? p) (set! found-goal? p))
                                            p)])
                         (printf "At depth ~s fringe has ~a positions~%" depth (set-count current-fringe))
                         
                         #|(when (member depth '(5 6))
                           (for ([s (sort (for/list ([p current-fringe])
                                            (stringify p))
                                          string<?)])
                             (printf "~a~%" s)))|#
                         (rename-file-or-directory "current-fringe" "prev-fringe" #t)
                         (write-fringe-to-disk (set->list new-fringe) "current-fringe")
                         (fringe-file-search (add1 depth) found-goal? (+ npos (set-count new-fringe))))])])))

;; fringe-mem-search: (setof position) (setof positions) int -> #f or position
;; search in memory
(define (fringe-mem-search prev-fringe current-fringe depth [found-goal? #f] [npos 1])
  ;;(printf "current-fringe: ~a~%" current-fringe)
  (cond [(or (set-empty? current-fringe) (> depth *max-depth*)) 
         (printf "exhausted the space after ~a positions~%" npos) #f]
        [else
         (cond [found-goal? (printf "found goal after encountering ~a positions~%" npos)
                            found-goal?]
               [else (let* ([new-fringe (for/set ([p (for/fold ([expansions (set)])
                                                       ([p current-fringe])
                                                       (set-union expansions (expand p)))]
                                                  #:unless (or (set-member? prev-fringe p)
                                                               (set-member? current-fringe p)))
                                          (when (is-goal? p) (set! found-goal? p))
                                          p)])
                       (printf "At depth ~s fringe has ~a positions~%" depth (set-count current-fringe))
                       (fringe-mem-search current-fringe new-fringe (add1 depth) found-goal? (+ npos (set-count new-fringe))))])]))


;(block10-init)
(climb12-init)
;(climb15-init)
(compile-ms-array! *piece-types* *bh* *bw*)

(write-fringe-to-disk empty "prev-fringe")
(write-fringe-to-disk (list *start*) "current-fringe")

;(time (fringe-file-search 1))
(time (fringe-mem-search (set) (set *start*) 1))
