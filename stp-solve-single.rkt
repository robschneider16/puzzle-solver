#lang racket

(require "stp-init.rkt")
(require "stp-solve-base.rkt")


(define *max-depth* 10)(set! *max-depth* 32)

;; write-to-disk: (listof position) string -> file(listof position)
;; write-to-disk takes a fringe and creates a file on disk with that fringe
(define (write-to-disk fringe file-name)
  (let ([my-output (open-output-file file-name #:exists 'replace)])
    (write fringe my-output)
    (close-output-port my-output)))

;; read-from-disk: file -> fringe
;; reads a file from a file path (if you are in the current directory just simply the file-name)
;; and returns the fringe that was in that file.
(define (read-from-disk file-path)
  (with-input-from-file file-path read))



;; fringe-search: [file(listof position)] [file(listof position)] int -> ...
;; perform a fringe BFS starting at the given state until depth is 0
(define (fringe-search depth)
  (let ([prev-fringe (read-from-disk "prev-fringe")]
        [current-fringe (read-from-disk "current-fringe")]) ; current fringe is a FILE
    (cond [(or (empty? current-fringe) (> depth *max-depth*)) #f]
          [else
           (let ([maybe-goal (goal-in-fringe? current-fringe)])
             (cond [maybe-goal
                    (print "found goal")
                    maybe-goal]
                   [else (let* ([new-fringe (for/list ([p (for/fold ([expansions (list)])
                                                            ([p current-fringe])
                                                            (cons expansions p))]
                                                       #:unless (or (set-member? prev-fringe p)
                                                                    (set-member? current-fringe p)))
                                              p)])
                           (printf "At depth ~s fringe has ~a positions~%" depth (count current-fringe))
                           
                           #|(when (member depth '(5 6))
                           (for ([s (sort (for/list ([p current-fringe])
                                            (stringify p))
                                          string<?)])
                             (printf "~a~%" s)))|#
                           
                           (rename-file-or-directory "current-fringe" "prev-fringe" #t)
                           (write-to-disk new-fringe "current-fringe")
                           (fringe-search (add1 depth)))]))])))



(block10-init)
;(climb12-init)

(write-to-disk empty "prev-fringe")
(write-to-disk (list *start*) "current-fringe")


(time (fringe-search 1))
