#lang racket
(require 2htdp/batch-io)

;; a tile-spec is a triple, (list a b c), where a is the tile-type and b and c are row and col

;; a state is a (vectorof tile-spec)
(define *goal*
  '#((0 0 0)
     (1 0 1)
     (2 0 2)
     (3 1 0)
     (4 1 1)
     (5 1 2)
     (6 2 0)
     (7 2 1)
     (8 2 2)))

(define a-start
  (vector '(0 0 2)
          '(1 0 0)
          '(2 0 1)
          '(3 1 0)
          '(4 1 1)
          '(5 1 2)
          '(6 2 0)
          '(7 2 1)
          '(8 2 2)))
     

;; expand: state -> (listof state)
;; generate next states from this one
(define (expand s)
  (filter (lambda (x) x)
          (list (check-move s '(-1 0))
                (check-move s '(0 1))
                (check-move s '(1 0))
                (check-move s '(0 -1)))))

;; check-move: state movement -> state OR #f
;; make a new state from the movement-specification (if possible)
(define (check-move s m)
  (let ((new-space (list 0
                         (+ (first m) (second (vector-ref s 0)))
                         (+ (second m) (third (vector-ref s 0))))))
    (if (and (< -1 (second new-space) 3)
             (< -1 (third new-space) 3))
        (swap-locations (vector->list s) new-space (vector-copy s))
        #f)))

;; swap-locations: (listof tile-spec) tile-spec state -> state
;; given the tile-spec for the new location of the space, find the tile there and put it where
;; the space currently is found
(define (swap-locations s ts v [i 0])
  (cond [(empty? s) (error 'swap-locations "ran off the end")]
        [(equal? (cdr (car s)) (cdr ts))
         (vector-set! v i (cons (car (car s)) (cdr (vector-ref v 0))))
         (vector-set! v 0 ts)
         v]
        [else (swap-locations (cdr s) ts v (add1 i))]))
                                    


;; fringe-search: (listof state) int -> ...
;; perform a fringe BFS starting at the given state until depth is 0
(define (fringe-search prev-fringe current-fringe depth)
  (cond [(or (empty? current-fringe) (zero? depth)) #f]
        [(member *goal* current-fringe)
         (print current-fringe)(newline)
         #t]
        [else (let ((new-fringe
                     (apply append ;; '((1 2)(3 4)) -> '(1 2 3 4)
                            (map expand current-fringe))))
                (print current-fringe)(newline)
                (fringe-search current-fringe
                               (filter (lambda (s) (and (not (member s prev-fringe))
                                                        (not (member s current-fringe))))
                                       new-fringe)
                               (sub1 depth)))]))

;; save-to-disk: (listof state) -> ...
;; saves a fringe to disk
(define (save-to-disk fringe)
  (...))