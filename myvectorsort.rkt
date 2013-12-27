#lang racket

(provide vector-sort!)


;; vector-sort!: (vectorof X) (X X -> boolean) [N] [N] -> void
;; sort the vector according to the compare between start [0] and end [vector-length]
(define (vector-sort! v compare [start 0] [end (vector-length v)])
  (qsort-aux v compare start (sub1 end)))


;; After looking in the rnrs/sorting-6 code for vector-sort!, which uses vector->list, list->vector, and vector-copy!,
;; decided to compare performance to the in-place qsort provided in HtDP

;; qsort-aux : (vectorof X) (X X -> boolean) N N  ->  (vectorof number)
;; effect: sort the interval [left,right] (inclusive) of vector V
(define (qsort-aux V compare left right)
  (cond
    [(>= left right) V]
    [else (local ((define new-pivot-position (alt-partition V compare left right (+ left (random (add1 (- right left)))))))
            (begin (qsort-aux V compare left (sub1 new-pivot-position))
                   (qsort-aux V compare (add1 new-pivot-position) right)))]))

;; alt-partition: (vectorof X) (X X -> boolean) N N N -> N
;; from wikipedia
(define (alt-partition v compare l r pividx)
  (let ([pivot-value (vector-ref v pividx)]
        [store-index l])
    (swap v pividx r) ; move pivot-value to end of array
    (for ([i (in-range l r)])
      (when (compare (vector-ref v i) pivot-value)
        (swap v i store-index)
        (set! store-index (add1 store-index))))
    (swap v store-index r)
    store-index))

;; partition : (vectorof X) (X X -> boolean) N N  ->  N
;; to determine the proper position p of the pivot-item 
;; effect: rearrange the vector V so that 
;; -- all items in V in [left,p) are "smaller" than the pivot item
;; -- all items of V in (p,right] are not-smaller than the pivot item
;; generative recursion
(define (partition V compare left right)
  (letrec ((pivot-position left)
           (the-pivot (vector-ref V left))
           (partition-aux 
            (lambda (left right)
              (let ((new-right (find-new-right V compare the-pivot left right))
                    (new-left (find-new-left V compare the-pivot left right)))
                (cond
                  [(>= new-left new-right)
                   (swap V pivot-position new-right)
                   new-right]
                  [else ; (< new-left new-right)
                   (swap V new-left new-right)
                   (partition-aux new-left new-right)])))))
    (partition-aux left right)))

;; find-new-side : (vectorof X) (X X -> boolean) number N N [>= left]  ->  N
;; to determine an index i between left and right (inclusive)
;; such that (compare (vector-ref V i) the-pivot) holds on range [i+1,right]
(define (find-new-right V compare the-pivot left right)
  (cond
    [(= right left) right]
    [else (cond
	    [(compare (vector-ref V right) the-pivot) right]
	    [else (find-new-right V compare the-pivot left (sub1 right))])]))

(define (find-new-left V compare the-pivot left right)
  (cond
    [(= right left) left]
    [else (cond
            [(not (compare (vector-ref V left) the-pivot)) left]
            [else (find-new-left V compare the-pivot (add1 left) right)])]))

;; swap : (vectorof X) N N -> void 
(define (swap V i j)
  (let ((temp (vector-ref V i)))
    (vector-set! V i (vector-ref V j))
    (vector-set! V j temp)))

(define myv1 (vector 5 14 11 14 8 0 2 3 7 7))
(define myv2 (vector #\x #\u #\d #\m #\a #\k))
(set! myv1 myv1)

;(vector-sort! myv <)
myv1