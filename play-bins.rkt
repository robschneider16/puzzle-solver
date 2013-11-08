#lang racket

(require racket/generator)

; work space for boundary code

(define *num-proto-fringe-slices* 10)
(define *num-bins* (* *num-proto-fringe-slices* 20))
(define *most-negative-fixnum* 0)
(define *most-positive-fixnum* 1000000)

(define fcounts (build-vector *num-bins* (lambda (_) (sqr (random (sqrt 1000000))))))
(define ftotal (for/sum ([i fcounts]) i))

#|(define g (generator ()
              (let loop ([x (random 100)])
                (begin
                  (yield x)
                  (loop (random 100))))))|#

(define *bin-boundaries* 
  (build-vector *num-bins* 
                (lambda (i) (cons (* i (/ (- *most-positive-fixnum* *most-negative-fixnum*) *num-bins*))
                                  (* (add1 i) (/ (- *most-positive-fixnum* *most-negative-fixnum*) *num-bins*))))))

(define (simple-interpolate bin-count k bin-index)
  (let ([bin-range (vector-ref *bin-boundaries* bin-index)])
    (+ (car bin-range)
       (ceiling (* bin-count (/ k (- (cdr bin-range) (car bin-range))))))))


(define (derive-boundaries frequency-counts total)
  (let ([k (/ total *num-proto-fringe-slices*)]
        [slice-low *most-negative-fixnum*]
        [slice-high 'unknown]
        [slice-boundaries (make-vector (add1 *num-proto-fringe-slices*))]
        [current-bin 0])
    ;; for each slice
    (do ([i 1 (add1 i)])
      ((> i *num-proto-fringe-slices*)
       ; finish and clean up
       )
      ;; for bins until slice-count accumulated
      (do ([j current-bin (add1 j)]
           [this-slice-count 0 (+ this-slice-count (vector-ref frequency-counts j))])
        ((> (+ this-slice-count (vector-ref frequency-counts j)) k)
         ; determine the boundary within the current bin based on the proportion of ...
         ; update current-bin
         )
        )
      )
    ))