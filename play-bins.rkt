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

;; simple-interpolate: N N N -> number
;; compute the value within the bin-range expected to account for k of the bin-count positions
(define (simple-interpolate bin-count k bin-index)
  (let ([bin-range (vector-ref *bin-boundaries* bin-index)])
    (+ (car bin-range)
       (ceiling (* (/ k bin-count) (- (cdr bin-range) (car bin-range)))))))


;; goble-one: (vectorof N) N number N N -> number
;; span the range of bins to gather one slice, returning the value expected to cover slice count
;; given the vector of counts, the starting bin, the starting value of the new rang, how many of bin still available, and how many needed
;; compute the upper boundary, possibly in another bin --
;; return list: current-bin, boundary-value, how many of this bin consumed, how many of this bin remain
(define (goble-one fcounts bin0 bin-start bin-remain need-count)
  (do ([g need-count (- g (vector-ref fcounts bi))]
       [bi bin0 (add1 bi)])
    ((or (= bi (vector-length fcounts))
         (> (vector-ref fcounts bi) g))
     ;;*** find value here, maybe using simple-interpolate
     (if (= bi (vector-length fcounts))
         (list bi *most-positive-fixnum* (vector-ref fcounts (sub1 bi)) 0)
         (list bi
               (simple-interpolate (vector-ref fcounts bi) (+ (if (= bin0 bi) bin-start 0) g) bi)
               (+ (if (= bin0 bi) bin-start 0) g)
               (- (vector-ref fcounts bi) (+ (if (= bin0 bi) bin-start 0) g))))
     )))


(define (derive-boundaries frequency-counts total)
  (let ([k (/ total *num-proto-fringe-slices*)]
        [slice-low *most-negative-fixnum*]
        [slice-high 'unknown]
        [slice-boundaries (make-vector (add1 *num-proto-fringe-slices*))]
        [current-bin 1])
    ;; for each slice
    (do ([gobleres (goble-one frequency-counts 0 0 (vector-ref frequency-counts 0) k)
                   (goble-one frequency-counts (first gobleres) (third gobleres) (fourth gobleres) k)])
      ((> current-bin *num-proto-fringe-slices*)
       ; finish and clean up
       slice-boundaries
       )
      (vector-set! slice-boundaries current-bin (second gobleres))
      (set! current-bin (add1 current-bin)))))
      
      