#lang racket

;(require srfi/25) ;; multi-dimensional arrays

(provide (all-defined-out))


;; INITIALIZE STUFF FOR SLIDING-TILE-SOLVER

(define *num-piece-types* 0)
(define *piece-types* empty)
(define *start* empty)
(define *target* empty)
(define *bw* 0)
(define *bh* 0)
(define *bsz* 0)


(define (set-em! ptv s t nrow ncol)
  (set! *bh* nrow)
  (set! *bw* ncol)
  (set! *bsz* (* nrow ncol))
  (set! *num-piece-types* (vector-length ptv)) ;; must come before positionify/(pre-compress)
  (set! *piece-types* (for/vector ([cell-specs ptv])
                                  (list->set cell-specs)));****
  (set! *start* (positionify s))
  (set! *target* t)
  )


;; pre-spaces: pre-position -> (listof cell)
;; extract the spaces *** expected to be at the end of the initialization lists ***
(define (pre-spaces p)
  (last p))

;; pre-compress: pre-position -> (listof (cons tile-id (listof cell)))
;; collapse pieces of the same type and give spaces their unique id of -1
(define (pre-compress p)
  (cons (cons 0 (pre-spaces p))
        (for/list ([i (in-range 1 *num-piece-types*)])
          (cons i
                (map cdr
                     (filter (lambda (a-piece) (= i (first a-piece))) (drop-right p 1)))))))

;; positionify: pre-position -> position
;; create a 'position' representation of a board state based on the given start-list pre-position format
(define (positionify pre-position)
  (for/vector ([pspec (sort (pre-compress pre-position) < #:key first)]
               [i (in-range *num-piece-types*)])
    (unless (= i (first pspec)) (error 'positionify "mis-matched piece-type in vector representation of position"))
    (sort (map cell-to-loc (cdr pspec)) <)))

;; cell-to-loc: cell -> int
;; convert ordered pair to row-major-order rank location
(define (cell-to-loc pair)
  (+ (* (first pair) *bw*) (second pair)))

;; loc-to-cell: int -> cell
(define (loc-to-cell i)
  (list (floor (/ i *bw*)) (modulo i *bw*)))


;;------------------------------------------------------------------------------------------------------
;; BLOCK-10 PUZZLE INIT (variant 12)
(define *block10-piece-types*
  '#('reserved-spaces        ; 0 reserved for spaces in actual position representation
     ((0 0)(0 1)(1 0)(1 1))  ; 1  2x2
     ((0 0)(0 1)(1 0))       ; 2  Upper Left pointing L
     ((0 0)(1 -1)(1 0))      ; 3  Lower Right pointing L
     ((0 0)(1 0))            ; 4  2x1 vertical rectangle
     ((0 0))))               ; 5  1x1 unit square

(define *block10-start* ; variant 12
  '((1 4 1)
    (2 3 0)
    (3 1 3)
    (4 1 0)
    (4 3 3)
    (5 2 1)
    (5 3 2)
    (5 5 0)
    (5 5 3)
    ((0 1) (0 2) (1 1) (1 2)) ; spaces
    ))

(define *block10-target* '((1 0 1)))

(define (block10-init)
  (set-em! *block10-piece-types* *block10-start* *block10-target* 6 4))

;;------------------------------------------------------------------------------------------------------
;; CLIMB-12 PUZZLE INIT
;; piece-type is implicit in position within list, each pair specifies the cells of the piece
;; and their location relative to the (arbitrary) origin of that piece, (0 0).
(define *climb12-piece-types*
  '#('reserved-spaces
     ((0 0)(1 -1)(1 0)(1 1))             ; 1  4 square T (stem up)
     ((0 0)(0 1)(1 0))                   ; 2  Upper Left pointing L
     ((0 0)(1 -1)(1 0))                  ; 3  Lower Right pointing L
     ((0 0)(1 0))                        ; 4  2x1 vertical rectangle
     ((0 0)(0 1))                        ; 5  1x2 horizontal rectangle
     ((0 0))))                           ; 6  1x1 unit square

;; specify board-state by triples: piece-type, board-row, board-col
(define *climb12-start*
  '((1 4 2)
    (2 2 1)
    (3 2 3)
    (4 1 0)
    (4 1 4)
    (5 4 0)
    (5 4 3)
    (6 3 0)
    (6 3 4)
    (6 5 0)
    (6 5 4)
    ((0 2) (1 1) (1 2) (1 3))  ; spaces
    ))

;; specify target as triple: piece-type, board-row, board-col
(define *climb12-target* '((1 0 2)))

(define (climb12-init)
  (set-em! *climb12-piece-types* *climb12-start* *climb12-target* 6 5))
;;------------------------------------------------------------------------------------------------------

