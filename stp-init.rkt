#lang racket

(provide (all-defined-out))

(define *num-piece-types* 0)
(define *piece-types* empty)
(define *start* empty)
(define *target* empty)
(define *bw* 0)
(define *bh* 0)
(define *bsz* 0)


;; set-em!: piece-type-vector pre-position-list target int int -> void
;; generic setter for use by puzzle-specific initialization functions
(define (set-em! ptv s t nrow ncol)
  (set! *bh* nrow)
  (set! *bw* ncol)
  (set! *bsz* (* nrow ncol))
  (set! *num-piece-types* (vector-length ptv)) ;; must come before positionify/(pre-compress)
  (set! *piece-types* (for/vector ([cell-specs ptv])
                                  (list->set cell-specs)));****
  ;(set! *start* (positionify s))
  (set! *start* (bw-positionify s))
  (set! *target* (for/list ([tile-spec t]) (list (first tile-spec) (list-to-bwrep (list (cell-to-loc (cdr tile-spec)))))))
  )

;; positionify: pre-position -> position
;; create a 'position' representation of a board state based on the given start-list pre-position format
(define (positionify pre-position)
  (for/vector ([pspec (sort (pre-compress pre-position) < #:key first)]
               [i (in-range *num-piece-types*)])
    (unless (= i (first pspec)) (error 'positionify "mis-matched piece-type in vector representation of position"))
    (sort (map cell-to-loc (cdr pspec)) <)))

;; bw-positionify: pre-position -> bw-position
;; create a bitwise-'position' representation of a board state based on the given start-list pre-position format
(define (bw-positionify pre-position)
  (for/vector ([pspec (pre-compress pre-position)]
               [i (in-range *num-piece-types*)])
    (unless (= i (first pspec)) (error 'positionify "mis-matched piece-type in vector representation of position"))
    (list-to-bwrep (map cell-to-loc (cdr pspec)))))

;; list-to-bwrep: (listof loc) -> int
;; convert the list of locations into a bitwise representation
(define (list-to-bwrep lo-loc)
  (foldl (lambda (a-loc bwint)
           (+ (arithmetic-shift 1 a-loc) bwint))
         0
         lo-loc))

;; bwrep-to-list: int -> (listof loc)
;; extract the locs encoded in the given int
(define (bwrep-to-list n)
  (for/list ([i (in-range (integer-length n))]
             #:when (bitwise-bit-set? n i))
    i))    


;; cell-to-loc: cell -> int
;; convert ordered pair to row-major-order rank location
(define (cell-to-loc pair)
  (+ (* (first pair) *bw*) (second pair)))

;; loc-to-cell: int -> cell
(define (loc-to-cell i)
  (list (floor (/ i *bw*)) (modulo i *bw*)))


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


;;------------------------------------------------------------------------------------------------------
;; BLOCK-10 PUZZLE INIT (variant 12)
(define *block10-piece-types*
  '#((reserved-spaces)        ; 0 reserved for spaces in actual position representation
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
  '#((reserved-spaces)
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
;; CLIMB-15 PUZZLE INIT
;; (variation 1: 104 moves)
(define *climb15-piece-types*
  '#((reserved-spaces)
     ((0 0)(1 -1)(1 0)(1 1))             ; 1  4 square T (stem up)
     ((0 0)(0 1)(1 0))                   ; 2  Upper Left pointing L
     ((0 0)(1 -1)(1 0))                  ; 3  Lower Right pointing L
     ((0 0)(1 0)(1 1))                   ; 4  Lower Left pointing L
     ((0 0)(0 1)(1 1))                   ; 5  Upper Right pointing L
     ((0 0)(1 0))                        ; 6  2x1 vertical rectangle
     ((0 0)(0 1))                        ; 7  1x2 horizontal rectangle
     ((0 0))                             ; 8  1x1 unit square
     ((0 0)(0 1)(1 0)(1 1))))            ; 9  2x2 square
     
(define *climb15-start*
  '((1 6 2)
    (2 2 0)
    (3 2 2)
    (4 4 2)
    (5 4 3)
    (6 2 3)
    (6 2 4)
    (7 6 0)
    (7 6 3)
    (8 1 0)
    (8 1 4)
    (8 7 0)
    (8 7 4)
    (9 4 0)
    ((0 2)(1 1)(1 2)(1 3))
    ))

(define *climb15-target* '((1 0 2)))

(define (climb15-init)
  (set-em! *climb15-piece-types* *climb15-start* *climb15-target* 8 5))

;(block10-init)