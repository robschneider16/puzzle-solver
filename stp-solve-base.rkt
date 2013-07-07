#lang racket

;(require srfi/25) ;; multi-dimensional arrays

(provide (all-defined-out))


(define *num-pieces* 0)
(define *piece-types* empty)
(define *start* empty)
(define *target* empty)
(define *bw* 0)
(define *bh* 0)
(define *bsz* 0)


(define (set-em! ptv s t nrow ncol)
  (set! *num-pieces* (vector-length ptv)) ;; must come before hashify/(pre-compress)
  (set! *piece-types* (for/vector ([cell-specs ptv])
                                  (list->set cell-specs)))
  (set! *start* (positionify s))
  (set! *target* t)
  (set! *bh* nrow)
  (set! *bw* ncol)
  (set! *bsz* (* nrow ncol)))

;; pre-spaces: pre-position -> (listof cell)
;; extract the spaces *** expected to be at the end of the initialization lists ***
(define (pre-spaces p)
  (last p))

;; pre-compress: pre-position -> (listof (cons tile-id (listof cell)))
;; collapse pieces of the same type and give spaces their unique id of -1
(define (pre-compress p)
  (cons (cons -1 (pre-spaces p))
        (for/list ([i (in-range *num-pieces*)])
          (cons i
                (map cdr
                     (filter (lambda (a-piece) (= i (first a-piece))) (drop-right p 1)))))))

;; positionify: pre-position -> position
;; create a hash-of-set representation for positions from start-list pre-position format
(define (positionify position)
  (for/hash ([pspec (pre-compress position)])
    (values (first pspec)
            (for/set ([cell (cdr pspec)])
                     cell))))


;; BLOCK-10 PUZZLE INIT (variant 12)
(define *block10-piece-types*
  '#(((0 0)(0 1)(1 0)(1 1))  ; 0 2x2
     ((0 0)(0 1)(1 0))       ; 1  Upper Left pointing L
     ((0 0)(1 -1)(1 0))      ; 2  Lower Right pointing L
     ((0 0)(1 0))            ; 3  2x1 vertical rectangle
     ((0 0))))               ; 4  1x1 unit square

(define *block10-start* ; variant 12
  '((0 4 1)
    (1 3 0)
    (2 1 3)
    (3 1 0)
    (3 3 3)
    (4 2 1)
    (4 3 2)
    (4 5 0)
    (4 5 3)
    ((0 1) (0 2) (1 1) (1 2)) ; spaces
    ))

(define *block10-target* '((0 0 1)))

(define (block10-init)
  (set-em! *block10-piece-types* *block10-start* *block10-target* 6 4))

(define (block10-stringify position)
  (let ((names (vector "spcs" "gpc" "ifL" "bwL" "2x1" "1x1")))
    (for/fold ([res ""])
      ([i '(5 4 3 1 2 0)])
      (string-append res 
                     (vector-ref names i) ":"
                     (apply string-append
                            (map (lambda (n) (string-append "," (number->string n)))
                                 (sort (for/list ([c (hash-ref position (sub1 i))])
                                         (cell-to-loc c))
                                       <)))
                     ";"))))


;; CLIMB-12 PUZZLE INIT
;; piece-type is implicit in position within list, each pair specifies the cells of the piece
;; and their location relative to the (arbitrary) origin of that piece, (0 0).
(define *climb12-piece-types*
  '#(((0 0)(1 -1)(1 0)(1 1))             ; 0  4 square T (stem up)
     ((0 0)(0 1)(1 0))                   ; 1  Upper Left pointing L
     ((0 0)(1 -1)(1 0))                  ; 2  Lower Right pointing L
     ((0 0)(1 0))                        ; 3  2x1 vertical rectangle
     ((0 0)(0 1))                        ; 4  1x2 horizontal rectangle
     ((0 0))))                           ; 5  1x1 unit square

;; specify board-state by triples: piece-type, board-row, board-col
(define *climb12-start*
  '((0 4 2)
    (1 2 1)
    (2 2 3)
    (3 1 0)
    (3 1 4)
    (4 4 0)
    (4 4 3)
    (5 3 0)
    (5 3 4)
    (5 5 0)
    (5 5 4)
    ((0 2) (1 1) (1 2) (1 3))  ; spaces
    ))

;; specify target as triple: piece-type, board-row, board-col
(define *climb12-target* '((0 0 2)))

(define (climb12-init)
  (set-em! *climb12-piece-types* *climb12-start* *climb12-target* 6 5))

;;-----------------


;; move trans for up, right, down and left respectively
(define *prim-move-translations* '((-1 0) (0 1) (1 0) (0 -1)))


;; a cell is a pair, (list r c), for row and column r and c

;; a location (loc for short) is an int, representing the row-major rank of a cell

;; a tile-spec is a triple, (cons a c), where a is the tile-type and c is the cell of the piece-type's origin

;; a pre-position is a (append (listof tile-spec) (listof cell))

;; a position is a (hash [tile-type : (listof cell)])
;; ******************************************************************************
;; a NEW-position is (or will be) a (vectorof (vectorof int))
;; where the index of the top-level vectors reflect the piece-type as given in the init,
;; and the ints in the secondary vectors are the SORTED locations of the pieces of that type
;; ******************************************************************************


;; cell-to-loc: cell -> int
;; convert ordered pair to row-major-order rank location
(define (cell-to-loc pair)
  (+ (* (first pair) *bw*) (second pair)))

;; loc-to-cell: int -> cell
(define (loc-to-cell i)
  (list (floor (/ i *bw*)) (modulo i *bw*)))

;; spaces: position -> (listof cell)
;; return the list of space cells
(define (spaces p)
  (hash-ref p -1))

;; translate-cell: cell trans-spec -> cell
(define (translate-cell c trans) 
  (list (+ (first c) (first trans)) (+ (second c) (second trans))))

;; translate-piece: (setof cells) trans-spec -> (setof cells)
(define (translate-piece cell-set trans)
  (for/set ([cell cell-set])
           (translate-cell cell trans)))

;; translate-spaces: (setof cell) move-schema -> (setof cell)
;; for a move-in-progress, create the new set of space cells for the given piece move
(define (translate-spaces spaces ms)
  (set-union (set-subtract spaces
                           (third ms))
             (fourth ms)))

;; basic-move-schema: tile-spec trans-spec -> (list (setof cell) (setof cell) (setof cell) (setof cell) cell)
;; the resulting schema specifies the translation in question, the precondition spaces, and the post-condition spaces
(define (basic-move-schema tile trans)
  (let* ((current-cell-set (translate-piece (vector-ref *piece-types* (first tile)) (cdr tile)))
         (cell-set-to (translate-piece current-cell-set trans)))
    (list current-cell-set
          cell-set-to
          (set-subtract cell-set-to current-cell-set)
          (set-subtract current-cell-set cell-set-to)
          (translate-cell (cdr tile) trans))))

;; one-piece-one-step: tile-spec position (setof position) -> (setof (list tile-spec position))
;; for a given piece in a given position, generate all next-positions obtained by moving one-step (only)
;; but for each next-position, return a tile-spec/position pair for the tile-spec of the piece just moved
(define (one-piece-one-step tile-to-move position prior-pos)
  (let ((mv-schema empty))
    (for/set ([dir-trans *prim-move-translations*]
              #:when (begin (set! mv-schema (basic-move-schema tile-to-move dir-trans))
                            (valid-move? tile-to-move dir-trans (spaces position) mv-schema)))
             (list
              (cons (first tile-to-move) (fifth mv-schema))      ; tile-spec of the moved tile that gives rise to this position
              (for/hash ([(tile-type tile-type-cells) position]) ; build the new position
                (values
                 tile-type
                 (cond [(= tile-type (first tile-to-move))
                        (set-add (set-remove tile-type-cells (cdr tile-to-move))
                                 (fifth mv-schema))]
                       [(= tile-type -1) (translate-spaces tile-type-cells mv-schema)]
                       [else tile-type-cells])))))))

;; expand-piece: tile-spec position -> (setof position)
;; for a given tile-spec (piece), return the list of new positions resulting from moving the given piece in the given position
(define (expand-piece tile-to-move position)
  (do ([return-positions (set position) 
                         (set-union return-positions (for/set ([ts-pos new-positions]) (second ts-pos)))]
       [new-positions (one-piece-one-step tile-to-move position (set))
                      (for/fold ([new-add (set)])
                        ([ts-pos new-positions]
                         #:unless (set-member? return-positions (second ts-pos)))
                        (set-union new-add (one-piece-one-step (first ts-pos) (second ts-pos) return-positions)))])
    ((set-empty? new-positions) (set-remove return-positions position))))


;; valid-move?: tile-spec trans-spec (setof cell) (list (listof cell) (listof cell) (setof cell) (setof cell)) -> boolean
;; determine if the proposed move is on the board and has the spaces in the right position
(define (valid-move? tile ts spaces ms)
  (let ((current-cells (first ms))
        (moved-cells (second ms)))
    (and (for/and ([mv-cell moved-cells])
           (onboard? mv-cell)) 
         (for/and ([needed-space (third ms)])
           (set-member? spaces needed-space)))))
  
;; onboard?: cell -> boolean
(define (onboard? c)
  (and (< -1 (first c) *bh*)
       (< -1 (second c) *bw*)))


;; expand: position -> (setof state)
;; generate next states from this one
(define (expand s)
  (for/fold ([all-moves (set)])
    ([(piece-type cells) s]
     #:unless (= piece-type -1))
    (set-union all-moves
               (for/fold ([new-moves (set)])
                 ([pcell cells])
                 (set-union new-moves
                            (expand-piece (cons piece-type pcell) s))))))


;;------------------------------------------------------------------------------------

;; is-goal?: position -> boolean
(define (is-goal? p)
  (andmap (lambda (tile-spec) (set-member? (hash-ref p (first tile-spec)) (cdr tile-spec)))
          *target*))
  
;; goal-in-fringe?: (setof position) -> position OR #f
(define (goal-in-fringe? f)
  (for/first ([pos f]
              #:when (is-goal? pos))
    pos))

;; stringify: position -> string
;; general make-string from position
(define (stringify position)
  (let ((sorted-keys (sort (hash-keys position) <)))
    (for/fold ([res ""])
      ([i sorted-keys])
      (string-append res 
                     (number->string i) ":"
                     (apply string-append
                            (map (lambda (n) (string-append "," (number->string n)))
                                 (sort (for/list ([c (hash-ref position i)])
                                         (cell-to-loc c))
                                       <)))
                     ";"))))