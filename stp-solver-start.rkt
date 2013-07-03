#lang racket

(require srfi/25) ;; multi-dimensional arrays

(define *piece-types* empty)
(define *start* empty)
(define *target* empty)
(define *bw* 0)
(define *bh* 0)
(define *bsz* 0)

(define (set-em! pt s t nrow ncol)
  (set! *piece-types* pt)
  (set! *start* s)
  (set! *target* t)
  (set! *bh* nrow)
  (set! *bw* ncol)
  (set! *bsz* (* nrow ncol)))

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
    (4 2 2)
    (4 3 3)
    (4 5 0)
    (4 5 3)
    ((0 1) (0 2) (1 1) (1 2)) ; spaces
    ))

(define *block10-target* '((0 0 1)))

(define (block10-init)
  (set-em! *block10-piece-types* *block10-start* *block10-target* 5 4))
           

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

;; a tile-spec is a triple, (cons a c), where a is the tile-type and c is the cell of the piece-type's origin

;; a position is a (listof tile-spec)

;; spaces: position -> (listof cell)
;; return the list of space cells
(define (spaces p)
  (last p))

;; translate-cell: cell trans-spec -> cell
(define (translate-cell c trans) 
  (list (+ (first c) (first trans)) (+ (second c) (second trans))))

;; translate-piece: (setof cells) trans-spec -> (setof cells)
(define (translate-piece cell-set trans)
  (for/set ([cell cell-set])
           (translate-cell cell trans)))

;; translate-spaces: (listof cell) move-schema -> (listof cell)
(define (translate-spaces spaces ms)
  (set->list (set-union (set-subtract (list->set spaces)
                                      (third ms))
                        (fourth ms))))

;; basic-move-schema: tile-spec trans-spec -> (list (setof cell) (setof cell) (setof cell) (setof cell) cell)
;; the resulting schema specifies the translation in question, the precondition spaces, and the post-condition spaces
(define (basic-move-schema tile trans)
  (let* ((current-cell-set (translate-piece (list->set (vector-ref *piece-types* (first tile))) (cdr tile)))
         (cell-set-to (translate-piece current-cell-set trans)))
    (list current-cell-set
          cell-set-to
          (set-subtract cell-set-to current-cell-set)
          (set-subtract current-cell-set cell-set-to)
          (translate-cell (cdr tile) trans))))

;; expand-piece: tile-spec position -> (listof position)
;; for a given tile-spec (piece), return the list of new positions resulting from moving the given piece in the given position
(define (expand-piece tile-to-move position)
  (filter (lambda (x) x)
          (for/list ([dir-trans *prim-move-translations*])
            (let* ((mv-schema (basic-move-schema tile-to-move dir-trans)))
              (if (valid-move? tile-to-move dir-trans (spaces position) mv-schema)
                  (for/list [(tile-spec position)] ; build the new position
                    (cond [(equal? (cdr tile-spec) (cdr tile-to-move)) (cons (first tile-spec) (fifth mv-schema))]
                          [(equal? tile-spec (spaces position)) (translate-spaces (spaces position) mv-schema)]
                          [else tile-spec]))
                  #f)))))

;; valid-move?: tile-spec trans-spec (listof cell) (list (listof cell) (listof cell) (setof cell) (setof cell)) -> boolean
;; determine if the proposed move is on the board and has the spaces in the right position
(define (valid-move? tile ts spaces ms)
  (let ((spaceset (list->set spaces))
        (current-cells (first ms))
        (moved-cells (second ms)))
    (and (for/and ([mv-cell moved-cells])
           (onboard? mv-cell)) 
         (for/and ([needed-space (third ms)])
           (member needed-space spaces)))))
  
;; onboard?: cell -> boolean
(define (onboard? c)
  (and (< -1 (first c) *bh*)
       (< -1 (second c) *bw*)))


(block10-init)

;; expand: position -> (listof state)
;; generate next states from this one
(define (expand s)
  (apply append
         (map (lambda (p) (expand-piece p s))
              (drop-right s 1))))


;; ******************** BELOW NOT INTEGRATED YET ************************

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
        [(member *target* current-fringe)
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