#lang racket

;(require srfi/25) ;; multi-dimensional arrays
(require "stp-init.rkt")

(provide (all-defined-out))

;;-------------------------------------------------------------------------------
;; COMMON UTILITIES TO BOTH GENERIC FRINGE-SEARCH AND CLUSTER-FRINGE-SEARCH


;; move trans for up, right, down and left respectively
(define *prim-move-translations* '((-1 0) (0 1) (1 0) (0 -1)))


;; ******************************************************************************
;; a cell is a pair, (list r c), for row and column r and c

;; a location (loc for short) is an int, representing the row-major rank of a cell

;; a tile-spec is a triple, (cons a c), where a is the tile-type and c is the cell of the piece-type's origin

;; a pre-position is a (append (listof tile-spec) (listof cell))

;; a position is a (vectorof (listof int))
;; where the index of the top-level vectors reflect the piece-type as given in the init,
;; and the ints in the secondary vectors are the SORTED locations of the pieces of that type
;; ******************************************************************************


;; position<?: position position -> boolean
(define (position<? p1 p2)
  (string<? (stringify p1) (stringify p2)))

;; cell<?: cell cell -> boolean
(define (cell<? c1 c2)
  (< (cell-to-loc c1) (cell-to-loc c2)))

;; position-in-vec?: (vectorof position) position -> boolean
;; determine if given position is in vector of positions
(define (position-in-vec? v p)
  (vec-member? v p position<?))

;; vec-member?: (vectorof X) X (X X -> boolean) [int] [int] -> boolean
;; determine if the given item appears in the SORTED vector of positions
(define (vec-member? v x compare? [low 0] [high (vector-length v)])
  (let ((mid (floor (/ (+ low high) 2)))) 
    (cond [(>= low high) #f]
          [(equal? x (vector-ref v mid)) (vector-ref v mid)]
          [(compare? x (vector-ref v mid)) (vec-member? v x compare? low mid)]
          [else (vec-member? v x compare? (add1 mid) high)])))

;; list-union: (listof X) (listof X) (X X -> boolean) -> (listof X)
;; ASSUME lists are sorted
(define (list-union l1 l2 comp?)
  (cond [(empty? l1) l2]
        [(empty? l2) l1]
        [(and (not (empty? (rest l1))) (equal? (first l1) (second l1))) (list-union (rest l1) (rest l2) comp?)]
        [(equal? (first l1) (first l2)) (list-union l1 (rest l2) comp?)]
        [(comp? (first l1) (first l2)) (cons (first l1)
                                             (list-union (rest l1) l2 comp?))]
        [else (cons (first l2) (list-union l1 (rest l2) comp?))]))

        
;; list-subtract: (listof X) (listof X) (X X -> boolean) -> (listof X)
;; ASSUME lists are sorted
(define (list-subtract l1 l2 comp?)
  (cond [(or (empty? l1) (empty? l2)) l1]
        [(equal? (first l1) (first l2)) (list-subtract (rest l1) l2 comp?)]
        [(comp? (first l1) (first l2)) (cons (first l1) (list-subtract (rest l1) l2 comp?))]
        [else (list-subtract l1 (rest l2) comp?)]))


;; spaces: position -> (listof cell)
;; return the list of space cells
(define (spaces p)
  (vector-ref p 0))

;; translate-loc: loc trans-spec -> loc
(define (translate-loc l trans)
  (cell-to-loc (translate-cell (loc-to-cell l) trans)))

;; translate-cell: cell trans-spec -> cell
(define (translate-cell c trans) 
  (list (+ (first c) (first trans)) (+ (second c) (second trans))))

;; translate-piece: (listof cells) trans-spec -> (listof cells)
(define (translate-piece cell-list trans)
  (for/list ([cell cell-list])
    (translate-cell cell trans)))

;; translate-spaces: (listof loc) move-schema -> (listof cell)
;; for a move-in-progress, create the new list of space cells for the given piece move
(define (translate-spaces spaces ms)
  (list-union (list-subtract spaces
                             (map cell-to-loc (third ms)) <)
              (map cell-to-loc (fourth ms))
              <))

;; basic-move-schema: tile-spec trans-spec -> (list (setof cell) (setof cell) (setof cell) (setof cell) cell)
;; the resulting schema specifies:
;; 0. listof current cells occupied by piece
;; 1. list of cells occupied by piece after the translation in question
;; 2. list of precondition cells for spaces,
;; 3. list of post-condition cells for spaces after move, and
;; 4. the translated cell (origin) of the piece
(define (basic-move-schema tile trans)
  (let* ((current-cell-list (sort (translate-piece (vector-ref *piece-types* (first tile)) (cdr tile)) cell<?))
         (cell-list-to (sort (translate-piece current-cell-list trans) cell<?)))
    (list current-cell-list
          cell-list-to
          (list-subtract cell-list-to current-cell-list cell<?)
          (list-subtract current-cell-list cell-list-to cell<?)
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
              (for/vector ([tile-type (in-range *num-piece-types*)] ; build the new position
                           [tile-type-locs position]) 
                (cond [(= tile-type (first tile-to-move))
                       (sort (cons (cell-to-loc (fifth mv-schema))
                                   (remove (cell-to-loc (cdr tile-to-move)) tile-type-locs)) <)]
                      [(= tile-type 0) (translate-spaces tile-type-locs mv-schema)]
                      [else tile-type-locs]))))))

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


;; valid-move?: tile-spec trans-spec (listof loc) move-schema -> boolean
;; determine if the proposed move is on the board and has the spaces in the right position
(define (valid-move? tile ts space-locs ms)
  (let ((current-cells (first ms))
        (moved-cells (second ms)))
    (and (for/and ([mv-cell moved-cells])
           (onboard? mv-cell)) 
         (for/and ([needed-space (third ms)])
           (member (cell-to-loc needed-space) space-locs)))))
  
;; onboard?: cell -> boolean
(define (onboard? c)
  (and (< -1 (first c) *bh*)
       (< -1 (second c) *bw*)))

;; expand: position -> (setof position)
;; generate next states from this one
(define (expand s)
  (for/fold ([all-moves (set)])
    ([piece-type (in-range 1 *num-piece-types*)])
    (set-union all-moves
               (for/fold ([new-moves (set)])
                 ([ploc (vector-ref s piece-type)])
                 (set-union new-moves
                            (expand-piece (cons piece-type (loc-to-cell ploc)) s))))))


;;------------------------------------------------------------------------------------

;; is-goal?: position -> boolean
(define (is-goal? p)
  (andmap (lambda (tile-spec) (member (cell-to-loc (cdr tile-spec)) (vector-ref p (first tile-spec))))
          *target*))

;; goal-in-fringe?: (setof position) -> position OR #f
(define (goal-in-fringe? f)
  (for/first ([pos f]
              #:when (is-goal? pos))
    pos))

;; stringify: position -> string
;; general make-string from position
(define (stringify position)
  (for/fold ([res ""])
    ([i (in-range *num-piece-types*)])
    (string-append res 
                   (number->string i) ":"
                   (apply string-append
                          (map (lambda (n) (string-append "," (number->string n)))
                               (vector-ref position i))) ;; SORTING THIS SHOULD NOT BE NECESSARY ANYMORE SINCE KEEPING LOCATIONS SORTED
                   ";")))

