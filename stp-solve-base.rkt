#lang racket

(require
 srfi/25 ;; multi-dimensional arrays
 racket/fixnum
 racket/set
 test-engine/racket-tests
 racket/generator
 "stp-init.rkt"
 )
                  

(provide compile-ms-array!
         position<?
         lexi<?
         position-in-vec?
         expand
         is-goal?)


;; INITIALIZE STUFF FOR SLIDING-TILE-SOLVER

;; move trans for up, right, down and left respectively
(define *prim-move-translations* '((-1 0) (0 1) (1 0) (0 -1)))

;; move-schema-array for compiling move requirements
(define *ms-array* #f)(set! *ms-array* *ms-array*)


;; ******************************************************************************
;; DATA DEFINITIONS

;; a cell is a pair, (list r c), for row and column r and c

;; a location (loc for short) is an int, representing the row-major rank of a cell

;; a tile-spec is a triple, (cons a c), where a is the tile-type and c is the cell of the piece-type's origin

;; a tile-loc-spec (tlspec), is a (list t l), where t is the tile-type and l is the loc of that tile

;; a pre-position is a (append (listof tile-spec) (listof cell))

;; a old-position is a (vectorof (listof int))
;; where the index of the top-level vectors reflect the piece-type as given in the init,
;; and the ints in the secondary vectors are the SORTED locations of the pieces of that type

;; a bw-position is a (vector int)
;; where each int is a bitwise representation of the locations of the pieces of that type

;; a hc-position (hcpos for short) is a structure: (make-hc-position hc bwrep)
;; where hc is the equal-hash-code for the bw-position bwrep


;; ******************************************************************************

(define-struct hc-position (hc bs))
;; the hc is the hashcode of the bytestring

;; make-hcpos: bw-position -> hc-position
;; wrapper for the position rep augmented with the hashcode
(define (make-hcpos bwrep) (make-hc-position (equal-hash-code bwrep) bwrep))

;; hcposition<?: position position -> boolean
(define (hcposition<? p1 p2)
  (or (< (hc-position-hc p1) (hc-position-hc p2))
      (and (= (hc-position-hc p1) (hc-position-hc p2))
           (blexi<? (hc-position-bs p1) (hc-position-bs p2)))))

;; blexi<?: hc-position hc-position -> boolean
;; lexicographic fallback for hash collision
(define (blexi<? p1 p2)
  (do ([i 0 (add1 i)])
    ((or (= i (bytes-length p1))
         (not (= (bytes-ref p1 i) (bytes-ref p2 i))))
     (and (< i (bytes-length p1))
          (< (bytes-ref p1 i) (bytes-ref p2 i))))))


;;-------------------------------------------------------------------------------
;; COMMON UTILITIES TO BOTH GENERIC FRINGE-SEARCH AND CLUSTER-FRINGE-SEARCH


;; Set-like Operations on Lists

;; list-subtract: (listof X) (listof X) (X X -> boolean) -> (listof X)
;; ASSUME lists are sorted
(define (list-subtract l1 l2 comp?)
  (cond [(or (empty? l1) (empty? l2)) l1]
        [(equal? (first l1) (first l2)) (list-subtract (rest l1) l2 comp?)]
        [(comp? (first l1) (first l2)) (cons (first l1) (list-subtract (rest l1) l2 comp?))]
        [else (list-subtract l1 (rest l2) comp?)]))


;;------------------------------------------------------------------------------------------------------
;; Compiling move-schemas

;; a ms-array is an array indexed by piece-type, location, move-direction (0-3 starting with up)
;; where each location contains a move-schema produced by basic-move-schema

;; compile-ms-array!: (vectorof (setof cell)) int int -> void
;; where the vector is the piece-type specification, *piece-types*, and the ints are the width and height
(define (compile-ms-array! piece-type-specs bh bw)
  (when (or (zero? bh) (zero? bw)) (error 'compile-ms-array "must be called after an appropriate initialization call"))
  (let ((a (make-array (shape 1 (vector-length piece-type-specs) 0 (* bh bw) 0 4))))
    (for ([piece-type-spec (vector-drop piece-type-specs 1)]
          [pti (in-range 1 (vector-length piece-type-specs))])
      (for ([loc (in-range (* bh bw))])
        (for ([dir (in-range (length *prim-move-translations*))])
          (array-set! a pti loc dir
                      (if (andmap onboard? (translate-piece piece-type-spec (translate-cell (list-ref *prim-move-translations* dir) (loc-to-cell loc))))
                          (better-move-schema (cons pti (loc-to-cell loc)) (list-ref *prim-move-translations* dir))
                          #f)))))
    (set! *ms-array* a)))

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


;; better-move-schema: tile-spec trans-spec -> (list int int int)
;; a better-move-schema (better-ms) is a list:
;; first:   bit-rep of space preconditions
;; second:  xor of space preconditions and space postconditions ("changed-blanks": all that change)
;; third:   xor of current location and translated location (origin) of the piece
;; fourth:  new location of the moved tile's origin
(define (better-move-schema tile trans)
  (let* ([current-cell-list (translate-piece (vector-ref *piece-types* (first tile)) (cdr tile))]
         [current-loc-list (sort (map cell-to-loc current-cell-list) <)]
         [cell-list-to (translate-piece current-cell-list trans)]
         [loc-list-to (sort (map cell-to-loc cell-list-to) <)])
    (list (list->bwrep (list-subtract loc-list-to  current-loc-list <))
          (bitwise-xor (list->bwrep (list-subtract loc-list-to  current-loc-list <))
                       (list->bwrep (list-subtract current-loc-list loc-list-to <)))
          (list->bwrep (list (cell-to-loc (cdr tile))
                               (cell-to-loc (translate-cell (cdr tile) trans))))
          (cell-to-loc (translate-cell (cdr tile) trans)))))


;; lexi<?: position position -> boolean
;; lexicographic less-than test on two positions (that presumably have a primary hash collision)
#|
(check-expect (lexi<? #((1 2 3) (1 3) (1 2 3)) #((1 2 3) (1 3) (1 2 3))) #f)
(check-expect (lexi<? #((1 2 3) (2 3) (1 2 3)) #((1 2 3) (1 3) (1 2 3))) #f)
(check-expect (lexi<? #((1 2 3) (1 3) (1 2 3)) #((1 2 3) (2 3) (1 2 3))) #t)
|#
(define (lexi<? p1 p2)
  (for/first ([tile-types1 p1]
              [tile-types2 p2]
              #:when (not (equal? tile-types1 tile-types2)))
    (if (and (number? tile-types1) (number? tile-types2))
        (< tile-types1 tile-types2)
        (for/first ([tile1 tile-types1]
                    [tile2 tile-types2]
                    #:when (not (= tile1 tile2)))
          (< tile1 tile2)))))

;; position<?: position position -> boolean
(define (position<? p1 p2)
  (let ([hc1 (equal-hash-code p1)]
        [hc2 (equal-hash-code p2)])
    #|(when (and (not (equal? p1 p2))
               (fx= hc1 hc2))
      (printf "hash collision on ~a and ~a at ~a~%" p1 p2 hc1))|#
    (or (fx< hc1 hc2)
        (and (fx= hc1 hc2)
             (lexi<? p1 p2)))))

;; position-in-vec?: (vectorof position) position -> boolean
;; determine if given position is in vector of positions
(define (position-in-vec? v p)
  (vec-member? v p position<?))

;; find-pos-index: fixnum (vectorof position) -> int
;; find the index of the *FIRST* position (if present) or of the first position greater than where it would be
;; *** THIS IS NOT _EXACTLY_ CORRECT: assumes only used to pick responsibility-ranges
(define (find-pos-index pos-hashcode vop (low 0) (high (vector-length vop)))
  (let* ([mid (floor (/ (+ low high) 2))]
         [mid-hashcode (and (< mid (vector-length vop)) (equal-hash-code (vector-ref vop mid)))])
    (cond [(>= low high) low]
          [(fx= pos-hashcode mid-hashcode)
           (or (for/last ([index (in-range mid -1 -1)]
                          #:when (fx= (equal-hash-code (vector-ref vop index)) mid-hashcode))
                 index)
               0)]
          [(fx< pos-hashcode mid-hashcode) (find-pos-index pos-hashcode vop low mid)]
          [else (find-pos-index pos-hashcode vop (add1 mid) high)])))

;; vec-member?: (vectorof X) X (X X -> boolean) [int] [int] -> boolean
;; determine if the given item appears in the SORTED vector of positions
(define (vec-member? v x compare? [low 0] [high (vector-length v)])
  (let ((mid (floor (/ (+ low high) 2)))) 
    (cond [(>= low high) #f]
          [(equal? x (vector-ref v mid)) (vector-ref v mid)]
          [(compare? x (vector-ref v mid)) (vec-member? v x compare? low mid)]
          [else (vec-member? v x compare? (add1 mid) high)])))

;; make-new-move: position move-schema int -> position
;; create a new position from an existing position and a move schema
(define (make-new-move position mv-schema piece-type)
  (let ((new-vec (vector-copy position)))                          ; build the new position
    ;; update piece
    (vector-set! new-vec piece-type
                 (bitwise-xor (vector-ref new-vec piece-type)
                              (third mv-schema)))
    ;; update spaces
    (vector-set! new-vec 0
                 (bitwise-xor (vector-ref new-vec 0)
                              (second mv-schema)))
    new-vec))

;; bw-1pc-1step: int int bw-position (vectorof loc) (box int) -> (listof (pair loc bw-position))
;; for a given piece, identified by piece-type and location, generate all next-positions obtained by moving one-step (only)
;; but for each next-position, return a tile-loc/position pair for the given piece just moved
(define (bw-1pc-1step piece-type piece-loc position prior-locs expcount)
  (let ([mv-schema empty])
    (for ([dir-trans *prim-move-translations*]
          [dir-i (in-range (length *prim-move-translations*))]
          #:when (begin (set! mv-schema (array-ref *ms-array* piece-type piece-loc dir-i))
                        (and mv-schema
                             (not (vector-ref prior-locs (fourth mv-schema)))
                             (bw-valid-move? (vector-ref position 0) (first mv-schema)))))
      (vector-set! prior-locs (fourth mv-schema) #t)
      (vector-set! *expandpos* (unbox expcount) 
                   (cons (fourth mv-schema)      ; new location of the moved tile that gives rise to this position
                         (make-new-move position mv-schema piece-type)))
      (set-box! expcount (add1 (unbox expcount))))
    (unbox expcount)))

;; expand-piece: int int bw-position (box int) -> (listof bw-position)
;; for a given piece (identified by piece-type and location of tile of that type),
;; return the list or set of all new positions reachable by moving the given piece starting in the given position
(define (expand-piece piece-type piece-loc position expcount)
  (vector-set! *piecelocvec* piece-loc #t)
  (do ([start-check (unbox expcount) new-positions-to-check]
       ;; new-positions is a set of (list piece-loc bw-position) for this piece
       [new-positions-to-check (bw-1pc-1step piece-type piece-loc position *piecelocvec* expcount)
                               (for/last ([i (in-range start-check new-positions-to-check)])
                                 (bw-1pc-1step piece-type (car (vector-ref *expandpos* i)) (cdr (vector-ref *expandpos* i)) *piecelocvec* expcount))])
    ;; until no further moves of this piece
    ((= new-positions-to-check start-check))))

;; expand: bw-position -> (setof bw-position)
;; generate next states from this one
(define (expand s)
  (let ([expand-count (box 0)])
    (vector-fill! *expandpos* #f)
    (for ([piece-type (in-range 1 *num-piece-types*)])
      (for ([piece-loc (bwrep->list (vector-ref s piece-type))])
        (vector-fill! *piecelocvec* #f) ;reset for the next piece
        (expand-piece piece-type piece-loc s expand-count)))
    (for/set ([i (unbox expand-count)])
      (cdr (vector-ref *expandpos* i)))))



(define (bw-valid-move? space-int space-prereq)
  (= (bitwise-and space-int space-prereq)
     space-prereq))
  
;; onboard?: cell -> boolean
(define (onboard? c)
  (and (< -1 (first c) *bh*)
       (< -1 (second c) *bw*)))
;; loc-onboard?: loc -> boolean
(define (loc-onboard? loc)
  (< -1 loc *bsz*))

;;------------------------------------------------------------------------------------

;; is-goal?: position -> boolean
(define (is-goal? p)
  (andmap (lambda (tile-type-target-pair) 
            (positive? (bitwise-and (vector-ref p (first tile-type-target-pair))
                                    (second tile-type-target-pair))))
          *target*))

;; goal-in-fringe?: (setof position) -> position OR #f
(define (goal-in-fringe? f)
  (for/first ([pos f]
              #:when (is-goal? pos))
    pos))

;(block10-init)
(climb15-init)
(compile-ms-array! *piece-types* *bh* *bw*)
;(test)