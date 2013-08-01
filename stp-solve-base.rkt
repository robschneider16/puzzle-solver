#lang racket

(require srfi/25) ;; multi-dimensional arrays
(require "stp-init.rkt")
(require racket/fixnum)
(require racket/set)
(require mzlib/string)

(provide (all-defined-out))


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

;; a pre-position is a (append (listof tile-spec) (listof cell))

;; a position is a (vectorof (listof int))
;; where the index of the top-level vectors reflect the piece-type as given in the init,
;; and the ints in the secondary vectors are the SORTED locations of the pieces of that type
;; ******************************************************************************

;;-------------------------------------------------------------------------------
;; COMMON UTILITIES TO BOTH GENERIC FRINGE-SEARCH AND CLUSTER-FRINGE-SEARCH

;; Fringe Writing/Reading 

;; write-fringe-to-disk: (listof or vectorof position) string -> void
;; writes the positions from the given fringe (whether list or vector) into a file with given file-name
(define (write-fringe-to-disk fringe file-name)
  (let ([my-output (open-output-file file-name #:exists 'replace)])
    (for ([position fringe])
      (fprintf my-output "~a~%" position))
    (close-output-port my-output)))

;; read-fringe-from-disk: file -> fringe
;; reads a file from a file path (if you are in the current directory just simply the file-name)
;; and returns the fringe that was in that file.
(define (read-fringe-from-disk file-path)
  (with-input-from-file file-path port->list))

;; position-count-in-file: string -> number
;; reports the number of positions in the given fringe file assuming the file was written with write-fringe-to-disk
(define (position-count-in-file f)
  (read-from-string (with-output-to-string (lambda () (system (string-append "wc -l " f))))))


;; Set-like Operations on Lists

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

;; sorted-remove-dups: (listof X) -> (listof X)
(define (sorted-remove-dups lox)
  (cond [(or (empty? lox) (empty? (rest lox))) lox]
        [(equal? (first lox) (second lox)) (sorted-remove-dups (rest lox))]
        [else (cons (first lox) (sorted-remove-dups (cdr lox)))]))

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
                          (basic-move-schema (cons pti (loc-to-cell loc)) (list-ref *prim-move-translations* dir))
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

;; translate-spaces: (listof loc) move-schema -> (listof loc)
;; for a move-in-progress, create the new list of space cells for the given piece move
(define (translate-spaces spaces ms)
  (append (list-subtract spaces (third ms) <)
          (fourth ms)))

;***  move-schema has cells -- where used?


;; basic-move-schema: tile-spec trans-spec -> (list (listof loc) (listof loc) (listof loc) (listof loc) loc)
;; the resulting schema specifies:
;; 0. listof current locs occupied by piece
;; 1. list of locs occupied by piece after the translation in question
;; 2. list of precondition locs for spaces,
;; 3. list of post-condition locs for spaces after move, and
;; 4. the translated loc (origin) of the piece
;; This is only called by compile-ms-array! once before a solution of a particular puzzle so time here is less important
(define (basic-move-schema tile trans)
  (let* ((current-cell-list (translate-piece (vector-ref *piece-types* (first tile)) (cdr tile)))
         (current-loc-list (sort (map cell-to-loc current-cell-list) <))
         (cell-list-to (translate-piece current-cell-list trans))
         (loc-list-to (sort (map cell-to-loc cell-list-to) <)))
    (list current-loc-list
          loc-list-to
          (list-subtract loc-list-to  current-loc-list <)
          (list-subtract current-loc-list loc-list-to <)
          (cell-to-loc (translate-cell (cdr tile) trans)))))


;; position<?: position position -> boolean
(define (position<? p1 p2)
  (let ([hc1 (equal-hash-code p1)]
        [hc2 (equal-hash-code p2)])
    (or (fx< hc1 hc2)
        (and (fx= hc1 hc2)
             (fx< (equal-secondary-hash-code p1) (equal-secondary-hash-code p2))))))

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

;; spaces: position -> (listof cell)
;; return the list of space cells
(define (spaces p)
  (vector-ref p 0))

;; one-piece-one-step: tile-spec position (setof position) -> (setof (list tile-spec position))
;; for a given piece in a given position, generate all next-positions obtained by moving one-step (only)
;; but for each next-position, return a tile-spec/position pair for the tile-spec of the piece just moved
(define (one-piece-one-step tile-to-move position prior-pos)
  (let ((mv-schema empty)
        (tile-to-move-loc (cell-to-loc (cdr tile-to-move))))
    (for/set ([dir-trans *prim-move-translations*]
              [dir-i (in-range (length *prim-move-translations*))]
              #:when (begin (set! mv-schema (array-ref *ms-array* (first tile-to-move) tile-to-move-loc dir-i))
                            (and mv-schema
                                 (valid-move? (spaces position) (third mv-schema)))))
             (list
              (cons (first tile-to-move) (loc-to-cell (fifth mv-schema)))      ; tile-spec of the moved tile that gives rise to this position
              (let ((new-vec (vector-copy position)))                          ; build the new position
                (vector-set! 
                 new-vec (first tile-to-move)
                 (sort (cons (fifth mv-schema) (remove tile-to-move-loc (vector-ref new-vec (first tile-to-move))))
                       <))
                (vector-set! new-vec 0 (sort (translate-spaces (vector-ref new-vec 0) mv-schema) <))
                new-vec)))))

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


;; valid-move?: (listof loc) (listof loc) -> boolean
;; determine if the proposed move has the current-spaces in the right position
;; as required by the move-schema (INDEPENDENT OF if on the board)
(define (valid-move? current-space-locs prerequisite-spaces)
  (for/and ([needed-space prerequisite-spaces])
    (member needed-space current-space-locs)))
  
;; onboard?: cell -> boolean
(define (onboard? c)
  (and (< -1 (first c) *bh*)
       (< -1 (second c) *bw*)))
;; loc-onboard?: loc -> boolean
(define (loc-onboard? loc)
  (< -1 loc *bsz*))

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

