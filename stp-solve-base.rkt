#lang racket/base

(require
 srfi/25 ;; multi-dimensional arrays
 racket/list
 racket/vector
 ;test-engine/racket-tests
 ;racket/generator
 "stp-init.rkt"
 )
                  
#|
(provide hcposition<?
         blexi<?
         *ms-array*
         compile-ms-array!
         position-in-vec?
         expand
         is-goal?
         seconds->time)
|#
(provide (all-defined-out))

;; hcposition<?: hc-position hc-position -> boolean
(define (hcposition<? p1 p2)
  (or (< (hc-position-hc p1) (hc-position-hc p2))
      (and (= (hc-position-hc p1) (hc-position-hc p2))
           (bytes<? (hc-position-bs p1) (hc-position-bs p2)))))

;; blexi<?: hc-position hc-position -> boolean
;; lexicographic fallback for hash collision
(define (blexi<? p1 p2)
  (bytes<? (hc-position-bs p1) (hc-position-bs p2)))


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

;; move-schema-array for compiling move requirements
(define *ms-array* #f)(set! *ms-array* *ms-array*)


;; compile-ms-array!: (vectorof (setof cell)) int int -> void
;; where the vector is the piece-type specification, *piece-types*, and the ints are the width and height
(define (compile-ms-array! piece-type-specs bh bw)
  (when (or (zero? bh) (zero? bw)) (error 'compile-ms-array "must be called after an appropriate initialization call"))
  (let ((a (make-array (shape 1 (vector-length piece-type-specs) 0 *bsz* 0 4))))
    (for ([piece-type-spec (vector-drop piece-type-specs 1)]
          [pti (in-range 1 (vector-length piece-type-specs))])
      (for ([loc *bsz*])
        (for ([dir (in-range (length *prim-move-translations*))]
              [dir-trans *prim-move-translations*])
          (let* ([loc-cell (loc-to-cell loc)]
                 [start-spots (translate-piece piece-type-spec loc-cell)]
                 [moved-spots (translate-piece piece-type-spec (translate-cell loc-cell dir-trans))])
            (array-set! a pti loc dir
                        (if (and (andmap onboard? start-spots)
                                 (andmap onboard? moved-spots))
                            (better-move-schema (loc-to-cell loc) dir-trans start-spots moved-spots)
                            #f))))))
    (set! *ms-array* a)))

;; translate-loc: loc trans-spec -> loc
(define (translate-loc l trans)
  (cell-to-loc (translate-cell (loc-to-cell l) trans)))

;; translate-cell: cell trans-spec -> cell
;; given a trans-spec as (delta-row . delta-col) pair, return a new pair
(define (translate-cell c trans) 
  (cons (+ (car c) (car trans)) (+ (cdr c) (cdr trans))))

;; translate-piece: (listof cells) trans-spec -> (listof cells)
(define (translate-piece cell-list trans)
  (for/list ([cell cell-list])
    (translate-cell cell trans)))


;; better-move-schema: cell trans-spec (listof cell) (listof cell) -> (list int int int)
;; a better-move-schema (better-ms) is a list:
;; first:   bit-rep of space preconditions
;; second:  xor of space preconditions and space postconditions ("changed-blanks": all that change)
;; third:   xor of current location and translated location (origin) of the piece
;; fourth:  new location of the moved tile's origin
(define (better-move-schema cell trans start-cell-list moved-cell-list)
  (let* ([current-loc-list (sort (map cell-to-loc start-cell-list) <)]
         [loc-list-to (sort (map cell-to-loc moved-cell-list) <)])
    (list (list->bwrep (list-subtract loc-list-to  current-loc-list <))
          (bitwise-xor (list->bwrep (list-subtract loc-list-to  current-loc-list <))
                       (list->bwrep (list-subtract current-loc-list loc-list-to <)))
          (list->bwrep (list (cell-to-loc cell)
                             (cell-to-loc (translate-cell cell trans))))
          (cell-to-loc (translate-cell cell trans)))))

;; position-in-vec?: (vectorof position) position -> boolean
;; determine if given position is in vector of positions
(define (position-in-vec? v p)
  (vec-member? v p hcposition<?))

;; position-in-hash?: (HashTableof position) position -> boolean
;; determine if given position is in HashTable of positions
(define (position-in-hash? h p)
  (hash-ref h (hc-position-hc p) #f))

;; find-pos-index: fixnum (vectorof position) -> int
;; find the index of the *FIRST* position (if present) or of the first position greater than where it would be
;; *** THIS IS NOT _EXACTLY_ CORRECT: assumes only used to pick responsibility-ranges
(define (find-pos-index pos-hashcode vop (low 0) (high (vector-length vop)))
  (error "find-pos-index: cannot process hc-positions")
  (let* ([mid (floor (/ (+ low high) 2))]
         [mid-hashcode (and (< mid (vector-length vop)) (equal-hash-code (vector-ref vop mid)))])
    (cond [(>= low high) low]
          [(= pos-hashcode mid-hashcode)
           (or (for/last ([index (in-range mid -1 -1)]
                          #:when (= (equal-hash-code (vector-ref vop index)) mid-hashcode))
                 index)
               0)]
          [(< pos-hashcode mid-hashcode) (find-pos-index pos-hashcode vop low mid)]
          [else (find-pos-index pos-hashcode vop (add1 mid) high)])))

;; vec-member?: (vectorof hc-position) hc-position (hc-position hc-position -> boolean) [int] [int] -> boolean
;; determine if the given item appears in the SORTED vector of positions
(define (vec-member? v x compare? [low 0] [high (vector-length v)])
  (let ((mid (floor (/ (+ low high) 2)))) 
    (cond [(>= low high) #f]
          [(= (hc-position-hc x) (hc-position-hc (vector-ref v mid))) (vector-ref v mid)]
          [(compare? x (vector-ref v mid)) (vec-member? v x compare? low mid)]
          [else (vec-member? v x compare? (add1 mid) high)])))


;; hash-member?: (hashtableof hc-position) hc-position (hc-position hc-position -> boolean) [int] [int] -> boolean
;; determine if the given item appears in the SORTED hashtable of positions
(define (hash-member? h x compare? [low 0] [high (hash-count h)])
  (let ((mid (floor (/ (+ low high) 2)))
        (v (list->vector (hash-values h)))) 
    (cond [(>= low high) #f]
          [(= (hc-position-hc x) (hc-position-hc (vector-ref v mid))) (vector-ref v mid)]
          [(compare? x (vector-ref v mid)) (vec-member? v x compare? low mid)]
          [else (vec-member? v x compare? (add1 mid) high)])))



;; update-expandbuf!: int bytestring int int move-schema int -> void
;; update the *expandbuf* vector with the new location and altered bytestring
(define (update-expandbuf! bufindex src-bspos nu-ploc space-int mv-schema piece-type)
  (let* ([the-pair (vector-ref  *expandbuf* bufindex)]
         [targetbs (mcdr the-pair)]
         [piece-start (for/sum ([i piece-type]) (vector-ref *piece-type-template* i))]
         [piece-end (+ piece-start (vector-ref *piece-type-template* piece-type))]
         )
    (set-mcar! the-pair nu-ploc)
    ;; set spaces
    (bytes-copy! targetbs 0 
                 (charify-int (bitwise-xor space-int ;; do the spaces at the front
                                           (second mv-schema))))
    ;; copy unchanged
    (bytes-copy! targetbs *num-spaces* src-bspos *num-spaces* piece-start)
    ;; set moved piece
    (bytes-copy! targetbs piece-start
                 (charify-int (bitwise-xor (intify src-bspos piece-start piece-end)
                                           (third mv-schema))))
    ;; copy remaining
    (bytes-copy! targetbs piece-end
                 src-bspos piece-end)))

;; hc-1pc-1step: int (pair int bytestring) (vectorof loc) (box int) -> int
;; for a given piece, identified by piece-type and location, generate all next-positions obtained by moving one-step (only)
;; but for each next-position, return a tile-loc/position pair for the given piece just moved
(define (hc-1pc-1step piece-type loc-pos-pair expcount plocvec)
  (let* ([mv-schema empty]
         [space-int (intify (mcdr loc-pos-pair) 0 *num-spaces*)])
    (for ([dir-trans *prim-move-translations*]
          [dir-i (in-range (length *prim-move-translations*))]
          #:when (begin (set! mv-schema (array-ref *ms-array* piece-type (mcar loc-pos-pair) dir-i))
                        (and mv-schema
                             (not (vector-ref plocvec (fourth mv-schema)))
                             (bw-valid-move? space-int (first mv-schema)))))
      
      (update-expandbuf! (unbox expcount) (mcdr loc-pos-pair)
                         (fourth mv-schema) ; new location of the moved tile that gives rise to this position
                         space-int mv-schema piece-type) ; new bytestring position stored at new location
      (vector-set! plocvec (fourth mv-schema) #t)
      (set-box! expcount (add1 (unbox expcount))))
    (unbox expcount)))

;; expand-piece: int int bs-position (box int) (vectorof boolean) -> void
;; for a given piece (identified by piece-type and location of tile of that type), and given position,
;; expand and generate all successors, writing them as side-effect into the *bsbuffer*
(define (expand-piece piece-type piece-loc position expcount-b piecelocvec)
  (vector-set! piecelocvec piece-loc #t)
  (do ([start-check (unbox expcount-b) new-positions-to-check];; position-wise index into *bsbuffer*
       ;; new-positions is the index past the last of the new successors that were generated and need to be checked
       [new-positions-to-check (hc-1pc-1step piece-type (mcons piece-loc position) expcount-b piecelocvec)
                               (for/last ([i (in-range start-check new-positions-to-check)])
                                 (hc-1pc-1step piece-type 
                                               (vector-ref *expandbuf* i)
                                               expcount-b piecelocvec))])
    ;; until no further moves of this piece
    ((= new-positions-to-check start-check))))


;; bw-valid-move?: number number -> boolean
;; determine if the current location of the spaces supports a move's prerequisites given as space-prereq
(define (bw-valid-move? space-int space-prereq)
  (= (bitwise-and space-int space-prereq)
     space-prereq))
  
;; onboard?: cell -> boolean
(define (onboard? c)
  (and (< -1 (car c) *bh*)
       (< -1 (cdr c) *bw*)
       (cell-to-loc c)))
;; loc-onboard?: loc -> boolean
(define (loc-onboard? loc)
  (< -1 loc *bsz*))

;;------------------------------------------------------------------------------------

;; is-goal?: hc-position -> boolean
;;****** relies on special-case of goal where single tile of type with only one tile needs to be in certain location
(define (is-goal? hcp)
  ;(and #f
       (= (bytes-ref (hc-position-bs hcp) (car *target*))
          (cdr *target*))
  ;)
)

;; goal-in-fringe?: (setof position) -> position OR #f
(define (goal-in-fringe? f)
  (for/first ([pos f]
              #:when (is-goal? pos))
    pos))

;; seconds->time: int -> string
;; format the given number of seconds as hours (if non-zero), minutes (if non-zero), and seconds
(define (seconds->time ts)
  (let* ([hrs (floor (/ ts 3600))]
         [min (floor (/ (- ts (* hrs 3600)) 60))]
         [sec (- ts (* hrs 3600) (* min 60))])
    (cond [(and (zero? hrs) (zero? min)) (format "~a sec." sec)]
          [(and (zero? hrs) (positive? min)) (format "~a min., ~a sec." min sec)]
          [else (format "~a hrs., ~a min., ~a sec." hrs min sec)])))


;(block10-init)
;(climb15-init)
;(compile-ms-array! *piece-types* *bh* *bw*)
;(expand *start*)
;(test)