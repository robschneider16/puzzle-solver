#lang racket

(require
 srfi/25 ;; multi-dimensional arrays
 racket/fixnum
 racket/set
 ;test-engine/racket-tests
 ;racket/generator
 "stp-init.rkt"
 "stp-solve-base.rkt"
 )
                  

;(provide )



;;------------------------------------------------------------------------------------------------------
;; Support for creating an index from blank configurations to valid move schemas

; create 
(define (all-space-config)
  (for*/fold ([r empty])
             ([b1 (- *bsz* 3)]
              [b2 (in-range (add1 b1) (- *bsz* 2))]
              [b3 (in-range (add1 b2) (- *bsz* 1))]
              [b4 (in-range (add1 b3) *bsz*)]
              #:when (andmap onboard? (map loc-to-cell (list b1 b2 b3 b4))))
             (append (one-space-config (list->bwrep (list b1 b2 b3 b4))) r)))


;; extract moves for given space configuration
(define (one-space-config spaceint)
  (let ([plocvec (make-vector *bsz* #f)])
    (for/fold ([r empty])
      ([ptype (in-range 1 (vector-length *piece-types*))])
      (append (for/fold ([r empty])
                ([loc *bsz*])
                (append (for/list
                            ([dir 4]
                             ;(vector-fill plocvec #f)
                             #:when (can-move spaceint ptype loc dir plocvec))
                          (build-even-better-move-schema spaceint ptype loc dir))
                        r))
              r))))

;; can-move : fixnum N N (vectorof boolean) -> boolean
;; determine if the proposed move can work
(define (can-move spaceint ptype loc dir plocvec)
  (let ([ms (array-ref *ms-array* ptype loc dir)])
    (and ms ;; the retrieved move-schema non-false
         ;; loc is on the board
         (not (member loc *invalid-locs*))
         ; have we been there already
         (not (vector-ref plocvec (fourth ms)))
         ; piece-on-board
         (andmap onboard? 
                 (for/list ([cell (translate-piece (vector-ref *piece-types* ptype) (loc-to-cell loc))]) cell))
         ; is the move valid
         (bw-valid-move? spaceint (first ms))
         ;; 3. does the piece not overlay with the spaces
         (not-on-blanks ms spaceint ptype loc dir)
    )))

(define (not-on-blanks ms spaceint ptype loc dir)
  (let ([pcells (vector-ref *piece-types* ptype)])
    (zero? (bitwise-and spaceint (list->bwrep (for/list ([cell (translate-piece pcells (loc-to-cell loc))]) (cell-to-loc cell)))))))

;; build-even-better-move-schema: fixnum N N N -> (listof even-better-move-schema)
;; find the possible moves for this combination
;; *** Still need to do subsequent steps if the given piece-type could keep moving
(define (build-even-better-move-schema spaceint ptype loc dir)
  (list spaceint ptype loc dir))

;; even-better-move-schema: 
;; an even better move-schema (e-better-ms) is a list:
;; first:   xor of space preconditions and space postconditions ("changed-blanks": all that change)
;; second:  xor of current location and translated location (origin) of the piece
;; third:   new location of the moved tile's origin
(define (even-better-move-schema tile trans)
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


;; update-expandbuf!: int bytestring int int move-schema int -> void
;; update the *expandbuf* vector with the new location and altered bytestring
(define (update-expandbuf!* bufindex src-bspos nu-ploc space-int mv-schema piece-type)
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
(define (hc-1pc-1step* piece-type loc-pos-pair expcount plocvec)
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
(define (expand-piece* piece-type piece-loc position expcount-b piecelocvec)
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

;; expand: hc-position int -> int
;; generate next states from this one
;; *expandbuf*: holds all the pairs of piece-loc and bytstring expansions/successors of the given hc-position
;; *piecelocvec*: holds the bytestring successors of a single piece
;; expand-count: counts expansions/successors for a single piece
(define expand*
  (local ([define expand-count (box 0)]
          )
    (lambda (hc-s exp-ptr)
      (let ([bs (hc-position-bs hc-s)]
            [target-hc-pos 'mutable-hc-pos-in-*expansion-space*])
        (set-box! expand-count 0)
        (for ([i (in-range *num-spaces* *num-pieces*)]) ;; start after spaces
          (let ([ptype (vector-ref *bs-ptype-index* i)]
                [ploc (- (bytes-ref bs i) *charify-offset*)])
            (vector-fill! *piecelocvec* #f) ; reset for next piece
            (expand-piece ptype ploc bs expand-count *piecelocvec*)
            ))
        (for ([i (unbox expand-count)])
          (set! target-hc-pos (vector-ref *expansion-space* (+ exp-ptr i)))
          (set-hc-position-hc! target-hc-pos (equal-hash-code (mcdr (vector-ref *expandbuf* i))))
          ;; copy bytes to the *expansion-space*
          (bytes-copy! (hc-position-bs target-hc-pos) 0 (mcdr (vector-ref *expandbuf* i))))
        (+ exp-ptr (unbox expand-count))))))

(block10-init)   ;  176890 prospective even-better-move-schema
;(climb15-init)   ; 2280811
(compile-ms-array! *piece-types* *bh* *bw*)
;(expand *start*)
;(test)