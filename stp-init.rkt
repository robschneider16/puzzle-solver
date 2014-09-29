#lang typed/racket

(require srfi/25 ;; multi-dimensional arrays
         racket/list
         racket/set
         )

(provide EXPAND-SPACE-SIZE
         hc-position hc-position-hc hc-position-bs hc-position? set-hc-position-hc!
         make-hcpos
         *prim-move-translations* *charify-offset* *max-board-size*
         *puzzle-name*
         *invalid-cells*
         *num-piece-types* *piece-types* *num-pieces*
         *bs-ptype-index*
         *target* *bw* *bh* *bsz*
         ;*expandpos*
         *expandbuf*
         *expansion-space*
         *piecelocvec* 
         *start*
         *piece-type-template*
         *num-spaces*
         charify charify-int decharify intify
         ;old-positionify ;** temp for testing
         list->bwrep ;; used only during initialization in compile-ms-array! via better-move-schema
         bwrep-direct
         bwrep->list
         ;bwrep->list
         cell-to-loc *cell-to-loc*
         loc-to-cell *loc-to-cell*
         block10-init
         climb12-init
         climb15-init
         climbpro24-init)


;; ******************************************************************************
;; DATA DEFINITIONS

;; a cell is a pair, (list r c), for row and column r and c
(define-type Cell (Pairof Byte Byte))

;; a location (loc for short) is an int, representing the row-major rank of a cell
(define-type Loc Byte)

;; a tile-spec is a triple, (cons a c), where a is the tile-type and c is the cell of the piece-type's origin
(define-type TileSpec (Pairof Byte Cell))

;; a tile-loc-spec (tlspec), is a (list t l), where t is the tile-type and l is the loc of that tile

;; a pre-position is a (append (listof tile-spec) (listof cell))
(struct prepos ([tspecs : (Listof TileSpec)] [spaces : (Listof Cell)]))
;(define-type PrePosition (List* TileSpec ... (Listof Cell)))

;; a old-position is a (vectorof (listof int))
;; where the index of the top-level vectors reflect the piece-type as given in the init,
;; and the ints in the secondary vectors are the SORTED locations of the pieces of that type

;; a bw-position is a (vector int)
;; where each int is a bitwise representation of the locations of the pieces of that type

;; a bs-position is a bytestring, where each byte represents the location of the corresponding tile

;; a hc-position (hcpos for short) is a structure: (make-hc-position hc bwrep)
;; where hc is the equal-hash-code for the bytestring of the position, bwrep


;; ******************************************************************************

(struct hc-position ([hc : Fixnum] [bs : Bytes]) #:transparent #:mutable)
;; the hc is the hashcode of the bytestring

;; make-hcpos: bs-position -> hc-position
;; wrapper for the position rep augmented with the hashcode
(define: (make-hcpos [bsrep : Bytes]) : hc-position (hc-position (equal-hash-code bsrep) bsrep))


;; INITIALIZE STUFF FOR SLIDING-TILE-SOLVER

(define EXPAND-SPACE-SIZE 500000)
;(define EXPAND-SPACE-SIZE 2000000)

;; move trans for up, right, down and left respectively
(define *prim-move-translations* '((-1 . 0) (0 . 1) (1 . 0) (0 . -1)))
(define *charify-offset* 48)
(define *max-board-size* 64)

;; puzzle specific parameters
(define *puzzle-name* "a string identifying the puzzle for selecting possible configuration files")
(define *invalid-cells* empty)
(define: *num-piece-types* : Byte 0)
(define *piece-types* (vector))
(define *num-pieces* 0)
(define *start* empty)
(define: *piece-type-template* : (Vectorof Byte) (vector)) ; for each piece-type index, stores how many blocks of that type there are
(define *num-spaces* 0)
(define: *bs-ptype-index* : (Vectorof Byte) (vector)) ;; for a byte's index in a position, store the byte's piece-type
(define *target* empty)
(define *bw* 0)
(define *bh* 0)
(define *bsz* 0)
;(define *expandpos* (vector))  ;; a (vectorof position) contains locations to index into *piecelocvec*
(define *expandbuf* (vector)) ;; a vector of mutable pairs holding piece-type and location
(define *expansion-space* (vector))
(define *piecelocvec* (vector));; vector boolean representing used move locations where the index is the location to which a single piece was moved
;(define *bsbuffer* #"") ;; a reusable buffer for holding expansions of a given position
(define *cell-to-loc* "2d array of row-col indexing to locations skipping invalid squares")
(define *loc-to-cell* "1d vector of loc indexing to row-col cell-pairs")


;; init-all!: piece-type-vector pre-position-list target N N (listof (N . N)) string -> void
;; generic setter for use by puzzle-specific initialization functions
(define (init-all! ptv s t nrow ncol invalid pzlname)
  (init-cell-loc-maps! nrow ncol invalid)
  (set! *puzzle-name* pzlname)
  (set! *bh* nrow)
  (set! *bw* ncol)
  (set! *bsz* (- (* nrow ncol) (length invalid)))
  (set! *num-piece-types* (vector-length ptv)) ;; must come before bw-positionify/(pre-compress)
  (set! *piece-types* (for/vector ([cell-specs ptv])
                                  (list->set cell-specs)));****
  (set! *invalid-cells* invalid)
  (set! *num-pieces* (+ (length s) -1 (length (last s)))) ;; includes spaces -- may be used as length of position bytestring instead of bytes-length
  (set! *start* (make-hcpos (charify (bw-positionify (pre-compress s)))))
  (set! *piece-type-template* (for/vector ([pt (old-positionify (bw-positionify (pre-compress s)))]) (ann (length pt) Byte)))
  (set! *num-spaces* (vector-ref *piece-type-template* 0))
  ;(set! *expandpos* (make-vector (vector-ref *piece-type-template* 0) #f)) ;; any single piece can never generate more than the number of spaces
  (set! *expandbuf* (build-vector (* (vector-ref *piece-type-template* 0) *num-pieces*) (lambda (_) (mcons 0 (make-bytes *num-pieces*)))))
  (set! *expansion-space* (build-vector (+ EXPAND-SPACE-SIZE *bsz*) (lambda (_) (hc-position 0 (make-bytes *num-pieces*)))))
  (set! *piecelocvec* (make-vector *bsz* #f))
  ;(set! *bsbuffer* (make-bytes (* 4 *num-pieces*) 0))
  #|(set! *bs-ptype-index* (for/vector: : (Vectorof Byte) #:length *num-pieces* 
                           ([i : Byte *num-pieces*])
                           (for/last: : Byte ([ptindex-for-i : Byte (in-range *num-piece-types*)] ;; at most, once for each piece type
                                              #:break (< i (for/sum: : Byte ;; sum the number of pieces that have alread been covered
                                                             ([ptype-count : Byte (in-vector *piece-type-template*)]
                                                              [x : Byte (in-range ptindex-for-i)])
                                                             ptype-count)))
                             ptindex-for-i)))|#
  
  (set! *bs-ptype-index*
        (list->vector
         (for/fold: : (Listof Byte) ([res : (Listof Byte) '()])
           ([ptype-count : Byte (in-vector *piece-type-template*)]
            [index : Byte *num-piece-types*])
           (append res (for/list: : (Listof Byte) ([i : Byte ptype-count]) index)))))
  
  ;; should set *target* to a bytestring index and an expected location for that indexed value
  ;;******** this only works for a single goal-spec for a tile-type with only one instance, but ....
  (set! *target* (cons (for/sum ([ntypes *piece-type-template*]
                                 [i *num-piece-types*]
                                 #:break (= i (car (car t))))
                         ntypes)
                       (+ (cell-to-loc (cdr (car t))) *charify-offset*)))
  )

;;---------------------------------------------------------------------------------
;; cells are (row . col) pairs; locations are semi-row-major index

;; init-cell-loc-maps!: N N (listof (N . N)) -> void
;; called only at initialization: init the cell-to-loc and loc-to-cell arrays
(define: (init-cell-loc-maps! [nrow : Byte] [ncol : Byte] [invalid : (Listof Cell)]) : Void
  (set! *cell-to-loc* (make-array (shape 0 nrow 0 ncol) #f))
  (set! *loc-to-cell* (for*/vector ([r nrow][c ncol] #:unless (member (cons r c) invalid))
                        (cons r c)))
  (for ([i (vector-length *loc-to-cell*)]) 
    (array-set! *cell-to-loc* (car (vector-ref *loc-to-cell* i)) (cdr (vector-ref *loc-to-cell* i)) i)))

;; cell-to-loc: cell -> int
;; convert ordered pair to row-major-order rank location
(define (cell-to-loc pair)
  (array-ref *cell-to-loc* (car pair) (cdr pair)))

;; loc-to-cell: int -> cell
(define (loc-to-cell i)
  (vector-ref *loc-to-cell* i))

;;--------------------------------------------------------------------------------

;; charify: bw-position -> bytearray
;; convert a bitwise represented position into a series of bytes
(define: (charify [bw-p : (Listof Integer)]) : Bytes
  (for/fold ([res #""])
    ([pt (in-vector bw-p)])
    (bytes-append res (charify-int pt))))

;; charify-int: int -> bytearray
;; convert a single int to a bytearray rep of each 1 appearing in the int's binary representation
;; that is, the resulting bytearray will be as long as the number of 1's in the given int
(define: (charify-int [i : Integer]) : Bytes
  (for/fold ([res #""])
    ([b (integer-length i)]
     #:when (bitwise-bit-set? i b))
    (bytes-append res (bytes (+ b *charify-offset*)))))

;; decharify: bytestring -> bw-position
;; for the inverse of charify
(define (decharify ba)
  (if (eof-object? ba)
      ba
      (let ([running-start 0])
        (for/vector ([num-of-pt (in-vector *piece-type-template*)])
          (let ([res (intify ba running-start (+ running-start num-of-pt))])
            (set! running-start (+ running-start num-of-pt))
            res)))))

;; intify: bytestring [int] [int] -> int
;; convert a given series of bytes to a bitwise overlay of their corresponding positions
#|(define (intify bs (start 0) (end (bytes-length bs)))
  (for/fold ([newnum 0])
    ([ploc bs])
    (+ newnum (arithmetic-shift 1 (- ploc *charify-offset*)))))|#
(: intify (->* (Bytes) (Byte Byte) Integer))
(define (intify bs (start 0) (end *num-pieces*))
  (for/sum ([pref (in-range start end)])
    (arithmetic-shift 1 (- (bytes-ref bs pref) *charify-offset*))))

;; bw-positionify: (listof (cons tile-id (listof cell))) -> bw-position
;; create a bitwise-'position' representation of a board state based on the given start-list pre-position format
;;*** called only during initialization
(define (bw-positionify old-position)
  (for/vector ([pspec (in-list old-position)]
               [i (in-range *num-piece-types*)])
    (unless (= i (first pspec)) (error 'positionify "mis-matched piece-type in vector representation of position"))
    (list->bwrep (map cell-to-loc (cdr pspec)))))

;; old-positionify: bw-position -> old-position
(define (old-positionify bw-position)
  (for/vector ([bwrep (in-vector bw-position)])
    (bwrep->list bwrep)))

;; list->bwrep: (listof loc) -> int
;; convert the list of locations into a bitwise representation
(define: (list->bwrep [lo-loc : (Listof Byte)]) : Integer
  #|(foldl (lambda (a-loc bwint)
           (+ (arithmetic-shift 1 a-loc) bwint))
         0
         lo-loc)|#
  (for/sum ([a-loc (in-list lo-loc)]) (arithmetic-shift 1 a-loc)))

;; bwrep-direct: N N N N -> fixnum
;; get the blank-int directly from the locations of the four blanks
(define (bwrep-direct b1 b2 b3 b4)
  (+ (arithmetic-shift 1 b1)
     (arithmetic-shift 1 b2)
     (arithmetic-shift 1 b3)
     (arithmetic-shift 1 b4)))


;; bwrep->list: int -> (listof loc)
;; extract the locs encoded in the given int
(define: (bwrep->list [n : Positive-Integer]) : (Listof Byte)
  (for/list ([i (in-range (integer-length n))]
             #:when (bitwise-bit-set? n i))
    i))    


;; pre-spaces: pre-position -> (listof cell)
;; extract the spaces *** expected to be at the end of the initialization lists ***
(define: (pre-spaces [p : prepos]) : (Listof Cell)
  (prepos-spaces p))

;; pre-compress: pre-position -> (listof (cons tile-id (listof cell)))
;; collapse pieces of the same type and give spaces their unique id of -1
(define: (pre-compress [p : prepos]) : (Listof (Pairof Byte (Listof Cell)))
  (cons (cons 0 (pre-spaces p))
        (for/list: : (Listof (Pairof Byte (Listof Cell))) ([i : Byte (in-range 1 *num-piece-types*)])
          (cons i
                (for/list: : (Listof Cell) ([a-piece : TileSpec (prepos-tspecs p)]
                                            #:when (= i (car a-piece)))
                  (cdr a-piece))))))


;;------------------------------------------------------------------------------------------------------
;; BLOCK-10 PUZZLE INIT (variant 12)
(define *block10-piece-types*
  '#((reserved-spaces)               ; 0 reserved for spaces in actual position representation
     ((0 . 0)(0 . 1)(1 . 0)(1 . 1))  ; 1  2x2
     ((0 . 0)(0 . 1)(1 . 0))         ; 2  Upper Left pointing L
     ((0 . 0)(1 . -1)(1 . 0))        ; 3  Lower Right pointing L
     ((0 . 0)(1 . 0))                ; 4  2x1 vertical rectangle
     ((0 . 0))))                     ; 5  1x1 unit square

(define *block10-start* ; variant 12
  '((1 4 . 1)
    (2 3 . 0)
    (3 1 . 3)
    (4 1 . 0)
    (4 3 . 3)
    (5 2 . 1)
    (5 3 . 2)
    (5 5 . 0)
    (5 5 . 3)
    ((0 . 1) (0 . 2) (1 . 1) (1 . 2)) ; spaces
    ))

(define *block10-name* "block10v12")
(define *block10-target* '((1 0 . 1)))
(define *block10-invalid-cells* '((0 . 0) (0 . 3)))

(define (block10-init)
  (init-all! *block10-piece-types* *block10-start* *block10-target* 6 4 *block10-invalid-cells* *block10-name*))

;;------------------------------------------------------------------------------------------------------
;; CLIMB-12 PUZZLE INIT
;; piece-type is implicit in position within list, each pair specifies the cells of the piece
;; and their location relative to the (arbitrary) origin of that piece, (0 0).
(define *climb12-piece-types*
  '#((reserved-spaces)
     ((0 . 0)(1 . -1)(1 . 0)(1 . 1))           ; 1  4 square T (stem up)
     ((0 . 0)(0 . 1)(1 . 0))                   ; 2  Upper Left pointing L
     ((0 . 0)(1 . -1)(1 . 0))                  ; 3  Lower Right pointing L
     ((0 . 0)(1 . 0))                          ; 4  2x1 vertical rectangle
     ((0 . 0)(0 . 1))                          ; 5  1x2 horizontal rectangle
     ((0 . 0))))                               ; 6  1x1 unit square

;; specify board-state by triples: piece-type, board-row, board-col
(define *climb12-start*
  '((1 4 . 2)
    (2 2 . 1)
    (3 2 . 3)
    (4 1 . 0)
    (4 1 . 4)
    (5 4 . 0)
    (5 4 . 3)
    (6 3 . 0)
    (6 3 . 4)
    (6 5 . 0)
    (6 5 . 4)
    ((0 . 2) (1 . 1) (1 . 2) (1 . 3))  ; spaces
    ))

;; specify target as triple: piece-type, board-row, board-col
(define *climb12-name* "climb12")
(define *climb12-target* '((1 0 . 2)))
(define *climb12-invalid-cells* '((0 . 0) (0 . 1) (0 . 3) (0 . 4)))

(define (climb12-init)
  (init-all! *climb12-piece-types* *climb12-start* *climb12-target* 6 5 *climb12-invalid-cells* *climb12-name*))

;;------------------------------------------------------------------------------------------------------
;; CLIMB-15 PUZZLE INIT
;; (variation 1: 104 moves)
(define *climb15-piece-types*
  '#((reserved-spaces)
     ((0 . 0)(1 . -1)(1 . 0)(1 . 1))           ; 1  4 square T (stem up)
     ((0 . 0)(0 . 1)(1 . 0))                   ; 2  Upper Left pointing L
     ((0 . 0)(1 . -1)(1 . 0))                  ; 3  Lower Right pointing L
     ((0 . 0)(1 . 0)(1 . 1))                   ; 4  Lower Left pointing L
     ((0 . 0)(0 . 1)(1 . 1))                   ; 5  Upper Right pointing L
     ((0 . 0)(1 . 0))                          ; 6  2x1 vertical rectangle
     ((0 . 0)(0 . 1))                          ; 7  1x2 horizontal rectangle
     ((0 . 0))                                 ; 8  1x1 unit square
     ((0 . 0)(0 . 1)(1 . 0)(1 . 1))))          ; 9  2x2 square
     
(define *climb15-start*
  '((1 6 . 2)
    (2 2 . 0)
    (3 2 . 2)
    (4 4 . 2)
    (5 4 . 3)
    (6 2 . 3)
    (6 2 . 4)
    (7 6 . 0)
    (7 6 . 3)
    (8 1 . 0)
    (8 1 . 4)
    (8 7 . 0)
    (8 7 . 4)
    (9 4 . 0)
    ((0 . 2)(1 . 1)(1 . 2)(1 . 3))
    ))

(define *climb15-name* "climb15")
(define *climb15-target* '((1 0 . 2)))
(define *climb15-invalid-cells* '((0 . 0) (0 . 1) (0 . 3) (0 . 4)))

(define (climb15-init)
  (init-all! *climb15-piece-types* *climb15-start* *climb15-target* 8 5 *climb15-invalid-cells* *climb15-name*))

;;------------------------------------------------------------------------------------------------------
;; CLIMB-24-PRO PUZZLE INIT
;; 22? moves
;;  From Minoru Abe -- http://www.johnrausch.com/slidingblockpuzzles/abe.htm
;;           _
;; 0   _____|x|_____
;; 1  |___|x x x|___|
;; 2  |   |_____|   |
;; 3  |___| |_  |___|
;; 4  |_  |___|_|  _|
;; 5  | |_| |_| |_| |
;; 6  |_| |_|_|_| |_|
;; 7  |___|_____|___|
;; 8  |   |_| |_|   |
;; 9  |___|_____|___|

(define *climbpro24-name* "climbpro24")
(define *climbpro24-target* '((1 0 . 3)))
(define *climbpro24-invalid-cells* '((0 . 0)(0 . 1)(0 . 2)(0 . 4)(0 . 5)(0 . 6)))

(define *climbpro24-piece-types*
  '#((reserved-spaces)
     ((0 . 0)(1 . -1)(1 . 0)(1 . 1))           ; 1  4 square T (stem up)
     ((0 . 0)(0 . 1)(1 . 0)(1 . 1))            ; 2  2x2 square
     ((0 . 0)(0 . 1)(1 . 0))                   ; 3  Upper Left pointing L
     ((0 . 0)(0 . 1)(1 . 1))                   ; 4  Upper Right pointing L
     ((0 . 0)(1 . -1)(1 . 0))                  ; 5  Lower Right pointing L
     ((0 . 0)(1 . 0)(1 . 1))                   ; 6  Lower Left pointing L
     ((0 . 0)(1 . 0))                          ; 7  2x1 vertical rectangle
     ((0 . 0)(0 . 1))                          ; 8  1x2 horizontal rectangle
     ((0 . 0)(0 . 1)(0 . 2))                   ; 9  1x3 horizontal rectangle
     ((0 . 0))))                               ; 10 1x1 unit square

(define *climbpro24-start*
  '((1 8 . 3)    ; T piece
    (2 2 . 0)    ; 2x2
    (2 2 . 5)    ; 2x2
    (2 8 . 0)    ; 2x2
    (2 8 . 5)    ; 2x2
    (3 4 . 5)    ; up-left L
    (4 3 . 3)    ; up-right L
    (4 4 . 0)    ; up-right L
    (5 6 . 1)    ; down-right L
    (6 3 . 2)    ; down-left L
    (6 6 . 5)    ; down-left L
    (7 5 . 0)    ; 2x1 (vertical)
    (7 5 . 2)    ; 2x1 (vertical)
    (7 5 . 4)    ; 2x1 (vertical)
    (7 5 . 6)    ; 2x1 (vertical)
    (8 1 . 0)    ; 1x2 (horizontal)
    (8 1 . 5)    ; 1x2 (horizontal)
    (9 2 . 2)    ; 1x3 (horizontal)
    (9 7 . 2)    ; 1x3 (horizontal)
    (10 5 . 3)   ; 1x1
    (10 6 . 3)   ; 1x1
    (10 8 . 2)   ; 1x1
    (10 8 . 4)   ; 1x1
    ((0 . 3)(1 . 2)(1 . 3)(1 . 4))
    ))

(define (climbpro24-init)
  (init-all! *climbpro24-piece-types* *climbpro24-start* *climbpro24-target* 10 7 *climbpro24-invalid-cells* *climbpro24-name*))


;;------------------------------------------------------------------------------------------------------
;(block10-init) ; for local testing
;(climb15-init)
(climbpro24-init)