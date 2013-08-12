#lang racket

(require (planet soegaard/gzip:2:2))
(require srfi/25) ;; multi-dimensional arrays
(require "stp-init.rkt")
(require racket/fixnum)
(require racket/set)
(require mzlib/string)
(require test-engine/racket-tests)

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

;; a fspec is a list: (list string int int)
;; where the string is the file-name-(path), count of positions, and file-size in bytes

;; ******************************************************************************

;;-------------------------------------------------------------------------------
;; COMMON UTILITIES TO BOTH GENERIC FRINGE-SEARCH AND CLUSTER-FRINGE-SEARCH

;; Fringe Writing/Reading 

;; write-fringe-to-disk: (listof or vectorof position) string -> void
;; writes the positions from the given fringe (whether list or vector) into a file with given file-name.
;; If file-name has .gz extension, writes a gzipped file.
(define (write-fringe-to-disk fringe file-name (compress #t))
  (let ([my-output (if (string=? ".gz" (substring file-name (- (string-length file-name) 3)))
                       (open-output-gz-file (string->path file-name) #:replace #t)
                       (open-output-file file-name #:exists 'replace))])
    (for ([position fringe])
      (fprintf my-output "~a~%" position))
    (flush-output my-output)
    (close-output-port my-output)))

;; read-fringe-from-gzfile: string -> (listof position)
;; reads a file from a file path (if you are in the current directory just simply the file-name)
;; and returns the fringe that was in that file.
(define (read-fringe-from-gzfile file-name)
  (port->list read (open-input-gz-file (string->path file-name))))

;; position-count-in-file: string -> number
;; reports the number of positions in the given fringe file assuming the file was written with write-fringe-to-disk
(define (position-count-in-file f)
  (read-from-string (with-output-to-string (lambda () (system (format "gunzip -c ~a | wc -l" f))))))

;; touch: string -> void
;; create the file with given name
(define (touch fname) (display-to-file "" fname))

;; fringe-file-not-ready?: fspec [check-alt-flag #f] -> boolean
;; determine if the file exists on disk and has the appropriate size
;; with optional check-alt-flag will look on the nfs share and copy if found
(define (fringe-file-not-ready? spec-triple [check-alt-flag #f])
  (when (and check-alt-flag
             (not (file-exists? (first spec-triple)))
             (file-exists? (substring (car spec-triple) 5)))
    (copy-file (substring (car spec-triple) 5) (car spec-triple))) ;; YUCK!
  (or (not (file-exists? (first spec-triple)))
      (< (file-size (first spec-triple)) (third spec-triple))))
      

;; wait-for-files: (listof fspec) [check-alt-flag #f] -> 'ready
;; given a list of fringe-specs (list filename num-positions compressed-size), wait until the file is present on the local machine
;; with the specified size.  if check-alt-flag is true, then drop the /tmp/ and see if the file is available via NFS (copy if so!)
(define (wait-for-files lo-fspecs [check-alt-flag #f])
  (do ([fspecs (filter (lambda (fspec) (fringe-file-not-ready? fspec check-alt-flag)) lo-fspecs)
               (filter (lambda (fspec) (fringe-file-not-ready? fspec check-alt-flag)) fspecs)]
       [sleep-time 0.1 (* sleep-time 2)])
    ((empty? fspecs) 'ready)
    (printf "wait-for-files: waiting for ~a files such as ~a ... and sleeping ~a~%" (length fspecs) (first (first fspecs)) sleep-time)
    (sleep sleep-time)))

;; check-sorted-fringe?: string -> boolean
;; check to see that a given fringe file is indeed sorted
(define (check-sorted-fringe? f)
  (let* ([myin (open-input-gz-file (string->path f))]
         [prevpos (read myin)]
         [bool-res (for/and ([pos (in-port read myin)])
                     (let ([res (position<? prevpos pos)])
                       (set! prevpos pos)
                       res))])
    (close-input-port myin)
    bool-res))

;;---- fringehead structs and utilities

;; a fringehead in a struct
;; where next is a position, iprt is an input port, readcount is the number of positions read from this fringehead
;; and total is the number of positions expected to be able to read
(struct fringehead (next iprt readcount total) #:mutable)

;; fhdone?: fringehead -> boolean
;; #t if readcount >= total for the given fringehead -- that is, this fringehead is exhausted.
;; Note: readcount starts at 1, 
(define (fhdone? fh)
  (when (and (eof-object? (fringehead-next fh)) (<= (fringehead-readcount fh) (fringehead-total fh)))
    (error 'fhdone? "hit end of file before the appropriate number of positions had been read"))
  ;(> (fringehead-readcount fh) (fringehead-total fh))
  (eof-object? (fringehead-next fh))
  )

;; advance-fhead!: fringehead -> position OR void
;; move to the next position
(define (advance-fhead! fh)
  (unless (fhdone? fh)
    (set-fringehead-next! fh (read (fringehead-iprt fh)))
    (set-fringehead-readcount! fh (add1 (fringehead-readcount fh)))
    (fringehead-next fh)))

;; position-in-fhead?: position fringehead -> boolean
;; determines if given position is present in the fringe headed by the given fringehead
;; side-effect: advances the fringehead, assuming no position _less-than_ thi given position will subsequently be queried
;; if the given position is less than the head of the fringe, then we'll not find it further in the fringe
;; that is, advance the fh while it is strictly less-than the given position
(define (position-in-fhead? p fh)
  (do ([fhp (fringehead-next fh) (advance-fhead! fh)])
    ((or (fhdone? fh)
         (not (position<? fhp p))) 
     (equal? fhp p))))

;; fh-from-fspec: fspec -> fringehead
;; create a fringehead from a given fspec
;; *** the open port must be closed by the requestor of this fringehead
(define (fh-from-fspec fs)
  (let ([inprt (open-input-gz-file (string->path (first fs)))])
    (fringehead (read inprt) inprt 1 (second fs))))

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


;; lexi<?: position position -> boolean
;; lexicographic less-than test on two positions (that presumably have a primary hash collision)
(check-expect (lexi<? #((1 2 3) (1 3) (1 2 3)) #((1 2 3) (1 3) (1 2 3))) #f)
(check-expect (lexi<? #((1 2 3) (2 3) (1 2 3)) #((1 2 3) (1 3) (1 2 3))) #f)
(check-expect (lexi<? #((1 2 3) (1 3) (1 2 3)) #((1 2 3) (2 3) (1 2 3))) #t)
(define (lexi<? p1 p2)
  (for/first ([tile-types1 p1]
              [tile-types2 p2]
              #:when (not (equal? tile-types1 tile-types2)))
    (for/first ([tile1 tile-types1]
                [tile2 tile-types2]
                #:when (not (= tile1 tile2)))
      (< tile1 tile2))))

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

;(test)