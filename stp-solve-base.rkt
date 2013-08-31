#lang racket

;(require (planet soegaard/gzip:2:2))
(require file/gzip)
(require file/gunzip)
(require srfi/25) ;; multi-dimensional arrays
(require "stp-init.rkt")
(require racket/fixnum)
(require racket/set)
(require mzlib/string)
(require test-engine/racket-tests)
(require racket/generator)

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

;; a tile-loc-spec (tlspec), is a (list t l), where t is the tile-type and l is the loc of that tile

;; a pre-position is a (append (listof tile-spec) (listof cell))

;; a old-position is a (vectorof (listof int))
;; where the index of the top-level vectors reflect the piece-type as given in the init,
;; and the ints in the secondary vectors are the SORTED locations of the pieces of that type

;; a bw-position is a (vector int)
;; where each int is a bitwise representation of the locations of the pieces of that type

;; a fspec is a vector: (vector string string int int)
;; where the strings are the file-name and the base path to the file, the count of positions, and file-size in bytes
;; NOTE: an fspec must be a list or vector because they are passed over the riot layer
(define (make-fspec fname fbase pcount fsize) (vector fname fbase pcount fsize))
(define (fspec-fname an-fs) (vector-ref an-fs 0))
(define (fspec-fbase an-fs) (vector-ref an-fs 1))
(define (fspec-fullpath an-fs) (string-append (fspec-fbase an-fs) (fspec-fname an-fs)))
(define (fspec-pcount an-fs) (vector-ref an-fs 2))
(define (fspec-fsize an-fs) (vector-ref an-fs 3))

;; a fringe-index is a (listof (list number number fspec),
;; each tripple in a fringe-index describes one segment of the fringe, where the two numbers are the hash-codes
;; of the min and max position found in a segment-file identified by the fspec given in the third item
(define (segment-fspec a-findex-seg) (third a-findex-seg))
(define (segment-min-hc a-findex-seg) (first a-findex-seg))
(define (segment-max-hc a-findex-seg) (second a-findex-seg))
;; findex-pcount: fringe-index -> int
;; report the number of positions in the fringe represented by the given index
(define (findex-pcount a-fndx) (for/sum ([sgmnt a-fndx]) (fspec-pcount (segment-fspec sgmnt))))

;; make-findex-seq-reader: findex -> (sequenceof position)
;; given a fringe-index, create a sequence that will produce all of the positions in the list of fspecs
;; appearing in the fringe-index, in the order in which they appear.
;;*** WILL WE RUN INTO PROBLEMS WITH THE LAST OPEN-INPUT-FILE PORT LEFT OPEN ??????????
(define (make-findex-seq-reader findex)
  (in-generator 
   (for ([segment findex])
     (let* ([fspec (segment-fspec segment)]
            [iprt (open-input-file (fspec-fullpath fspec))])
       (for ([p (in-port (lambda (in) (decharify (read-bytes-line in))) iprt)])
         (yield p))
       (close-input-port iprt)))))

;; make-primed-findex-seq-reader: int fringe-index -> (sequence position)
;; given the assigned start, create a findex-seq-reader and advance it so the next position is first of the assigned range
(define (make-primed-findex-seq-reader start findex)
  (do ([pcounter start (- pcounter (fspec-pcount (segment-fspec (car segments))))]
       [segments findex (cdr segments)])
    ((< pcounter (fspec-pcount (segment-fspec (car segments))))
     ;; make the seq-reader and advance it
     (let*-values ([(fsr) (make-findex-seq-reader segments)]
                   [(more? next) (sequence-generate fsr)])
       (for ([i pcounter]) (next))
       fsr))
    ))
    

;; ******************************************************************************

;;-------------------------------------------------------------------------------
;; COMMON UTILITIES TO BOTH GENERIC FRINGE-SEARCH AND CLUSTER-FRINGE-SEARCH

;; Fringe Writing/Reading 

;; write-fringe-to-disk: (listof or vectorof position) string -> void
;; writes the positions from the given fringe (whether list or vector) into a file with given file-name.
(define (write-fringe-to-disk fringe file-name)
  (let ([my-output (open-output-file file-name #:exists 'replace)])
    (for ([position fringe])
      (fprintf my-output "~a~%" (charify position)))
    (flush-output my-output)
    (close-output-port my-output)))

;; read-fringe-from-file: string -> (listof position)
;; reads a file from a file path (if you are in the current directory just simply the file-name)
;; and returns the fringe that was in that file.
(define (read-fringe-from-file file-name)
  (let* ([iport (open-input-file file-name)]
         [the-fringe (port->list (lambda (in) (decharify (read-bytes-line in))) iport)])
    (close-input-port iport)
    the-fringe))

;; read-pos: input-port -> bw-position
;; read the (probably byte representation of a) position from the file attatched to the given input port
;; ASSUMES: fringe-file format is one position per line with bytes
(define (read-pos iprt)
  (decharify (read-bytes-line iprt)))

;; position-count-in-file: string -> number
;; reports the number of positions in the given fringe file assuming the file was written with write-fringe-to-disk
(define (position-count-in-file f)
  (read-from-string (with-output-to-string 
                     (lambda () (system (if (string=? (substring f (- (string-length f) 3)) ".gz")
                                            (format "zcat ~a | wc -l" f)
                                            (format "wc -l ~a" f)))))))
                    

;; touch: string -> void
;; create the file with given name
(define (touch fname) (display-to-file "" fname))

;; fringe-file-not-ready?: fspec [check-alt-flag #f] -> boolean
;; determine if the file exists on disk and has the appropriate size
;; with optional check-alt-flag will look on the nfs share and copy if found
(define (fringe-file-not-ready? fspec [check-alt-flag #f])
  (when (and check-alt-flag
             (not (file-exists? (fspec-fullpath fspec)))
             (file-exists? (fspec-fname fspec))) ;; check working (shared) directory
    (copy-file (fspec-fname fspec) (fspec-fullpath fspec))) ;; YUCK!
  (or (not (file-exists? (fspec-fullpath fspec)))
      (< (file-size (fspec-fullpath fspec)) (fspec-fsize fspec))))
      

;; wait-for-files: (listof fspec) [check-alt-flag #f] -> 'ready
;; given a list of fringe-specs, wait until the file is present in the specified location
;; with the specified size.  if check-alt-flag is true, then drop the fbase and see if the file is available via NFS (copy if so!)
(define (wait-for-files lo-fspecs [check-alt-flag #f])
  (do ([fspecs (filter (lambda (fspec) (fringe-file-not-ready? fspec check-alt-flag)) lo-fspecs)
               (filter (lambda (fspec) (fringe-file-not-ready? fspec check-alt-flag)) fspecs)]
       [sleep-time 0.01 (* sleep-time 2)])
    ((empty? fspecs) 'ready)
    (printf "wait-for-files: waiting for ~a files such as ~a ... and sleeping ~a~%" (length fspecs) (fspec-fullpath (first fspecs)) sleep-time)
    (sleep sleep-time)))

;; check-sorted-fringe?: string -> boolean
;; check to see that a given fringe file is indeed sorted
(define (check-sorted-fringe? f)
  (let* ([myin (open-input-file f)]
         [prevpos (read-pos myin)]
         [bool-res (for/and ([pos (in-port read-pos myin)])
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
    ;; try to reset 
    (error 'fhdone? "hit end of file before the appropriate number of positions had been read"))
  ;(> (fringehead-readcount fh) (fringehead-total fh))
  (eof-object? (fringehead-next fh))
  )

;; advance-fhead!: fringehead -> position OR void
;; move to the next position, but check to make sure something is available if expected
(define (advance-fhead! fh)
  (when (< (fringehead-readcount fh) (fringehead-total fh))
    (do ([sleep-time 0.01 (* sleep-time 2)])
      ((not (eof-object? (peek-bytes 1 1 (fringehead-iprt fh)))) 'proceed)
      (sleep sleep-time)))
  (unless (fhdone? fh)
    (set-fringehead-next! fh (read-pos (fringehead-iprt fh)))
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
  (let ([inprt (open-input-file (fspec-fullpath fs))])
    (fringehead (read-pos inprt) inprt 1 (fspec-pcount fs))))


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
    (list (list-to-bwrep (list-subtract loc-list-to  current-loc-list <))
          (bitwise-xor (list-to-bwrep (list-subtract loc-list-to  current-loc-list <))
                       (list-to-bwrep (list-subtract current-loc-list loc-list-to <)))
          (list-to-bwrep (list (cell-to-loc (cdr tile))
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
      (for ([piece-loc (bwrep-to-list (vector-ref s piece-type))])
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
(climb12-init)
(compile-ms-array! *piece-types* *bh* *bw*)
;(test)