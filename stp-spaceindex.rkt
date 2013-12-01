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

(provide (all-defined-out))

(define *spaceindex* "the hashtable to hold the possible moves indexed by space configurations")
(define ret-false (lambda () #f))

(define (choose n k)
  (if (>= k (/ n 2))
      (/ (for/product ([i (in-range (add1 k) (add1 n))]) i)
         (for/product ([i (in-range 1 (add1 (- n k)))]) i))
      (choose n (- n k))))

;;------------------------------------------------------------------------------------------------------
;; Support for creating an index from blank configurations to valid move schemas

(struct ebms (loc0 newblanks p-chgbits) #:transparent)
;; an even-better-move-schema (EBMS) is a (ebms N fixnum fixnum)
;; loc0: is the starting location of the piece to which this ebms applies
;; newblanks: is the bitwise-representation of the new blank postions (useable as index)
;; p-chgbits: bitwise rep of piece change-bits



;; a space(blank)-index is a hash-table: {fixnum : (vectorof (listof EBMS))}
;; where the fixnum is the bitwise representation of the (typically 4) blanks.


;; compile-spaceindex: string -> void
;; read or, if not present, initialize-and-write the *spaceindex* hashtable from or to the given file
(define (compile-spaceindex fname)
  (set! *spaceindex* 
        (if (file-exists? fname)
            (with-input-from-file fname read)
            (let ([ht (make-hash)]) ; hash-table of possible moves indexed by space configuration
              (compile-ms-array! *piece-types* *bh* *bw*)
              (all-space-config ht)
              (with-output-to-file fname (lambda () (write ht)))
              ht))))

;; all-space-config: (hash-table: spaceint (vectorof EBMS)) -> void
;; create the hash of possible moves for indexed by all possible configurations of blanks
(define (all-space-config ht)
  (for* ([b1 (- *bsz* 3)] ;; board-size minus the remaining blanks that still need to be placed
         [b2 (in-range (add1 b1) (- *bsz* 2))]
         [b3 (in-range (add1 b2) (- *bsz* 1))]
         [b4 (in-range (add1 b3) *bsz*)]
         )
    (let ([spaceint (bwrep-direct b1 b2 b3 b4)])
      (unless (hash-ref ht spaceint ret-false)
        (hash-set! ht spaceint (one-space-config ht spaceint))))))
                 

;; one-space-config: (hash-table spaceint (vectorof EBMS)) fixnum -> (listof EBMS)
;; collect all the possible even-better-move-schemas for a given space configuration
;; within a hash-table indexed by the combination of piece-type and location
(define (one-space-config ht spaceint)
  (for/vector ([ptype (in-range 1 (vector-length *piece-types*))])
    (for/fold ([r empty])
      ([loc *bsz*])
      (vector-set! *piecelocvec* loc #t)
      (let* ([inner-search-result (inner-search ht spaceint spaceint ptype loc loc *piecelocvec*
                                                0 0 0)]
             [res (if (empty? inner-search-result)
                      r
                      (append inner-search-result r))])
        (vector-fill! *piecelocvec* #f)
        res))))
          

;; inner-serch: (hash-table spaceint (vectorof EBMS)) fixnum fixnum N N N (vectorof boolean) fixnum fixnum fixnum -> (listof EBMS)
;; perform the inner-search for multi-place moves of this piece.  the loc0 parameter is the origin for the moves
;; currently under consideration.  the nu-loc is the position to which has been moved with the corresponding spaceint.
;; the accumulators (blank-prerequisits, blank-change-bits, piece-change-bits) get built up for the creation of the stored moveschema
(define (inner-search ht spaceint0 spaceint ptype loc0 moved-loc plocvec b-prereq-acc b-chgbit-acc p-chgbit-acc)
  (for/fold ([r empty])
    ([dir 4])
    (let ([ms (array-ref *ms-array* ptype moved-loc dir)])
      (cond [(can-move? spaceint ptype moved-loc plocvec ms) ; prevents moves that have already been processed in the search
             ; bundle the piece-type, location, direction and corresponding move-schema
             (let ([new-spaceint (bitwise-xor spaceint (second ms))] ; the new spaceint from this move-schema
                   [xored-b-chgbits (bitwise-xor b-chgbit-acc (second ms))]
                   [xored-p-chgbits (bitwise-xor p-chgbit-acc (third ms))])
               (vector-set! plocvec (fourth ms) #t)
               (append (cons (ebms ;ptype
                              loc0 
                              (bitwise-xor spaceint0 xored-b-chgbits) ; bitwise new blank-bits
                              xored-p-chgbits)                         ; bitwise piece change-bits
                             (inner-search ht 
                                           spaceint0
                                           new-spaceint   
                                           ptype
                                           loc0
                                           (fourth ms)
                                           plocvec                              ; the plocvec vector to prevent infinite loop
                                           (bitwise-ior b-prereq-acc (first ms))
                                           xored-b-chgbits
                                           xored-p-chgbits))
                       r))]
            [else r]))))

;; can-move?: fixnum N N (vectorof boolean) move-schema -> boolean
;; determine if the proposed move can work
(define (can-move? spaceint ptype loc plocvec ms)
  (and ms ;; the retrieved move-schema non-false
       ; have we been there already
       (not (vector-ref plocvec (fourth ms)))
       ; is the move valid
       (bw-valid-move? spaceint (first ms))
       ;; loc is on the board
       ;(not (member loc *invalid-locs*)) ;; onboard? now checks invalid-locs implicitly
       ; and
       (let ([loc-cell (loc-to-cell loc)]
             [translated-base-cell "piece cells translated to loc-cell"])
         (for/and ([base-cell (vector-ref *piece-types* ptype)])
           (set! translated-base-cell (translate-cell base-cell loc-cell))
           (and 
            ;; cell-on-board
            (onboard? translated-base-cell)
            ;; not ovelap any blank
            (zero? (bitwise-and spaceint (arithmetic-shift 1 (cell-to-loc translated-base-cell)))))))
       ))


;; ------------------------
;; Using the new spaceindex structure

;; generate-and-write-new-pos: N byte-string N EBMS -> void
;; generate the new position and write it into the expansion buffer
(define (generate-and-write-new-pos bufindex src-bspos spaceint piece-type an-ebms)
  (let* (;[piece-type (vector-ref an-ebms 0)]
         [the-hcpos (vector-ref *expansion-space* bufindex)]
         [targetbs (hc-position-bs the-hcpos)]
         [piece-start (for/sum ([i piece-type]) (vector-ref *piece-type-template* i))]
         [piece-end (+ piece-start (vector-ref *piece-type-template* piece-type))]
         )
    ;; initialize the target bytestring to the source position
    (bytes-copy! targetbs 0 src-bspos)
    ;; overwrite the new blank-locations
    (bytes-copy! targetbs 0 
                 (charify-int (ebms-newblanks an-ebms)
                              )) ;; do the spaces at the front ;; was 2
    ;; set moved piece
    (bytes-copy! targetbs piece-start
                 (charify-int (bitwise-xor (intify src-bspos piece-start piece-end)
                                           (ebms-p-chgbits an-ebms) ;; vector-ref was 3 when storing ptype & loc
                                           )))
    ;; set the hashcode
    (set-hc-position-hc! the-hcpos (equal-hash-code targetbs))
    ))


;; expand*: hc-position int -> int
;; the new successor generation utilizing the spaceindex
;; -> four-part EBMS version <-
(define (expand* hc-s exp-ptr)
  (let* ([bs (hc-position-bs hc-s)]     ; 
         [bwrep (decharify bs)]         ; bitwise vector representation of board state
         [spaceint (vector-ref bwrep 0)]
         [moves-to-check (hash-ref *spaceindex* spaceint)]
         [expanded-ptr exp-ptr])
    (for* ([pti (in-range 1 *num-piece-types*)]
           [m (vector-ref moves-to-check (sub1 pti))])
      ; get m's piecetype-int and see if one of the pieces of that type is in m's location
      (when (positive? (bitwise-and (vector-ref bwrep pti) (arithmetic-shift 1 (ebms-loc0 m))))
        ; if so,
        ; create the new position, and write it to the buffer 
        (generate-and-write-new-pos expanded-ptr bs spaceint pti m)
        ; and go to the next one
        (set! expanded-ptr (add1 expanded-ptr))
        ))
    expanded-ptr))

#| ; -> two-part double-hash version <-
(define (expand* hc-s exp-ptr)
  (let* ([bs (hc-position-bs hc-s)]     ; 
         ;[bwrep (decharify bs)]         ; bitwise vector representation of board state
         [spaceint (intify bs 0 4)]
         [possible-moves-hash (hash-ref *spaceindex* spaceint)]

         ;
         [expanded-ptr exp-ptr]
         [target-hc-pos 'mutable-hc-pos-in-*expansion-space*]
         )
    (for* ([i (in-range 4 *num-pieces*)]
           ;[loc (bwrep->list (vector-ref bwrep i))]
           ;[loc (in-range (integer-length (vector-ref ptnum i)))]
           ;#:when (bitwise-bit-set? ptnum loc)
           )
      (let ([moves-for-ptype-at-location
             (hash-ref possible-moves-hash 
                       (+ (* (vector-ref *bs-ptype-index* i) *bsz*) (- (bytes-ref bs i) *charify-offset*))
                       (lambda () #f))])
        (when moves-for-ptype-at-location
          (for ([ebms moves-for-ptype-at-location])
            ; create the new position, and write it to the buffer 
            (generate-and-write-new-pos expanded-ptr bs spaceint ebms (vector-ref *bs-ptype-index* i))
            ; and go to the next one
            (set! expanded-ptr (add1 expanded-ptr))
            ))))
    expanded-ptr))
   |#              



;(block10-init)   ;  160010 possible even-better-move-schema
;(climb12-init)
;(climb15-init)   ; 
(climbpro24-init)
;(time (compile-ms-array! *piece-types* *bh* *bw*))
(time (compile-spaceindex (format "~a~a-spaceindex.rkt" "stpconfigs/" *puzzle-name*)))
;(expand *start*)
;(test)