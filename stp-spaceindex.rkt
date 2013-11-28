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

;;------------------------------------------------------------------------------------------------------
;; Support for creating an index from blank configurations to valid move schemas

;; an even-better-move-schema (EBMS) is a (vector N N fixnum fixnum)
;; where:
;; 0. piece-type
;; 1. starting location of piece of this piece-type
;; 2. bitwise rep of the new blank-bits
;; 3. bitwise rep of piece change-bits

;; a space(blank)-index is a hash-table: {fixnum : (vectorof EBMS)}
;; where the fixnum is the bitwise representation of the (typically 4) blanks.


;; compile-spaceindex: string -> void
;; read or, if not present, initialize-and-write the *spaceindex* hashtable from or to the given file
(define (compile-spaceindex fname)
  (set! *spaceindex* 
        (if (file-exists? fname)
            (with-input-from-file fname read)
            (let ([ht "hash-table of possible moves indexed by space configuration"])
              (compile-ms-array! *piece-types* *bh* *bw*)
              (set! ht (all-space-config))
              (with-output-to-file fname (lambda () (write ht)))
              ht))))

; create the hash of possible moves for indexed by all possible configurations of blanks
(define (all-space-config)
  (for*/hash ([b1 (- *bsz* 3)]
              [b2 (in-range (add1 b1) (- *bsz* 2))]
              [b3 (in-range (add1 b2) (- *bsz* 1))]
              [b4 (in-range (add1 b3) *bsz*)]
              #:when (andmap (lambda (loc) (not (member loc *invalid-locs*))) (list b1 b2 b3 b4))
              ;;(andmap onboard? (map loc-to-cell (list b1 b2 b3 b4)))
              )
    (let ([spaceint (list->bwrep (list b1 b2 b3 b4))])
      (values spaceint (list->vector (one-space-config spaceint))))))

;; one-space-config: fixnum -> (listof EBMS)
;; collect all the possible even-better-move-schemas for a given space configuration
;; within a hash-table indexed by the combination of piece-type and location
(define (one-space-config spaceint)
  (let ([plocvec (make-vector *bsz* #f)])
    (for*/fold ([r empty])
      ([ptype (in-range 1 (vector-length *piece-types*))]
       [loc *bsz*])
      (vector-fill! plocvec #f)
      (let ([inner-search-result (inner-search spaceint spaceint ptype loc loc plocvec 0 0 0)])
        (if (empty? inner-search-result)
            r
            (append inner-search-result r))))))

;; inner-serch: fixnum fixnum N N N (vectorof boolean) fixnum fixnum fixnum -> (listof EBMS)
;; perform the inner-search for multi-place moves of this piece.  the loc0 parameter is the origin for the moves
;; currently under consideration.  the nu-loc is the position to which has been moved with the corresponding spaceint.
;; the accumulators (blank-prerequisits, blank-change-bits, piece-change-bits) get built up for the creation of the stored moveschema
(define (inner-search spaceint0 spaceint ptype loc0 moved-loc plocvec b-prereq-acc b-chgbit-acc p-chgbit-acc)
  (for*/fold ([r empty])
    ([dir 4]
     [ms (list (array-ref *ms-array* ptype moved-loc dir))]
     #:when (can-move? spaceint ptype moved-loc dir plocvec ms)) ; prevents moves that have already been processed in the search
    ; bundle the piece-type, location, direction and corresponding move-schema
    (vector-set! plocvec moved-loc #t)
    (vector-set! plocvec (fourth ms) #t)
    (append (cons (vector ptype
                          loc0 
                          (bitwise-xor spaceint0 (bitwise-xor b-chgbit-acc (second ms))) ; bitwise new blank-bits
                          (bitwise-xor p-chgbit-acc (third ms)))                         ; bitwise piece change-bits
                  (inner-search spaceint0
                                (bitwise-xor spaceint (second ms))   ; the new spaceint from this move-schema
                                ptype
                                loc0
                                (fourth ms)
                                plocvec                              ; the plocvec vector to prevent infinite loop
                                (bitwise-ior b-prereq-acc (first ms))
                                (bitwise-xor b-chgbit-acc (second ms))
                                (bitwise-xor p-chgbit-acc (third ms))))
            r)))

;; can-move?: fixnum N N (vectorof boolean) move-schema -> boolean
;; determine if the proposed move can work
(define (can-move? spaceint ptype loc dir plocvec ms)
  (and ms ;; the retrieved move-schema non-false
       ; have we been there already
       (not (vector-ref plocvec (fourth ms)))
       ;; loc is on the board
       (not (member loc *invalid-locs*))
       ; piece-on-board
       (andmap onboard? 
               (for/list ([cell (translate-piece (vector-ref *piece-types* ptype) (loc-to-cell loc))]) cell))
       ; is the move valid
       (bw-valid-move? spaceint (first ms))
       ;; 3. does the piece not overlay with the spaces
       (not-on-blanks? spaceint ptype loc dir)
       ))

;; not-on-blanks?: fixnum N N N -> boolean
;; make sure the piece-type in this location is not overlapping where the spaces are as given in spaceint
(define (not-on-blanks? spaceint ptype loc dir)
  (let ([pcells (vector-ref *piece-types* ptype)])
    (zero? (bitwise-and spaceint (list->bwrep (for/list ([cell (translate-piece pcells (loc-to-cell loc))]) (cell-to-loc cell)))))))


;; ------------------------
;; Using the new spaceindex structure

;; generate-and-write-new-pos: N byte-string EBMS -> void
;; generate the new position and write it into the expansion buffer
(define (generate-and-write-new-pos bufindex src-bspos spaceint an-ebms)
  (let* ([piece-type (vector-ref an-ebms 0)]
         [the-hcpos (vector-ref *expansion-space* bufindex)]
         [targetbs (hc-position-bs the-hcpos)]
         [piece-start (for/sum ([i piece-type]) (vector-ref *piece-type-template* i))]
         [piece-end (+ piece-start (vector-ref *piece-type-template* piece-type))]
         )
    ;; initialize the target bytestring to the source position
    (bytes-copy! targetbs 0 src-bspos)
    ;; overwrite the new blank-locations
    (bytes-copy! targetbs 0 
                 (charify-int (vector-ref an-ebms 2)
                              )) ;; do the spaces at the front ;; was 2
    ;; set moved piece
    (bytes-copy! targetbs piece-start
                 (charify-int (bitwise-xor (intify src-bspos piece-start piece-end)
                                           (vector-ref an-ebms 3) ;; vector-ref was 3 when storing ptype & loc
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
    (for ([m moves-to-check])
      ; get m's piecetype-int and see if one of the pieces of that type is in m's location
      (when (positive? (bitwise-and (vector-ref bwrep (vector-ref m 0)) (arithmetic-shift 1 (vector-ref m 1))))
        ; if so,
        ; create the new position, and write it to the buffer 
        (generate-and-write-new-pos expanded-ptr bs spaceint m)
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
;(compile-ms-array! *piece-types* *bh* *bw*)
;(compile-spaceindex (format "~a~a-spaceindex.rkt" "stpconfigs/" *puzzle-name*))
;(expand *start*)
;(test)