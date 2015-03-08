#lang racket/base

(require
 srfi/25 ;; multi-dimensional arrays
 racket/list
 ;racket/fixnum
 ;racket/set
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

;(struct ebms (newcloc newcbconf cbref) #:prefab)
;; EBMS: bytestring of 5-bytes
(define (ebms-newcloc e) (bytes-ref e 0))
(define (ebms-newcbconf e) (subbytes e 1 4))
(define (ebms-cbref e) (bytes-ref e 4))
(define (ebms nuloc nucbconf cbref) (bytes-append nuloc nucbconf cbref))
;; an even-better-move-schema (EBMS) is a five-byte string where
;; byte 0: newcloc: in the canonicalized rcbyte for the piece location after the move
;; bytes 1-3: newcbconf: is the three-byte canonical-blank-config of the blanks after the move
;; byte 4: cbref: provides the rcbyte reference for the new canonicalized blank-config

;; an rcbyte is a byte where the first four bits represent the row difference
;;   and the next four bits represent a signed column difference

;; a canonical-blank-config is a byte-string of three bytes, where each byte represents the row and column difference between:
;; rcbyte 1: first blank to second
;; rcbyte 2: second to third
;; rcbyte 3: third to fourth

;; a space(blank)-index is a hash-table: {blankconfig : (vectorof (listof EBMS))}
;; where the fixnum is the bitwise representation of the (typically 4) blanks.


;;---- UTILITIES ----------------------------------------------------------

;; canonize: N N N N -> byte-string
;; convert the four blank locations into a canonical (3-byte) blank-configuration
(define (canonize b1 b2 b3 b4)
  (bytes (locs->rcbyte b1 b2) (locs->rcbyte b2 b3) (locs->rcbyte b3 b4)))

;; decanonize: byte-string (N . N) -> (listof loc)
;; map a canonical rep into list of locations
(define (decanonize bs rcref)
  (let* ([b1b2 (rcbyte->rcpair (bytes-ref bs 0))]
         [b2b3 (rcbyte->rcpair (bytes-ref bs 1))]
         [b3b4 (rcbyte->rcpair (bytes-ref bs 2))]
         [b2 (rc+ rcref b1b2)]
         [b3 (rc+ b2 b2b3)]
         [b4 (rc+ b3 b3b4)])
    (list (cell-to-loc rcref)
          (cell-to-loc b2)
          (cell-to-loc b3)
          (cell-to-loc b4))))

;; locs->rcbyte: N N -> rcbyte
;; convert the  difference between two locations to single rcbyte
(define (locs->rcbyte loc1 loc2)
  (let* ([c1 (loc-to-cell loc1)]
         [c2 (loc-to-cell loc2)]
         [drow (- (car c2) (car c1))]
         [dcol (- (cdr c2) (cdr c1))])
    (rcpair->rcbyte (cons drow dcol))))

;; rcpair->rcbyte: (N . N) -> byte
;; convert a row-col pair where the row value in range [0,*bh*) and col in range [-*bw*,+*bw*] into a rcbyte
(define (rcpair->rcbyte rcp)
  (when (or (< (car rcp) -2) (< (cdr rcp) (- *bw*)))
    (error (format "rcpair->rcbyte: negative value in (~a,~a)~%" (car rcp) (cdr rcp))))
  (+ (bitwise-ior (arithmetic-shift (+ (car rcp) 2) 4) ;; the two is for the negative rows, the 4 is the high-four bits
                  (+ *bw* (cdr rcp)))
     *charify-offset*))

;; rcbyte->rcpair: byte -> (N . N)
;; recover the row-col pair from the byte
(define (rcbyte->rcpair b)
  (let ([decharified-b (- b *charify-offset*)])
    (cons (- (arithmetic-shift (bitwise-and decharified-b 240) -4) 2)
          (- (bitwise-and decharified-b 15) *bw*))))
          
;; register-loc-to-pair: N (N . N) -> (N . N)
;; given an actual location and the reference for the blank-config, convert location to canonical row-col pair
(define (register-loc-to-pair loc ref)
  (register-cell-to-pair (loc-to-cell loc) ref))

;; register-cell-to-pair: (N . N) (N . N) -> (N . N)
;; given actual cell and reference for blank-config, convert cell to canonical row-col pair
(define (register-cell-to-pair cell ref)
  (rc- cell ref))

(define (deregister-pair-to-cell ref pair)
  (register-cell-to-pair ref pair))

;; rc+/-: (N . N) (N . N) -> (N . N)
;; add or subtract the given cons pairs
(define (rc+ p1 p2) (cons (+ (car p1) (car p2)) (+ (cdr p1) (cdr p2))))
(define (rc- p1 p2) (cons (- (car p1) (car p2)) (- (cdr p1) (cdr p2))))

;;-------------------------------------------------------------------------

;; compile-spaceindex: string -> void
;; read or, if not present, initialize-and-write the *spaceindex* hashtable from or to the given file
(define (compile-spaceindex fname)
  (set! *spaceindex* 
        (if (file-exists? fname)
            (with-input-from-file fname read)
            (let ([ht (make-hash)]) ; mutable hash-table of possible moves indexed by space configuration
              (compile-ms-array! *piece-types* *bh* *bw*)
              (all-space-config ht)
              (with-output-to-file fname (lambda () (write ht)))
              ht))))

;; all-space-config: (hash-table: spaceint (vectorof EBMS)) -> void
;; populates the hash of possible moves for indexed by all possible configurations of blanks
(define (all-space-config ht)
  (for* ([b1 (- *bsz* 3)] ;; board-size minus the remaining blanks that still need to be placed
         [b2 (in-range (add1 b1) (- *bsz* 2))]
         [b3 (in-range (add1 b2) (- *bsz* 1))]
         [b4 (in-range (add1 b3) *bsz*)]
         )
    (let ([spaceint (bwrep-direct b1 b2 b3 b4)]
          [canonical-blankindex (canonize b1 b2 b3 b4)]
          )
      (unless (hash-ref ht canonical-blankindex ret-false)
        (hash-set! ht canonical-blankindex (make-hash)))
      (one-space-config (hash-ref ht canonical-blankindex) spaceint (loc-to-cell b1) canonical-blankindex)
      )))

;; one-space-config: (hash-table: pt-loc-bytes . (setof EBMS)) fixnum (N . N) blank-config -> void
;; collect all the possible even-better-move-schemas for a given space configuration
;; within a hash-table indexed by the combination of piece-type and location
(define (one-space-config ht spaceint config-ref-pair blank-config)
  (for* ([ptype (in-range 1 (vector-length *piece-types*))]
         [loc *bsz*])
      (vector-set! *piecelocvec* loc #t)
      (let ([inner-search-result (inner-search ht spaceint spaceint ptype loc loc *piecelocvec*
                                               0 0 0 
                                               config-ref-pair blank-config)]
            )
        (vector-fill! *piecelocvec* #f)
        )))

;; inner-serch: (hash-table: pt-loc : (listof EBMS)) fixnum fixnum N N N (vectorof boolean) fixnum fixnum fixnum (N . N) blank-config -> void
;; perform the inner-search for multi-place moves of this piece.  the loc0 parameter is the actual origin for the moves
;; currently under consideration.  the nu-loc is the position to which has been moved with the corresponding spaceint.
;; the accumulators (blank-prerequisits, blank-change-bits, piece-change-bits) get built up for the creation of the stored moveschema
;; the config-ref-pair specifies delta-row/col between actual blank-config and canonical-config, blank-config is the canonical rep
(define (inner-search ht spaceint0 spaceint ptype loc0 moved-loc plocvec b-prereq-acc b-chgbit-acc p-chgbit-acc config-ref-pair0 blank-config)
  (for ([dir 4])
    (let ([ms (array-ref *ms-array* ptype moved-loc dir)])
      (when (can-move? spaceint ptype moved-loc plocvec ms) ; prevents moves that have already been processed in the search
        ; bundle the piece-type, location, direction and corresponding move-schema
        (let* ([new-spaceint (bitwise-xor spaceint (second ms))] ; the new spaceint from this move-schema
               [xored-b-chgbits (bitwise-xor b-chgbit-acc (second ms))]
               [xored-p-chgbits (bitwise-xor p-chgbit-acc (third ms))]
               [canonical-rcloc (register-loc-to-pair loc0 config-ref-pair0)]      ;; canonicalized location of piece before move
               [new-blanks (bwrep->list (bitwise-xor spaceint0 xored-b-chgbits))] ;; actual new locations of blanks
               [new-blank-config (apply canonize new-blanks)]                     ;; canonical config of the new blanks
               [new-config-ref-pair (register-loc-to-pair (car new-blanks) config-ref-pair0)]
               [an-ebms (ebms (bytes (rcpair->rcbyte (rc- (loc-to-cell (fourth ms)) config-ref-pair0))) ; canonical loc where the piece ends up 
                              new-blank-config                                   ; new canonical config of blanks
                              (bytes (rcpair->rcbyte new-config-ref-pair)))  ; reference for new config
                        ]
               )
          (vector-set! plocvec (fourth ms) #t)
          ;; augment candidate successors for this piece-type/loc combination given this blank-configuration
          (hash-update! ht (bytes ptype (rcpair->rcbyte canonical-rcloc)) ;(cons ptype canonical-rcloc)
                        (lambda (prev)
                          (if (member an-ebms prev) prev (cons an-ebms prev)))
                        (lambda () empty))
          (inner-search ht 
                        spaceint0
                        new-spaceint   
                        ptype
                        loc0
                        (fourth ms)
                        plocvec                              ; the plocvec vector to prevent infinite loop
                        (bitwise-ior b-prereq-acc (first ms))
                        xored-b-chgbits
                        xored-p-chgbits
                        config-ref-pair0
                        new-blank-config))))))

;; can-move?: fixnum N N (vectorof boolean) move-schema -> boolean
;; determine if the proposed move can work
(define (can-move? spaceint ptype loc plocvec ms)
  (and ms ;; the retrieved move-schema non-false
       ; have we been there already
       (not (vector-ref plocvec (fourth ms)))
       ; is the move valid
       (bw-valid-move? spaceint (first ms))
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

;;Generate-and-write-new-pos-ht: N, byte-string, EBMS, N, N, (int . int) -> void
;;generates the new position, and writes the HC as the key, and the byte_string as Value, Into the global Hash Table named *expansion-hash* (defined in stp-init)
(define (generate-and-write-new-pos-ht src-bspos an-ebms piece-type ploc0 crc)
  (let* ([targetbs (bytes-copy src-bspos)]
         [piece-start (for/sum ([i piece-type]) (vector-ref *piece-type-template* i))]
         [piece-end (+ piece-start (vector-ref *piece-type-template* piece-type))]
         [new-hc-position 0]
         )
    
    
    ;; initialize the target bytestring to the source position
    (bytes-copy! targetbs 0 src-bspos)
    ;; overwrite the new blank-locations
    (bytes-set! targetbs 0 (rcpair->rcbyte (rc+ crc (rcbyte->rcpair (ebms-cbref an-ebms)))))
    (bytes-copy! targetbs 1
                 (ebms-newcbconf an-ebms))
    ;; set moved piece
    (bytes-copy! targetbs piece-start
                 (charify-int (bitwise-xor (intify src-bspos piece-start piece-end) ; piece(s) of this type in source position
                                           ; piece change-bits
                                           (+
                                            ; starting location of moved-piece
                                            (arithmetic-shift 1 ploc0)
                                            ; ending location of moved-piece
                                            (arithmetic-shift 1
                                                              (cell-to-loc (rc+ crc ;(ebms-cbref an-ebms)
                                                                                (rcbyte->rcpair (ebms-newcloc an-ebms)) ; canonical new-loc of piece
                                                                                )))
                                            ))))
    (set! new-hc-position (make-hcpos targetbs))
    ;; set the hashcode and the hc-position
    ;(set-hc-position-hc! the-hcpos (equal-hash-code targetbs))
    (hash-set! *expansion-hash* (equal-hash-code targetbs) new-hc-position);;Could also check out if we should use hash-ref before to see if it exists, or just replace.
    (when (is-goal? new-hc-position)
      (set-found-goal new-hc-position))
))

         


;; expand*: hc-position int -> void
;; the new successor generation utilizing the spaceindex
; -> two-part double-hash version <-
(define (expand* hc-s)
  (let* ([bs (hc-position-bs hc-s)]     ; 
         [config-ref-cell (rcbyte->rcpair (bytes-ref bs 0))] ; the row-col translation of the blank-config
         [canonical-blank-config (subbytes bs 1 4)]
         [possible-moves-hash (hash-ref *spaceindex* canonical-blank-config)]
         )
    
    ;(printf "index: ~a~%" canonical-blank-config)
    (for* ([i (in-range 4 *num-pieces*)]
           )
      (let* ([ptype (vector-ref *bs-ptype-index* i)]
             [ploc0 (- (bytes-ref bs i) *charify-offset*)]
             [canonical-pieceloc
              (let ([rlocp (register-loc-to-pair ploc0 config-ref-cell)])
                ;(printf "canonical-pieceloc: ~a~%" rlocp)
                #|(when (or (< (car rlocp) -2) (< (cdr rlocp) (- *bw*)))
                  (error (format "expand*: strange registered pieceloc as ~a from ploc0=~a which is cell=~a for blank-config=~a and ref=~a"
                                 rlocp ploc0 (loc-to-cell ploc0) canonical-blank-config config-ref-cell)))|#
                rlocp)]
             [moves-for-ptype-at-location
              (and (>= (car canonical-pieceloc) -2)
                   (>= (cdr canonical-pieceloc) (- *bw*))
                   (hash-ref possible-moves-hash 
                             (bytes ptype (rcpair->rcbyte canonical-pieceloc)) ;(cons ptype canonical-pieceloc)
                             ret-false))])
        (when moves-for-ptype-at-location
          (for ([ebms moves-for-ptype-at-location])
            ; create the new position, and write it to the buffer 
            ;(generate-and-write-new-pos expanded-ptr bs ebms ptype ploc0 config-ref-cell)
            
            ;create the new position, and add it to the hash table
            (generate-and-write-new-pos-ht bs ebms ptype ploc0 config-ref-cell)
            
            ; and go to the next one
            
            ))))
    ))



;(block10-init)   ;  160010 possible even-better-move-schema
;(climb12-init)
;(climb15-init)   ; 
;(climbpro24-init)
;(time (compile-ms-array! *piece-types* *bh* *bw*))
;(time (compile-spaceindex (format "~a~a-spaceindex.rkt" "stpconfigs/" *puzzle-name*)))

;; canonicalize the *start* blank-configuration
#|
(let* ([spacelist (bwrep->list (intify (hc-position-bs *start*) 0 4))]
       [cbref (rcpair->rcbyte (loc-to-cell (car spacelist)))]
       [canonical-spaces (apply canonize spacelist)])
  (bytes-set! (hc-position-bs *start*) 0 cbref)
  (bytes-copy! (hc-position-bs *start*) 1 canonical-spaces)
  (hc-position-bs *start*))
|#
;(expand* *start* 0)
;(test)