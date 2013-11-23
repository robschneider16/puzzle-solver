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

; create the hash of possible moves for indexed by all possible configurations of blanks
(define (all-space-config)
  (for*/hasheq ([b1 (- *bsz* 3)]
                [b2 (in-range (add1 b1) (- *bsz* 2))]
                [b3 (in-range (add1 b2) (- *bsz* 1))]
                [b4 (in-range (add1 b3) *bsz*)]
                #:when (andmap (lambda (loc) (not (member loc *invalid-locs*))) (list b1 b2 b3 b4))
                ;;(andmap onboard? (map loc-to-cell (list b1 b2 b3 b4)))
                )
    (let ([spaceint (list->bwrep (list b1 b2 b3 b4))])
      (values spaceint (one-space-config spaceint)))))


;; extract moves for given space configuration
(define (one-space-config spaceint)
  (let ([plocvec (make-vector *bsz* #f)])
    (for*/fold ([r empty])
      ([ptype (in-range 1 (vector-length *piece-types*))]
       [loc *bsz*])
      (append (for*/list
                  ([dir 4]
                   [ms (list (array-ref *ms-array* ptype loc dir))]
                   #:when (can-move spaceint ptype loc dir plocvec ms))
                ; bundle the piece-type, location, direction and corresponding move-schema
                (list ptype loc dir ms))
              r))))

;; can-move : fixnum N N (vectorof boolean) move-schema -> boolean
;; determine if the proposed move can work
(define (can-move spaceint ptype loc dir plocvec ms)
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
       ))

(define (not-on-blanks ms spaceint ptype loc dir)
  (let ([pcells (vector-ref *piece-types* ptype)])
    (zero? (bitwise-and spaceint (list->bwrep (for/list ([cell (translate-piece pcells (loc-to-cell loc))]) (cell-to-loc cell)))))))


;; ------------------------
;; Using the new spaceindex structure

;; expand*: hc-position
;; the new successor generation utilizing the spaceindex




(block10-init)   ;  131179 prospective even-better-move-schema
;(climb15-init)   ; 2280811
(compile-ms-array! *piece-types* *bh* *bw*)
;(expand *start*)
;(test)