#lang racket

(require "stp-init.rkt"
         "stp-solve-base.rkt"
         "stp-fringefilerep.rkt"
         "stp-spaceindex.rkt")
(require racket/fixnum)


(define *max-depth* 10)(set! *max-depth* 31)
(define *most-positive-fixnum* 0)
(define *most-negative-fixnum* 0)
(cond [(fixnum? (expt 2 61))
       (set! *most-positive-fixnum* (fx+ (expt 2 61) (fx- (expt 2 61) 1)))  ;; ****** only on 64-bit architectures *****
       (set! *most-negative-fixnum* (fx+ (fx* -1 (expt 2 61)) (fx* -1 (expt 2 61))))];; ***** likewise *****
      [else 
       ;; For the cluster platform use the following:
       (set! *most-positive-fixnum* (fx+ (expt 2 29) (fx- (expt 2 29) 1)))  ;; ****** only on our cluster, wcp *****
       (set! *most-negative-fixnum* (fx+ (fx* -1 (expt 2 29)) (fx* -1 (expt 2 29))))]);; ***** likewise *****

(define *found-goal* #f)


(define f-score (make-hash)) ;; hash from raw-position to value
(define g-score (make-hash)) ;; hash from raw-position to value
(define closed-set (set)) ;; set of raw-position
(define open-set (set)) ;; set of raw-position
(define open-list empty) ;; sorted by f-value
(define pseudo-depth 0)

;; a*-search:  int -> #f or position
;; A* search in memory in order to estimate savings of heuristics
;; closed: is hash-table positions that have been expanded already
;; open: is list of positions sorted by f=g+h values to be expanded
(define (a*-search)
  (when (> (hash-ref g-score (first open-list)) pseudo-depth)
    (printf "pseudo-depth ~a w/ pseudo-fringe-size ~a~%" (hash-ref g-score (first open-list)) (set-count open-set))
    (set! pseudo-depth (hash-ref g-score (first open-list))))
  (cond [(set-empty? open-set)
         (printf "exhausted the space~%") #f]
        [else
         (cond [*found-goal* (printf "found goal with ~a closed positions and ~a on open-list~%"
                                    (set-count closed-set) 
                                    (length open-list))
                             *found-goal*]
               [else 
                
                (let* ([current (first open-list)]
                       ;; expand the first position in the open list
                       [successors (expand-a* current)])
                  (set! open-set (set-remove open-set current))
                  (set! open-list (rest open-list))
                  (set! closed-set (set-add closed-set current))
                  ;; insert successors that are not found in closed into sorted open
                  (for ([s successors])
                    (let* ([tent-gscore (add1 (hash-ref g-score current))]
                           [tent-fscore (+ tent-gscore 0)])
                      ;(printf "for rawpos: ~a tent-gscore=~a and tent-fscore=~a~%" s tent-gscore tent-fscore)
                      (cond [(and (set-member? closed-set s)
                                  (>= tent-fscore (hash-ref f-score s)))]
                            [(or (not (set-member? open-set s))
                                 (< tent-fscore (hash-ref f-score s)))
                             (hash-set! g-score s tent-gscore)
                             (hash-set! f-score s tent-fscore)
                             (unless (set-member? open-set s)
                               (set! open-set (set-add open-set s))
                               (set! open-list (cons s open-list)))])))
                  (set! open-list (sort open-list (lambda (p1 p2) (< (hash-ref f-score p1) (hash-ref f-score p2)))))
                  (a*-search)
                  )])]))

;; expand-a*: raw-position -> (vectorof raw-position)
;; expand a single raw-position using the expand* functionality
;; create a fake hc-position wrapper and then unwrap the resulting successors
;; returning a vector of raw-positions
(define (expand-a* p)
  ;(printf "begin expand-a* of raw pos ~a~%" p)
  (let* ([num-expanded (expand* (hc-position -1 p) 0)]
         ;[ignore (printf "finished expand* and got ~a successors~%" num-expanded)]
         [res (for/vector ([i num-expanded])
                          (when (is-goal? (vector-ref *expansion-space* i)) (set! *found-goal* (vector-ref *expansion-space* i)))
                          (bytes-copy (hc-position-bs (vector-ref *expansion-space* i))))])
    ;(printf "finishing expand-a* after making the vector of raw positions from the expansion-space~%")
    res
    ))


(block10-init)
;(climb12-init)
;(climb15-init)
;(climbpro24-init)
(compile-spaceindex (format "~a~a-spaceindex.rkt" "stpconfigs/" *puzzle-name*))

;; canonicalize the *start* blank-configuration
(let* ([spacelist (bwrep->list (intify (hc-position-bs *start*) 0 4))]
       [cbref (rcpair->rcbyte (loc-to-cell (car spacelist)))]
       [canonical-spaces (apply canonize spacelist)])
  (bytes-set! (hc-position-bs *start*) 0 cbref)
  (bytes-copy! (hc-position-bs *start*) 1 canonical-spaces)
  (hc-position-bs *start*))

(set! open-set (set-add open-set (hc-position-bs *start*)))
(set! open-list (list (hc-position-bs *start*)))
(hash-set! g-score (hc-position-bs *start*) 0)
(hash-set! f-score (hc-position-bs *start*) (+ 0 0))

(time (a*-search))
