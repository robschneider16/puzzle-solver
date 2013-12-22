#lang racket

(require "stp-init.rkt"
         "stp-solve-base.rkt"
         "stp-fringefilerep.rkt"
         "stp-spaceindex.rkt")
(require racket/fixnum)


(define *max-depth* 10)(set! *max-depth* 200)
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
(define open-list-new empty)
(define pseudo-depth 0)
(define positions-handled 0) ;; number of successors generated and checked for duplicate etc.

(define vools (make-vector 230 empty))
(define *target-cell* '(r . c))

;; a*-search:  int -> #f or position
;; A* search in memory in order to estimate savings of heuristics
;; fdepth is an actual literal index into the vools vector of open-lists -- only fscores need to be translated
;; via fscore->voolindex
(define (a*-search fdepth) 
  (when (empty? open-list) ;(> (hash-ref g-score (first open-list-new)) pseudo-depth)
    (printf "fdepth ~a w/ going deeper after handling ~a successors~%" 
            fdepth positions-handled)
    (set! open-list (vector-ref vools fdepth))
    (vector-set! vools fdepth empty)
    (set! positions-handled 0)
    )
  (cond [(>= fdepth *max-depth*) (printf "exhausted the space~%") #f]
        [(and (empty? open-list) (empty? (vector-ref vools fdepth)))
         (a*-search (add1 fdepth))]
        [*found-goal* (printf "found goal with ~a closed positions and ~a on open-list~%"
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
           (set! positions-handled (+ (vector-length successors) positions-handled))
           ;; insert successors that are not found in closed into sorted open
           (for ([s successors])
             (let* ([tent-gscore (add1 (hash-ref g-score current))]
                    [tent-fscore (+ tent-gscore (heuristic s))])
               ;(printf "for rawpos: ~a tent-gscore=~a and tent-fscore=~a~%" s tent-gscore tent-fscore)
               (cond [(and (set-member? closed-set s)
                           (>= tent-fscore (hash-ref f-score s)))]
                     [(or (not (set-member? open-set s))
                          (< tent-fscore (hash-ref f-score s)))
                      (when (and (set-member? open-set s) (< tent-fscore (hash-ref f-score s)))
                        (vector-set! vools (fscore->voolindex (hash-ref f-score s))
                                     (remove s (vector-ref vools (fscore->voolindex (hash-ref f-score s)))))
                        (vector-set! vools (fscore->voolindex tent-fscore)
                                     (cons s (vector-ref vools (fscore->voolindex tent-fscore)))))
                      (hash-set! g-score s tent-gscore)
                      (hash-set! f-score s tent-fscore)
                      (unless (set-member? open-set s)
                        (set! open-set (set-add open-set s))
                        (vector-set! vools (fscore->voolindex tent-fscore)
                                     (cons s (vector-ref vools (fscore->voolindex tent-fscore)))))])))
           (a*-search fdepth)
           )]))

;; fscore->voolindex: number -> number
;; translate fscore to index into vools (by multiplying by 2)
(define (fscore->voolindex fs)
  (* 2 fs))

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

;;--- HEURISTICS ------------------------------------------------------
;; these could/should be pre-computed and cached in a lookup table
;; * if target piece is more than one move (not distance) away, then we may be able to multiply distance by two
;;   in order to account for the need to move the blanks back around in order to move the target again

;; b10-heuristic: raw-position -> number
;; computes an admissible estimate of the number of moves from this position to the goal
;; using the modified manhattan distance of the target tile to its goal location
(define (b10-heuristic p)
  (let* ([pt1-loc (- (bytes-ref p 4) *charify-offset*)]
         [pt1-cell (loc-to-cell pt1-loc)]
         )
    (+ (/ (abs (- (car *target-cell*) (car pt1-cell))) 2) ;; row-displacemint divided by 2
       (/ (abs (- (cdr *target-cell*) (cdr pt1-cell))) 2) ;; col-displacemint divided by 2
       )))

(define (c12-heuristic p)
  (let* ([pt1-loc (- (bytes-ref p 4) *charify-offset*)]
         [pt1-cell (loc-to-cell pt1-loc)])
    (+ (abs (- (car *target-cell*) (car pt1-cell)))       ;; row-displacemint 
       (/ (abs (- (cdr *target-cell*) (cdr pt1-cell))) 2) ;; col-displacemint divided by 2
       )))

(define heuristic c12-heuristic)

    ;;--- HEURISTICS ------------------------------------------------------
    
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

;; initialization
(set! *target-cell* (loc-to-cell (- (cdr *target*) *charify-offset*)))
(set! open-set (set-add open-set (hc-position-bs *start*)))
(vector-set! vools (fscore->voolindex (heuristic (hc-position-bs *start*)))
             (list (hc-position-bs *start*)))
(hash-set! g-score (hc-position-bs *start*) 0)
(hash-set! f-score (hc-position-bs *start*) (+ 0 (heuristic (hc-position-bs *start*))))


(time (a*-search 0))
