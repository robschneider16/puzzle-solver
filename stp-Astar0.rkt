#lang racket

(require "stp-init.rkt"
         "stp-solve-base.rkt"
         "stp-fringefilerep.rkt"
         "stp-spaceindex.rkt")

(define *max-depth* 10)(set! *max-depth* 200)
(define *target-cell* '(r . c))
(define *found-goal* #f)

(define scores (make-hash))  ;; hash for (mcons g . f) pairs
(define closed-set (set)) ;; set of raw-position
(define open-set (set)) ;; set of raw-position
(define open-list empty) ;; sorted by f-value
(define open-list-new empty)
(define pseudo-depth 0)
(define local-positions-handled 0) ;; number of successors generated and checked for duplicate within given A* iteration
(define total-positions-handled 0) ;; total number of successors ever generated

(define vools (make-vector 230 empty))
(define max-fval-sofar 0)

;;--- G and F SCORE PAIRS ---------------------------------------

;; get-scores: raw-position -> (mcons g . f)
(define (get-scores p) (hash-ref scores p (lambda () #f)))
;; g-score: (mcons g . f) -> number
;; extract the g-score for the position
(define (g-score p [mpair (get-scores p)]) (and mpair (mcar mpair)))
;; set-g-score!: raw-position number -> void
;; sets the g-score if present, or throws error if not (could consider adding a pair with unset f-score)
(define (set-g-score! p g) (set-mpair-field! p g set-mcar! "set-g-score!"))
;; f-score: raw-position -> number
(define (f-score p [mpair (get-scores p)]) (mcdr mpair))
;; set-f-score!: raw-position number -> void
;; sets the f-score if present, or throws error if not (could consider adding a pair with unset g-score)
(define (set-f-score! p f) (set-mpair-field! p f set-mcdr! "set-f-score!"))
;; add-scores!: raw-position number number -> void
(define (add-scores! p g f) (hash-set! scores p (mcons g f)))
;; set-mpair-field: raw-position number (mpair -> void) string -> void
(define (set-mpair-field! p n setter! name)
  (let ([scores (get-scores p)]) 
    (unless scores (error (format "~a: attempt to set score value for position ~a not in hash" name p)))
    (setter! scores n)))

;;--------------------------------------------------------------

;;--- VOOS OPEN-SET --------------------------------------------



;;--------------------------------------------------------------

;; a*-search:  int -> #f or position
;; A* search in memory in order to estimate savings of heuristics
;; fdepth is an actual literal index into the vools vector of open-lists -- only fscores need to be translated
;; via fscore->voolindex
(define (a*-search fdepth) 
  (let ([min-voolindex fdepth])
    (when (empty? open-list) ;(> (hash-ref g-score (first open-list-new)) pseudo-depth)
      (printf "fdepth ~a w/ going 'deeper' after handling ~a successors~%" 
              fdepth local-positions-handled)
      (set! open-list (vector-ref vools fdepth))
      (vector-set! vools fdepth empty)
      (set! total-positions-handled (+ total-positions-handled local-positions-handled))
      (set! local-positions-handled 0)
      )
    (cond [(>= fdepth *max-depth*) (printf "exhausted the space~%") #f]
          [(and (empty? open-list) (empty? (vector-ref vools fdepth)))
           (a*-search (add1 fdepth))]
          [*found-goal* (printf "found goal after ~a moves with ~a closed positions and ~a on unfinished open-lists and total successors handled ~a~%"
                                (g-score (hc-position-bs *found-goal*)) (set-count closed-set) (for/sum ([l vools]) (length l)) total-positions-handled)
                        *found-goal*]
          [else 
           (let* ([current (first open-list)]
                  ;; expand the first position in the open list
                  [successors (expand-a* current)])
             (set! open-set (set-remove open-set current))
             (set! open-list (rest open-list))
             (set! closed-set (set-add closed-set current))
             (set! local-positions-handled (+ (vector-length successors) local-positions-handled))
             ;; insert successors that are not found in closed into sorted open
             (for ([s successors])
               (let* ([score-pairs (get-scores current)]
                      [tent-gscore (add1 (g-score current score-pairs))]
                      [tent-fscore (+ tent-gscore (heuristic s))])
                 (when (< (fscore->voolindex tent-fscore) min-voolindex)
                   ;(printf "skipped over at fdepth=~a: ~a tent-gscore=~a and tent-fscore=~a~%" fdepth s tent-gscore tent-fscore)
                   (set! min-voolindex (fscore->voolindex tent-fscore)))
                 (cond [(and (set-member? closed-set s)
                             (>= tent-fscore (f-score s)))]
                       [(or (not (set-member? open-set s))
                            (< tent-fscore (f-score s)))
                        (when (and (set-member? open-set s) (< tent-fscore (f-score s)))
                          ;(printf "bring ~a forward from ~a to ~a while fdepth=~a~%" s (hash-ref f-score s) tent-fscore fdepth)
                          (vector-set! vools (fscore->voolindex (f-score s))
                                       (remove s (vector-ref vools (fscore->voolindex (f-score s)))))
                          (vector-set! vools (fscore->voolindex tent-fscore)
                                       (cons s (vector-ref vools (fscore->voolindex tent-fscore)))))
                        (add-scores! s tent-gscore tent-fscore)
                        (unless (set-member? open-set s)
                          (set! open-set (set-add open-set s))
                          (vector-set! vools (fscore->voolindex tent-fscore)
                                       (cons s (vector-ref vools (fscore->voolindex tent-fscore)))))])))
             (a*-search min-voolindex)
             )])))

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
                          (when (is-goal? (vector-ref *expansion-space* i)) 
                            (set! *found-goal* (vector-ref *expansion-space* i)))
                          (bytes-copy (hc-position-bs (vector-ref *expansion-space* i))))])
    ;(printf "finishing expand-a* after making the vector of raw positions from the expansion-space~%")
    res
    ))

;;--- HEURISTICS ------------------------------------------------------
;; these could/should be pre-computed and cached in a lookup table
;; * if target piece is more than one move (not distance) away, then we may be able to multiply distance by two
;;   in order to account for the need to move the blanks back around in order to move the target again

;; a-heuristic: raw-position -> number
;; abstract heusristic for puzzles with a 4-square inverted T target tile
(define (a-heuristic p)
  (let* ([pt1-loc (- (bytes-ref p 4) *charify-offset*)]
         [pt1-cell (loc-to-cell pt1-loc)]
         [r-diff (abs (- (car *target-cell*) (car pt1-cell)))]
         [c-diff (abs (- (cdr *target-cell*) (cdr pt1-cell)))]
         [half-c-diff (/ c-diff 2)]
         [mmd (+ half-c-diff r-diff)]
         )
    (cond [(> r-diff c-diff) r-diff]
          [(even? c-diff) (add1 half-c-diff)]
          [else (ceiling half-c-diff)])))


;; b10-heuristic: raw-position -> number
;; computes an admissible estimate of the number of moves from this position to the goal
;; using the modified manhattan distance of the target tile to its goal location
(define (b10-heuristic p)
  (let* ([pt1-loc (- (bytes-ref p 4) *charify-offset*)]
         [pt1-cell (loc-to-cell pt1-loc)]
         [r-diff (abs (- (car *target-cell*) (car pt1-cell)))]
         [c-diff (abs (- (cdr *target-cell*) (cdr pt1-cell)))]
         )
    (+ (* (sub1 r-diff) 2)
       (/ (min 1 r-diff)  2) 
       (/ c-diff 2) ;; col-displacemint divided by 2
       )))

(define (c12-heuristic p)
  (let* ([pt1-loc (- (bytes-ref p 4) *charify-offset*)]
         [pt1-cell (loc-to-cell pt1-loc)]
         [r-diff (abs (- (car *target-cell*) (car pt1-cell)))]
         [c-diff (abs (- (cdr *target-cell*) (cdr pt1-cell)))]
         )
    (+ r-diff
       (/ c-diff 2)
       )))

(define (c12-heuristic+ p)
  (let* ([pt1-loc (- (bytes-ref p 4) *charify-offset*)]
         [pt1-cell (loc-to-cell pt1-loc)]
         [r-diff (abs (- (car *target-cell*) (car pt1-cell)))]
         [c-diff (abs (- (cdr *target-cell*) (cdr pt1-cell)))]
         [half-c-diff (/ c-diff 2)]
         [mmd (+ half-c-diff r-diff)]
         )
    (+ mmd (floor (sqr (sub1 (max 1 mmd)))))))

(define (c15-heuristic p)
  (let* ([pt1-loc (- (bytes-ref p 4) *charify-offset*)]
         [pt1-cell (loc-to-cell pt1-loc)]
         [r-diff (abs (- (car *target-cell*) (car pt1-cell)))]
         [c-diff (abs (- (cdr *target-cell*) (cdr pt1-cell)))]
         [half-c-diff (/ c-diff 2)]
         [mmd (+ half-c-diff r-diff)]
         )
    (+ mmd (floor (sqr (max 0 (sub1 mmd)))))))



;;--- HEURISTICS ------------------------------------------------------

(block10-init)
;(climb12-init)
;(climb15-init)
;(climbpro24-init)
(compile-spaceindex (format "~a~a-spaceindex.rkt" "stpconfigs/" *puzzle-name*))

(define heuristic
  (case *puzzle-name*
    [("block10v12") b10-heuristic]
    [("climb12") c12-heuristic+]))

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
(add-scores! (hc-position-bs *start*) 0 (+ 0 (heuristic (hc-position-bs *start*))))

(time (a*-search 0))
