#lang racket/base

(require racket/list
         racket/set
         racket/math
         "stp-init.rkt"
         "stp-solve-base.rkt"
         "stp-fringefilerep.rkt"
         "stp-spaceindex.rkt"
         ;rnrs/sorting-6 ;; provides vector-sort!
         "myvectorsort.rkt"
         )

(define *max-depth* 10)(set! *max-depth* 200)
(define *target-cell* '(r . c))
(define *found-goal* #f)

(define scores (make-hash))  ;; hash for (mcons g . f) pairs indexed by position
(define closed-set (set)) ;; set of raw-position
(define open-set (set)) ;; set of raw-position
(define open-list empty) ;; sorted by f-value
(define open-list-new empty)
(define pseudo-depth 0)
(define local-positions-handled 0) ;; number of successors generated and checked for duplicate within given A* iteration
(define total-positions-handled 0) ;; total number of successors ever generated

(define vools (make-vector 230 empty))
(define max-fval-sofar 0)

;;--- DATA DEFINITIONS ------------------------------------------

;; a raw-position is a bytestring of length *num-pieces*, one byte for each piece

;; a a*pos is a bytstring one longer than a raw-position with the first byte being the f-score (combineg h and g)


;;--- UTILITIES -------------------------------------------------


;;--- G and F SCORE PAIRS ---------------------------------------

;; get-scores: raw-position -> (mcons g . f)
;; retrieve the stored g and f scores for the given raw position
(define (get-scores p) (hash-ref scores p (lambda () #f)))

;; g-score: raw-position -> number
;; retrieve the g score as stored in the scores hashtable for this raw-position
(define (g-score p [mpair (get-scores p)]) (and mpair (mcar mpair)))

;; f-score: raw-position -> number
(define (f-score p [mpair (get-scores p)]) (and mpair (mcdr mpair)))

;; set-g-score!: raw-position number -> void
;; sets the g-score if present, or throws error if not (could consider adding a pair with unset f-score)
(define (set-g-score! p g) (set-mpair-field! p g set-mcar! "set-g-score!"))

;; set-f-score!: raw-position number -> void
;; sets the f-score if present, or throws error if not (could consider adding a pair with unset g-score)
(define (set-f-score! p f) (set-mpair-field! p f set-mcdr! "set-f-score!"))

;; set-mpair-field: raw-position number (mpair -> void) string -> void
(define (set-mpair-field! p n setter! name)
  (let ([scores (get-scores p)]) 
    (unless scores (error (format "~a: attempt to set score value for position ~a not in hash" name p)))
    (setter! scores n)))

;; add-scores!: raw-position number number -> void
;; dass the g and f pair to the scores hashtable with key the given raw-position
(define (add-scores! p g f) (hash-set! scores p (mcons g f)))


;; get-f-val: a*pos -> byte
(define (get-f-val p) (bytes-ref p 0))
;; get-g-val: a*pos -> byte
(define (get-g-val p) (- (get-f-val p) (heuristic (subbytes p 1))))
;; make-a*pos: number(0-255) byte-string -> byte-string
(define (make-a*pos f bs) (bytes-append (bytes f) bs))

;;--------------------------------------------------------------


;;--- FILE-BASED A* SEARCH -------------------------------------
;; Rational reconstruction of Korf's A* w/ DDD based on his description in AAAI04
;; define: best-f as lowest g + h in open nodes
;; open and closed mixed in one list, with closed having f-values lower than best-f
;; define: a*pos is f-val byte, followed by raw-position
;; our f-values should be less than 255 and may therefore fit in one byte

;; fake-buffer: vector of bytestrings for output
(define fb-capacity 2000000)
(define fakebuffer (make-vector (+ fb-capacity 200) (make-bytes (add1 *num-pieces*))))
(define fb-ptr 0)

;; write-to-fb: a*pos -> void
(define (write-to-fb a*p)
  (vector-set! fakebuffer fb-ptr a*p)
  (set! fb-ptr (add1 fb-ptr)))

;; fb-full?:  -> boolean
(define (fb-full?) (> fb-ptr fb-capacity))

;; flush-fb:  -> void
;; write fakebuffer to file but remove duplicates -- preserving the position with lowest g value
(define (flush-fb [ofile "tobemerged"])
  (let ([last-pos #"noneseenyet"]
        [actually-written 0])
    (printf "flushing fakebuffer with ~a positions~%" fb-ptr)
    (with-output-to-file ofile
      (lambda ()
        (for ([n fb-ptr]
              [p fakebuffer]
              #:unless (bytes=? (subbytes p 1) last-pos))
          (write-bs->file p (current-output-port) (add1 *num-pieces*))
          (set! actually-written (add1 actually-written))))
      #:exists 'replace)
    (set! fb-ptr 0)
    (printf "........ wrote ~a non-duplicate positions~%" actually-written)))

;; write-fb-to-file: -> void
;; sort and write fakebuffer to file
(define (write-fb-to-file)
  (vector-sort! fakebuffer 
                (lambda (p1 p2) 
                  (let ([p1pos (subbytes p1 1)]
                        [p2pos (subbytes p2 1)])
                    (or (bytes<? p1pos p2pos)
                        (and (bytes=? p1pos p2pos)
                             ;; in this case, both positions have the same h value so whichever f value is less has lower g value
                             (< (bytes-ref p1 0) (bytes-ref p2 0))))))
                0 fb-ptr)
  (flush-fb))

;; merge-two-files: (listof file) -> void
;; merge two files assumed to be sorted by position  files in lof removing duplicate positions, 
;; retaining lowest f (i.e., g) score of any duplicates
(define (merge-two-files f1 f2 [ofile "newastarnodelist"])
  (let ([i1 (open-input-file f1)]
        [i2 (open-input-file f2)]
        [last #"nonprevious"])
    (with-output-to-file ofile
      (lambda ()
        (let mrg ([p1 (read-bytes (add1 *num-pieces*) i1)]
                  [p2 (read-bytes (add1 *num-pieces*) i2)]
                  [last-written last])
          (cond [(and (eof-object? p1) (eof-object? p2)) void]
                [(eof-object? p1) (write-bs->file p2 (current-output-port) (add1 *num-pieces*))
                                  (mrg p1 (read-bytes (add1 *num-pieces*) i2) p2)]
                [(eof-object? p2) (write-bs->file p1 (current-output-port) (add1 *num-pieces*))
                                  (mrg (read-bytes (add1 *num-pieces*) i1) p2 p1)]
                [(bytes=? (subbytes p1 1) (subbytes last-written 1)) (mrg (read-bytes (add1 *num-pieces*) i1) p2 last-written)]
                [(bytes=? (subbytes p2 1) (subbytes last-written 1)) (mrg p1 (read-bytes (add1 *num-pieces*) i2) last-written)]
                [(bytes<? (subbytes p1 1) (subbytes p2 1)) (write-bs->file p1 (current-output-port) (add1 *num-pieces*))
                                                           (mrg (read-bytes (add1 *num-pieces*) i1) p2 p1)]
                [(and (bytes=? (subbytes p1 1) (subbytes p2 1))
                      (< (bytes-ref p1 0) (bytes-ref p2 0))) (write-bs->file p1 (current-output-port) (add1 *num-pieces*))
                                                             (mrg (read-bytes (add1 *num-pieces*) i1) p2 p1)]
                [else (write-bs->file p2 (current-output-port) (add1 *num-pieces*))
                      (mrg p1 (read-bytes (add1 *num-pieces*) i2) p2)]
                )))
      #:exists 'replace)
    (close-input-port i1)
    (close-input-port i2)))


;; a*-file-search: string number ->
;; 
;; while goal-not-found or more-work-to-do
;;  for each position p in file
;;   when f-val of p = current-f-val, 
;;    for each successor s in expansion of p
;;     when s is goal, report success
;;     write s to buffer for periodic sort/write to disk
;;     if f-val of s is <= best-f, recursively expand s
;;  merge sorted files together with node-list, removing duplicates, retaining lowest g-score (or f-score) among duplicates
(define (a*-file-search file-name best-f)
  (printf "A*-file-search: best-f-depth=~a~%" best-f)
  (let ([iport (open-input-file file-name)]
        [num-read 0])
    (for ([a*p (in-port (lambda (i) (read-bytes (add1 *num-pieces*) i)) iport)])
      (set! num-read (add1 num-read))
      (when (= (get-f-val a*p) best-f)
        (process-position a*p best-f)))
    (write-fb-to-file)
    ;; merge duplicates, etc.
    (close-input-port iport)
    (cond [*found-goal*
           (printf "found goal after ~a moves with ~a closed positions and ~a on unfinished open-lists and total successors handled ~a~%"
                   (g-score (hc-position-bs *found-goal*)) (set-count closed-set) (for/sum ([l vools]) (length l)) total-positions-handled)
           *found-goal*]
          [else 
           (merge-two-files "astarnodelist" "tobemerged")
           (rename-file-or-directory "newastarnodelist" file-name #t)
           (printf "... finished pass reading ~a positions from file~%" num-read)
           (a*-file-search file-name (add1 best-f))])
    ))

;; process-position: a*pos number  -> 
;; expand and do whatever is appropriate for each successor
(define (process-position a*p best-f)
  (let* ([parent-f-val (get-f-val a*p)]
         [parent-pos (subbytes a*p 1)]
         [parent-g-val (- parent-f-val (heuristic parent-pos))])
    (for ([s (expand-a* parent-pos)])
      (let* ([s-h (heuristic s)]
             [s-f (+ s-h parent-g-val 1)]
             [a*s (make-a*pos s-f s)])
        (write-to-fb a*s) 
        (when (fb-full?) (write-fb-to-file))
        (when (<= s-f best-f)
          ;(printf "process-position: filling in expansion for f-value (~a) better than best-f (~a)~%" s-f best-f)
          (process-position a*s best-f))))))


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
          [*found-goal* 
           (printf "found goal after ~a moves with ~a closed positions and ~a on unfinished open-lists and total successors handled ~a~%"
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
                            (printf "found goal: ~s~%" (hc-position-bs (vector-ref *expansion-space* i)))
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
    (ceiling (+ (/ c-diff 2)
                (/ (- r-diff c-diff) 2)
                (* (max 0 (sub1 r-diff)) 2)))))

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
(with-output-to-file "astarnodelist"
  (lambda () (write-bs->file (bytes-append (bytes (f-score (hc-position-bs *start*))) (hc-position-bs *start*))
                             (current-output-port) (add1 *num-pieces*)))
  #:exists 'replace)

;(time (a*-search 0))
(time (a*-file-search "astarnodelist" (f-score (hc-position-bs *start*))))