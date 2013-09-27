#lang racket

(require "stp-init.rkt"
         "stp-solve-base.rkt"
         "stp-fringefilerep.rkt")
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



;; fringe-file-search: [file(listof position)] [file(listof position)] int -> ...
;; using file for storing fringes, perform a fringe BFS starting at the given state until depth is 0
(define (fringe-file-search depth [found-goal? #f] [npos 1])
  (let ([prev-fringe (list->set (read-fringe-from-file "prev-fringe"))]
        [current-fringe (list->set (read-fringe-from-file "current-fringe"))]) ; current fringe is a FILE
    ;;(printf "current-fringe: ~a~%" current-fringe)
    (cond [(or (set-empty? current-fringe) (> depth *max-depth*)) 
           (printf "exhausted the space after ~a positions~%" npos) #f]
          [else
           (cond [found-goal? (printf "found goal after encountering ~a positions~%" npos)
                              found-goal?]
                 [else (let* ([new-fringe (for/set ([p (for/fold ([expansions (set)])
                                                         ([p current-fringe])
                                                        (set-union expansions (expand p)))]
                                                    #:unless (or (set-member? prev-fringe p)
                                                                 (set-member? current-fringe p)))
                                            (when (is-goal? p) (set! found-goal? p))
                                            p)])
                         (printf "At depth ~s fringe has ~a positions~%" depth (set-count current-fringe))
                         
                         #|(when (member depth '(5 6))
                           (for ([s (sort (for/list ([p current-fringe])
                                            (stringify p))
                                          string<?)])
                             (printf "~a~%" s)))|#
                         (rename-file-or-directory "current-fringe" "prev-fringe" #t)
                         (write-fringe-to-disk (sort (set->list new-fringe) hcposition<?) "current-fringe")
                         (fringe-file-search (add1 depth) found-goal? (+ npos (set-count new-fringe))))])])))

;; fringe-mem-search: (setof position) (setof positions) int -> #f or position
;; search in memory
(define (fringe-mem-search prev-fringe current-fringe depth [found-goal? #f] [npos 1])
  ;;(printf "current-fringe: ~a~%" current-fringe)
  (cond [(or (set-empty? current-fringe) (> depth *max-depth*)) 
         (printf "exhausted the space after ~a positions~%" npos) #f]
        [else
         (cond [found-goal? (printf "found goal after encountering ~a positions~%" npos)
                            found-goal?]
               [else (let* ([new-fringe (for/set ([p (for/fold ([expansions (set)])
                                                       ([p current-fringe])
                                                       (set-union expansions (expand p)))]
                                                  #:unless (or (set-member? prev-fringe p)
                                                               (set-member? current-fringe p)))
                                          (when (is-goal? p) (set! found-goal? p))
                                          p)])
                       (printf "At depth ~s fringe has ~a positions~%" depth (set-count current-fringe))
                       (fringe-mem-search current-fringe new-fringe (add1 depth) found-goal? (+ npos (set-count new-fringe))))])]))

;; expand-fringe-self: fringe fringe int -> fringe
;; expand just the current-fringe and remove duplicates in the expansion and repeats from prev-fringe
;; returning the new fringe
(define (expand-fringe-self pf cf depth)
  (let* ([prev-fringe-set (for/fold ([the-fringe (set)])
                            ([sgmnt (fringe-segments pf)])
                            (set-union the-fringe
                                       (list->set (read-fringe-from-file (filespec-fullpathname sgmnt)))))] ; pf- and cf-spec's in expand-fringe-self should have empty fbase
         [current-fringe-vec 
          (list->vector (for/fold ([the-fringe empty])
                          ([sgmnt (reverse (fringe-segments cf))])
                          (append (read-fringe-from-file (filespec-fullpathname sgmnt)) the-fringe)))]
         [new-cf-name (format "fringe-d~a" depth)]
         ;[prntmsg (printf "finished reading the fringes~%")]
         [exp-ptr 0]
         [expand-them (for ([p-to-expand current-fringe-vec])
                        (set! exp-ptr (expand p-to-expand exp-ptr)))]
         [res (set->list (for/set ([i exp-ptr]
                                   #:unless (or (set-member? prev-fringe-set (vector-ref *expansion-space* i))
                                                (position-in-vec? current-fringe-vec (vector-ref *expansion-space* i))))
                           (when (is-goal? (vector-ref *expansion-space* i)) (set! *found-goal* (vector-ref *expansion-space* i)))
                           (vector-ref *expansion-space* i)))]
         )
    #|(printf "Finished the work packet generating a set of ~a positions~%" (set-count res))
    (for ([p res])
      (printf "pos: ~a~%~a~%" (stringify p) p))|#
    (for ([sgmnt (fringe-segments pf)]) (delete-file (filespec-fullpathname sgmnt)))
    (write-fringe-to-disk (sort res hcposition<?) new-cf-name)
    (make-fringe "" (list (make-filespec *most-negative-fixnum* *most-positive-fixnum* new-cf-name (length res) (file-size new-cf-name) "")) (length res))))


(block10-init)
;(climb12-init)
;(climb15-init)
;(climbpro24-init)
(compile-ms-array! *piece-types* *bh* *bw*)

(write-fringe-to-disk empty "prev-fringe")
(write-fringe-to-disk (list *start*) "current-fringe")

(time (fringe-file-search 1))
;(time (fringe-mem-search (set) (set *start*) 1))
