#lang racket

(require (planet gcr/riot))
(require rnrs/sorting-6)
(require data/heap)
(require srfi/1)
(require racket/fixnum)
(require racket/set)

(require "stp-init.rkt")
(require "stp-solve-base.rkt")
;(require profile)
;(instrumenting-enabled #t)
;(profiling-enabled #t)

(provide (all-defined-out))

(define *n-processors* 8)

(define *most-positive-fixnum* (fx+ (expt 2 61) (fx- (expt 2 61) 1)))  ;; ****** only on 64-bit architectures *****
(define *most-negative-fixnum* (fx+ (fx* -1 (expt 2 61)) (fx* -1 (expt 2 61))));; ***** likewise *****

;; Cluster/multi-process specific code for the sliding-tile puzzle solver
;; Currently assumes all in memory 
;; UPDATE: Adding write/read to/from disk functionality

;; write-to-disk: (listof position) string -> file(listof position)
;; write-to-disk takes a fringe and creates a file on disk with that fringe
(define (write-to-disk fringe file-name)
  (let ([my-output (open-output-file file-name #:exists 'replace)])
    (write fringe my-output)
    (close-output-port my-output)))

;; read-from-disk: file -> fringe
;; reads a file from a file path (if you are in the current directory just simply the file-name)
;; and returns the fringe that was in that file.
(define (read-from-disk file-path)
  (with-input-from-file file-path read))

;;----------------------------------------------------------------------------------------


;; expand-fringe-portion: (list int int) (setof position) (vectorof position) -> (vectorof position)
;; expand just the portion of the sorted-fringe-vector specified by the indices in the given range-pair.  
;; ASSUME: current-fringe-vec is sorted
(define (expand-fringe-portion range-pair prev-fringe-set current-fringe-vec)
  (let ((res (for/vector ([p (for/fold ([expansions (set)])
                               ([i (in-range (first range-pair) (second range-pair))])
                               (set-union expansions
                                          (expand (vector-ref current-fringe-vec i))))]
                          #:unless (or (set-member? prev-fringe-set p)
                                       (position-in-vec? current-fringe-vec p)))
               p)))
    #|(printf "Finished the work packet generating a set of ~a positions~%" (set-count res))
    (for ([p res])
      (printf "pos: ~a~%~a~%" (stringify p) p))|#
    ;;(vector-sort! position<? res) ; sort unneeded as resulting positions put into set
    res))

;;----------------------------------------------------------------------------------------
;; DISTRIBUTED EXPANSION AND MERGING OF FRINGES

;; a sampling-stat is a (vector int int int (vectorof int)
;; where the elements are:s
;; 0. total number of positions in the corresponding expansion
;; 1. minimum hash value of the positions
;; 2. maximum hash value of the positions
;; 3. vector of bin-counts for the range min-to-max divided into *n-processors* ranges

;; EXPANSION .....

;; make-vector-ranges: int -> (listof (list int int)
;; create the pairs of indices into the current-fringe-vector that will specify the part of the fringe each worker tackles
(define (make-vector-ranges vlength)
  (if (< vlength 10) ;;1000
      (list (list 0 vlength))
      (let ((start-list (build-list *n-processors* (lambda (i) (floor (* i (/ vlength *n-processors*)))))))
        (foldr (lambda (x r) (cons (list x (first (first r))) r)) 
               (list (list (last start-list) vlength)) 
               (drop-right start-list 1)))))

;; remote-expand-part-fringe: (vectorof position) (list int int) int -> (list sampling-stat (vectorof position))
;; given a vector containing the current fringe, a pair of indices into that vector, and a samping frequency,
;; expand the positions in the indices range, ignoring duplicates other than within the new fringe being constructed.
;; While building the expansion, maintain stats on hash-code values and a list of sampled hash-code values.
;; Return a pair containing the stats from this expansion and the expansion itself.
(define (remote-expand-part-fringe current-fringe ipair sample-freq)
  (let* ([start (first ipair)]
         [end (second ipair)]
         [sample-stats (vector 0 *most-positive-fixnum* *most-negative-fixnum* empty)]
         [resv (for/vector ([p (for*/fold ([expansions (set)])
                                 ([i (in-range start end)])
                                 ;; do the expansion of the indexed position, adding in any new positions found
                                 (set-union expansions (expand (vector-ref current-fringe i))))])
                           (vector-set! sample-stats 0 (add1 (vector-ref sample-stats 0)))
                           (vector-set! sample-stats 1 (fxmin (vector-ref sample-stats 1) (equal-hash-code p)))
                           (vector-set! sample-stats 2 (fxmax (vector-ref sample-stats 2) (equal-hash-code p)))
                           (when (zero? (modulo (vector-ref sample-stats 0) sample-freq))
                             (vector-set! sample-stats 3 (cons (equal-hash-code p) (vector-ref sample-stats 3))))
                           p)])
    (vector-sort! position<? resv)
    ;; resv should be sorted and have no duplicates w/in itself because it came from a set
    (vector-set! sample-stats 3 (sort (vector-ref sample-stats 3) fx<))
    #|
    (printf "remote-expand-part-fringe: starting w/ ~a positions, expansion has ~a/~a positions and sampled ~a hashcodes~%"
            (- end start) (vector-ref sample-stats 0) (vector-length resv) (length (vector-ref sample-stats 3)))
    (printf "remote-expand-part-fringe: HAVE EXPANSIONS:~%")
    (for ([p resv]) (displayln p))
    |#
    (list sample-stats resv)))

;; MERGING .....

;; merge-expansions: (list int int) (listof (vectorof position)) (vectorof position) (vectorof position) -> (listof position)
;; given a collection of (partial) fringe expansions and a specification of a range of _position-hash-codes_ to consider
;; go through all of them and merge the positions (removing duplicates) in that range into a single collection
(define (remote-merge-expansions my-range lovo-positions prev-fringe-vec current-fringe-vec)
  ;; as first step:
  ;; ...  need to read all the partial fringe-files int a local variable called lovo-positions
  (let* ([fastforwarded-lolops 
          (filter-not empty?
                      (map (lambda (vop)
                             #|(printf "remote-merge-expansions: fastforwarding vec of ~a positions to ~a positions (indices [~a-~a])~%" 
                                     (vector-length vop) (- (find-pos-index (second my-range) vop) (find-pos-index (first my-range) vop)) 
                                     (find-pos-index (first my-range) vop) (find-pos-index (second my-range) vop))|#
                             (for/list ([i (in-range (find-pos-index (first my-range) vop)
                                                     (find-pos-index (second my-range) vop))])
                               (vector-ref vop i)))
                           lovo-positions))]
         ;; this will become a heap of file-handles
         [heap-o-position-lists (let ([lheap (make-heap (lambda (l1 l2) (position<? (car l1) (car l2))))])
                                  (heap-add-all! lheap fastforwarded-lolops)
                                  lheap)]
         ;; now remove duplicates
         [sorted-merged-expansions
          (sorted-remove-dups
           (for/list ([lops (in-heap/consume! heap-o-position-lists)]
                      #:break (fx>= (equal-hash-code (car lops)) (second my-range))
                      #:unless (and (or ;;(equal? (car lops) (car (heap-min heap-o-position-lists)))
                                     (position-in-vec? prev-fringe-vec (car lops))
                                     (position-in-vec? current-fringe-vec (car lops)))
                                    (unless (empty? (cdr lops)) (heap-add! heap-o-position-lists (cdr lops)))));;******???????
             (unless (empty? (cdr lops)) (heap-add! heap-o-position-lists (cdr lops)))
             (car lops)))])
    (printf "remote-merge-expansions: fw-lolop-lengths=~a [total ~a]~%" (map length fastforwarded-lolops) (for/sum ([l fastforwarded-lolops]) (length l)))
    ;(printf "remote-merge-expansions: merged-expns-length=~a~%" (length sorted-merged-expansions))
    ;;***error-check
    #|
    (unless (= (length sorted-merged-expansions) (set-count (list->set sorted-merged-expansions))) 
      (error 'remote-merge-expansions "list-merging vs. set mis-match"))
    |#
    sorted-merged-expansions
    ))


;; make-one-merge-range: int int (heapof (listof number)) -> (list int int)
;; consume the elements from the sorted lists in the heap until the target-num is reach, returning the range pair
(define (make-one-merge-range target-n start a-heap)
  (for ([i (in-range target-n)]
        #:break (and (= (heap-count a-heap) 1)
                     (= (length (heap-min a-heap)) 1)))
    (unless (empty? (cdr (heap-min a-heap))) (heap-add! a-heap (cdr (heap-min a-heap))))
    (heap-remove-min! a-heap))
  (list start (car (heap-min a-heap))))

;; make-merge-ranges-from-expansions: (listof sampling-stats) -> (list merge-ranges)
;; decide how to partition the space of positions as distributed over the expansions reflected in the sampling-stats
;;***** consider and compare this to simply dividing the hash-code range by *n-processors*
;;***** compare such an approach in terms of the uniformity of the distribution of positions assigned to each bin
(define (make-merge-ranges-from-expansions lo-sample-stat)
  (let* (;;[total-num-positions (foldl (lambda (ss sum) (+ (vector-ref ss 0) sum)) 0 lo-sample-stat)]
         [overall-min (foldl (lambda (ss tmin) (fxmin (vector-ref ss 1) tmin)) *most-positive-fixnum* lo-sample-stat)]
         [overall-max (foldl (lambda (ss tmax) (fxmax (vector-ref ss 2) tmax)) *most-negative-fixnum* lo-sample-stat)]
         [total-num-samples (foldl (lambda (ss sum) (+ (length (vector-ref ss 3)) sum)) 0 lo-sample-stat)]
         [heap-o-sample-hash-lists (let ([lheap (make-heap (lambda (l1 l2) (fx< (first l1) (first l2))))])
                                     (heap-add-all! lheap (map (lambda (ss) (sort (vector-ref ss 3) fx<)) lo-sample-stat))
                                     (heap-add! lheap (list overall-min))
                                     lheap)]
         [made-merge-ranges (append 
                             (for/list ([processor (in-range 1 *n-processors*)])
                               (make-one-merge-range (/ total-num-samples *n-processors*) (car (heap-min heap-o-sample-hash-lists)) heap-o-sample-hash-lists))
                             (list (list (car (heap-min heap-o-sample-hash-lists)) (add1 overall-max))))])
    ;;(printf "make-merge-ranges-from-expansions: returning ~a~%" made-merge-ranges)
    made-merge-ranges))


;; distributed-expand-fringe: (vectorof position) (vectorof position) -> (vectorof position)
;; Distributed version of expand-fringe
;; convert prev-fringe set to vector to pass riot-net
(define (distributed-expand-fringe)
  ;;(printf "distributed-expand-fringe: ~a nodes in prev and ~a in current fringes~%" (vector-length prev-fringe-vec) (vector-length current-fringe-vec))
  (let* ([current-fringe-vec (read-from-disk "current-fringe")]
         [prev-fringe-vec (read-from-disk "prev-fringe")]
         [samp-freq (floor (/ (vector-length current-fringe-vec) (* 100 *n-processors*)))]
         ;; Distribute the expansion work
         [stats+expansions (for/list #|work|# ([range-pair (make-vector-ranges (vector-length current-fringe-vec))])
                             ;; bind current-fringe-vec to read-fringe-from-disk
                             ;; bind local-expansion to ...
                             (remote-expand-part-fringe current-fringe-vec range-pair samp-freq)
                             ;; write local-expansion to disk *** need naming convention 
                             ;; return the sampling-stats
                             )]
         [sampling-stats (map first stats+expansions)] ;; this is all that will be returned by workers so no need to map first
         [just-expansions (map second stats+expansions)]
         ;; Distribute the merging work
         [merge-ranges (make-merge-ranges-from-expansions sampling-stats)]
         [sorted-expansion-parts
          (let* ([merged-expansion-parts (for/list #|work|# ([merge-range merge-ranges])
                                           ;; bind prev-fringe-vec to read-fringe-from-disk
                                           ;; bind current-fringe-vec to read-fringe-from-disk
                                           ;;(printf "remote-expand-fringe: merge-range = ~a~%" merge-range)
                                           ;; replace just-expansions with a list of file-names
                                           ;; ...  containing the partial expansions
                                           (remote-merge-expansions merge-range just-expansions prev-fringe-vec current-fringe-vec)
                                           ;; return the responsibility-range with the worker host name
                                           )]
                 [expan-lengths (map length merged-expansion-parts)]
                 [min-expan (argmin identity expan-lengths)]
                 [max-expan (argmax identity expan-lengths)]
                 [variation-percent (/ (round (* 10000.0 (/ (- max-expan min-expan) (/ (for/sum ([i expan-lengths]) i) (length expan-lengths))))) 100.0)])
            (printf "distributed-expand-fringe: lengths = ~a [spread ~a or ~a%]~%" 
                    expan-lengths (- max-expan min-expan) variation-percent)
            (sort merged-expansion-parts
                  fx< #:key (lambda (x) (equal-hash-code (car x)))))])
    ;;***error-check
    #|
    (for/first ([i (in-range (length sorted-expansion-parts))]
                #:when (or (for/or ([h1 (map equal-hash-code (list-ref sorted-expansion-parts i))]
                                    [h2 (rest (map equal-hash-code (list-ref sorted-expansion-parts i)))])
                             (fx>= h1 h2))
                           (and (< i (sub1 (length sorted-expansion-parts)))
                                (fx> (equal-hash-code (last (list-ref sorted-expansion-parts i)))
                                     (equal-hash-code (first (list-ref sorted-expansion-parts (add1 i))))))))
      (error 'remote-expand-fringe "mucked-up expansion parts"))
    |#
    ;; instead of apply append, concatenate the files into one new fringe-file
    ;; return the result
    (apply append ;; GAK! -- do something else here
           sorted-expansion-parts)))
;;----------------------------------------------------------------------------------------

;; expand-fringe: (setof position) (setof position) -> (setof position)
;; Given a current fringe to expand, and the immediately previous fringe, 
;; return the new fringe by breaking it up into ranges
(define (expand-fringe prev-fringe current-fringe)
  (let* ((current-fringe-vec  (vectorize-set current-fringe)))
    (vector-sort! position<? current-fringe-vec)
    ;;***error-check
    ;;(unless (= (vector-length current-fringe-vec) (set-count current-fringe)) (error 'expand-fringe "vectorization screw-up"))
    (for/set ([p (if (< (vector-length current-fringe-vec) 1000)
                     ;; do it myself
                     (expand-fringe-portion (list 0 (vector-length current-fringe-vec)) prev-fringe current-fringe-vec)
                     ;; else call distributed-expand, which will farm out to workers
                     (begin 
                       #|
                       (printf "calling distributed-exp... with ~a and ~a positions in prev and current fringes, respectively~%"
                               (set-count prev-fringe) (vector-length current-fringe-vec))
                       |#
                       (distributed-expand-fringe (let ([prev-vec (vectorize-set prev-fringe)])
                                                    (vector-sort! position<? prev-vec)
                                                    prev-vec)
                                                  current-fringe-vec)))])
      p)))

;; vectorize-set: (setof position) -> (vectorof position)
;; convert a set of positions into a vector for easy/efficient partitioning
(define (vectorize-set f)
  (for/vector #:length (set-count f) ([p f]) p))

;; fringe-merge: (listof position) (listof position) -> (listof X)
;; ASSUMES both lists are sorted and creates a new list with duplicates removed
(define (fringe-merge l1 l2)
  (cond [(empty? l1) l2]
        [(empty? l2) l1]
        [(position<? (first l1) (first l2)) (cons (first l1) (fringe-merge (rest l1) l2))]
        [(equal? (first l1) (first l2)) (fringe-merge l1 (rest l2))]
        [else (cons (first l2) (fringe-merge l1 (rest l2)))]))


(define *max-depth* 10)(set! *max-depth* 61)

;; cluster-fringe-search: (setof position) (setof position) int -> ...
;; perform a fringe BFS starting at the given state until depth is 0
(define (cluster-fringe-search depth)
  (let ([prev-fringe (list->set (read-from-disk "prev-fringe"))]
        [current-fringe (list->set (read-from-disk "current-fringe"))])
    (cond [(or (set-empty? current-fringe) (> depth *max-depth*)) #f]
          [else
           (let ([maybe-goal (goal-in-fringe? current-fringe)])
             (cond [maybe-goal
                    (print "found goal")
                    maybe-goal]
                   [else (let ([new-fringe (expand-fringe prev-fringe current-fringe)])
                           (printf "At depth ~a current-fringe has ~a positions (and new-fringe ~a)~%" 
                                   depth (set-count current-fringe) (set-count new-fringe))
                           ;;(for ([p current-fringe]) (displayln p))
                           (cluster-fringe-search (add1 depth)))]))])))


(block10-init)
;(climb12-init)
(compile-ms-array! *piece-types* *bh* *bw*)

#|
(module+ main
  (connect-to-riot-server! "localhost")
  (define search-result (time (cluster-fringe-search (set) (set *start*) 1)))
  (print search-result))
|#
;; initialization of empty fringe files
(write-to-disk empty "prev-fringe")
(write-to-disk (list *start*) "current-fringe")

(time (cluster-fringe-search 1))