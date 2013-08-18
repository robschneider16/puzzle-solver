#lang racket

(require (planet gcr/riot))
;(require (planet soegaard/gzip:2:2))
(require file/gzip)
(require file/gunzip)
(require rnrs/sorting-6)
(require data/heap)
(require srfi/1)
(require racket/fixnum)
(require racket/set)
(require mzlib/string) ;; supposedly depricated but seems to need the require for 5.3.5

(require "stp-init.rkt")
(require "stp-solve-base.rkt")
;(require profile)
;(instrumenting-enabled #t)
;(profiling-enabled #t)

(provide (all-defined-out))

(define *master-name* "the name of the host where the master process is running")
(define *local-store* "the root portion of path to where workers can store temporary fringe files")
;(set! *master-name* "localhost")
;(set! *local-store* "/space/fringefiles/")
;(define *n-processors* 4)
(set! *master-name* "wcp")
(set! *local-store* "/state/partition1/fringefiles/")
(define *n-processors* 31)

(define *expand-multiplier* 1)
(define *diy-threshold* 3000)
(define *min-pre-proto-fringe-size* 2000)

(define *most-positive-fixnum* 0)
(define *most-negative-fixnum* 0)
(cond [(fixnum? (expt 2 61))
       (set! *most-positive-fixnum* (fx+ (expt 2 61) (fx- (expt 2 61) 1)))  ;; ****** only on 64-bit architectures *****
       (set! *most-negative-fixnum* (fx+ (fx* -1 (expt 2 61)) (fx* -1 (expt 2 61))));; ***** likewise *****
       ]
      [else 
       ;; For the cluster platform use the following:
       (set! *most-positive-fixnum* (fx+ (expt 2 29) (fx- (expt 2 29) 1)))  ;; ****** only on our cluster, wcp *****
       (set! *most-negative-fixnum* (fx+ (fx* -1 (expt 2 29)) (fx* -1 (expt 2 29))));; ***** likewise *****
       ])

(define *found-goal* #f)


;; Cluster/multi-process specific code for the sliding-tile puzzle solver


;;----------------------------------------------------------------------------------------


;; expand-fringe-self: (setof position) (vectorof position) int -> fspec
;; expand just the portion of the sorted-fringe-vector specified by the indices in the given range-pair,
;; returning the number of positions in the newly expanded and merged fringe
;; ASSUME: current-fringe-vec is sorted.
(define (expand-fringe-self pf-spec cf-spec depth)
  (let* ([prev-fringe-set (list->set (read-fringe-from-file (fspec-fname pf-spec)))] ; pf- and cf-spec's in expand-fringe-self should have empty fbase
         [current-fringe-vec (vectorize-list (read-fringe-from-file (fspec-fname cf-spec)))]
         [new-cf-name (format "current-fringe-d~a" depth)]
         [res (for/list ([p (for/fold ([expansions (set)])
                              ([p-to-expand current-fringe-vec])
                              (set-union expansions
                                         (expand p-to-expand)))]
                         #:unless (or (set-member? prev-fringe-set p)
                                      (position-in-vec? current-fringe-vec p)))
                (when (is-goal? p) (set! *found-goal* p))
                p)])
    #|(printf "Finished the work packet generating a set of ~a positions~%" (set-count res))
    (for ([p res])
      (printf "pos: ~a~%~a~%" (stringify p) p))|#
    (delete-file (fspec-fname pf-spec))
    (rename-file-or-directory (fspec-fname cf-spec) (format "prev-fringe-d~a" depth) #t)
    (write-fringe-to-disk (sort res position<?) new-cf-name)
    (make-fspec new-cf-name "" (length res) (file-size new-cf-name))))

;;----------------------------------------------------------------------------------------
;; DISTRIBUTED EXPANSION AND MERGING OF FRINGES

;; a sampling-stat is a (vector int fixnum fixnum (listofof fixnum) boolean string int)
;; where the elements are:s
;; 0. total number of positions in the corresponding expansion
;; 1. minimum hash value of the positions
;; 2. maximum hash value of the positions
;; 3. [deprecated] list of sampled hash-codes
;; 4. boolean goal-found if found when expanding the assigned positions
;; 5. output file name, e.g., partial-expansionXX.gz
;; 6. compressed file size

;; -----------------------------------------------------------------------------------------------------
;; EXPANSION .....

;; make-vector-ranges: int -> (listof (list int int)
;; create the pairs of indices into the current-fringe-vector that will specify the part of the fringe each worker tackles
(define (make-vector-ranges vlength)
  (if (< vlength 10) ;;1000
      (list (list 0 vlength))
      (let ((start-list (build-list (* *expand-multiplier* *n-processors*)
                                    (lambda (i) (floor (* i (/ vlength (* *expand-multiplier* *n-processors*))))))))
        (foldr (lambda (x r) (cons (list x (first (first r))) r)) 
               (list (list (last start-list) vlength)) 
               (drop-right start-list 1)))))


;; remove-dupes: fspec fspec (listof fspec) string int -> sampling-stat
;; Remove duplicate positions from the list of expand-fspec files, for any positions that also appear in the prev- or current-fringes.
;; Write the non-duplicate positions to a "partial-expansionDD" file in a *local-store* folder, but get a gzipped copy to the working directory.
;; Accordingly, the sampling-stat return value has a filename pointing to the working directory with a .gz extension.
(define (remove-dupes pfspec cfspec lo-expand-fspec ofile-name depth)
  ;; the ofile-name is just the file-name -- no *local-store* path where needed
  #|(printf "EXPAND PHASE 2 (REMOVE DUPLICATES) pfspec: ~a~%cfspec: ~a~%first of lo-expand-fspec: ~a~%ofile-name: ~a~%depth: ~a~%"
          pfspec cfspec (first lo-expand-fspec) ofile-name depth)|#
  ;; EXPAND PHASE 2 (REMOVE DUPLICATES)
  (let* ([pffh (cond [(file-exists? (string-append *local-store* (fspec-fname pfspec))) ;; moved to this from current-fringe during prev depth
                      (fh-from-fspec (make-fspec (fspec-fname pfspec) *local-store* (fspec-pcount pfspec) (fspec-fsize pfspec)))]
                     [else (copy-file (fspec-fname pfspec) (string-append *local-store* (fspec-fname pfspec)))
                           (fh-from-fspec (make-fspec (fspec-fname pfspec) *local-store* (fspec-pcount pfspec) (fspec-fsize pfspec)))])]
         [cffh (fh-from-fspec cfspec)]
         [lo-effh (for/list ([an-fspec lo-expand-fspec]) (fh-from-fspec an-fspec))]
         [heap-o-fheads (let ([lheap (make-heap (lambda (fh1 fh2) (position<? (fringehead-next fh1) (fringehead-next fh2))))])
                          (heap-add-all! lheap lo-effh)
                          lheap)]
         [expand-out (open-output-file (string-append *local-store* ofile-name) #:exists 'replace)]
         [unique-expansions 0]
         [sample-stats 
          (vector 0 *most-positive-fixnum* *most-negative-fixnum* empty #f (string-append ofile-name ".gz") 0)]
         )
    ;; locally merge the pre-proto-fringes, removing duplicates from prev- and current-fringes
    (for ([an-fhead (in-heap/consume! heap-o-fheads)])
      (let ([efpos (fringehead-next an-fhead)])
        (unless (or (position-in-fhead? efpos pffh)
                    (position-in-fhead? efpos cffh))
          (fprintf expand-out "~a~%" efpos)
          (when (is-goal? efpos) (vector-set! sample-stats 4 efpos))
          (vector-set! sample-stats 1 (fxmin (vector-ref sample-stats 1) (equal-hash-code efpos)))
          (vector-set! sample-stats 2 (fxmax (vector-ref sample-stats 2) (equal-hash-code efpos)))
          (set! unique-expansions (add1 unique-expansions))
          )
        (advance-fhead! an-fhead)
        (unless (fhdone? an-fhead) ;;(eof-object? (peek-byte (fringehead-iprt an-fhead) 1))
          (heap-add! heap-o-fheads an-fhead))))
    ;(printf "remote-expand-part-fringe: HAVE EXPANSIONS:~%")
    ;(for ([p resv]) (displayln p))
    ;; close input and output ports
    (for-each (lambda (fh) (close-input-port (fringehead-iprt fh))) (cons pffh (cons cffh lo-effh))) 
    (close-output-port expand-out)
    ;; compress and copy the partial-expansion file from /tmp to home for other processes to have when merging
    ;; but do it in one swell foop
    (gzip (string-append *local-store* ofile-name) (string-append ofile-name ".gz"))
    ;; complete the sampling-stat
    (vector-set! sample-stats 0 unique-expansions)
    (vector-set! sample-stats 6 (file-size (string-append ofile-name ".gz")))
    ;; move local copy of current-fringe to local prev-fringe of next depth, unless we're on same machine as master
    (unless (string=? *master-name* "localhost")
      (rename-file-or-directory (fspec-fullpath cfspec)
                                (format "~aprev-fringe-d~a" *local-store* depth)))
    ;; delete files that are no longer needed
    (for ([efspec lo-expand-fspec]) (delete-file (fspec-fullpath efspec)))
    (when (file-exists? (string-append *local-store* (fspec-fname pfspec))) (delete-file (string-append *local-store* (fspec-fname pfspec))))
    #|(printf "remove-dupes: starting w/ ~a positions, expansion has ~a/~a positions~%"
            (fspec-pcount expand-fspec) unique-expansions (position-count-in-file ofile-name))|#
    #|(unless (check-sorted-fringe? ofile-name)
      (error 'remove-dupes "phase 2: failed to generate a sorted fringe file ~a" ofile-name))|#
    sample-stats))


;; process-proto-fringe: (setof position) string int (listof fspec) -> (listof fspec)
;; convert set to vector, sort it and write the sorted vector to the given filename,
;; returning the file name to which the proto-fringe was written
(define (process-proto-fringe sop pre-ofile-template pre-ofile-counter pre-ofiles)
  (cons (let* ([resv (for/vector #:length (set-count sop) ([p sop]) p)] ;; convert set to vector
               [f (format "~a~a" pre-ofile-template pre-ofile-counter)]
               [fullpath (string-append *local-store* f)])
          (vector-sort! position<? resv)
          ;(set! resv (vector-sort position<? resv))
          (write-fringe-to-disk resv fullpath)
          (make-fspec f *local-store* (vector-length resv) (file-size fullpath)))
        pre-ofiles))
                        

;; remote-expand-part-fringe: (list int int) int fspec fspec int -> sampling-stat
;; given a pair of indices into the current-fringe that should be expanded by this process, a process-id,
;; and the fspecs for the prev- and current-fringes, ...
;; expand the positions in the indices range, ignoring duplicates other than within the new fringe being constructed.
(define (remote-expand-part-fringe ipair process-id pf-spec cf-spec depth)
  ;; prev-fringe spec points to default directory; current-fringe spec points to /tmp folder
  ;(printf "remote-expand-part-fringe: pf-spec: ~a, and cf-spec: ~a~%" pf-spec cf-spec)
  ;; EXPAND PHASE 1
  (let* ([pre-ofile-template-fname (format "partial-expansion~a" (~a process-id #:left-pad-string "0" #:width 2 #:align 'right))]
         [pre-ofile-counter 0]
         [pre-ofiles empty]
         ;; *** Dynamically choose the size of the pre-proto-fringes to keep the number of files below 500 ***
         [dynamic-proto-fringe-size (max *min-pre-proto-fringe-size* (/ (fspec-pcount cf-spec) 500))]
         [start (first ipair)]
         [end (second ipair)]
         [assignment-count (- end start)]
         [expanded-phase1 1];; technically, not yet, but during initialization in pre-resv do loop
         [cffh (do ([i 0 (add1 i)]
                    [fh (fh-from-fspec cf-spec)])
                 ((>= i start) fh)
                 (advance-fhead! fh))])
    ;; do the actual expansions
    (do ([i 1 (add1 i)]
         [expansions (expand (fringehead-next cffh)) (set-union expansions (expand (fringehead-next cffh)))])
      ((>= i assignment-count)
       (set! pre-ofiles
             (process-proto-fringe expansions pre-ofile-template-fname pre-ofile-counter pre-ofiles)))
      (when (fhdone? cffh) 
        (error 'remote-expand-part-fringe (format "hit end of cur-fringe after ~a of ~a expansions" expanded-phase1 assignment-count)))
      (when (> (set-count expansions) dynamic-proto-fringe-size)
        (set! pre-ofiles
              (process-proto-fringe expansions pre-ofile-template-fname pre-ofile-counter pre-ofiles))
        (set! pre-ofile-counter (add1 pre-ofile-counter))
        (set! expansions (set)))
      (advance-fhead! cffh)
      (set! expanded-phase1 (add1 expanded-phase1)))
    #|(printf "remote-exp-part-fring: PHASE 1: expanding ~a positions yielding ~a/~a positions~%" 
            expanded-phase1 (vector-length pre-resv) (position-count-in-file pre-ofile-name))|#
    (when (< expanded-phase1 assignment-count)
      (error 'remote-expand-part-fringe
             (format "only expanded ~a of the assigned ~a (~a-~a) positions" expanded-phase1 assignment-count start end)))
    (close-input-port (fringehead-iprt cffh))
    ;; PHASE 2: now pass through the proto-fringe expansion file(s) as well as prev-fringe and current-fringe to remove duplicates
    (remove-dupes pf-spec cf-spec pre-ofiles pre-ofile-template-fname depth)))


;; remote-expand-fringe: (listof (list fixnum fixnum)) fspec fspec int -> (listof sampling-stat)
;; trigger the distributed expansion according to the given ranges
;; The master should provide a cf-spec that points to a *local-store* copy of the current-fringe, where the copy is provided by master also
(define (remote-expand-fringe ranges pf-spec cf-spec depth)
  ;;(printf "remote-expand-fringe: current-fringe of ~a split as: ~a~%" cur-fringe-size (map (lambda (pr) (- (second pr) (first pr))) ranges))
  (let* ([distrib-results (for/work ([range-pair ranges]
                                     [i (in-range (length ranges))])
                                    (when (> depth *max-depth*) (error 'distributed-expand-fringe "ran off end")) ;;prevent riot cache-failure
                                    (wait-for-files (list pf-spec cf-spec) #t)
                                    (remote-expand-part-fringe range-pair i pf-spec cf-spec depth))])
    ;(printf "remote-expand-fringe: respective expansion counts: ~a~%" (map (lambda (ssv) (vector-ref ssv 0)) distrib-results))
    ;; when workers on same host as master, move /tmp/current-fringeX to /tmp/prev-fringeX+1
    (when (string=? *master-name* "localhost")
      (rename-file-or-directory (fspec-fullpath cf-spec)
                                (format "~aprev-fringe-d~a" *local-store* depth)))
    distrib-results))


;; ------------------------------------------------------------------------------------------
;; MERGING .....

;; bring-local-partial-expansions: (listof fspec) -> void
;; copy the gzipped partial expansions from the shared disk to our local /tmp, uncompress the copy and delete the gzipped version
(define (bring-local-partial-expansions lo-expand-specs)
  (for ([fs lo-expand-specs])
    (let* ([base-fname (fspec-fname fs)] ; has .gz extension
           [tmp-partexp-name (string-append *local-store* (substring base-fname 0 (- (string-length base-fname) 3)))])
      (unless (file-exists? tmp-partexp-name) ; unless this process id is here from expansion
        (gunzip base-fname (lambda (a b) (string->path tmp-partexp-name)))))))
                                

;; merge-expansions: (list int int) (listof fspec) int string -> (list string number)
;; given a specification of a range of _position-hash-codes_ to consider,
;; a list of specs, and the counts on prev and current fringes,
;; go through all the partial-expansions and merge the positions (removing duplicates) in that range into a single collection.
;; write the results to a local /tmp file, then finally gzip it, copy to the home directory (NFS share), and delete the files.
;; Return a list with the merged filename and the size of that segment
(define (remote-merge-expansions my-range expand-files-specs depth ofile-name)
  ;; expand-files-specs are of pattern: "partial-expansionDD.gz", pointing to working (shared) directory 
  ;;NEW: ofile-name is of pattern: "partial-mergeDD", where the DD is a process identifier
  (let* ([mrg-segment-oport (open-output-file (string-append *local-store* ofile-name))]
         [copy-partial-expansions-to-local-disk ;; but only if not sharing host with master
          (unless (string=? *master-name* "localhost")
            ;; copy shared-drive expansions to *local-store*, uncompress, and delete compressed version
            (bring-local-partial-expansions expand-files-specs))]
         [to-merge-ports 
          (for/list ([i (in-range (length expand-files-specs))])
            (open-input-file (format "~apartial-expansion~a" *local-store* (~a i #:left-pad-string "0" #:width 2 #:align 'right))))]
         [fastforwarded-fheads
          (filter-not (lambda (fh) (eof-object? (fringehead-next fh)))
                      (for/list ([iprt to-merge-ports]
                                 [partial-expansion-size (map fspec-pcount expand-files-specs)])
                        (let* ([fhead (fringehead (read iprt) iprt 1 partial-expansion-size)]
                               [fhpos (fringehead-next fhead)])
                          (cond [(or (eof-object? fhpos)
                                     (fx>= (equal-hash-code fhpos) (first my-range)))
                                 fhead]
                                [else (do ([lfhpos fhpos (advance-fhead! fhead)])
                                        ((or (eof-object? lfhpos)
                                             (fx>= (equal-hash-code lfhpos) (first my-range)))
                                         fhead))]))))]
         [heap-o-fheads (let ([lheap (make-heap (lambda (fh1 fh2) (position<? (fringehead-next fh1) (fringehead-next fh2))))])
                          (heap-add-all! lheap fastforwarded-fheads)
                          lheap)]
         [segment-size (let ([last-pos (void)]
                             [keep-pos (void)]
                             [num-written 0])
                         (for/list ([an-fhead (in-heap/consume! heap-o-fheads)]
                                    #:break (fx>= (equal-hash-code (fringehead-next an-fhead)) (second my-range)))
                           (set! keep-pos (fringehead-next an-fhead))
                           (advance-fhead! an-fhead)
                           (unless (fhdone? an-fhead) ;;(eof-object? (peek-byte (fringehead-iprt an-fhead) 1))
                             (heap-add! heap-o-fheads an-fhead))
                           (unless (equal? keep-pos last-pos)
                             (fprintf mrg-segment-oport "~a~%" keep-pos)
                             (set! num-written (add1 num-written)))
                           (set! last-pos keep-pos))
                         num-written)])
    ;(printf "remote-merge-expansions: fw-lolop-lengths=~a [total ~a]~%" (map length fastforwarded-lolops) (for/sum ([l fastforwarded-lolops]) (length l)))
    ;(printf "remote-merge-expansions: merged-expns-length=~a~%" (length sorted-merged-expansions))
    (close-output-port mrg-segment-oport)
    (for ([iprt to-merge-ports]) (close-input-port iprt))
    ;; arrange to move a compressed version of ofile-name to the shared disk
    (gzip (string-append *local-store* ofile-name) (string-append ofile-name ".gz"))
    ;(printf "remote-merge-expansions: about to try deleting ofile-name, ~a (and w/ .gz extension)~%" ofile-name)
    (delete-file (string-append *local-store* ofile-name))
    (unless (string=? *master-name* "localhost")
      (for ([fspc expand-files-specs]) 
        (delete-file (string-append *local-store* (substring (fspec-fname fspc) 0 (- (string-length (fspec-fname fspc)) 3)))))) ;remove the local uncompressed expansions
    (list (string-append ofile-name ".gz") segment-size)))

;; remote-merge: (listof (list fixnum fixnum)) (listof fspec) int -> (listof string int)
(define (remote-merge merge-ranges expand-files-specs depth)
  (when (string=? *master-name* "localhost")
    (bring-local-partial-expansions expand-files-specs))
  (let ([merge-results
         (for/work ([merge-range merge-ranges] ;; merged results should come back in order of merge-ranges assignments
                    [i (in-range (length merge-ranges))])
                   (when (> depth *max-depth*) (error 'distributed-expand-fringe "ran off end"))
                   (let* ([ofile-name (format "partial-merge~a" (~a i #:left-pad-string "0" #:width 2 #:align 'right))]
                          [merged-fname-and-resp-rng-size (remote-merge-expansions merge-range expand-files-specs depth ofile-name)]
                          )
                     ;;(printf "distributed-expand-fringe: merge-range = ~a~%~a~%" merge-range merged-responsibility-range)
                     ;(write-fringe-to-disk merged-responsibility-range ofile-name)
                     merged-fname-and-resp-rng-size))])
    ;(printf "remote-merge: merged segment names and lengths ~a~%" merge-results)
    (when (string=? *master-name* "localhost")
      (for ([f expand-files-specs])
        ;(delete-file (first f)) ; these should have been deleted before the call to for/work
        (delete-file (string-append *local-store* (substring (fspec-fname f) 0 (- (string-length (fspec-fname f)) 3))))))
    merge-results))


;; simple-merge-ranges: (listof sampling-stats) -> (list merge-ranges)
;; just divide the span between the global min and max evenly among the processors
(define (simple-merge-ranges lo-sample-stat)
  (let* (;;[total-num-positions (foldl (lambda (ss sum) (+ (vector-ref ss 0) sum)) 0 lo-sample-stat)]
         [overall-min (foldl (lambda (ss tmin) (fxmin (vector-ref ss 1) tmin)) *most-positive-fixnum* lo-sample-stat)]
         [overall-max (foldl (lambda (ss tmax) (fxmax (vector-ref ss 2) tmax)) *most-negative-fixnum* lo-sample-stat)]
         [segment-width (fx+ (fxquotient overall-min (- *n-processors*)) (fxquotient overall-max *n-processors*))]
         [start-list (foldl (lambda (dinc res) (cons (list (fx- (car (car res)) dinc) (car (car res))) res))
                            (list (list (fx- overall-max segment-width)
                                        (if (fx= overall-max *most-positive-fixnum*)
                                            (error 'simple-merge-ranges "max hash-code is *most-positive-fixnum*")
                                            (fx+ 1 overall-max))))
                            (make-list (- *n-processors* 2) segment-width))]
         [final-list (cons (list overall-min (car (car start-list))) start-list)])
    ;(printf "simple-merge-ranges: generated ranges: ~a~%" final-list)
    final-list))

;; ------------------------------------------------------------------------------------------
;; DISTRIBUTED EXPAND AND MERGE

;; distributed-expand-fringe: fspec fspec int -> (list string int int)
;; Distributed version of expand-fringe
;; given prev and current fringe-specs and the present depth, expand and merge the current fringe,
;; returning the fringe-spec of the newly expanded and merged fringe.
(define (distributed-expand-fringe pf-spec cf-spec depth)
  #|(printf "distributed-expand-fringe: at depth ~a, pf-spec: ~a; cf-spec: ~a~%" 
          depth pf-spec cf-spec)|#
  ;; push newest fringe (current-fringe) to all workers
  (cond [(string=? *master-name* "localhost")
         (copy-file (fspec-fullpath cf-spec) (format "~a~a" *local-store* (fspec-fname cf-spec)))]
        [else (gzip (fspec-fullpath cf-spec))
              (system (format "rocks run host compute 'scp wcp:puzzle-solver/~a.gz ~a; gunzip ~a~a.gz'" (fspec-fname cf-spec) *local-store* *local-store* (fspec-fname cf-spec)))])
  (let* (;; EXPAND
         [ranges (make-vector-ranges (fspec-pcount cf-spec))]
         [rcf-spec (make-fspec (fspec-fname cf-spec) *local-store* (fspec-pcount cf-spec) (fspec-fsize cf-spec))]
         ;; --- Distribute the actual expansion work ------------------------
         [infomsg1 (printf "starting expand, ... ")]
         [expand-start (current-seconds)]
         [sampling-stats (remote-expand-fringe ranges pf-spec rcf-spec depth)]
         [expand-time-msg (printf "expand time: ~a~%" (~r (/ (- (current-seconds) expand-start) 60.0) #:precision 4))]
         ;; -----------------------------------------------------------------
         [check-for-goal (for/first ([ss sampling-stats]
                                     #:when (vector-ref ss 4))
                           (set! *found-goal* (vector-ref ss 4)))]
         ;; make list of triples: partial-expansionDD.gz, number of positions, file-size
         [expand-files-specs (for/list ([ss sampling-stats])
                               #|(printf "exp-spec: ~a should have ~a postions: seeing ~a of expected ~a~%" 
                                       (vector-ref ss 5) (vector-ref ss 0) (file-size (vector-ref ss 5)) (vector-ref ss 6))|#
                               (list (vector-ref ss 5) (vector-ref ss 0) (vector-ref ss 6))
                               (make-fspec (vector-ref ss 5) "" (vector-ref ss 0) (vector-ref ss 6)))]
         ;; need to wait for write to complete -- i.e., all data to appear on master
         [wait-for-partial-expansions (wait-for-files expand-files-specs)]
         #|[error-check1 (for/first ([i (in-range (length sampling-stats))]
                                   [ss sampling-stats]
                                   #:unless (= (vector-ref ss 0) (position-count-in-file (format "partial-expansion~a" (~a i #:left-pad-string "0" #:width 2 #:align 'right)))))
                         (error 'distributed-expand-fringe (format "err-chk1: partial-expansion sizes do not match up for ~a which should be ~a" i (vector-ref ss 0))))]|#
         ;; MERGE
         ;;[merge-ranges (make-merge-ranges-from-expansions sampling-stats)]
         [merge-ranges (simple-merge-ranges sampling-stats)]
         ;; --- Distribute the merging work ----------
         [infomsg2 (printf "starting merge, ... ")]
         [merge-start (current-seconds)]
         [sorted-expansion-files-lengths
          (let ([merged-expansion-files-lens (remote-merge merge-ranges expand-files-specs depth)]
                )
            #|(printf "distributed-expand-fringe: lengths = ~a [spread ~a or ~a%]~%" 
                    expan-lengths (- max-expan min-expan) variation-percent)|#
            ;;(error 'distributed-expand-fringe "stop to check partial files")
            merged-expansion-files-lens
            )]
         [merge-end (current-seconds)]
         [merge-time-msg (printf "merge time: ~a~%" (~r (/ (- merge-end merge-start) 60.0) #:precision 4))]
         ;; -------------------------------------------
         [sorted-expansion-files (map first sorted-expansion-files-lengths)]
         [sef-lengths (map second sorted-expansion-files-lengths)]
         #|[error-check2 (for/first ([f sorted-expansion-files]
                                   [len sef-lengths]
                                   #:unless (= len (position-count-in-file f)))
                         (error 'distributed-expand-fringe (format "err-chk2: partial-merges do not match up for ~a which should be ~a" f len)))]|#
         [new-cf-name (format "current-fringe-d~a" depth)]
         )
    ;; move current current-fringe to prev-fringe of appropriate depth
    (rename-file-or-directory (fspec-fullpath cf-spec)
                              (format "prev-fringe-d~a" depth) #t) ;;(system "mv current-fringe prev-fringe")
    ;; create the _new_ current-fringe
    (for ([f sorted-expansion-files])
      ;(printf "distributed-expand-fringe: concatenating ~a~%" f)
      (system (format "zcat ~a >> current-fringe-d~a" f depth)))
    ;; delete files we don't need anymore
    (delete-file (fspec-fullpath pf-spec))
    (system "rm partial-expansion* partial-merge*")
    (unless (string=? *master-name* "localhost") (delete-file (string-append (fspec-fname cf-spec) ".gz")))
    (printf "distributed-expand-fringe: file manipulation ~a, and total at depth ~a: ~a~%" 
            (~r (/ (- (current-seconds) merge-end) 60.0) #:precision 4) depth (~r (/ (- (current-seconds) expand-start) 60.0) #:precision 4))
    (make-fspec new-cf-name "" (foldl + 0 sef-lengths) (file-size new-cf-name))))


;;----------------------------------------------------------------------------------------

;; expand-fringe: int (list string int int) int -> (list string int int)
;; Given the size of the current fringe to expand, and the current depth of search,
;; do the expansions and merges as appropriate, returning the fringe-spec of the new fringe
(define (expand-fringe pf-spec cf-spec depth)
  (if (< (fspec-pcount cf-spec) *diy-threshold*)
      ;; do it myself
      (expand-fringe-self pf-spec cf-spec depth)
      ;; else call distributed-expand, which will farm out to workers
      (distributed-expand-fringe pf-spec cf-spec depth)))

;; vectorize-list: (listof position) -> (vectorof position)
;; convert a list of positions into a vector for easy/efficient partitioning
(define (vectorize-list f)
  (let ([new-vec (list->vector f)])
    (vector-sort! position<? new-vec)
    new-vec))

;; fringe-merge: (listof position) (listof position) -> (listof X)
;; ASSUMES both lists are sorted and creates a new list with duplicates removed
(define (fringe-merge l1 l2)
  (cond [(empty? l1) l2]
        [(empty? l2) l1]
        [(position<? (first l1) (first l2)) (cons (first l1) (fringe-merge (rest l1) l2))]
        [(equal? (first l1) (first l2)) (fringe-merge l1 (rest l2))]
        [else (cons (first l2) (fringe-merge l1 (rest l2)))]))


(define *max-depth* 10)(set! *max-depth* 105)

;; cfs-file: fspec fspec int -> position
;; perform a file-based cluster-fringe-search at given depth
;; using previous and current fringes stored in corresponding fringe-specs 
(define (cfs-file pf-spec cf-spec depth)
  (cond [(or (zero? (fspec-pcount cf-spec)) (> depth *max-depth*)) #f]
        [*found-goal*
         (print "found goal")
         *found-goal*]
        [else (let ([new-fringe-spec (expand-fringe pf-spec cf-spec depth)])
                (printf "At depth ~a: current-fringe has ~a positions (and new-fringe ~a)~%" 
                        depth (fspec-pcount cf-spec) (fspec-pcount new-fringe-spec))
                ;;(for ([p current-fringe]) (displayln p))
                (cfs-file (make-fspec (format "prev-fringe-d~a" depth) "" (fspec-pcount cf-spec) (fspec-fsize cf-spec)) ;; use current-fringe as prev-fringe at next level
                          new-fringe-spec
                          (add1 depth)))]))

(define (start-cluster-fringe-search start-position)
  ;; initialization of fringe files
  (write-fringe-to-disk empty "prev-fringe-d0")
  (write-fringe-to-disk (list start-position) "current-fringe-d0")
  (cfs-file (make-fspec "prev-fringe-d0" "" 0 (file-size "prev-fringe-d0"))
            (make-fspec "current-fringe-d0" "" 1 (file-size "current-fringe-d0"))
            1))
  

;(block10-init)
(climb12-init)
;(climb15-init)
(compile-ms-array! *piece-types* *bh* *bw*)

;;#|
(module+ main
  ;; Switch between these according to if using the cluster or testing on multi-core single machine
  (connect-to-riot-server! *master-name*)
  (define search-result (time (start-cluster-fringe-search *start*)))
  (print search-result))
;;|#

;;(time (start-cluster-fringe-search *start*))
