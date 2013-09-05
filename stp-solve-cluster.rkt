#lang racket

(require (planet gcr/riot))
;(require (planet soegaard/gzip:2:2))
;(require file/gzip)
;(require file/gunzip)
(require rnrs/sorting-6)
(require data/heap)
(require srfi/1)
(require racket/fixnum)
(require racket/set)
(require mzlib/string) ;; supposedly depricated but seems to need the require for 5.3.5


(require "stp-init.rkt")
(require "stp-solve-base.rkt")
(require "stp-fringefilerep.rkt")
;(require profile)
;(instrumenting-enabled #t)
;(profiling-enabled #t)

(provide (all-defined-out))

(define *master-name* "the name of the host where the master process is running")
(define *local-store* "the root portion of path to where workers can store temporary fringe files")
#|
(set! *master-name* "localhost")
(set! *local-store* "/space/fringefiles/")
(define *n-processors* 4)
;(define *n-processors* 8)
|#
;#|
(set! *master-name* "wcp")
(set! *local-store* "/state/partition1/fringefiles/")
(define *n-processors* 32)
;|#

(define *expand-multiplier* 1)
(define *diy-threshold* 1000)

(define *min-pre-proto-fringe-size* 3000) ; to be replaced by *max-size-...*

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
    (for ([sgmnt (fringe-segments pf)]) (delete-file (filespec-fullpathname sgmnt)))
    (write-fringe-to-disk (sort res position<?) new-cf-name)
    (make-fringe "" (list (make-filespec *most-negative-fixnum* *most-positive-fixnum* new-cf-name (length res) (file-size new-cf-name) "")) (length res))))

;;----------------------------------------------------------------------------------------
;; DISTRIBUTED EXPANSION AND MERGING OF FRINGES

;; a sampling-stat is a (vector int fixnum fixnum (listofof fixnum) boolean string int)
;; where the elements are:s
;; 0. total number of positions in the corresponding expansion
;; 1. minimum hash value of the positions
;; 2. maximum hash value of the positions
;; 3. [deprecated] list of sampled hash-codes
;; 4. boolean goal-found if found when expanding the assigned positions
;; 5. output file name, e.g., partial-expansionXX
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


;; remove-dupes: fringe fringe (listof filespec) string int -> sampling-stat
;; Running in distributed worker processes:
;; Remove duplicate positions from the list of expand-fspec files, for any positions that also appear in the prev- or current-fringes.
;; Write the non-duplicate positions to a "proto-fringe-dXX-NN" file -- previously we wrote this in a *local-store* folder
;; and later delivered a copy to the working directory to share with all the other compute-nodes;
;; Try writing directly to the shared NFS drive as a way to spread out network traffic.  This will clean up file name in the return sampling-stat...
;; Accordingly, the sampling-stat return value has a filename pointing to the working directory.
(define (remove-dupes pf cf lo-expand-fspec ofile-name depth)
  ;; the ofile-name is just the file-name -- no *local-store* path where needed
  #|(printf "EXPAND PHASE 2 (REMOVE DUPLICATES) pf: ~a~%cf: ~a~%all of lo-expand-fspec: ~a~%ofile-name: ~a~%depth: ~a~%"
          pf cf lo-expand-fspec ofile-name depth);|#
  ;; EXPAND PHASE 2 (REMOVE DUPLICATES)
  (let* (;[use-ofilename (string-append *local-store* ofile-name)]
         [rpf (copy-fringe pf *local-store*)]
         [pffh (fh-from-fringe rpf)]
         [cffh (fh-from-fringe cf)]
         [lo-effh (for/list ([an-fspec lo-expand-fspec]) (fh-from-filespec an-fspec))]
         [heap-o-fheads (let ([lheap (make-heap (lambda (fh1 fh2) (position<? (fringehead-next fh1) (fringehead-next fh2))))])
                          (heap-add-all! lheap lo-effh)
                          lheap)]
         [expand-out (open-output-file ofile-name #:exists 'replace)] ; writing directly to NFS doesn't seem any slower than *local-store* and then copy
         [unique-expansions 0]
         [sample-stats 
          (vector 0 *most-positive-fixnum* *most-negative-fixnum* empty #f ofile-name 0)] ;; here, use the shared ofile-name instead of *local-store*
         )
    ;; locally merge the pre-proto-fringes, removing duplicates from prev- and current-fringes
    (for ([an-fhead (in-heap/consume! heap-o-fheads)])
      (let ([efpos (fringehead-next an-fhead)])
        (unless (or (position-in-fhead? efpos pffh)
                    (position-in-fhead? efpos cffh))
          (fprintf expand-out "~a~%" (charify efpos))
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
    (for ([fh (cons pffh (cons cffh lo-effh))]) (close-input-port (fringehead-iprt fh)))
    (close-output-port expand-out)
    ;; copy the proto-fringe file from *local-store* to shared working directory for other processes to have when merging
    ;(copy-file use-ofilename ofile-name)
    ;; complete the sampling-stat
    (vector-set! sample-stats 0 unique-expansions)
    (vector-set! sample-stats 6 (file-size ofile-name))
    ;; delete files that are no longer needed
    (for ([efspec lo-expand-fspec]) (delete-file (filespec-fullpathname efspec)))
    (unless (string=? *master-name* "localhost") (delete-fringe rpf))
    ;(delete-file use-ofilename)
    ;;**** THIS STRIKES ME AS DANGEROUS: IF ONE PROCESS ON MULTI-CORE MACHINE FINISHES FIRST ....
    ;(when (file-exists? (string-append *local-store* (fspec-fname pfspec))) (delete-file (string-append *local-store* (fspec-fname pfspec))))
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
          (make-filespec (equal-hash-code (vector-ref resv 0))
                         (fx+ (equal-hash-code (vector-ref resv (sub1 (vector-length resv)))) 1)
                         f (vector-length resv) (file-size fullpath) *local-store*))
        pre-ofiles))
                        

;; remote-expand-part-fringe: (list int int) int fringe fringe int -> sampling-stat
;; given a pair of indices into the current-fringe that should be expanded by this process, a process-id,
;; and the prev- and current-fringes, and the depth ...
;; expand the positions in the indices range, ignoring duplicates other than within the new fringe being constructed.
(define (remote-expand-part-fringe ipair process-id pf cf depth)
  ;; prev-fringe spec points to default shared directory; current-fringe spec points to *local-store* folder
  ;(printf "remote-expand-part-fringe: starting with pf: ~a, and cf: ~a~%" pf cf)
  ;; EXPAND PHASE 1
  (let* ([pre-ofile-template-fname (format "partial-expansion~a" (~a process-id #:left-pad-string "0" #:width 2 #:align 'right))]
         [pre-ofile-counter 0]
         [pre-ofiles empty]
         ;; *** Dynamically choose the size of the pre-proto-fringes to keep the number of files below 500 ***
         [dynamic-proto-fringe-size (max *min-pre-proto-fringe-size* (/ (fringe-pcount cf) 500))]
         [start (first ipair)]
         [end (second ipair)]
         [assignment-count (- end start)]
         [expanded-phase1 1];; technically, not yet, but during initialization in pre-resv do loop
         ;; make the fringehead advanced to the right place
         [cffh (fh-from-fringe cf start)]
         )
    ;; do the actual expansions
    (do ([i 1 (add1 i)]
         [expansions (expand (fringehead-next cffh))
                     (set-union expansions (expand (fringehead-next cffh)))])
      ((>= i assignment-count)
       (set! pre-ofiles
             (process-proto-fringe expansions pre-ofile-template-fname pre-ofile-counter pre-ofiles)))
      ;; When we have collected the max number of expansions, create another pre-proto-fringe file
      (when (>= (set-count expansions) dynamic-proto-fringe-size)
        (set! pre-ofiles
              (process-proto-fringe expansions pre-ofile-template-fname pre-ofile-counter pre-ofiles))
        (set! pre-ofile-counter (add1 pre-ofile-counter))
        (set! expansions (set)))
      (advance-fhead! cffh)
      (set! expanded-phase1 (add1 expanded-phase1)))
    #|(printf "remote-exp-part-fringe: PHASE 1: expanding ~a positions of assigned ~a~%" 
            expanded-phase1 assignment-count);|#
    (when (< expanded-phase1 assignment-count)
      (error 'remote-expand-part-fringe
             (format "only expanded ~a of the assigned ~a (~a-~a) positions" expanded-phase1 assignment-count start end)))
    ;(close-input-port (fringehead-iprt cffh))
    ;; PHASE 2: now pass through the proto-fringe expansion file(s) as well as prev-fringe and current-fringe to remove duplicates
    (remove-dupes pf cf pre-ofiles 
                  (format "proto-fringe-d~a-~a" depth (~a process-id #:left-pad-string "0" #:width 2 #:align 'right)) ;; ofile-name
                  depth)))


;; remote-expand-fringe: (listof (list fixnum fixnum)) fring fringe int -> (listof sampling-stat)
;; trigger the distributed expansion according to the given ranges
;; In theory, it shouldn't matter where the files pointed to by the fringe are located,
;; but we expect they will point to a *local-store* copy of the current-fringe,
;; where the copy is arranged-for by the master also
(define (remote-expand-fringe ranges pf cf depth)
  ;;(printf "remote-expand-fringe: current-fringe of ~a split as: ~a~%" cur-fringe-size (map (lambda (pr) (- (second pr) (first pr))) ranges))
  (let* ([distrib-results (for/work ([range-pair ranges]
                                     [i (in-range (length ranges))])
                                    (when (> depth *max-depth*) (error 'distributed-expand-fringe "ran off end")) ;;prevent riot cache-failure
                                    ;; need alternate version of wait-for-files that just checks on the assigned range
                                    ;; but for now, just append the fspecs
                                    #| push the wait into where we're trying to access positions 
                                    (wait-for-files (append (map (lambda (seg) (segment-fspec seg)) pf-findex)
                                                            (map (lambda (seg) (segment-fspec seg)) cf-findex)) #t)|#
                                    (remote-expand-part-fringe range-pair i pf cf depth)
                                    )])
    ;(printf "remote-expand-fringe: respective expansion counts: ~a~%" (map (lambda (ssv) (vector-ref ssv 0)) distrib-results))
    distrib-results))


;; ------------------------------------------------------------------------------------------
;; MERGING .....

;; bring-local-partial-expansions: (listof fspec) -> void
;; copy the partial expansions from the shared disk to our local /tmp, 
(define (bring-local-partial-expansions lo-expand-specs)
  (for ([fs lo-expand-specs])
    (let* ([base-fname (filespec-fname fs)] 
           [tmp-partexp-name (string-append *local-store* base-fname)])
      (unless (file-exists? tmp-partexp-name) ; unless this process id is here from expansion
        (copy-file base-fname tmp-partexp-name)))))
                                

;; remote-merge-proto-fringes: (list int int) (listof fspec) int string -> (list string number)
;; given a specification of a responsibility range of _position-hash-codes_ to consider,
;; a list of specs, and the counts on prev and current fringes,
;; go through all the partial-expansions and merge the positions (removing duplicates) in that range into a single collection.
;; write the results to a local /tmp file, then finally copy to the home directory (NFS share), and delete the files.
;; Return a list with the merged filename and the size of that segment
(define (remote-merge-proto-fringes my-range expand-files-specs depth ofile-name)
  ;; expand-files-specs are of pattern: "proto-fringe-dXX-NN" for depth XX and proc-id NN, pointing to working (shared) directory 
  ;;NEW: ofile-name is of pattern: "fringe-segment-dX-NN", where the X is the depth and the NN is a process identifier
  (let* (;[mrg-segment-oport (open-output-file (string-append *local-store* ofile-name))]
         [mrg-segment-oport (open-output-file ofile-name)] ; try writing directly to NFS
         [copy-partial-expansions-to-local-disk ;; but only if not sharing host with master
          (unless (string=? *master-name* "localhost")
            ;; copy shared-drive expansions to *local-store*, uncompress, and delete compressed version
            (bring-local-partial-expansions expand-files-specs))]
         [local-protofringe-fspecs (for/list ([fs expand-files-specs]) (rebase-filespec fs *local-store*))]
         [to-merge-fheads 
          (for/list ([exp-fspec local-protofringe-fspecs])
            (fh-from-filespec exp-fspec))]
         [fastforward-the-to-merge-fheads ; for the side-effect
          (for ([fh to-merge-fheads]
                [partial-expansion-size (map filespec-pcount local-protofringe-fspecs)])
            (let ([fnext (fringehead-next fh)])
              (unless (or (eof-object? fnext)
                          (fx>= (equal-hash-code fnext) (first my-range)))
                (do ([lfhpos fnext (advance-fhead! fh)])
                  ((or (eof-object? lfhpos)
                       (fx>= (equal-hash-code lfhpos) (first my-range))))))))]
         [heap-o-fheads (let ([lheap (make-heap (lambda (fh1 fh2) (position<? (fringehead-next fh1) (fringehead-next fh2))))])
                          (heap-add-all! lheap (filter-not (lambda (fh) (eof-object? (fringehead-next fh))) to-merge-fheads))
                          lheap)]
         [segment-size (let ([last-pos (void)]
                             [keep-pos (void)]
                             [num-written 0])
                         (for ([an-fhead (in-heap/consume! heap-o-fheads)]
                               #:break (fx>= (equal-hash-code (fringehead-next an-fhead)) (second my-range)))
                           (set! keep-pos (fringehead-next an-fhead))
                           (advance-fhead! an-fhead)
                           (unless (fhdone? an-fhead) ;;(eof-object? (peek-byte (fringehead-iprt an-fhead) 1))
                             (heap-add! heap-o-fheads an-fhead))
                           (unless (equal? keep-pos last-pos)
                             (fprintf mrg-segment-oport "~a~%" (charify keep-pos))
                             (set! num-written (add1 num-written)))
                           (set! last-pos keep-pos))
                         num-written)])
    ;(printf "remote-merge-expansions: fw-lolop-lengths=~a [total ~a]~%" (map length fastforwarded-lolops) (for/sum ([l fastforwarded-lolops]) (length l)))
    ;(printf "remote-merge-expansions: merged-expns-length=~a~%" (length sorted-merged-expansions))
    (close-output-port mrg-segment-oport)
    (for ([fhead to-merge-fheads]) (close-input-port (fringehead-iprt fhead)))
    ;; arrange to move a compressed version of ofile-name to the shared disk
    ;(copy-file (string-append *local-store* ofile-name) ofile-name)
    ;(printf "remote-merge-expansions: about to try deleting ofile-name, ~a~%" ofile-name)
    ;(delete-file (string-append *local-store* ofile-name))
    ;;**** maybe not remove this as would be the prev-fringe on the next cycle ****????
    (unless (string=? *master-name* "localhost")
      (for ([fspc local-protofringe-fspecs]) 
        (delete-file (filespec-fullpathname fspc)))) ;remove the local expansions *** but revisit when we reduce work packet size for load balancing
    (list ofile-name segment-size)))

;; remote-merge: (listof (list fixnum fixnum)) (listof fspec) int -> (listof string int)
(define (remote-merge merge-ranges expand-files-specs depth)
  (when (string=? *master-name* "localhost")
    (bring-local-partial-expansions expand-files-specs))
  (let ([merge-results
         (for/work ([merge-range merge-ranges] ;; merged results should come back in order of merge-ranges assignments
                    [i (in-range (length merge-ranges))])
                   (when (> depth *max-depth*) (error 'distributed-expand-fringe "ran off end"))
                   (let* ([ofile-name (format "fringe-segment-d~a-~a" depth (~a i #:left-pad-string "0" #:width 2 #:align 'right))]
                          [merged-fname-and-resp-rng-size (remote-merge-proto-fringes merge-range expand-files-specs depth ofile-name)]
                          )
                     ;;(printf "distributed-expand-fringe: merge-range = ~a~%~a~%" merge-range merged-responsibility-range)
                     ;(write-fringe-to-disk merged-responsibility-range ofile-name)
                     merged-fname-and-resp-rng-size))])
    ;(printf "remote-merge: merged segment names and lengths ~a~%" merge-results)
    (when (string=? *master-name* "localhost")
      (for ([f expand-files-specs])
        ;(delete-file (first f)) ; these should have been deleted before the call to for/work
        (delete-file (string-append *local-store* (filespec-fname f)))))
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

;; distributed-expand-fringe: fringe fringe int -> (list string int int)
;; Distributed version of expand-fringe
;; given prev and current-fringes and the present depth, expand and merge the current fringe,
;; returning the fringe-spec of the newly expanded and merged fringe.
(define (distributed-expand-fringe pf cf depth)
  #|(printf "distributed-expand-fringe: at depth ~a, pf-spec: ~a; cf-spec: ~a~%" 
          depth pf-spec cf-spec)|#
  ;; push newest fringe (current-fringe) to all workers
  (cond [(string=? *master-name* "localhost")
         (for ([fsegment (fringe-segments cf)]
               #:unless (file-exists? (format "~a~a" *local-store* (filespec-fname fsegment))))
           (copy-file (filespec-fullpathname fsegment) (format "~a~a" *local-store* (filespec-fname fsegment))))]
        [else (system (format "sync; rocks run host compute 'sync; cp ~a ~a'"      ; just copy from shared drive to local
                              (for/fold ([filesstring ""])
                                ([fspec (fringe-segments cf)])
                                (string-append filesstring " puzzle-solver/" (filespec-fname fspec)))
                              *local-store*))])
  (let* (;; EXPAND
         [ranges (make-vector-ranges (fringe-pcount cf))]
         ;;make remote fringe
         [rcf (make-fringe (fringe-fbase cf)
                           (for/list ([seg (fringe-segments cf)]) 
                             (make-filespec (filespec-minhc seg) (filespec-maxhc seg)
                                            (filespec-fname seg) (filespec-pcount seg) (filespec-fsize seg) *local-store*))
                           (fringe-pcount cf))]
         ;; --- Distribute the actual expansion work ------------------------
         [infomsg1 (printf "starting expand, ... ")]
         [expand-start (current-seconds)]
         [sampling-stats (remote-expand-fringe ranges pf rcf depth)]
         [expand-time-msg (printf "expand time: ~a~%" (~r (/ (- (current-seconds) expand-start) 60.0) #:precision 4))]
         ;; -----------------------------------------------------------------
         [check-for-goal (for/first ([ss sampling-stats]
                                     #:when (vector-ref ss 4))
                           (set! *found-goal* (vector-ref ss 4)))]
         ;; make filespecs for proto-fringe-dXX-NN the relevant data in the sampling-stats
         [proto-fringe-fspecs (for/list ([ss sampling-stats])
                                #|(printf "exp-spec: ~a should have ~a postions: seeing ~a of expected ~a~%" 
                                       (vector-ref ss 5) (vector-ref ss 0) (file-size (vector-ref ss 5)) (vector-ref ss 6))|#
                                (make-filespec (vector-ref ss 1) (vector-ref ss 2) (vector-ref ss 5) (vector-ref ss 0) (vector-ref ss 6) ""))]
         ;; need to wait for write to complete -- i.e., all data to appear on master
         ;; push this wait into the place we're trying to access the file ... [wait-for-partial-expansions (wait-for-files expand-files-specs)]
         #|[error-check1 (for/first ([i (in-range (length sampling-stats))]
                                   [ss sampling-stats]
                                   #:unless (= (vector-ref ss 0) (position-count-in-file (format "partial-expansion~a" (~a i #:left-pad-string "0" #:width 2 #:align 'right)))))
                         (error 'distributed-expand-fringe (format "err-chk1: partial-expansion sizes do not match up for ~a which should be ~a" i (vector-ref ss 0))))]|#
         ;; MERGE
         [merge-ranges (simple-merge-ranges sampling-stats)]
         [infomsg2 (printf "starting merge, ... ")]
         ;; --- Distribute the merging work ----------
         [merge-start (current-seconds)]
         [sorted-segment-fspecs (remote-merge merge-ranges proto-fringe-fspecs depth)]
         [merge-end (current-seconds)]
         [merge-time-msg (printf "merge time: ~a~%" (~r (/ (- merge-end merge-start) 60.0) #:precision 4))]
         ;; -------------------------------------------
         [sorted-expansion-files (map first sorted-segment-fspecs)]
         [sef-lengths (map second sorted-segment-fspecs)]
         #|[error-check2 (for/first ([f sorted-expansion-files]
                                   [len sef-lengths]
                                   #:unless (= len (position-count-in-file f)))
                         (error 'distributed-expand-fringe (format "err-chk2: partial-merges do not match up for ~a which should be ~a" f len)))]|#
         [new-cf-name (format "fringe-d~a" depth)]
         )
    ;; create the _new_ current-fringe
    #|
    (for ([f sorted-expansion-files])
      ;(printf "distributed-expand-fringe: concatenating ~a~%" f)
      (system (format "cat ~a >> fringe-d~a" f depth)))|#
    ;;--- delete files we don't need anymore ---------
    ;; delete previous fringe
    (delete-fringe pf)
    (when (string=? *master-name* "localhost") ; delete the *local-store* prev-fringe
      (delete-fringe pf *local-store*))
    (for ([fspec proto-fringe-fspecs]) (delete-file (filespec-fullpathname fspec)))
    ;(system "rm partial-expansion* partial-merge*")
    ;(unless (string=? *master-name* "localhost") (delete-file (fspec-fname cf-spec)))
    (printf "distributed-expand-fringe: file manipulation ~a, and total at depth ~a: ~a~%" 
            (~r (/ (- (current-seconds) merge-end) 60.0) #:precision 4) depth (~r (/ (- (current-seconds) expand-start) 60.0) #:precision 4))
    ;; make the new fringe to return
    (make-fringe ""
                 (for/list ([mrg-range merge-ranges]
                            [segmentfile sorted-expansion-files]
                            [length sef-lengths])
                   (make-filespec (first mrg-range) (second mrg-range) segmentfile length (file-size segmentfile) ""))
                 (for/sum ([len sef-lengths]) len))
    ))


;;----------------------------------------------------------------------------------------

;; expand-fringe: fringe fringe int -> fringe
;; Given the prev- and current-fringes, and the current depth of search,
;; do the expansions and merges as appropriate, returning the new fringe
(define (expand-fringe prev-fringe current-fringe depth)
  (if (< (fringe-pcount current-fringe) *diy-threshold*)
      ;; do it myself
      (expand-fringe-self prev-fringe current-fringe depth)
      ;; else call distributed-expand, which will farm out to workers
      (distributed-expand-fringe prev-fringe current-fringe depth)))

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

;; cfs-file: fringe fringe int -> position
;; perform a file-based cluster-fringe-search at given depth
;; using given previous and current fringes
(define (cfs-file prev-fringe current-fringe depth)
  (cond [(or (zero? (fringe-pcount current-fringe)) (> depth *max-depth*)) #f]
        [*found-goal*
         (print "found goal")
         *found-goal*]
        [else (let ([new-fringe (expand-fringe prev-fringe current-fringe depth)])
                (printf "At depth ~a: current-fringe has ~a positions (and new-fringe ~a)~%" 
                        depth (fringe-pcount current-fringe) (fringe-pcount new-fringe))
                ;;(for ([p current-fringe]) (displayln p))
                (cfs-file current-fringe ;; use current-fringe as prev-fringe at next level
                          new-fringe
                          (add1 depth)))]))

;; start-cluster-fringe-search: bw-position -> ...
(define (start-cluster-fringe-search start-position)
  ;; initialization of fringe files
  (write-fringe-to-disk empty "fringe-d-1")
  (write-fringe-to-disk (list start-position) "fringe-d0")
  (cfs-file (make-fringe "" (list (make-filespec *most-negative-fixnum* *most-positive-fixnum* "fringe-d-1" 0 (file-size "fringe-d-1") "")) 0)
            (make-fringe "" (list (make-filespec *most-negative-fixnum* *most-positive-fixnum* "fringe-d0" 1 (file-size "fringe-d0") "")) 1)
            1))
  

;(block10-init)
;(climb12-init)
(climb15-init)
(compile-ms-array! *piece-types* *bh* *bw*)

;;#|
(module+ main
  ;; Switch between these according to if using the cluster or testing on multi-core single machine
  (connect-to-riot-server! *master-name*)
  (define search-result (time (start-cluster-fringe-search *start*)))
  (print search-result))
;;|#

;;(time (start-cluster-fringe-search *start*))
