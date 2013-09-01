#lang racket

(require 
 ;(planet soegaard/gzip:2:2))
 ;file/gzip
 ;file/gunzip
 mzlib/string
 racket/generator
 "stp-init.rkt"
 "stp-solve-base.rkt")

(provide (all-defined-out))

#|====================================================================================
stp-fringefilerep: Utilities for processing fringes that are stored in files

When fringes are stored on files, need to support creation/access/manipulation/etc. 
and provide uniform interface for solvers to access fringes as if they were simple sequence.

Data definitions included here:
- fringe: (vector full-path-to-file (listof segment-spec) number-of-positions)
- filespec is a list: (list min-hashcode max-hashcode fname position-count file-size) [assuming the positions in the file are sorted]
- fhead (short for fringehead) is a structure (make-fringehead position input-port readcount totalcount)

findex (short for fringe-index): (listof segment-spec) [assumes the list of segment-specs is sorted]
|#

;; -----------------------------------------------------------------------------------
;; --- FRINGE ------------------------------------------------------------------------

;; a fringe is a vector: (vector string (listof filespec) int)
;; where the first string is the base-path to the files, the list of segment-specs describe the segments,
;; and the ints are the count of positions summed over the segments
;; NOTE: an fspec must be a list or vector because they are passed over the riot layer
(define (make-fringe fbase segments pcount) (vector fbase segments pcount))
(define (fringe-fbase an-fs) (vector-ref an-fs 0))
(define (fringe-segments an-fs) (vector-ref an-fs 1))
(define (fringe-fullpathnames an-fs)
  (for/list ([seg (fringe-segments an-fs)]) (string-append (fringe-fbase an-fs) (filespec-fname seg))))
(define (fringe-pcount an-fs) (vector-ref an-fs 2))


;; -----------------------------------------------------------------------------------
;; --- FILESPEC (SEGMENT) ------------------------------------------------------------

;; a filespec is a vector: (vector min-hashcode max-hashcode fname position-count file-size basepath)
(define (make-filespec minhc maxhc fname pcount fsize fbase) (vector minhc maxhc fname pcount fsize fbase))
(define (filespec-minhc fs) (vector-ref fs 0))
(define (filespec-maxhc fs) (vector-ref fs 1))
(define (filespec-fname fs) (vector-ref fs 2))
(define (filespec-pcount fs) (vector-ref fs 3))
(define (filespec-fsize fs) (vector-ref fs 4))
(define (filespec-fbase fs) (vector-ref fs 5))
(define (filespec-fullpathname fs) (string-append (filespec-fbase fs) (filespec-fname fs)))


;; -----------------------------------------------------------------------------------
;; --- FRINGEHEAD --------------------------------------------------------------------

;; a fringehead in a struct
(struct fringehead (next iprt filespecs readcount total fbase) #:mutable)
;; where next is a position, iprt is the current input-port, filespecs is the list of filespecs for the segments making up this fringe
;; with the first corresponding to the current intput-port, readcount is the number of positions read from this fringehead
;; total is the number of positions expected to be able to read, and basepath is the path to the files in the filespecs

;; fhdone?: fringehead -> boolean
;; #t if readcount >= total for the given fringehead -- that is, this fringehead is exhausted.
;; Note: readcount starts at 1, 
(define (fhdone? fh)
  (when (and (eof-object? (fringehead-next fh)) (<= (fringehead-readcount fh) (fringehead-total fh)))
    ;; try to reset 
    (error 'fhdone? "hit end of file before the appropriate number of positions had been read"))
  (and (eof-object? (fringehead-next fh))
       (empty? (rest (fringehead-filespecs fh)))))

;; advance-fhead!: fringehead -> position OR void
;; move to the next position, but check to make sure something is available if expected
(define (advance-fhead! fh)
  (when (< (fringehead-readcount fh) (fringehead-total fh))
    (do ([sleep-time 0.01 (* sleep-time 2)])
      ((not (eof-object? (peek-bytes 1 1 (fringehead-iprt fh)))) 'proceed)
      (sleep sleep-time)))
  (unless (fhdone? fh)
    (when (eof-object? (fringehead-next fh)) ; advance to next segment
      (set-fringehead-filespecs! fh (cdr (fringehead-filespecs fh)))
      (close-input-port (fringehead-iprt fh))
      (set-fringehead-iprt! fh (open-input-file (string-append (fringehead-fbase fh) (filespec-fname (car (fringehead-filespecs fh)))))))
    (set-fringehead-next! fh (read-pos (fringehead-iprt fh)))
    (set-fringehead-readcount! fh (add1 (fringehead-readcount fh)))
    (fringehead-next fh)))

;; position-in-fhead?: position fringehead -> boolean
;; determines if given position is present in the fringe headed by the given fringehead
;; side-effect: advances the fringehead, assuming no position _less-than_ thi given position will subsequently be queried
;; if the given position is less than the head of the fringe, then we'll not find it further in the fringe
;; that is, advance the fh while it is strictly less-than the given position
(define (position-in-fhead? p fh)
  (do ([fhp (fringehead-next fh) (advance-fhead! fh)])
    ((or (fhdone? fh)
         (not (position<? fhp p))) 
     (equal? fhp p))))

;; fh-from-fspec: fringe [int 0] -> fringehead
;; create a fringehead from a given fringe and advance it to the requested start
;; *** the open port must be closed by the requestor of this fringehead
(define (fh-from-fringe f [start 0])
  (let* ([firstfullpathname (string-append (fringe-fbase f) (filespec-fname (first (fringe-segments f))))]
         [inprt (open-input-file firstfullpathname)]
         [new-fh (fringehead (read-pos inprt) inprt (fringe-segments f) 1 (fringe-pcount f) (fringe-fbase f))])
    (for ([i start]) (advance-fhead! new-fh))
    new-fh))


;; -----------------------------------------------------------------------------------
;; --- BULK FRINGE READING/WRITING ---------------------------------------------------

;; write-fringe-to-disk: (listof or vectorof position) string -> void
;; writes the positions from the given fringe (whether list or vector) into a file with given file-name.
(define (write-fringe-to-disk fringe file-name)
  (let ([my-output (open-output-file file-name #:exists 'replace)])
    (for ([position fringe])
      (fprintf my-output "~a~%" (charify position)))
    (flush-output my-output)
    (close-output-port my-output)))

;; read-fringe-from-file: string -> (listof position)
;; reads a file from a file path (if you are in the current directory just simply the file-name)
;; and returns the fringe that was in that file.
(define (read-fringe-from-file file-name)
  (let* ([iport (open-input-file file-name)]
         [the-fringe (port->list (lambda (in) (decharify (read-bytes-line in))) iport)])
    (close-input-port iport)
    the-fringe))


;; -----------------------------------------------------------------------------------
;; --- MISC UTILITIES ----------------------------------------------------------------

;; read-pos: input-port -> bw-position
;; read the (probably byte representation of) position from the file attatched to the given input port
;; ASSUMES: fringe-file format is one position per line with bytes
(define (read-pos iprt)
  (decharify (read-bytes-line iprt)))

;; fringe-exists?: fringe -> boolean
;; report if all the files in the fringe are present (for now, ignore the sizes)
(define (fringe-exists? f)
  (for/and ([segment (fringe-segments f)])
    (file-exists? (filespec-fullpathname segment))))

;; delete-fringe: fringe -> void
;; remove all the files that make up the given fringe
(define (delete-fringe f)
  (for ([seg (fringe-segments f)]) (delete-file (filespec-fullpathname seg))))

;; fringe-file-not-ready?: string string int [check-alt-flag #f] -> boolean
;; determine if the single given file exists on disk and has the appropriate size
;; with optional check-alt-flag will look on the nfs share and copy if found
(define (fringe-file-not-ready? basepath fname fsize [check-alt-flag #f])
  (let ([fullname (string-append basepath fname)])
    (when (and check-alt-flag
               (not (file-exists? fullname))
               (file-exists? fname) ;; check working (shared) directory
               (not (string=? fullname fname)))
      (copy-file fname fullname)) ;; YUCK!
    (or (not (file-exists? fullname))
        (< (file-size fullname) fsize))))
      

;; wait-for-files: (listof fspec) [check-alt-flag #f] -> 'ready
;; given a list of fringe-specs, wait until the file is present in the specified location
;; with the specified size.  if check-alt-flag is true, then drop the fbase and see if the file is available via NFS (copy if so!)
;;******* CAUTION: not yet reflecting intent to move wait closer to seeking next position
(define (wait-for-files lo-fspecs [check-alt-flag #f])
  (do ([fspecs (filter (lambda (fspec) (fringe-file-not-ready? fspec check-alt-flag)) lo-fspecs)
               (filter (lambda (fspec) (fringe-file-not-ready? fspec check-alt-flag)) fspecs)]
       [sleep-time 0.01 (* sleep-time 2)])
    ((empty? fspecs) 'ready)
    (printf "wait-for-files: waiting for ~a files such as ~a ... and sleeping ~a~%" (length fspecs) (first (fringe-fullpathnames (first fspecs))) sleep-time)
    (sleep sleep-time)))

;; position-count-in-file: string -> number
;; reports the number of positions in the given fringe file assuming the file was written with write-fringe-to-disk
(define (position-count-in-file f)
  (read-from-string (with-output-to-string 
                     (lambda () (system (if (string=? (substring f (- (string-length f) 3)) ".gz")
                                            (format "zcat ~a | wc -l" f)
                                            (format "wc -l ~a" f)))))))
                    
;; check-sorted-fringe?: string -> boolean
;; assuming the string, f, points to a sorted file of positions, check to make sure they are sorted
(define (check-sorted-fringe? f)
  (let* ([myin (open-input-file f)]
         [prevpos (read-pos myin)]
         [bool-res (for/and ([pos (in-port read-pos myin)])
                     (let ([res (position<? prevpos pos)])
                       (set! prevpos pos)
                       res))])
    (close-input-port myin)
    bool-res))

;; touch: string -> void
;; create the file with given name
(define (touch fname) (display-to-file "" fname))
