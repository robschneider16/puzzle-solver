#lang racket

;(require (planet soegaard/gzip:2:2))
;(require file/gunzip)
;(require file/gzip)
(require "stp-init.rkt"
         "stp-solve-base.rkt"
         "stp-fringefilerep.rkt")
(require rnrs/sorting-6)


(define *ntimes* 50)

(define-struct hcpos (hc bs))
;; the hc is the hashcode of the bytestring

;; position<?: position position -> boolean
(define (hcposition<? p1 p2)
  (or (< (hcpos-hc p1) (hcpos-hc p2))
      (and (= (hcpos-hc p1) (hcpos-hc p2))
           (blexi<? (hcpos-bs p1) (hcpos-bs p2)))))

(define (llexi<? p1 p2)
  (do ([pt 0 (add1 pt)]
       [bv1 (vector-ref p1 0) (vector-ref p1 pt)]
       [bv2 (vector-ref p2 0) (vector-ref p2 pt)]
       [result 0]
       )
    ((or (not (zero? result)) (>= pt (vector-length p1))) result)
    (when (not (= bv1 bv2))
      (set! result 
            (if (< bv1 bv2)
                -1
                1)))))

(define (blexi<? p1 p2)
  (do ([i 0 (add1 i)])
    ((or (= i (bytes-length p1))
         (not (= (bytes-ref p1 i) (bytes-ref p2 i))))
     (and (< i (bytes-length p1))
          (< (bytes-ref p1 i) (bytes-ref p2 i))))))

;; TESTING HASH-CODE-BASED position<? TO LEXICOGRAPHIC VERSION
#|
condition,  position<?                 lexi<?                       blexi<?/llexi<?
50,000 positions
bytestrings 0.114(0)                   0.389(48)                    0.274(0)
bwreps      0.183(0)                   0.237(20)                    0.168(0)
500,000 postions
bytestrings 1.637(57)                  6.498(1.628)                 4.254(68)
bwreps      2.452(96)                  5.012(2.072)                 2.190(0.156)
5,000,000
bytestrings 23.370(1.028)              116.678(45.494)              61.682(1.720)
bwreps      39.997(10.388)             85.013(44.984)               26.639(6.561)
|#

;; bwreps      32.037 sec rt (3.196 gc),  86.331 sec rt (46.064 gc),   28.536 rt (8.993 gc)
;; bytestrings 24.108 (1.128 gc),         110.809 (39.921),            59.774 rt (0.920 gc)


;; Read a fringe, expand it and sort the result (might as well ignore duplicates in order to make it larger)

;; create generator from the fringehead
(define HOWMANY 5000000)
(define iprt (open-input-file "hold-current-fringe-d104"))
(define bfv (for/vector #:length HOWMANY ([p (in-port read iprt)])
              (let ([charpos (charify (set-first (expand p)))])
                (make-hcpos (equal-hash-code charpos) charpos))))
(define bfv1 (vector-copy bfv))
"augmented hash-code positions"
(time (vector-sort! (lambda (p1 p2) (< (hcpos-hc p1) (hcpos-hc p2))) bfv1))
;(define bfv2 (vector-copy bfv))
;(define bfv3 (vector-copy bfv))
;"using position<? with charified positions:"
;(time (vector-sort! position<? bfv1))
;"using lexi<? with charified positions:"
;(time (vector-sort! lexi<? bfv2))
;"using blexi<? with charified positions:"
;(time (vector-sort! blexi<? bfv3))
;(define bfv4 (for/vector #:length HOWMANY ([p bfv]) (decharify p)))
;(define bfv5 (vector-copy bfv4))
;(define bfv6 (vector-copy bfv4))
;(define bfv7 (vector-copy bfv4))
;"using postion<? with bwreps:"
;(time (vector-sort! position<? bfv5))
;"using lexi<? with bwreps:"
;(time (vector-sort! lexi<? bfv6))
;"using llexi<? with bwreps:"
;(time (vector-sort! llexi<? bfv7))



#|;; For *ntimes* = 50, takes 5 minutes, 31.039 seconds 
(time (for ([i *ntimes*])
        (let ([iprt (open-input-gz-file (string->path "current-fringeC12d59.gz"))]
              [oprt (open-output-gz-file (string->path "test-out.gz") #:replace #t)])
          (for ([p (in-port read iprt)]
                #:unless (= (vector-ref p 0) -99))
            (when (= (vector-ref p 0) -99) (printf "found it ~a~%" p))
            (fprintf oprt "~a~%" p))
          (close-input-port iprt)
          (close-output-port oprt))))

;; For *ntimes* = 50, takes 2 minutes, 39 seconds
(time (for ([i *ntimes*])
        (gunzip "current-fringeC12d59.gz")
        (let ([iprt (open-input-file "current-fringeC12d59")]
              [oprt (open-output-file "test-out" #:exists 'replace)])
          (for ([p (in-port read iprt)]
                #:unless (= (vector-ref p 0) -99))
            (when (= (vector-ref p 0) -99) (printf "found it ~a~%" p))
            (fprintf oprt "~a~%" p))
          (close-input-port iprt)
          (close-output-port oprt))
        (gzip "test-out")))
  

; g(un)zip-through-ports: For *ntimes* = 50, takes 2 minutes, 39 seconds
(time (for ([i *ntimes*])
        (let ([iprt (open-input-file "current-fringeC12d59.gz")]
              [oprt (open-output-file "test-out" #:exists 'replace)])
          (gunzip-through-ports iprt oprt)
          (close-input-port iprt)
          (close-output-port oprt))
        (delete-file "test-out")
        ))
(time (for ([i *ntimes*])
        (gunzip "current-fringeC12d59.gz" (lambda (f b) (string->path "test-out")))
        ;(displayln (system "wc -l test-out"))
        (delete-file "test-out")))
|#        

#| Tested fine at 500, when trying for 1000 it failed after 993
(define how-many-open-files (for/list ([i 00])
                              (displayln i)
                              (open-output-file (format "gak~a" i))))
|#
