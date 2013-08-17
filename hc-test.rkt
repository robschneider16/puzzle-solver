#lang racket

(require (planet soegaard/gzip:2:2))
(require file/gunzip)
(require file/gzip)

(define *ntimes* 50)

#| For *ntimes* = 50, takes 5 minutes, 31.039 seconds 
(time (for ([i *ntimes*])
        (let ([iprt (open-input-gz-file (string->path "current-fringeC12d59.gz"))]
              [oprt (open-output-gz-file (string->path "test-out.gz") #:replace #t)])
          (for ([p (in-port read iprt)]
                #:unless (= (vector-ref p 0) -99))
            (when (= (vector-ref p 0) -99) (printf "found it ~a~%" p))
            (fprintf oprt "~a~%" p))
          (close-input-port iprt)
          (close-output-port oprt))))
;|#

#| For *ntimes* = 50, takes 2 minutes, 39 seconds
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
  |#        
           
#| Tested fine at 500, when trying for 1000 it failed after 993
(define how-many-open-files (for/list ([i 00])
                              (displayln i)
                              (open-output-file (format "gak~a" i))))
|#
