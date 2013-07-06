#lang racket
(require racket/file)

;; number-adder: file -> file
;; takes a file with a single int (0) and increases it by one each
;; time the function is called
(define (number-adder input-file)
  (cond [(equal? '("10") (file->lines input-file)) #f]
        [else
         (let ([current-num (string->number (first (file->lines input-file)))]
               [out (open-output-file input-file #:exists 'truncate)])
           (write (- current-num 1) out)
           (close-output-port out) 
           (number-adder input-file))]))
        
