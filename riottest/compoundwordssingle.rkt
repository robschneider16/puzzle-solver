#lang racket
;; compoundwordssingle.rkt


(define dictionary
  ;; create the set of dictionary words
  (for/set ([word (in-list (file->lines "fewerwords"))]
            #:when (>= (string-length word) 3))
    word))


(define (word-combinations)
  (apply append
         (for/list ([first-word (in-set dictionary)])
           (for/list ([second-word (in-set dictionary)]
                      #:when (set-member? dictionary
                                          (string-append first-word second-word)))
             (cons first-word second-word)))))

(module+ main
  (define words (time (word-combinations)))
  (printf "There are ~a compound-words in the dictionary.\n" (length words))
  ;; Print a random subset of results
  (write (take (shuffle words) (min 20 (length words)))) (newline))
