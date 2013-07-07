#lang racket
;; compoundwords.rkt
(require (planet gcr/riot))

(define dictionary
  ;; create the set of dictionary words
  (for/set ([word (in-list (file->lines "evenfewerwords"))]
            #:when (>= (string-length word) 3))
    word))

;; package-dict: (listof string) -> (listof (listof string))
;; turn the list of words into a list of package-sized lists of words
(define (package-dict a-dict-list package-size)
  (cond [(empty? a-dict-list) empty]
        [(< (length a-dict-list) package-size) (list a-dict-list)]
        [else (cons (take a-dict-list package-size)
                    (package-dict (drop a-dict-list package-size) package-size))]))

(define (slow-combinations)
  (define local-dict (for/set ([word (in-list (file->lines "evenfewerwords"))]
                               #:when (>= (string-length word) 3))
                              word))
  (apply append
         (for/work ([bunch-of-words (in-list (package-dict (set->list local-dict) 3000))])
                   (apply append
                          (for/list ([first-word (in-list bunch-of-words)])
                            (for/list ([second-word (in-set local-dict)]
                                       #:when (set-member? local-dict
                                                           (string-append first-word second-word)))
                              (cons first-word second-word)))))))

(define (word-combinations)
  (apply append
         (for/work ([bunch-of-words (in-list (package-dict (set->list dictionary) 200))])
                   (apply append
                          (for/list ([first-word (in-list bunch-of-words)])
                            (for/list ([second-word (in-set dictionary)]
                                       #:when (set-member? dictionary
                                                           (string-append first-word second-word)))
                              (cons first-word second-word)))))))

(module+ main
  (connect-to-riot-server! "localhost")
  (define words (time (slow-combinations)))
  ;;(define words (time (word-combinations)))
  (printf "There are ~a compound-words in the dictionary.\n" (length words))
  ;; Print a random subset of results
  (write (take (shuffle words) 20)) (newline))
