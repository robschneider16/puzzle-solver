#lang racket
(require (planet gcr/riot))
(require racket/serialize)

(define (run n)
  (for/work ([i (in-range 20)])
            (local-f n i)))

(define (local-f n i)
  (string-append "n=" (number->string n) ", my-work i=" (number->string i)))

(define (test-run n)
  (for ([i (in-range n)])
    (let* ((t0 (current-milliseconds))
           (bigvec (for/vector ([x (in-range (+ i 1000000))]) x))
           (t1 (current-milliseconds))
           ;(serializedset (serialize bigset))
           ;(t2 (current-milliseconds))
           (res (for/work ([bl (list bigvec)])
                          (let* ((ot1 (current-milliseconds))
                                 ;(mybigset (deserialize bl))
                                 ;(ot2 (current-milliseconds))
                                 ;(tmp (serialize (set-add mybigset (list ot2 ot1 (set-count mybigset)))))
                                      )
                            (vector-set! bl 0 ot1)
                            (vector-set! bl 1 (current-milliseconds))
                            bl)))
           (t3 (current-milliseconds)))
      (printf "Trial ~a: ~a msec build vec, ~a (unused), ~a send to worker, ~a (unused), ~a (unused), ~a to get the result back~%"
              i 
              (- t1 t0); time to build
              0;(- t2 t1); time to convert
              (- (vector-ref (car res) 0) t1) ; time to send
              0;(- (second (caar res)) (first (caar res))) ; time to convert
              0;(- (third (caar res)) (second (caar res))) ; time to re-convert
              (- t3 (vector-ref (car res) 1)) ; time to return
              ))))

;; same as above with writing to disk included
(define (test-write-run n)
  (for ([i (in-range n)])
    (let* ([t0 (current-milliseconds)]
           [biglist (for/list ([x (in-range (+ i 1000000))]) x)]
           [t1 (current-milliseconds)]
           [file-stream (open-output-file "biglist" #:exists 'replace)]
           [biglist-file (write biglist file-stream)]
           [ignore (close-output-port file-stream)]
           [t2 (current-milliseconds)]
           [res (for/work ([bl (list (length biglist))])
                          (printf "worker sees ~a at ~a milliseconds ~%" bl (current-milliseconds))
                          (let* ([ot1 (current-milliseconds)]
                                 [biglist-file-read (with-input-from-file "biglist" read)] 
                                 ;[ot2 (current-milliseconds)]
                                 )
                            ;(set-mcar! bl ot1)
                            ;(set-mcdr! bl (current-milliseconds))
                            (list ot1 (current-milliseconds) (last biglist-file-read))))] 
           [t3 (current-milliseconds)])
      ;;(printf "~a ~%" res)
      (printf "Trial ~a: ~a msec build list, ~a write list to disk, ~a send to worker, ~a read list from disk, ~a (unused), ~a to get the result back~%"
              i 
              (- t1 t0); time to build
              (- t2 t1); time to write to disk
              (- (car (first res)) t2) ; time to send
              (-  (cadr (first res)) (car (first res))) ; time to read 
              0;(- (third (caar res)) (second (caar res))) ; time to re-convert
              (- t3 (cadr (first res))) ; time to return
              )))) 
    

#|
(for/list ([k (in-range n)])
      (run k)))
|#

(module+ main
	 (connect-to-riot-server! "localhost")
	 (displayln (test-run 5))
	 (displayln "Complete"))

