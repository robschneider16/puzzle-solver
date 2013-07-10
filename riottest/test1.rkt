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
      (printf "Trial ~a: ~a msec build set, ~a convert to list, ~a send to worker, ~a conv to set, ~a conv back to list, ~a to get the result back~%"
              i 
              (- t1 t0); time to build
              0;(- t2 t1); time to convert
              (- (vector-ref (car res) 0) t1) ; time to send
              0;(- (second (caar res)) (first (caar res))) ; time to convert
              0;(- (third (caar res)) (second (caar res))) ; time to re-convert
              (- t3 (vector-ref (car res) 1)) ; time to return
              ))))
    

#|
(for/list ([k (in-range n)])
      (run k)))
|#

(module+ main
	 (connect-to-riot-server! "localhost")
	 (displayln (test-run 5))
	 (displayln "Complete"))

