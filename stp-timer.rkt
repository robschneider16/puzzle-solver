#lang racket

(provide (all-defined-out))

(struct timer (count total started on) #:mutable #:transparent)
;; a timer is a structure: (timer c t s o) where c t and s are numbers and o is a boolean
;; count is the number of times the timer has been turned on
;; total is the accumulation of time while it has been on, recorded in milliseconds
;; started is the time when this was turned on (ignored when state is off)
;; on is boolean for on (#t) or off (#f)

; create an initialized timer object
(define (make-timer) (timer 0 0 0 #f))

; start this timer
(define (start-timer t) 
  (when (timer-on t) (error "start-timer: attempt to start a timer that is already on"))
  (set-timer-on! t #t)
  (set-timer-count! t (add1 (timer-count t)))
  (set-timer-started! t (current-inexact-milliseconds)))

; stop this timer, accumulating time in the total
(define (stop-timer t)
  (unless (timer-on t) (error "stop-timer: attempt to stop a timer that is already off"))
  (set-timer-total! t (+ (timer-total t) (- (current-inexact-milliseconds) (timer-started t))))
  (set-timer-on! t #f))
  
; reset this timer
(define (reset-timer t)
  (set-timer-count! t 0)
  (set-timer-total! t 0)
  (set-timer-on! t #f))

; wraps a call in a timer
(define-syntax-rule (time-this t e)
  (let ([res #f])
    (start-timer t)
    (set! res e)
    (stop-timer t)
    res))
