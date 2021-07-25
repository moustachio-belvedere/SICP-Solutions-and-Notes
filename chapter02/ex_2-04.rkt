#lang sicp

(define (cons_alt x y)
  (lambda (m) (m x y)))

(define (car_alt z)
  (z (lambda (p q) p)))

(define (cdr_alt z)
  (z (lambda (p q) q)))

(define x (cons_alt 3 5))
(car_alt x)
(cdr_alt x)

; (car_alt x)
; (x (lambda (p q) p))
; ((lambda (p q) p) 3 5)
