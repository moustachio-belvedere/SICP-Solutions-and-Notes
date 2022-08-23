#lang sicp

; still, Alyssa's question is unanswered, even though her
; workaround was not functional?

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (square x) (* x x))

(define (goodenough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (display guess)
  (newline)
  (cond ((goodenough? guess x) guess)
      (else (sqrt-iter (improve guess x)
                 x))))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 4)