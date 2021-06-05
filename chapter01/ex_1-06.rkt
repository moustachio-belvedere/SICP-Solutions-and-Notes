#lang sicp

; with 'new-if', applicate evaluation order means
; that, even if guess is goodneough?, we still enter another
; stack frame because the else-clause will be evaluated before
; sqrt-iter can ever return 'guess'

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

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
  (new-if (goodenough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 4)
