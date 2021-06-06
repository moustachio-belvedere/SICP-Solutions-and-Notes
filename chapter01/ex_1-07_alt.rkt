#lang sicp

; uses relative tolerance between consecutive guesses instead of absolute tolerance which has issues described in ex_1-07.rkt.

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (square x) (* x x))

(define (goodenough? guess_old guess_new x)
  (< (/ (abs (- guess_new guess_old)) guess_new) 0.01))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess_old guess_new x)
;  (display guess_new)
;  (newline)
  (cond ((goodenough? guess_old guess_new x) guess_new)
      (else (sqrt-iter guess_new (improve guess_new x)
                 x))))

(define (sqrt x)
  (sqrt-iter 2.0 1.0 x))

; simple test
(sqrt 4)

; big number example
(sqrt 12345678901234)

; small number examqle
(sqrt 0.0001)

; test works better for the small number. Big number stops which is an improvement over absolute tolerance, but numberical precision is still not great.
