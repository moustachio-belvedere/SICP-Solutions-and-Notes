#lang sicp

; uses relative tolerance between consecutive guesses instead of absolute tolerance which has issues described in ex_1-07.rkt.

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (square x) (* x x))

(define (goodenough? guess_old guess_new x)
  (< (/ (abs (- guess_new guess_old)) guess_new) 0.01))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cubrt-iter guess_old guess_new x)
;  (display guess_new)
;  (newline)
  (cond ((goodenough? guess_old guess_new x) guess_new)
      (else (cubrt-iter guess_new (improve guess_new x)
                 x))))

(define (cubert x)
  (cubrt-iter 2.0 1.0 x))

; simple test
(cubert 8)

; other tests
(cubert 64)
(cubert 27)
(cubert 729)
(cubert 343)
