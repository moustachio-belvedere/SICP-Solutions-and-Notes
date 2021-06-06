#lang sicp

; the issue with goodenough? is that it uses an absolute tolerance instead of a relative tolerance.

; approx. definitions of large and small numbers working on the assumption of approximately 16 decimal digits of computational precision.

; for very large numbers (x>1e13), their difference will only be less than 0.001 if they are exactly the same (i.e. perfect guess). Otherwise, their least significant digit will be higher than thousandth digit so the difference between the two cannot be less than 0.001. Unless starting guess is perfect, no solution will be accepted.

; for very small numbers (x<1e-3), the difference will become quickly smaller than the absolute tolerance, and after this no further improvement on the guess will be made.

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

; big number example (doesn't terminate)
;(sqrt 12345678901234)

; small number example (doesn't improve)
;(sqrt 0.0001)
