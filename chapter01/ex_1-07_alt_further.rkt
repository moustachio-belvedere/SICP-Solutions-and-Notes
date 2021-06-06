#lang sicp

; another modification in addition to that suggested in question, a relative tolerance based on square of guess and x

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (square x) (* x x))

(define (goodenough? guess x)
  (< (/ (abs (- (square guess) x)) x) 0.01))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
;  (display guess)
;  (newline)
  (cond ((goodenough? guess x) guess)
      (else (sqrt-iter (improve guess x)
                 x))))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; simple test
(sqrt 4)

; big number example
(sqrt 12345678901234)

; small number examqle
(sqrt 0.0001)

; similar performance as guess-wise relative tolerance
