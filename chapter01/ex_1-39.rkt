#lang sicp

(define (square x)
  (* x x))

(define (cont-frac n d x k)
  (define (cont-rec i)
    (if (< i k)
        (/ (n i x) (- (d i) (cont-rec (+ i 1))))
        (/ (n i x) (- (d i) (/ (n i x) (d i))))))
  (cont-rec 1))

(define (d i)
  (- (* 2 i) 1))

(define (n i x)
  (if (= i 1)
      x
      (square x)))

(define (tan-cf x k)
  (cont-frac n d x k))

(tan-cf 0.785398163397448309615 100) ; ~1.0
(tan-cf 3.141592653589793238462 100) ; ~0.0
(tan-cf 0.244979 100) ; ~0.25
