#lang sicp

(define (cube x)
  (* x x x))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
    (iter a 0))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum-iter f (+ a (/ dx 2.0)) add-dx b) 
     dx))

(display "Regular numerical integration:\n")
(integral cube 0 1 0.01)
(integral cube 0 1 0.001)
