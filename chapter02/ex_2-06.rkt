#lang sicp

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

((zero 0) 1)
((zero 0) 2)
((zero 0) 3)
((zero 0) 4)
