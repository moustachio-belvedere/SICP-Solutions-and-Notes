#lang sicp
(#%require "utils_sets.rkt")

(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((element-of-set? (car s1) s2)
            (union-set (cdr s1) s2))
        (else (union-set (cdr s1) (cons (car s1) s2)))))

(define x (list `x `y `z 2 3 5))
(define y (list `a `b `c 2 7))

(union-set x y)
