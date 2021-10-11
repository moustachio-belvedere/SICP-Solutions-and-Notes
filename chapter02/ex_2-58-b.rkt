#lang sicp
(#%require "utils_symdiff.rkt")

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        (else (error "unknown expression 
                      type: DERIV" exp))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) `+)))

(define (addend s) (car s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) `*)))

(define (multiplier m) (car m))
(define (multiplicand m) (caddr m))

(define tester `(x + (3 * (x + (y + 2)))))
(deriv tester `x)
