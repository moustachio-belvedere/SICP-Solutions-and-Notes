#lang sicp
(#%require "utils_getput.rkt")
(#%require "utils_symdiff.rkt")

;; can't assimilate `number?` and `variable?`
;; as they don't have an operator to dispatch
;; on.

(put 'deriv '+
     (lambda (ops var)
        (display "reached sum-deriv\n")
        (make-sum (deriv (car ops) var)
                  (deriv (cadr ops) var))))

(put 'deriv '*
     (lambda (ops var)
        (make-sum (make-product (car ops) 
                                (deriv (cadr ops) var))
                  (make-product (deriv (car ops) var)
                                (cadr ops)))))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? b 0) 0)
        ((=number? e 1) b)
        ((and (number? b) (number? e))
         (expt b e))
        (else (list `** b e))))

(put 'deriv '**
     (lambda (ops var)
       (let ((base (car ops)) (exponent (cadr ops)))
       (make-product exponent
                     (make-product (make-exponentiation base
                                                        (- exponent 1))
                                   (deriv base var))))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) 
           (if (same-variable? exp var) 
               1 
               0))
         (else ((get 'deriv (operator exp)) 
                (operands exp) 
                var))))

(define u (make-sum (make-product 'x 2) 1))
(define un (make-exponentiation u 2))
(deriv un 'x)

;; if changing arond order of get indexing,
;; we just have to install the procedures with
;; respect to the new order.
