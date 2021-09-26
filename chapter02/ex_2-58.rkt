#lang sicp
(#%require "utils_symdiff.rkt")
(#%require "ex_2-58_utils.rkt")

(define (sum? x)
  (and (pair? x) (eq? (cadr x) `+)))

(define (addend s) (car s))
(define (augend s) (caddr s))

(define x `(5 + 3))
(addend x)
(augend x)

