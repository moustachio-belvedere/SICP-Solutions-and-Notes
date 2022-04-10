#! /bin/racket
#lang sicp
;; simple 1D table implementation from chapter 3
(#%require "../chapter03/utils_table.rkt")

(define special-forms (make-table))
(define (install! key val) (insert! key val special-forms))
(define (special-form exp)
  (if (pair? exp)
      (lookup (car exp) special-forms)))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ;; begin data-directed section
        ((special-form exp)
         ((special-form exp) exp env))
        ;; end data-directed section
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values
                 (operands exp)
                 env)))
        (else
         (error "Unknown expression
                 type: EVAL" exp))))

;; install special forms in table
(install! 'quote (lambda (exp env) (text-of-quotation exp)))
(install! 'set! eval-assignment)
(install! 'define eval-definition)
(install! 'if eval-if)
(install! 'lambda
          (lambda (exp env) (make-procedure (lambda-parameters exp)
                                            (lambda-body exp)
                                            env)))
(install! 'begin
          (lambda (exp env) (eval-sequence (begin-actions exp) env)))
(install! 'cond
          (lambda (exp env) (eval (cond-if exp) env)))

;; only a 1D table needed, compared to the 2D table required
;; for the data-directed differentiation function.
;;
;; also, some unused args in the lambdas, but that seems OK,
;; and the extra flexibility nice.
