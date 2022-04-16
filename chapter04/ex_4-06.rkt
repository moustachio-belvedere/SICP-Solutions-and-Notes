#! /bin/racket
#lang sicp
(#%require "utils_dd_evalapply.rkt")

;; prep for exercise 4.8
(define (is-named-let? exp)
  (symbol? (cadr exp)))

(define (let->combination exp)
  (let ((params (map car (cadr exp)))
        (args   (map cadr (cadr exp)))
        (body   (cddr exp)))
   (append (list (make-lambda params body)) args)))

(install! 'let (lambda (exp env) (eval (let->combination exp) env)))

(driver-loop)
;;;; M-Eval input:
;(let ((a 5) (b 7)) (+ a b))
;
;;;; M-Eval value:
;12
