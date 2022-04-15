#! /bin/racket
#lang sicp
(#%require "utils_dd_evalapply.rkt")

(define (let*->combination exp)
  (let ((params (map car (cadr exp)))
        (args   (map cadr (cadr exp)))
        (body   (cddr exp)))
   (append (list (make-lambda params body)) args)))

(install! 'let* (lambda (exp env) (eval (let*->combination exp) env)))

;(driver-loop)
