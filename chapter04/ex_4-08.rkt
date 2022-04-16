#! /bin/racket
#lang sicp
(#%require "utils_dd_evalapply.rkt")

(define (is-named-let? exp)
  (symbol? (cadr exp)))

(define (stdlet->combination exp)
  (let ((params (map car (cadr exp)))
        (args   (map cadr (cadr exp)))
        (body   (cddr exp)))
   (append (list (make-lambda params body)) args)))

(define (namlet->combination exp)
  (let ((name   (cadr exp))
        (params (map car (caddr exp)))
        (args   (map cadr (caddr exp)))
        (body   (cdddr exp)))
        (sequence->exp (list (append (list 'define (cons name params)) body) (cons name args)))))

; (let nl ((x 3)) (if (= x 1) x (nl (- x 1))))
; 1

(define (let->combination exp)
  (if (is-named-let? exp)
      (namlet->combination exp)
      (stdlet->combination exp)))

(install! 'let (lambda (exp env) (eval (let->combination exp) env)))

(driver-loop)
