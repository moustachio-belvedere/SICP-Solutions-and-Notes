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
       (display "here\n")
       (display exp)
       (display name)
       (display params)
       (display args)
       (display body)
       (display (list 'begin (list 'define (list name))))
       (display (list 'begin (list 'define (list name params) body) (cons name args)))
       (list 'begin (list 'define (list name params) body) (cons name args))))
       ;(list 'begin (list 'define name (make-lambda params body)) (cons name args))))
       ;(list 'begin (list 'define name 0) (list 'define name (make-lambda params body)) (cons name args))))
       ;(list 'let (list (list name (make-lambda (cons 'fnc params) body))) (cons name (cons name args)))))
       ;(list 'let (list (list name 0) (list name (make-lambda params body))) (cons name args))))

(define (let->combination exp)
  (if (is-named-let? exp)
      (namlet->combination exp)
      (stdlet->combination exp)))

(install! 'let (lambda (exp env) (eval (let->combination exp) env)))

(driver-loop)
