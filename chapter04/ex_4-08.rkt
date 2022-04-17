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

;; named version is nice, but pollutes namespace
; (define (namlet->combination exp)
;   (let ((name   (cadr exp))
;         (params (map car (caddr exp)))
;         (args   (map cadr (caddr exp)))
;         (body   (cdddr exp)))
;         (sequence->exp (list (append (list 'define (cons name params)) body) (cons name args)))))

(define (namlet->combination exp)
  (let ((name   (cadr exp))
        (params (map car (caddr exp)))
        (args   (map cadr (caddr exp)))
        (body   (cdddr exp)))
        (cons (make-lambda params
                           (cons (cons 'define
                                       (cons (cons name params) body))
                           (list (cons name params))))
              args)))

; test case:
; (let nl ((x 3)) (if (= x 1) x (nl (- x 1))))
; 1

(define (let->combination exp)
  (if (is-named-let? exp)
      (begin (display (namlet->combination exp)) (newline)
             (namlet->combination exp))
      (stdlet->combination exp)))

(install! 'let (lambda (exp env) (eval (let->combination exp) env)))

(driver-loop)
