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

(define (let->combination exp)
  (if (is-named-let? exp)
      (namlet->combination exp)
      (stdlet->combination exp)))

(install! 'let (lambda (exp env) (eval (let->combination exp) env)))

;; list of:
;;  - variables to iterate on, initial conditions
;;  - incrementer functions, one per var above
;;  - finishing conditions as lamda funcs
;;
;; function passed to loop over needs two args
;;  - list of iterated args, incremented by above funcs
;;  - state to be passed on at each iteration
;;
;;
;; simpler, would be nice to be able to transform
;;
;;
; (define (fib n)
;   (let fib-iter ((a 1) (b 0) (count n))
;     (if (= count 0)
;         b
;         (fib-iter (+ a b)
;                   a
;                   (- count 1)))))
;;
;; to this:
; (for n fib-iter 1 0)
; where fib-iter:
; (let fib-iter ((a 1) (b 0) (count n))
;     (if (= count 0)
;         b
;         (fib-iter (+ a b)
;                   a
;                   (- count 1)))))
(driver-loop)
