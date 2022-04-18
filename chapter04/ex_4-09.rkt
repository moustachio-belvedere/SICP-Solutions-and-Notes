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
        (cons (make-lambda params
                           (cons (cons 'define
                                       (cons (cons name params) body))
                           (list (cons name params))))
              args)))

(define (let->combination exp)
  (if (is-named-let? exp)
      (namlet->combination exp)
      (stdlet->combination exp)))

(install! 'let (lambda (exp env) (eval (let->combination exp) env)))

;; PoC (without dotted args as our Scheme doesn't support this yet)
; (define (stop= n)
;   (lambda (x) (= x n)))
;
; (define (inc x) (+ x 1))
;
; (define (forz i-i finish? next func i-args)
;   (let iter ((i i-i) (args i-args))
;     (if (finish? i)
;         args
;         (iter (next i) (apply func args)))))
;
; (define (tfnk x y)
;   (list (+ x y) x))
;
; ;; fibonacci
; (car (forz 0 (stop= 10) inc tfnk (list 0 1)))
(define (finit exp) (cadr exp))     ;; initial i
(define (fendc exp) (caddr exp))    ;; end condition lambda
(define (fnext exp) (cadddr exp))
(define (ffunc exp) (cadr (cdddr exp)))
(define (fiarg exp) (caddr (cdddr exp)))

(define (for->tailiter exp)
  (let ((i-i (finit exp))
        (finish? (fendc exp))
        (next (fnext exp))
        (func (ffunc exp))
        (args (fiarg exp)))
    (list 'let 'iter (list (list 'i i-i) (list 'args args))
          (make-if (list finish? 'i)
                   'args
                   (list 'iter (list next 'i) (list 'apply func 'args))))))

(install! 'for (lambda (exp env) (eval (for->tailiter exp) env)))

;; Usage examples:
; (begin (define tfnk (lambda (x) (display "\n")(display x) (list (+ x 1)))) (for 0 (lambda (x) (= x 5)) (lambda (x) (+ x 1)) tfnk (list 0)))
; (begin (define (stop= n) (lambda (x) (= x n))) (define (inc x) (+ x 1)) (define (tfnk x y) (list (+ x y) x)) (for 0 (stop= 10) inc tfnk (list 0 1)))

(driver-loop)
