#! /bin/racket
#lang sicp
(#%require "utils_dd_evalapply.rkt")
(#%require "utils_list.rkt")

(define (let-bindings exp)
  (cadr exp))

(define (let-params exp)
  (map car (let-bindings exp)))

(define (let-args exp)
  (map cadr (let-bindings exp)))

(define (let-body exp)
  (cddr exp))

;; set-up from ex_4-06.rkt
(define (let->combination exp)
  (let ((params (let-params exp))
        (args   (let-args exp))
        (body   (let-body exp)))
   (cons (make-lambda params body)) args))

(install! 'let (lambda (exp env) (eval (let->combination exp) env)))

; (let* ((x 3)
;        (y (+ x 2))
;        (z (+ x y 5)))
;   (* x z))
;
;; equivalent to
;
; ((lambda (x) ((lambda (y) ((lambda (z) (* x z)) (+ x y 5))) (+ x 2))) 3)

(define (extract-inner exp body)
  (cons (make-lambda (list (tail (let-params exp))) body) (list (tail (let-args exp)))))

(define (letreduce exp body)
  (list 'let* (all-but-tail (let-bindings exp)) body))

(define (let*->nested-lets exp)
  (define (let-iter exp)
    (if (null? (let-bindings exp))
        (car (let-body exp))
        (let-iter (letreduce exp (extract-inner exp (let-body exp))))))
  (let-iter exp))

(install! 'let* (lambda (exp env) (eval (let*->nested-lets exp) env)))

(driver-loop)

;; example works and it seems that in general, yes it is
;; sufficient to install the eval clause as above.
;;
;; there are limitations with forward referencing covered
;; by letrec but the exercise doesn't mention that here
;; and they may also be solvable via expression derivation
;; (though I'm not certain about that).
;;
;; REVISITING - I realise that my solution is more accurately described
;; as let*->nested-lambdas but really there isn't much benefit to using
;; nested lets as an intermediary derived expression so I'ĺl leave this
;; as it is..
