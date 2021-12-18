#!/bin/racket
#lang sicp

(define (_car z)
  (z 'car))

(define (_cdr z)
  (z 'cdr))

(define (_cons x y)
  (lambda (m)
     (cond ((eq? m 'car) x)
           ((eq? m 'cdr) y))))

(define a (_cons 1 2))
(_car a)
(_cdr a)

(define (_set-car! x y)
  (_cons y (_cdr x)))

(define (_set-cdr! x y)
  (_cons (_car x) y))

(define newa (_set-car! a 5))
(_car newa)

(define newera (_set-cdr! newa 7))
(_cdr newera)

;; not true mutability

