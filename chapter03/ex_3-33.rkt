#!/bin/racket
#lang sicp
(#%require "utils_constraint.rkt")

(define a (make-connector))
(define b (make-connector))
(define avg (make-connector))

(define (averager a b avg)
  (let ((i0 (make-connector))
        (i1 (make-connector)))
    (adder a b i0)
    (constant 0.5 i1)
    (multiplier i0 i1 avg)
    'ok))

(averager a b avg)

(probe "a" a)
(probe "b" b)
(probe "avg" avg)

;; forward average
(set-value! a 10 'user)
(set-value! b 20 'user)
;; returns avg = 15.0

;; get b from a and avg
(forget-value! a 'user)
(set-value! avg 30 'user)
;; returns a = 40.0
