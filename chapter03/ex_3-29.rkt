#!/bin/racket
#lang sicp

;; from De Morgan's laws
;; (or x y) == (not (and (not x) (not y)))
;; which is nicely visualised as truth tables.

;; for wire labels:
;; --o1--> not --a--> | |
;;                      and --c--> not --output-->
;; --o2--> not --b--> | |
(define (or-gate o1 o2 output)
  (let ((a (make-wire))
        (b (make-wire))
        (c (make-wire)))
    (inverter o1 a)
    (inverter o2 b)
    (and-gate a b c)
    (inverter c output)))

;; assuming the first two inversions can happen concurrently,
;; the total delay is 2*(inverter-delay) + and-delay.
;; If cannot happen concurrently, then the delay is
;; 3*(inverter-delay) + and-delay.


