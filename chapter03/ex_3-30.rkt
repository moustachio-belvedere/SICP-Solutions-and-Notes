#!/bin/racket
#lang sicp

;; undefined where the final redundant carry bit should go
;; so ignoring for now.
(define (ripple-carry-adder a b s c-in)
  (let ((c-out (make-wire)))
       (full-adder (car a) (car b) c-in (car s) c-out)
       (if (null? (cdr a))
           'ok
           (ripple-carry-adder (cdr a) (cdr b) (cdr s) c-out))))

;; or alternatively

(define (ripple-carry-adder a b s c-in)
  (if (null? a)
    'ok
    (let ((c-out (make-wire)))
         (full-adder (car a) (car b) c-in (car s) c-out)
         (ripple-carry-adder (cdr a) (cdr b) (cdr s) c-out))))

;; assuming concurrecy where reasonable
;; delay of half adder: (max (+ and-delay inverter-delay) or-delay) + and-delay
;; delay of full adder: (+ (* 2 half-adder-delay) or-delay) ==
;; (+ (* 2 (+ (max (+ and-delay inverter-delay) or-delay) and-delay)) or-delay)
;; for n-bit ripple, total delay is n multiplied by the line above.
