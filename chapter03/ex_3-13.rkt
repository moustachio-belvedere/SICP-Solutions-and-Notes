#!/bin/racket
#lang sicp

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))
(display z) ;; #0=(a b c . #0#) ;; what does the hashed number indicate? Pointer?
