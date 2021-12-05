#!/bin/racket
#lang sicp

(define (get-f)
  (let ((state 0))
    (lambda (x)
      (cond ((= x 0)
             state)
            ((= x 1)
             (set! state (+ state 1))
             0)))))

(define f (get-f))
(+ (f 0) (f 1))

(define g (get-f))
(+ (g 1) (g 0))

;; so subexpressions are evaluated left -> right
