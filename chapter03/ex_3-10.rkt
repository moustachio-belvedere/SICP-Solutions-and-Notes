#!/bin/racket
#lang sicp

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds")))

(define (make-withdraw2 initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance 
                       (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw3 initial-amount)
  ((lambda (balance)
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance 
                       (- balance amount))
                 balance)
          "Insufficient funds"))) initial-amount))

;; make-withdraw2 is syntactic sugar for make-withdraw3
;; and both are distinct from make-withdraw from the
;; perspective of the environmental model of evaluation.

;; functionally, the behaviour of all three is the same.

(define a (make-withdraw 25))
(define b (make-withdraw2 33))
(define c (make-withdraw3 37))

(a 5)
(b 5)
(a 5)
(c 5)
(b 5)
(a 5)
