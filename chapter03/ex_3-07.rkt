#!/bin/racket
#lang sicp

(define (incorrect-pswd _)
  "Incorrect password")

(define (make-account pswd-set balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch pswd-given m)
    (if (eq? pswd-given pswd-set)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: 
                     MAKE-ACCOUNT" m)))
        incorrect-pswd))
  dispatch)

(define (make-joint acc orig-pass new-pass)
  (lambda (pswd-given m)
          (if (eq? pswd-given new-pass)
              (acc orig-pass m)
              incorrect-pswd)))

(define peter-acc (make-account 'open-sesame 100))

((peter-acc 'open-sesame 'withdraw) 50)

(define paul-acc (make-joint peter-acc
                             'open-sesame
                             'rosebud))

((paul-acc 'rosebud 'withdraw) 25)
