#lang sicp

(define withdrawV0
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))))

(define (withdrawV2 balance set-show)
  (let ((balance balance))
    (define (withdraw amount)
      (if (>= balance amount)
        (begin (

(withdrawV0 5)
(withdrawV0 5)

(withdrawV1 5)
(withdrawV1 5)
