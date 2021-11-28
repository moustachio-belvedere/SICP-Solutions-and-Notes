#lang sicp

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

  (define (incorrect-pswd _)
    "Incorrect password")

  (define (dispatch pswd-given m)
    (if (eq? pswd-given pswd-set)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: 
                     MAKE-ACCOUNT" m)))
        incorrect-pswd))
  dispatch)

(define acc0 (make-account 'hello123 100))

((acc0 'hello123 'withdraw) 40)
((acc0 'bye123 'withdraw) 40)
((acc0 'hello123 'withdraw) 20)
