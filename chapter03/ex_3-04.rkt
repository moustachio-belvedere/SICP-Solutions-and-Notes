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

  (define (call-the-cops)
    (error "ERROR: the COPS have been called."))

  (define bad-pswd-counter 0)

  (define (incorrect-pswd _)
    (if (< bad-pswd-counter 7)
        (begin (set! bad-pswd-counter (+ bad-pswd-counter 1))
               "Incorrect password")
        (call-the-cops)))

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
((acc0 'hello123 'withdraw) 20)
((acc0 'bye123 'withdraw) 40)
((acc0 'bye123 'withdraw) 40)
((acc0 'bye123 'withdraw) 40)
((acc0 'bye123 'withdraw) 40)
((acc0 'bye123 'withdraw) 40)
((acc0 'bye123 'withdraw) 40)
((acc0 'bye123 'withdraw) 40)
((acc0 'bye123 'withdraw) 40) ;; triggers error as expected
