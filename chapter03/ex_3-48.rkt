;; the sorting of accounts, such that
;; the account with the lower id is 
;; always used to serialize first,
;; constitutes a barrier of sorts.

;; no subsequent exchange call can break the
;; barrier of the first serialization
;; once it has been initiated.

(define (make-account-and-serializer balance)

  (define account-id (rand))

  (define (withdraw amount)
    (if (>= balance amount)
        (begin 
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer 
         (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer)
             balance-serializer)
            ((eq? m 'get-id) account-id)
            (else (error "Unknown request: 
                          MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (serialized-exchange account1 account2)
  (if (< (account1 'get-id)
         (account2 'get-id))
    (begin (define a-low account1)
           (define a-hi  account2))
    (begin (define a-low account2)
           (define a-hi  account1)))
  (let ((serializer1 (a-low 'serializer))
        (serializer2 (a-hi  'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))
