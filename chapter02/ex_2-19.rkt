#lang sicp

(define (first-denomination lin)
  (car lin))

(define (except-first-denomination lin)
  (cdr lin))

(define (no-more? lin)
  (null? lin))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) 
             (no-more? coin-values)) 
         0)
        (else
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount (first-denomination
                           coin-values))
                coin-values)))))

(define us-coins (list 50 25 10 5 1))
;(define us-coins (list 1 5 10 25 50))
;(define us-coins (list 1 5 25 10 50))

(cc 84 us-coins)
(cc 88 us-coins)
(cc 92 us-coins)
(cc 96 us-coins)
(cc 100 us-coins)

; order of list does not affect ways of counting
; (which is correct)
