#lang sicp

(define (last-pair lin)
  (if (null? (cdr lin))
      (car lin)
      (last-pair (cdr lin))))

(define test-list (list 2 3 5 7 11 13 17 19 23))

(last-pair test-list)
