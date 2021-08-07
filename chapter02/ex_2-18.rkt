#lang sicp

(define (reverse lin)
  (define (list-traverse list-build lin)
    (if (null? (cdr lin))
        (cons (car lin) list-build)
        (list-traverse (cons (car lin) list-build) (cdr lin))))
  (list-traverse (cons (car lin) nil) (cdr lin)))

(define test-list (list 2 3 5 7 11 13 17 19 23))
(display test-list)
(newline)

(define reverse-list (reverse test-list))
(display reverse-list)
  

