#lang sicp

(define (for-each-u2 func items)
  (define (inner items)
    (func (car items))
    (for-each-u2 func (cdr items)))
  (cond ((not (null? items))
       (inner items))))

(define (nice-printer x)
  (display "-- ")
  (display x)
  (display " --")
  (newline))

(define x (list 2 3 5 7 11 13 17 19 23))

(for-each-u2 nice-printer x)
