#lang sicp

(define (make-interval a b) (cons a b))

(define (lower-bound i)
  (min (car i)
       (cdr i)))

(define (upper-bound i)
  (max (car i)
       (cdr i)))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))
