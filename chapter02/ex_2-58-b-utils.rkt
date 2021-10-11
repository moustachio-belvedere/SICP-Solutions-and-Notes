#lang sicp
(#%provide popped
           is-sum-operator?
           is-prod-operator?)

(define (popped x)
  (define (x-iter acc xi)
    (if (null? (cdr xi))
      acc
      (x-iter (append acc (list (car xi)))
              (cdr xi))))
  (x-iter `() x))

(define (is-sum-operator? x)
  (eq? x `+))

(define (is-prod-operator? x)
  (eq? x `*))
