#lang sicp

(define (reverse items)
  (define (reverse-iter items acc)
    (if (null? items)
        acc
        (reverse-iter (cdr items) (cons (car items) acc))))
  (reverse-iter items nil))

(define (deep-reverse items)
  (let ((revi (reverse items)))
    (

;(define x (list (list 1 2 (list 8 9)) (list 3 4) (list 5 6) 7))
(define x (list (list 1 2) (list 3 4)))
(deep-reverse x)
