#!/bin/racket
#lang sicp

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;; following logic round for the simple list '(a b c d)
;; reveals it is a list reversal (with side effects...)

(define x (mystery '(a b c d)))
(display x)
(newline) ;; (d c b a)

;; see ex_3-14.png for side effects demo and for
;; programatic demo:

(define y '(a b c d))
(mystery y)
(display y)
(newline) ;; the only explicit mutation of y
          ;; is in the first loop iteration,
          ;; where y -> (set-cdr! y '())
          ;; which is equivalent to '(a)
          ;; or (car y) in the general case.
