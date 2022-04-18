#! /bin/racket
#lang sicp

;; regular named recursion
(define (fact x)
  (if (<= x 1)
      1
      (* x (fact (- x 1)))))

(fact 5)

;; function handle passsing
;; requires a different call signature
;; (is this a U Combinator?)
((lambda (f x)
  (f f x))
  (lambda (f x)
    (if (<= x 1)
        1
        (* x (f f (- x 1)))))
  5)

;; a variation on the above
;; is nice as function call
;; signature can remain the same
((lambda (f x)
  (f f x))
  (lambda (f x)
    (let ((fct (lambda (x) (f f x))))
      (if (<= x 1)
          1
          (* x (fct (- x 1))))))
  5)

;; which is syntactic sugar for
((lambda (f x)
  (f f x))
  (lambda (f x)
    ((lambda (fct)
      (if (<= x 1)
          1
          (* x (fct (- x 1)))))
     (lambda (x) (f f x))))
  5)
