#! /bin/racket
#lang sicp

;; see ex_plore_annonymousrecursion.rkt file
;; for different annonymous implementations
;; of the recursive factorial algorithm.

;; Fibonnaci
((lambda (n)
   ((lambda (f) (f f n))
    (lambda (fib k)
      (if (<= k 1)
          1
          (+ (fib fib (- k 1)) (fib fib (- k 2)))))))
 5)

;; even odd mutual annonymous recursion
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0)
         true
         (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0)
         false
         (ev? ev? od? (- n 1))))))

(f 4) ;; #t
(f 5) ;; #f


