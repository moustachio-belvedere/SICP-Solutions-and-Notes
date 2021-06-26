#lang sicp

(define (even? x)
  (= (/ x 2) 0))

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (iter-mult a b s)
  (cond ((= b 0) s)
        ((even? b) (iter-mult (double a) (halve b) s))
        (else (iter-mult a (- b 1) (+ s a)))))

(display "\n2*3 -> 2*8\n")
(iter-mult 2 3 0)
(iter-mult 2 4 0)
(iter-mult 2 5 0)
(iter-mult 2 6 0)
(iter-mult 2 7 0)
(iter-mult 2 8 0)

(display "\n5*3 -> 5*8\n")
(iter-mult 5 3 0)
(iter-mult 5 4 0)
(iter-mult 5 5 0)
(iter-mult 5 6 0)
(iter-mult 5 7 0)
(iter-mult 5 8 0)

(display "\n7*3 -> 7*8\n")
(iter-mult 7 3 0)
(iter-mult 7 4 0)
(iter-mult 7 5 0)
(iter-mult 7 6 0)
(iter-mult 7 7 0)
(iter-mult 7 8 0)
