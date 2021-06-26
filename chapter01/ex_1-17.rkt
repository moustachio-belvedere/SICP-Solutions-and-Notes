#lang sicp

(define (even? x)
  (= (/ x 2) 0))

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (norm-mult a b)
  (if (= b 0)
      0
      (+ a (norm-mult a (- b 1)))))

(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-mult (double a) (halve b)))
        (else (+ a (fast-mult a (- b 1))))))

(display "\n2*3 -> 2*8\n")
(fast-mult 2 3)
(fast-mult 2 4)
(fast-mult 2 5)
(fast-mult 2 6)
(fast-mult 2 7)
(fast-mult 2 8)

(display "\n5*3 -> 5*8\n")
(fast-mult 5 3)
(fast-mult 5 4)
(fast-mult 5 5)
(fast-mult 5 6)
(fast-mult 5 7)
(fast-mult 5 8)

(display "\n7*3 -> 7*8\n")
(fast-mult 7 3)
(fast-mult 7 4)
(fast-mult 7 5)
(fast-mult 7 6)
(fast-mult 7 7)
(fast-mult 7 8)
