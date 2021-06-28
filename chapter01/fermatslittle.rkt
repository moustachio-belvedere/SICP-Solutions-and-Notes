#lang sicp

(define (even? x)
  (= (remainder x 2) 0))

(define (square x)
  (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 7 5)
(fast-prime? 8 5)
(fast-prime? 9 5)
(fast-prime? 10 5)
(fast-prime? 11 5)
(fast-prime? 12 5)
(fast-prime? 13 5)
(fast-prime? 14 5)
(fast-prime? 15 5)
(fast-prime? 16 5)
(fast-prime? 17 5)
