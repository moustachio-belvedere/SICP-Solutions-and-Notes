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

(define (fermat-test n nm)
  (= (expmod nm n n) nm))

(define (carmichael-test n nm)
  (cond ((= nm 0) true)
        ((fermat-test n nm) 
         (carmichael-test n (- nm 1)))
        (else false)))

(define (carmichael-init n)
  (carmichael-test n (- n 1)))

;; check on some known primes/non-primes, works as expected
;(carmichael-init 5)
;(carmichael-init 7)
;(carmichael-init 11)
;(carmichael-init 13)
;(carmichael-init 17)
;(carmichael-init 18)
;(carmichael-init 19)
;(carmichael-init 20)
;(carmichael-init 23)
;(carmichael-init 27)
;(carmichael-init 29)

; test on some Carmichael numbers
(carmichael-init 561)
(carmichael-init 1105)
(carmichael-init 1729)
(carmichael-init 2465)
(carmichael-init 2821)
(carmichael-init 6601)

; All 'pass' the test but are not primes!
