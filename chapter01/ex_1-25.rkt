#lang sicp

; modified from book to only print if prime is found

(define (square x)
  (* x x))

(define (even? x)
  (= (remainder x 2) 0))

(define (expmod base exparg m)
  (cond ((= exparg 0) 1)
        ((even? exparg)
         (remainder 
          (square (expmod base (/ exparg 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exparg 1) m))
          m))))

(define (fast-expt b n)
  (cond ((= n 0) 
         1)
        ((even? n) 
         (square (fast-expt b (/ n 2))))
        (else 
         (* b (fast-expt b (- n 1))))))

(define (explar base exp m)
  (remainder (fast-expt base exp) m))

(define (fermat-test n wexp)
  (define (try-it a wexp)
    (if wexp
    (= (expmod a n n) a)
    (= (explar a n n) a)))
  (try-it (+ 1 (random (- n 1))) wexp))

(define (fast-prime? n times wexp)
  (cond ((= times 0) true)
        ((fermat-test n wexp) 
         (fast-prime? n (- times 1) wexp))
        (else false)))

(define (timed-prime-test n wexp)
  (start-prime-test n (runtime) wexp))

(define (start-prime-test n start-time wexp)
  (if (fast-prime? n 20 wexp)
      (report-prime n (- (runtime) 
                       start-time))))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes lo hi wexp)
  (timed-prime-test lo wexp)
  (prime-search-iter (+ lo 2) hi wexp))

(define (prime-search-iter x hi wexp)
  (if (>= x (+ hi 1))
      (display "")
      (search-for-primes x hi wexp)))

; use #t to use fast modulo-based exp, and #f to use slow exp that has to compute with massive numbers
(search-for-primes 10001 10043 #t)
(newline)
(search-for-primes 10001 10043 #f)

; definitely incorrect. Using integers larger than regular ALU width incurs massive computational overhead. This is overheaded if taking modulus at every iteration of the exponentiation process.
