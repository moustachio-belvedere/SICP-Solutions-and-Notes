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

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 20)
      (report-prime n (- (runtime) 
                       start-time))))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes lo hi)
  (timed-prime-test lo)
  (prime-search-iter (+ lo 2) hi))

(define (prime-search-iter x hi)
  (if (>= x (+ hi 1))
      (display "")
      (search-for-primes x hi)))

; even my 5+ year old laptop can handle much bigger prime searches here
(search-for-primes 1001 1019)
(newline)
(search-for-primes 10001 10037)
(newline)
(search-for-primes 100001 100043)
(newline)
(search-for-primes 1000001 1000037)
(newline)
(search-for-primes 10000001 10000103)
(newline)
(search-for-primes 100000001 100000039)
(newline)
(search-for-primes 1000000001 1000000021)

; higher numbers crash at random num generation stage

; base load is higher than regular prime? function due to all the overhead of getting random numbers and the various other calculations but rate of increase does look logarithmic. At larger numbers, fast-prime? easily beats prime?.
