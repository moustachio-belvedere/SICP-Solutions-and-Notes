#lang sicp

; modified from book to only print if prime is found

(define (square x)
  (* x x))

(define (smallest-divisor n)
  (if (divides? 2 n) 2
  (find-divisor n 3)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 2)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
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
(newline)
(search-for-primes 10000000001 10000000061)
(newline)
(search-for-primes 100000000001 100000000057)

; sqrt(10) is approx. 3. Indeed, average times increase by approximately a factor of 3 as we increase the search lower limit by a factor of 10.

; signifcant random fluctuation, probably due to processor allocation variability and cache access.
