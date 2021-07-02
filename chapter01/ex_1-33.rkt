#lang sicp

; utility functions
(define (inc x)
  (+ x 1))

(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

; fixed so as not to incorrectly identify 1 as a prime
(define (prime? n)
  (if (= n 1)
      #f
      (= n (smallest-divisor n))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; filtered accumulate
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (if (filter a)
            (iter (next a) (combiner (term a) result))
            (iter (next a) result))))
    (iter a null-value))

; sum of squared primes in interval [a, b]
(define (sum-sq-prime-interval a b)
  (filtered-accumulate prime? + 0 square a inc b))

(display "Sum of squared primes in stated interval:\n")
(display "[1, 9]: ")
(sum-sq-prime-interval 1 9)
(display "[7, 11]: ")
(sum-sq-prime-interval 7 11)
(newline)

; product of all positive integers i < n such that i, n are relatively prime
(define (prod-coprimes-lt-n n)
  (define (coprime? i)
    (= (gcd i n) 1))

  (filtered-accumulate coprime? * 1 (lambda (x) x) 1 inc n))

(display "Product of all positive integers i < n such that i,n are coprime\n")
(display "n = 5: ")
(prod-coprimes-lt-n 5) ; note, all numbers are coprime to a prime, so this correctly equals 4!
(display "n = 6: ")
(prod-coprimes-lt-n 6)
(newline)
