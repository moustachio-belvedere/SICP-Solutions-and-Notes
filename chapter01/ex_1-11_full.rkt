#lang sicp

; full scaled version

; the function can be thought of as a 3 node extension of
; the fibonacci sequence, with scaled previous terms

; note modified recursive interpretation of standard fibonacci
; (define (fib n)
;   (cond ((< n 2) n)
;   (else (+ (fib (- n 1))
;            (fib (- n 2))))))

; (googled, it is probably better described as a scaled _tribonacci_ sequence)
; https://mathworld.wolfram.com/TribonacciNumber.html
; although initial base cases not the same

; recursive
(define (f n)
  (if (< n 3) n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(display "Recursive approach:\n")
(f 0)
(f 1)
(f 2)
(f 3)
(f 4)
(f 5)
(f 6)
(f 7)

; iterative
(define (g n)
  (g-iter 1/9 1/3 0 n))

(define (g-iter a b c count)
  (if (= count 0)
      c
      (g-iter b c (+ c (* 2 b)  (* 3 a)) (- count 1))))

(display "\nIterative approach:\n")
(g 0)
(g 1)
(g 2)
(g 3)
(g 4)
(g 5)
(g 6)
(g 7)
(display "\n")

; can also do this, but more LOC
(define (g-other n)
  (if (< n 3) n
      (g-iter 0 1 2 (- n 2))))

(display "\nIterative approach 2:\n")
(g-other 0)
(g-other 1)
(g-other 2)
(g-other 3)
(g-other 4)
(g-other 5)
(g-other 6)
(g-other 7)
(display "\n")
