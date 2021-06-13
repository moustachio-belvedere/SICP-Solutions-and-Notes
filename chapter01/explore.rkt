#lang sicp

; simpler UNSCALED version below

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
      (+ (f (- n 1)) (* 1 (f (- n 2))) (* 1 (f (- n 3))))))

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
  (g-iter 0 1 0 n))

(define (g-iter a b c count)
  (if (= count 0)
      c
      (g-iter b c (+ a b c) (- count 1))))

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
