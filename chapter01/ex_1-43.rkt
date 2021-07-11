#lang sicp

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (inc x)
  (+ x 1))

(define (square x)
  (* x x))

; tail call recursive
(define (repeated funco n)
  (define (rep-iter funci n)
    (if (= n 1)
        funci
        (rep-iter (compose funco funci) (- n 1))))
  (rep-iter funco n))

((repeated square 2) 5)
((repeated inc 9) 0)
