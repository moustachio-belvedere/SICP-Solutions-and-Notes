#lang sicp

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (inc x)
  (+ x 1))

(define (square x)
  (* x x))

(define (repeated funco n)
  (define (rep-rec funci n)
    (if (= n 1)
        funci
        (rep-rec (compose funco funci) (- n 1))))
  (rep-rec funco n))

((repeated square 2) 5)
((repeated inc 9) 0)
