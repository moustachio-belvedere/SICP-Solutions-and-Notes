#! /bin/racket
#lang sicp
(#%require "utils_stream.rkt")

(define (sqrt-improve guess x)
  (display "Called\n")
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0 (stream-map
          (lambda (guess)
            (sqrt-improve guess x))
          guesses)))
  guesses)

(stream-ref (sqrt-stream 2) 5)

;; Only called once per stream-cdr due to memoization!
