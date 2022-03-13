#! /bin/racket
#lang sicp
(#%require "utils_stream.rkt")

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess)
                   (sqrt-improve guess x))
                  guesses)))
  guesses)

(define (stream-limit s tol)
  (let ((diff (- (stream-car s)
                 (stream-car (stream-cdr s)))))
    (if (< (abs diff) tol)
      (stream-car (stream-cdr s))
      (stream-limit (stream-cdr s) tol))))

(stream-limit (sqrt-stream 2) 1.0)
(stream-limit (sqrt-stream 2) 0.0001)
(stream-limit (sqrt-stream 2) 0.0000001)
