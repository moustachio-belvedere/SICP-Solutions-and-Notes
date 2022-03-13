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
     1.0
     (stream-map (lambda (guess)
                   (sqrt-improve guess x))
                  guesses)))
  guesses)

(stream-ref (sqrt-stream 2) 5)
(newline)

(define (sqrt-stream2 x)
  (cons-stream
   1.0
   (stream-map (lambda (guess)
                 (sqrt-improve guess x))
               (sqrt-stream2 x))))

(stream-ref (sqrt-stream2 2) 5)

;; Only called once per stream-cdr due to memoization!
;; the repeated `stream-map` generates a 'tower'
;; of delayed evaluations of `sqrt-improve`. However,
;; with memoization, only the top of the tower needs
;; to actually be evaluated at each call to `stream-cdr`.

;; without memoization, the number of calls required
;; would grow quadratically and would be the same for
;; both Louis' and Alyssa's implementation.

;; is the memoization circumvented in Louis' method
;; because each map creates a new stream which doesn't
;; know about the 'memory' of the parent stream?

;; would be nice to have some a idea of a more 'global'
;; memory that all streams within an environment could
;; have access to.
