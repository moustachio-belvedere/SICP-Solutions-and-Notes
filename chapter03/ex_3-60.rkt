#! /bin/racket
#lang sicp
(#%require "utils_stream.rkt")

;; not defined in sicp-lang scheme
(define pi 3.141592653589793)

;; utility func from ex 3.55
(define (partial-sums stream)
  (cons-stream (stream-car stream)
               (add-streams (partial-sums stream)
                            (stream-cdr stream))))

;; part A
(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (integrate-series stream)
  (div-streams stream integers))

;; part B
(define cosine-series
  (cons-stream 1
               (scale-stream (integrate-series sine-series)
                             -1)))

(define sine-series
  (cons-stream 0
               (integrate-series cosine-series)))

;; ex 3.60 continuation of ex 3.59
(define (mul-series s1 s2)
    (cons-stream (* (stream-car s1)
                    (stream-car s2))
                 (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                              (mul-series (stream-cdr s1) s2))))

;; testing
(define (make-exponent-stream x)
  (define x-stream (cons-stream x x-stream))
  (cons-stream 1 (stream-map expt x-stream integers)))

(define (streamed-sine x)
  (mul-streams (make-exponent-stream x) sine-series))

(define (streamed-cosine x)
  (mul-streams (make-exponent-stream x) cosine-series))

(define (s2 x) (mul-series (streamed-sine x) (streamed-sine x)))
(define (c2 x) (mul-series (streamed-cosine x) (streamed-cosine x)))
(define (added-streams x)
  (add-streams (s2 x) (c2 x)))

(define (check-equality x-arg n-terms)
  (define summed  (stream-ref (partial-sums (added-streams x-arg)) n-terms))
  (display summed)
  (newline))

(check-equality (/ pi 2) 20)
(check-equality (/ pi 4) 20)
(check-equality (/ pi 6) 20)
(check-equality (/ pi 8) 20)

;; this doesn't work for finite streams.
