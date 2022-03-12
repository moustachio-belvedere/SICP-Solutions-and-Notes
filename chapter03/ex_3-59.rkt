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

;; some testing functions
(define (make-exponent-stream x)
  (define x-stream (cons-stream x x-stream))
  (cons-stream 1 (stream-map expt x-stream integers)))

(define (streamed-trig operation-stream radians terms-to-use)
  (stream-ref (partial-sums (mul-streams (make-exponent-stream radians)
                                         operation-stream))
              terms-to-use))

(define (streamed-sine radians terms)
  (streamed-trig sine-series radians terms))

(streamed-sine (* pi 0.5) 100)
(streamed-sine (* pi 2.0) 100)

(define (streamed-cosine radians terms)
  (streamed-trig cosine-series radians terms))

(streamed-cosine (* pi 0.5) 100)
(streamed-cosine (* pi 2.0) 100)
