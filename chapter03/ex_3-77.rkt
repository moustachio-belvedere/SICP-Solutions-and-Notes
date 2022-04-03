#! /bin/racket
#lang sicp
(#%require "utils_stream.rkt")
(#%require "utils_morestreams.rkt")
(#%require "utils_misc.rkt")

(define (integralv0
         delayed-integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (let ((integrand
            (force delayed-integrand)))
       (add-streams
        (scale-stream integrand dt)
        int))))
  int)

(define (integralv1
         delayed-integrand initial-value dt)
  (cons-stream
   initial-value
   (if (stream-null? (force delayed-integrand))
       the-empty-stream
       (integralv1
        (delay (stream-cdr (force delayed-integrand)))
        (+ (* dt (stream-car (force delayed-integrand)))
           initial-value)
        dt))))

(define (solvev0 f y0 dt)
  (newline) ;; why is this required for the function to work?
  ;; seems like a small bug in `delay` implementation
  (define y (integralv0 (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (solvev1 f y0 dt)
  (newline) ;; why is this required for the function to work?
  ;; seems like a small bug in `delay` implementation
  (define y (integralv1 (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solvev0 (lambda (y) y) 1 0.001) 1000)
(stream-ref (solvev1 (lambda (y) y) 1 0.001) 1000)
