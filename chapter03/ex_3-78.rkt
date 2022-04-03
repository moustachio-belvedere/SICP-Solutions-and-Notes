#! /bin/racket
#lang sicp
(#%require "utils_stream.rkt")
(#%require "utils_morestreams.rkt")
(#%require "utils_misc.rkt")

(define (integral
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

(define (solve a b y0 dy0 dt)
  (newline) ;; why is this required for the function to work?
  ;; seems like a small bug in `delay` implementation
  (define   y (integral (delay  dy)   y0 dt))
  (define  dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream  y b)))
  y)

(stream-ref (solve 1 1 1 1 0.001) 1000)
