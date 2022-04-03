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

(define (RLC R L C dt)
  (lambda (vc0 il0)
    (newline) ;; why is this required for the function to work?
    ;; seems like a small bug in `delay` implementation
    (define  il (integral (delay dil) il0 dt))
    (define  vc (integral (delay dvc) vc0 dt))
    (define dvc (scale-stream il (/ -1 C)))
    (define dil (add-streams (scale-stream vc (/ 1 L))
                             (scale-stream il (/ (- R) L))))
    (cons vc il)))

(define streamz ((RLC 1 1 0.2 0.1) 10 0))
(stream-ref (car streamz) 100)
(stream-ref (cdr streamz) 100)
