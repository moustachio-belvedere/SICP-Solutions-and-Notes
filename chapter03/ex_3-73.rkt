#! /bin/racket
#lang sicp
(#%require "utils_stream.rkt")
(#%require "utils_morestreams.rkt")
(#%require "utils_misc.rkt")

; assume x0, x1 etc are integrand points scaled by dt
; (C,  (x0,  x1,  x2...)
;     +(C , (x0,  x1,  x2...)
;          +(C , (x0, x1, x2...)
;               +(C ...
;
; summing along columns provides a nice way of
; visualising how the integral is produced.
(define (integral integrand initial-value dt)
  (define int
    (cons-stream 
      initial-value
        (add-streams (scale-stream integrand dt)
                     int)))
  int)


(define (RC R C dt)
  (lambda (i v0)
    (add-streams (scale-stream i R)
                 (integral (scale-stream i (/ 1 C)) v0 dt))))

(define rc1 (RC 5 1 0.5))
(define rc1v (rc1 ones 0))
(display-n-stream rc1v 10)
    
