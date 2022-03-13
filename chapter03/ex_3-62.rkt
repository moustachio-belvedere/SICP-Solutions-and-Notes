#! /bin/racket
#lang sicp
(#%require "utils_stream.rkt")

;; solution
;; --------
(define (mul-series s1 s2)
    (cons-stream (* (stream-car s1)
                    (stream-car s2))
                 (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                              (mul-series (stream-cdr s1) s2))))

(define (get-X Sr)
  (cons-stream 1
               (scale-stream (mul-series (get-X Sr)
                                         (stream-cdr Sr))
                                 -1)))

(define (div-series num denom)
  (let ((init-denom (stream-car denom)))
    (if (= 0 init-denom)
        (error "division by 0 attempt")
        (mul-series num (scale-stream (get-X denom) (/ 1 init-denom))))))

;; tests
;; -----
(define (partial-sums stream)
  (cons-stream (stream-car stream)
               (add-streams (partial-sums stream)
                            (stream-cdr stream))))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (integrate-series stream)
  (div-streams stream integers))

(define cosine-series
  (cons-stream 1
               (scale-stream (integrate-series sine-series)
                             -1)))

(define sine-series
  (cons-stream 0
               (integrate-series cosine-series)))

(define (make-exponent-stream x)
  (define x-stream (cons-stream x x-stream))
  (cons-stream 1 (stream-map expt x-stream integers)))
  ;(stream-map expt x-stream integers))

(define tan-series (div-series sine-series cosine-series))

(define (streamed-tan x)
  (mul-streams (make-exponent-stream x) tan-series))

(define (stan x terms)
  (stream-ref (partial-sums (streamed-tan x)) terms))

(define (square x) (* x x))
(define pi 3.141592653589793)
(display "stan (/ pi 4): ")
(display (square (sqrt (stan  (/ pi 4) 100))))
(newline)
(display "tan (/ pi 4): ")
(display (tan  (/ pi 4)))
(newline)
(display "stan (/ pi 8): ")
(display (square (sqrt (stan  (/ pi 8) 100))))
(newline)
(display "tan (/ pi 8): ")
(display (tan  (/ pi 8)))
(newline)
;; results are pretty close :)
