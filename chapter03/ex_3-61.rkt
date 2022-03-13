#! /bin/racket
#lang sicp
(#%require "utils_stream.rkt")

(define (mul-series s1 s2)
    (cons-stream (* (stream-car s1)
                    (stream-car s2))
                 (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                              (mul-series (stream-cdr s1) s2))))

(define (get-X Sr)
  (cons-stream 1
               (scale-stream (mul-series (get-X Sr) Sr)
                             -1)))
