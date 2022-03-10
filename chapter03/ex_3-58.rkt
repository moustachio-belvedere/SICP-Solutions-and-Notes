#! /bin/racket
#lang sicp
(#%require "utils_stream.rkt")

;; this is essentially a stream-lined
;; implementation of the long division
;; algorithm.

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) 
           den 
           radix)))

(define S (expand 1 7 10))

(stream-ref S  0)
(stream-ref S  1)
(stream-ref S  2)
(stream-ref S  3)
(stream-ref S  4)
(stream-ref S  5)
(stream-ref S  6)
(stream-ref S  7)
(stream-ref S  8)
(stream-ref S  9)
(stream-ref S 10)
(stream-ref S 11)

(newline)
(define T (expand 3 8 10))

(stream-ref T  0)
(stream-ref T  1)
(stream-ref T  2)
(stream-ref T  3)
(stream-ref T  4)
