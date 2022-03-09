#! /bin/racket
#lang sicp
(#%require "utils_stream.rkt")

(define (partial-sums stream)
  (cons-stream (stream-car stream)
               (add-streams (partial-sums stream)
                            (stream-cdr stream))))

(define s (partial-sums integers))

(stream-ref s  0)
(stream-ref s  1)
(stream-ref s  2)
(stream-ref s  3)
(stream-ref s  4)
(stream-ref s  5)
