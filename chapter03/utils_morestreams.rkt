#! /bin/racket
#lang sicp
(#%require "utils_stream.rkt")
(#%provide partial-sums)

(define (partial-sums stream)
  (cons-stream (stream-car stream)
               (add-streams (partial-sums stream)
                            (stream-cdr stream))))
