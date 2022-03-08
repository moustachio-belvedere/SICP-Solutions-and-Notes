#! /bin/racket
#lang sicp
(#%require "utils_stream.rkt")

;; (define s (cons-stream 1 (add-streams s s)))
;; 1, 2, 4, 8, 16...

(define s (cons-stream 1 (add-streams s s)))

;; test
(stream-ref s  0)
(stream-ref s  1)
(stream-ref s  2)
(stream-ref s  3)
(stream-ref s  4)
(stream-ref s  5)
(stream-ref s  6)
(stream-ref s  7)
(stream-ref s  8)
(stream-ref s  9)
(stream-ref s 10)
(stream-ref s 11)
(stream-ref s 12)
(stream-ref s 13)
(stream-ref s 14)
(stream-ref s 15)
(stream-ref s 16)
