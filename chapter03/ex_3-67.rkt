#! /bin/racket
#lang sicp
(#%require "utils_stream.rkt")
(#%require "utils_morestreams.rkt")

(define (more-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
    (interleave
     (more-pairs (stream-cdr s) (stream-cdr t))
     (stream-map (lambda (x) (list x (stream-car t))) (stream-cdr s))))))

(define thang (more-pairs integers integers))

(stream-ref thang  0)
(stream-ref thang  1)
(stream-ref thang  2)
(stream-ref thang  3)
(stream-ref thang  4)
(stream-ref thang  5)
(stream-ref thang  6)
(stream-ref thang  7)
(stream-ref thang  8)
(stream-ref thang  9)
(stream-ref thang 10)
(stream-ref thang 11)
(stream-ref thang 12)
(stream-ref thang 13)
(stream-ref thang 14)
(stream-ref thang 15)
