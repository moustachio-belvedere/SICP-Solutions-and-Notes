#! /bin/racket
#lang sicp
(#%require "utils_stream.rkt")
(#%require "utils_morestreams.rkt")

(define (pairs s t)
  (interleave
   (stream-map
    (lambda (x)
      (list (stream-car s) x))
    t)
   (pairs (stream-cdr s)
          (stream-cdr t))))

;(define thang (pairs integers integers))

;; line above doesn't stop running until
;; stack overflow as `interleave` does
;; not have delayed evaluation of its args
;; so pairs is recursively called indefinitely.
