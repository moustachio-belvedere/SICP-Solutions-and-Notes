#! /bin/racket
#lang sicp
(#%require "utils_stream.rkt")
(#%require "utils_morestreams.rkt")
(#%require "utils_misc.rkt")

(define (sign-change-detector c p)
  (cond ((and (>= c 0)
              (<  p 0))  1)
        ((and (>= p 0)
              (<  c 0)) -1)
        (else 0)))

; added if condition for finite streams
(define (make-zero-crossingsv0
         input-stream last-value)
  (if (stream-null? input-stream)
      the-empty-stream
      (cons-stream
       (sign-change-detector
        (stream-car input-stream)
        last-value)
       (make-zero-crossingsv0
        (stream-cdr input-stream)
        (stream-car input-stream)))))

(define sense-data
  (cons-stream 1
    (cons-stream 0.5
      (cons-stream -0.5
        (cons-stream 5 the-empty-stream)))))

(define zero-crossingsv0
  (make-zero-crossingsv0 sense-data 0))

(display-stream zero-crossingsv0)

(define zero-crossingsv1
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))

(display-stream zero-crossingsv1)
