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

(define sense-data
  (cons-stream 1
    (cons-stream -0.5
      (cons-stream -0.5
        (cons-stream 5 the-empty-stream)))))

; again, modified for finite streams
; bug is that the average actually takes
; increasingly long history into account
; rather than just a local average of two
; most recent points
(define (make-zero-crossingsv0
         input-stream last-value)
  (if (stream-null? input-stream)
      the-empty-stream
  (let ((avpt
         (/ (+ (stream-car input-stream)
               last-value)
            2)))
    (cons-stream
     (sign-change-detector avpt last-value)
     (make-zero-crossingsv0
      (stream-cdr input-stream) avpt)))))

(define (make-zero-crossingsv1
         input-stream last-value last-avg)
  (if (stream-null? input-stream)
      the-empty-stream
  (let ((avpt
         (/ (+ (stream-car input-stream)
               last-value)
            2)))
    (cons-stream
     (sign-change-detector avpt last-avg)
     (make-zero-crossingsv1
      (stream-cdr input-stream) (stream-car input-stream) avpt)))))

(define zero-crossingsv0
  (make-zero-crossingsv0 sense-data 0))
(display-stream zero-crossingsv0)

(define zero-crossingsv1
  (make-zero-crossingsv1 sense-data 0 0))
(display-stream zero-crossingsv1)
