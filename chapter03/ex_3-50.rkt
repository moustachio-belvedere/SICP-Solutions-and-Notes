#! /bin/racket
#lang sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

;; not well behaved for streams of uneven duration
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc
                    (map stream-cdr
                         argstreams))))))

(define (stream-enumerate-interval lo hi)
  (if (> lo hi)
    the-empty-stream
    (cons-stream
      lo
      (stream-enumerate-interval (+ lo 1) hi))))

(define t0 (stream-enumerate-interval 1 4))
(define t1 (stream-enumerate-interval 11 14))

(define t-stream (stream-map + t0 t1))

(stream-car t-stream)
(stream-car (stream-cdr t-stream))
(stream-car (stream-cdr (stream-cdr t-stream)))
(stream-car (stream-cdr (stream-cdr (stream-cdr t-stream))))
(stream-cdr (stream-cdr (stream-cdr (stream-cdr t-stream))))
