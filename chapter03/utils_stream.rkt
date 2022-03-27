#! /bin/racket
#lang sicp
(#%provide add-streams
           display-line
           display-stream
           display-n-stream
           integers
           ones
           scale-stream
           show
           stream-car
           stream-cdr
           stream-enumerate-interval
           stream-filter
           stream-for-each
           stream-map
           stream-ref)

(define (display-line x)
  (display x)
  (newline))

(define (show x)
  (display-line x)
  x)

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

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

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream)
         the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream
          (stream-car stream)
          (stream-filter
           pred
           (stream-cdr stream))))
        (else (stream-filter
               pred
               (stream-cdr stream)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
        (proc (stream-car s))
        (stream-for-each proc
                         (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-n-stream s n)
  (if (= n 0)
      'Done
      (begin (display (stream-car s))
             (newline)
             (display-n-stream (stream-cdr s)
                               (- n 1)))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
