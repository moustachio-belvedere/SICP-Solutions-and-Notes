#! /bin/racket
#lang sicp

(define (display-line x)
  (display x)
  (newline))

(define (show x)
  (display-line x)
  x)

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

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define x 
  (stream-map 
   show 
   (stream-enumerate-interval 0 10)))

(stream-car (stream-cdr x))

(stream-cdr x)
(newline)

(stream-ref x 5)
(newline)
(stream-ref x 7)

;; (force 5) => 5
;; once an element of the stream has been `force`d,
;; it is no longer a `promise`, it has been evaluated
;; to the corresponding number of the interval.
;;
;; This is why `show` only acts once per stream element.
