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

(stream-ref x 5)
;; each stream item up to the 6th
;; is displayed because `stream-cdr`
;; forces evaluation of the function.
;;
;; the final value, 5, is printed twice as
;; SICP Racket scheme prints the function
;; result by default.
(newline)

(stream-ref x 7)
;; due to the memoization mentioned
;; in-text, the results of the promises
;; evaluated so far have been cached.
;;
;; So the `(display-line)` is not called
;; again on the stream elements traversed
;; in the previous call.
