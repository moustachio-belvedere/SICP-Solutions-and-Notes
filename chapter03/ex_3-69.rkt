#! /bin/racket
#lang sicp
(#%require "utils_stream.rkt")
(#%require "utils_morestreams.rkt")
(#%require "utils_racket.rkt")
(#%require "utils_misc.rkt")

(define (square x) (* x x))

(define (Pythagorean? lst)
  (= (+ (square (car lst))
        (square (cadr lst)))
     (square (caddr lst))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x)
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples-v1 s t u)
  (stream-map flatten-list
    (pairs s (pairs t u))))

(define Pythagorean-triples-v1
  (stream-filter (lambda (x) (< (car x) (cadr x)))
  (stream-filter Pythagorean? (triples-v1 integers integers integers))))

;; very slow past the third element, but it works
;; probably slow due to the duplicates that have to
;; be filtered out
(stream-ref Pythagorean-triples-v1 0)
(stream-ref Pythagorean-triples-v1 1)
(stream-ref Pythagorean-triples-v1 2)

(define (triples-v2 s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
      (stream-map (lambda (x) (append (list (stream-car s)) x))
                  (pairs t u))
      (triples-v2 (stream-cdr s) (stream-cdr t) (stream-cdr u))
     )))

(define Pythagorean-triples-v2
  (stream-filter Pythagorean? (triples-v2 integers integers integers)))

;; much better alternative, avoids duplication of work
;; stream is interleavement of:
;; - first element of s appended to all pairs of t & u
;; - next element of s appended to all pairs of (s-cdr t) & (scdr u)
;; - and so on
(stream-ref Pythagorean-triples-v2 0)
(stream-ref Pythagorean-triples-v2 1)
(stream-ref Pythagorean-triples-v2 2)
