#! /bin/racket
#lang racket

;; smith/fletcher, cooper/fletcher
(define (adjacent? x y)
  (= (abs (- x y)) 1))

(define (same? x y)
  (= x y))

;; miller > c
(define (higher? m c)
  (> m c))

;; b != 5
(define (b-illegal? b)
  (= b 5))
(define (b-legal? b)
  (not (= b 5)))

;; c != 1
(define (c-illegal? c)
  (= c 1))
(define (c-legal? c)
  (not (= c 1)))

;; f != 1
(define (f-illegal? f)
  (or (= f 1) (= f 5)))
(define (f-legal? f)
  (not (or (= f 1) (= f 5))))

(define (s lst)
  (car (cddddr lst)))
(define (m lst)
  (car (cdddr lst)))
(define (f lst)
  (car (cddr lst)))
(define (c lst)
  (car (cdr lst)))
(define (b lst)
  (car lst))

(define base '(1 2 3 4 5))
(define perms (permutations base))

;; can brute-force it as not too many
;; permutations to test
(define (try-all ps)
  (let ((p (car ps)))
    (if (and (b-legal? (b p))
             (c-legal? (c p))
             (f-legal? (f p))
             (not (adjacent? (s p) (f p)))
             (not (adjacent? (f p) (c p)))
             (higher? (m p) (c p)))
      p
      (try-all (cdr ps)))))

(try-all perms)
