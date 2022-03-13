#! /bin/racket
#lang sicp
(#%require "utils_stream.rkt")
(#%require "utils_morestreams.rkt")

(define (ln2-summands n)
  (cons-stream
   (/ 1.0 n)
   (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
   (partial-sums (ln2-summands 1)))

(display "Regala, ain't nothin fancy\n")
(stream-ref ln2-stream  0)
(stream-ref ln2-stream 10)
(stream-ref ln2-stream 20)
(stream-ref ln2-stream 30)
(stream-ref ln2-stream 40)
(stream-ref ln2-stream 50)
(stream-ref ln2-stream 60)
(stream-ref ln2-stream 70)
(stream-ref ln2-stream 80)
(stream-ref ln2-stream 90)
(stream-ref ln2-stream 100)
(stream-ref ln2-stream 110)
(stream-ref ln2-stream 120)
(stream-ref ln2-stream 130)
(stream-ref ln2-stream 140)
(stream-ref ln2-stream 150)
(stream-ref ln2-stream 160)
(stream-ref ln2-stream 170)
(stream-ref ln2-stream 180)
(stream-ref ln2-stream 190)
(stream-ref ln2-stream 200)
(stream-ref ln2-stream 210)
(stream-ref ln2-stream 220)
(stream-ref ln2-stream 230)
(stream-ref ln2-stream 240)
(stream-ref ln2-stream 250)
(stream-ref ln2-stream 260)
(stream-ref ln2-stream 270)
(stream-ref ln2-stream 280)
(stream-ref ln2-stream 290)
(stream-ref ln2-stream 300)
(newline)(newline)

(define (square x) (* x x))
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))     ; Sₙ₋₁
        (s1 (stream-ref s 1))     ; Sₙ
        (s2 (stream-ref s 2)))    ; Sₙ₊₁
    (cons-stream
     (- s2 (/ (square (- s2 s1))
              (+ s0 (* -2 s1) s2)))
     (euler-transform (stream-cdr s)))))

(define enhanced-ln2-v1
  (euler-transform ln2-stream))

(display "A little bit of bombastic fancy\n")
(stream-ref enhanced-ln2-v1  0)
(stream-ref enhanced-ln2-v1 10)
(stream-ref enhanced-ln2-v1 20)
(stream-ref enhanced-ln2-v1 30)
(stream-ref enhanced-ln2-v1 40)
(stream-ref enhanced-ln2-v1 50)
(stream-ref enhanced-ln2-v1 60)
(stream-ref enhanced-ln2-v1 70)
(newline)(newline)

(define (make-tableau transform s)
  (cons-stream
   s
   (make-tableau
    transform
    (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define enhanced-ln2-v2
  (accelerated-sequence euler-transform ln2-stream))

(display "Steaming hot meringue cakes with custard on top\n")
(stream-ref enhanced-ln2-v2  0)
(stream-ref enhanced-ln2-v2 1)
(stream-ref enhanced-ln2-v2 2)
(stream-ref enhanced-ln2-v2 3)
(stream-ref enhanced-ln2-v2 4)
(newline)(newline)

;; the tableau version works an order of magnitude faster
;; (very approximately) than the once-transformed version,
;; and the once-transformed version is approximately an
;; order of magnitude faster than the basic version.
