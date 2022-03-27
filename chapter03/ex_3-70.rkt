#! /bin/racket
#lang sicp
(#%require "utils_stream.rkt")
(#%require "utils_morestreams.rkt")
(#%require "utils_misc.rkt")

;; part a
(define (weighted-merge s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let* ((s1car (stream-car s1))
                (s2car (stream-car s2))
                (ws1 (weight s1car))
                (ws2 (weight s2car)))
           (cond ((<= ws1 ws2)
                  (cons-stream
                   s1car
                   (weighted-merge (stream-cdr s1)
                                   s2
                                   weight)))
                 ((> ws1 ws2)
                  (cons-stream
                   s2car
                   (weighted-merge s1
                                   (stream-cdr s2)
                                   weight))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (weighted-merge
      (stream-map (lambda (x)
                  (list (stream-car s) x))
                (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
      weight)))

(define part-a (weighted-pairs integers integers (lambda (x) (apply + x))))
(display-n-stream part-a 20)

;; part b
; we need the opposite of the Hamming numbers
; in question 3.56. There's probably a better
; way to find these, but for now just filter the
; integers
(define not-Hamming (stream-filter
                     (lambda (x)
                       (and (not-divisible? x 2)
                            (not-divisible? x 3)
                            (not-divisible? x 5)))
                     integers))

(define part-b (weighted-pairs not-Hamming
                               not-Hamming
                               (lambda (x)
                                 (let ((i (car x))
                                       (j (cadr x)))
                                   (+ (* 2 i)
                                      (* 3 j)
                                      (* 5 i j))))))

(display-n-stream part-b 20)
