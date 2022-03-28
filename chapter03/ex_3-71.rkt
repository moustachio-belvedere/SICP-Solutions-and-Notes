#! /bin/racket
#lang sicp
(#%require "utils_stream.rkt")
(#%require "utils_morestreams.rkt")
(#%require "utils_misc.rkt")

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

(define (pair-cubes lst)
        (let ((x (car lst))
              (y (cadr lst)))
        (+ (* x x x) (* y y y))))

(define wp (weighted-pairs integers integers pair-cubes))
(define cwp (stream-map (lambda (x y) (list x y)) wp (stream-cdr wp)))
(define mwp (stream-filter (lambda (lst)
                             (= (pair-cubes (car lst))
                                (pair-cubes (cadr lst))))
                           cwp))

(define (pp x)
  (display (pair-cubes (car x)))
  (display ": ")
  (display (car x))
  (display ", ")
  (display (pair-cubes (cadr x)))
  (display ": ")
  (display (cadr x))
  (display "\n\n"))

(pp (stream-ref mwp 0))
(pp (stream-ref mwp 1))
(pp (stream-ref mwp 2))
(pp (stream-ref mwp 3))
(pp (stream-ref mwp 4))
(pp (stream-ref mwp 5))
