#! /bin/racket
#lang sicp
(#%require "utils_stream.rkt")
(#%require "utils_morestreams.rkt")
(#%require "utils_misc.rkt")

;; a linear congruential generator
;; with inspiration from:
;; https://stackoverflow.com/questions/3062746/special-simple-random-number-generator
;; this uses the same constants as Knuth's MMIX:
;; https://en.wikipedia.org/wiki/Linear_congruential_generator#Parameters_in_common_use
(define m (expt 2 64))

(define (rand-update x)
  (let ((a 6364136223846793005)
        (c 1442695040888963407))
  (modulo (+ (* x a) c) m)))

;; stream interface implemented here requires that any
;; reset in the request stream comes as a pair with the
;; 'reset symbol first and the value to seed second
(define (random-numbers init request-stream)
  (define rand-stream
  (cons-stream (rand-update init)
    (stream-map (lambda (r req)
                  (cond ((eq? req 'generate) (rand-update r))
                        ((and (pair? req) (eq? (car req) 'reset))
                          (rand-update (cdr req)))))
                rand-stream
                request-stream)))
  rand-stream)

(define (square x)
  (* x x))

(define request-stream
  (cons-stream 'generate request-stream))

(define (randf seed) (scale-stream (random-numbers seed request-stream) (/ 1 m)))

(define (bool->int x)
  (if x
      1
      0))

(define unit-circle-predicate
  (stream-map (lambda (x y)
                (let ((rx (- (* 2 x) 1))
                      (ry (- (* 2 y) 1)))
                  (bool->int (<= (+ (square rx) (square ry)) 1))))
              (randf 3)
              (randf 5)))

(define (rectangle-area x-lo x-hi y-lo y-hi)
  (* (- x-hi x-lo) (- y-hi y-lo)))

(define (estimate-integral P x-lo x-hi y-lo y-hi)
  (let ((rectangle (rectangle-area x-lo x-hi y-lo y-hi)))
    (stream-map (lambda (p-sum count)
                (* rectangle (/ p-sum count)))
                (partial-sums P)
                integers)))

(define integral-estimate-stream
  (estimate-integral unit-circle-predicate -1  1 -1  1))

;; sqrt and square to convert to float
(define pi-estimate-stream
  (stream-map (lambda (x)
                (square (sqrt x)))
              integral-estimate-stream))

(stream-ref pi-estimate-stream 10000)
;; 3.109...
