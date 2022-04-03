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

(define rand
  (let ((randvar 5))
    (lambda (msg)
      (cond ((eq? msg 'generate) (set! randvar (rand-update randvar)) randvar)
            ((eq? msg 'reset) (lambda (x) (set! randvar x))))
      )))

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

(rand 'generate)
(rand 'generate)
(rand 'generate)
((rand 'reset) 3)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)

(define request-stream
  (cons-stream 'generate
  (cons-stream 'generate
  (cons-stream (cons 'reset 3)
  (cons-stream 'generate
  (cons-stream 'generate
  (cons-stream 'generate
  (cons-stream 'generate
   the-empty-stream))))))))

(display "\nStreamwise:\n")
(define rs (random-numbers 5 request-stream))
(display-n-stream rs 7)
