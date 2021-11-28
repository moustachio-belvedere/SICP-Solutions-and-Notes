#lang sicp
(#%provide rand-update
           rand-seedr
           rand)

;; with inspirtaion from:
;; https://stackoverflow.com/questions/3062746/special-simple-random-number-generator

;; this uses the same constants as glibc:
;; https://en.wikipedia.org/wiki/Linear_congruential_generator#Parameters_in_common_use
(define (rand-update x)
  (let ((a 1103515245)
        (c 12345)
        (m (expt 2 31)))
  (modulo (+ (* x a) c) m)))

(define (rand-seedr seed)
  (let ((x seed))
    (lambda () (set! x (rand-update x)) x)))

(define rand
  (rand-seedr 5))
