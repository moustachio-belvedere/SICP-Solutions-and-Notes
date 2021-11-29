#lang sicp
(#%require "utils_round.rkt")
(#%provide rand-update
           rand-seedr
           rand
           randf
           nice-real)

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

(define (rand-seedr seed)
  (let ((x seed))
    (lambda () (set! x (rand-update x)) x)))

(define rand (rand-seedr 5))

(define (square x)
  (* x x))

(define (nice-real non-real)
  (define decimal-places 15)

  (define (truncator non-real)
    (let ((x (square (sqrt non-real))))
    (let ((i (truncate x)) (dp decimal-places))
    (let ((d (exact-round (* (- x i) (expt 10 dp)))))
         (+ i (* d (expt 10 (* -1 dp))))))))

  (if (< non-real 0)
      (* -1 (truncator (* -1 non-real)))
      (truncator non-real)))

;; provides random real numbers
;; between 0 and 1
(define (randf)
 (nice-real (/ (rand) m)))
