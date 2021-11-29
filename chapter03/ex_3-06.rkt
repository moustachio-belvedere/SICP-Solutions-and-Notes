#lang sicp

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

(define rand
  (let ((randvar 5))
    (lambda (in)
      (cond ((eq? in 'generate) (set! randvar (rand-update randvar))
                                randvar)
            ((eq? in 'reset) (lambda (x) (set! randvar x)))))))

(rand 'generate)
(rand 'generate)
((rand 'reset) 5)
(rand 'generate)
(rand 'generate)
