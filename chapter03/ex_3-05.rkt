#lang sicp
(#%require "utils_random.rkt")

(define (square x)
  (* x x))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (rectangle-area x-lo x-hi y-lo y-hi)
  (* (- x-hi x-lo) (- y-hi y-lo)))

(define (estimate-integral P x-lo x-hi y-lo y-hi iters)
  (let ((rectangle (rectangle-area x-lo x-hi y-lo y-hi))
        (prop-area (monte-carlo iters P)))
    (* prop-area rectangle)))

(define (unit-circle-predicate)
  (let ((rx (- (* 2 (randf)) 1))
        (ry (- (* 2 (randf)) 1)))
    (<= (+ (square rx) (square ry)) 1)))

(define pi-estimate (estimate-integral unit-circle-predicate
                                       -1 1 -1 1
                                       50000))

(display "Pi estimate: ")
(display (nice-real pi-estimate))
(newline)
