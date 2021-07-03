#lang sicp

(define (average x y)
  (/ (+ x y) 2))

(define tolerance 0.00001)

(define (report sol num)
  (display "Solution: ")
  (display sol)
  (display " found in ")
  (display num)
  (display " iterations.")
  (newline))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess num)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          (report next num)
          (try next (+ num 1)))))
  (try first-guess 1))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.5)
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.5)
