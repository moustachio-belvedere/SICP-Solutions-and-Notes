#lang sicp

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) 
    (average x (f x))))

(define tolerance 0.001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; tail call recursive
(define (repeated funco n)
  (define (rep-iter funci n)
    (if (= n 1)
        funci
        (rep-iter (compose funco funci) (- n 1))))
  (rep-iter funco n))

(define (nrt x n)
  (let ((nm1 (- n 1)))
  (fixed-point ((repeated average-damp nm1)
                (lambda (y) (/ x (expt y nm1))))
               1.0)))

(nrt 4 2)
(nrt 8 3)
(nrt 16 4)
(newline)

(nrt 9 2)
(nrt 27 3)
(nrt 81 4)
(newline)

(nrt 25 2)
(nrt 125 3)
(nrt 625 4)
