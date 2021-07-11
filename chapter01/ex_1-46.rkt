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

(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) 
     tolerance))

(define (iterative-improve good-enough? improver)
  (define (main-doer guess)
    (define a (improver guess))
    (let ((next (improver guess)))
      (if (good-enough? guess next)
          next
          (main-doer next))))
  (lambda (guess) (main-doer guess)))

(define (sqrt x)
  ((iterative-improve close-enough? (average-damp (lambda (y) (/ x y)))) 
   1.0))

(sqrt 4)

(define (fixed-point f first-guess)
  ((iterative-improve close-enough? f) first-guess))

; sqrt 2 via fixed-point test
(fixed-point (average-damp (lambda (y) (/ 4 y))) 1.0)
