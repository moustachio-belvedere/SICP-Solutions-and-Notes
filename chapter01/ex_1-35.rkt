#lang sicp

; golden ratio r satisfies the property
; r^2 == r + 1
; which is equivalent to
; r = 1 + 1/r
; hence the latter is the transformation that has a fixed point of 
; the golden ratio

(define tolerance 0.00001)

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

(define (golden-ratio-map x)
  (+ 1 (/ 1 x)))

(fixed-point golden-ratio-map 1.0)
