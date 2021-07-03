#lang sicp

; function evaluates a given unary function with input `2`
(define (f g)
  (g 2))

(f (lambda (x) (* x x))) ; 2*2
(f (lambda (z) (* z (+ z 1)))) ; 2*3

(f f)

; (f f)
; (f 2)
; (2 2)
; There is no function called `2` which can be evaluated, hence error: `not a procedure`
