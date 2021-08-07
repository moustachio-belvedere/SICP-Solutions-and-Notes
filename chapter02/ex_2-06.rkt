#lang sicp

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; define 1 via substitution
; (add-1 0)
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
; (lambda (f) (lambda (x) (f x)))
(define one (lambda (f) (lambda (x) (f x))))

; following same substitution process using
; (add-1 one)
; yields
(define two (lambda (f) (lambda (x) (f (f x)))))

; noticing the pattern that composed applications of `f` corresponds to the integer,
; seek functions that compose as required. Exploring 1 + 1
; A: ((lambda (f) (lambda (x) (f x))) f) = (lambda (x) (f x))
; B: (((lambda (f) (lambda (x) (f x))) f) x) = (f x)
; Applying B as operand to function A yields
; C: ((lambda (x) (f x)) (f x)) = (f (f x))
; which is the required body of the 2 integer function.
; generalising this yields:
(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
