#lang sicp

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (square x)
  (* x x))

; tail call recursive
(define (repeated funco n)
  (define (rep-iter funci n)
    (if (= n 1)
        funci
        (rep-iter (compose funco funci) (- n 1))))
  (rep-iter funco n))

(define dx 0.1)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (smooth-n f n)
  ((repeated smooth n) f))

((smooth square) 4)
((smooth (smooth square)) 4)
((smooth (smooth (smooth square))) 4)
((smooth (smooth (smooth (smooth square)))) 4)
(newline)

((smooth-n square 1) 4)
((smooth-n square 2) 4)
((smooth-n square 3) 4)
((smooth-n square 4) 4)
