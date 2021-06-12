#lang sicp

; Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))

(define (tester n func)
  (define (iterfunc i n func)
    (cond ((< i n)
           (display (func i))
           (display "\n")
           (iterfunc (+ i 1) n func))
          (else (display (func n)))))
  (iterfunc 0 n func))

(display "f function\n")
(tester 5 f)
(display "\n\n")
; equivalent to 2*n
; multiplication

(display "g function\n")
(tester 5 g)
(display "\n\n")
; equivalent to 2^n
; exponentiation

(display "h function\n")
(tester 4 h)
(display "\n\n")
; equivalent to 2^2^2... (n times)
; tetration
