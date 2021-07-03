#lang sicp

(define (cont-frac-iter n d k)
  (define (cont-iter i accum)
    (if (= i 0)
        accum
        (cont-iter (- i 1) (/ (n i) (+ (d i) accum)))))
    
  (cont-iter (- k 1) (/ (n k) (d k))))

(define (euler-d i)
  (let ((mod3 (remainder i 3)))
    (if (= mod3 2)
        (* (/ (+ i 1) 3) 2)
        1.0)))

(define (e-approx terms)
  (+ (cont-frac-iter (lambda (x) 1.0) euler-d terms) 2))

(e-approx 20)
