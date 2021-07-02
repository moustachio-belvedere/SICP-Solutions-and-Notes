#lang sicp

(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) 
     dx))

(display "Regular numerical integration:\n")
(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

(define (sintegral f a b n)
  (define h (/ (- b a) n))
  (define (adder1 x) (+ x h))
  (define (adder2 x) (+ x h h))
  (* (+ (f a)
        (f b)
        (* 2 (sum f (+ a h) adder1 (- b h)))
        (* 2 (sum f (+ a h) adder2 (- b h))))
     (/ h 3)))

(newline)
(display "Simpson rule integration:\n")
(sintegral cube 0.0 1.0 100)
(sintegral cube 0.0 1.0 1000)
; probably worse due to the way I summed the floats
