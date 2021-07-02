#lang sicp

; utility functions
(define (cube x)
  (* x x x))

(define (even? x)
  (= (remainder x 2) 0))

; recursive tests
(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-rec combiner null-value term (next a) next b))))

(define (integral-rec f a b dx)
  (define (add-dx x) (+ x dx))
  (* (accumulate-rec + 0 f (+ a (/ dx 2.0)) add-dx b) 
     dx))

(display "[=== Recursive tests ===]\n")
(display "[sum] integration-rec\n")
(integral-rec cube 0 1 0.01)
(integral-rec cube 0 1 0.001)
(newline)

(define (factorial-rec n)
  (accumulate-rec * 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(display "[product] factorial-rec:\n")
(factorial-rec 3)
(factorial-rec 4)
(factorial-rec 5)
(factorial-rec 6)
(newline)

(define (pio4-rec n)
  (define (term x)
    (define hn (+ (ceiling (/ x 2)) 1))
    (define hd (+ (ceiling (/ (+ x 1) 2))))
    (/ (* 2 hn) (+ (* 2 hd) 1)))
  (accumulate-rec * 1 term 0 (lambda (x) (+ x 1)) n))  

; get floating point version to get quick sense of accuracy
(define (get-pi-rec acc)
  (* 4.0 (pio4-rec acc)))

(display "[product] get-pi-rec:\n")
(get-pi-rec 10)
(get-pi-rec 100)
(get-pi-rec 1000)
(newline)

; iterative tests
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
    (iter a null-value))

(define (integral-iter f a b dx)
  (define (add-dx x) (+ x dx))
  (* (accumulate-iter + 0 f (+ a (/ dx 2.0)) add-dx b) 
     dx))

(display "[=== Iterative tests ===]\n")
(display "[sum] integration-iter:\n")
(integral-iter cube 0 1 0.01)
(integral-iter cube 0 1 0.001)
(newline)

(define (factorial-iter n)
  (accumulate-iter * 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(display "[product] factorial-iter:\n")
(factorial-iter 3)
(factorial-iter 4)
(factorial-iter 5)
(factorial-iter 6)
(newline)

(define (pio4-iter n)
  (define (term x)
    (define hn (+ (ceiling (/ x 2)) 1))
    (define hd (+ (ceiling (/ (+ x 1) 2))))
    (/ (* 2 hn) (+ (* 2 hd) 1)))
  (accumulate-iter * 1 term 0 (lambda (x) (+ x 1)) n))  

; get floating point version to get quick sense of accuracy
(define (get-pi-iter acc)
  (* 4.0 (pio4-iter acc)))

(display "[product] get-pi-iter:\n")
(get-pi-iter 10)
(get-pi-iter 100)
(get-pi-iter 1000)
(newline)
