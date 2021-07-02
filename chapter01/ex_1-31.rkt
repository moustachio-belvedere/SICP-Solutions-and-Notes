#lang sicp

(display "[==== Iterative approach ====]\n")

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
    (iter a 1))

(define (factorial-iter n)
  (product-iter (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(display "factorial-iter:\n")
(factorial-iter 3)
(factorial-iter 4)
(factorial-iter 5)
(factorial-iter 6)
(newline)

; nice approach but requires a custom comparator argument in product
; (define (pio4-iter n)
;   (define (next x)
;     (define num (numerator x))
;     (define den (denominator x))
;     (if (> num den)
;         (/ num (+ den 2))
;         (/ (+ num 2) den)))
; 
;   (product-iter (lambda (x) x) 2/3 next ..))

(define (even? x)
  (= (remainder x 2) 0))

(define (pio4-iter n)
  (define (term x)
    (define hn (+ (ceiling (/ x 2)) 1))
    (define hd (+ (ceiling (/ (+ x 1) 2))))
    (/ (* 2 hn) (+ (* 2 hd) 1)))
  (product-iter term 0 (lambda (x) (+ x 1)) n))  

; get floating point version to get quick sense of accuracy
(define (get-pi-iter acc)
  (* 4.0 (pio4-iter acc)))

(display "get-pi-iter:\n")
(get-pi-iter 10)
(get-pi-iter 100)
(get-pi-iter 1000)
(newline)

(display "[==== Recursive approach ====]\n")

(define (product-rec term a next b)
  (if (> a b)
      1 
      (* (term a)
         (product-rec term (next a) next b))))

(define (factorial-rec n)
  (product-rec (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(display "factorial-rec:\n")
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
  (product-rec term 0 (lambda (x) (+ x 1)) n))  

; get floating point version to get quick sense of accuracy
(define (get-pi-rec acc)
  (* 4.0 (pio4-rec acc)))

(display "get-pi-rec:\n")
(get-pi-rec 10)
(get-pi-rec 100)
(get-pi-rec 1000)
(newline)
