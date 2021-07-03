#lang sicp

;;;;;;;;;;;;;;;;;;;;
; Recursive approach
(define (cont-frac-rec n d k)
  (define (cont-rec i)
    (if (< i k)
        (/ (n i) (+ (d i) (cont-rec (+ i 1))))
        (/ (n i) (+ (d i) (/ (n i) (d i))))))
  (cont-rec 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Iterative (tail-call) approach
(define (cont-frac-iter n d k)
  (define (cont-iter i accum)
    (if (= i 0)
        accum
        (cont-iter (- i 1) (/ (n i) (+ (d i) accum)))))
    
  (cont-iter (- k 1) (/ (n k) (d k))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Approximation helper function
(define (phi-approx method terms)
  (/ 1 (method (lambda (x) 1.0) (lambda (x) 1.0) terms)))

; get number of terms required for 4dp accurate approximation
(define (phi-approx-4dp method)
  (define (print-help approx iterations)
    (display "Approximation: ")
    (display approx)
    (display " reached in ")
    (display iterations)
    (display " iterations\n"))

  (define (good? approx)
    (< (abs (- approx 1.6180339887498948482)) 0.00002))

  (define (iter i)
    (let ((approx (phi-approx method i)))
      (if (good? approx)
          (print-help approx i)
          (iter (+ i 1)))))
  (iter 1))

(phi-approx-4dp cont-frac-rec)
(phi-approx-4dp cont-frac-iter)
