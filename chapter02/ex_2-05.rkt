#lang sicp

(define (not-div-by-2 x)
  (not (= 0
          (remainder x 2))))

(define (not-div-by-3 x)
  (not (= 0
          (remainder x 3))))

(define (cons_alt x y)
  (* (expt 2 x) (expt 3 y)))

(define (get-log-2-inv x)
  (inexact->exact (/ (log x) (log 2))))

(define (get-log-3-inv x)
  (inexact->exact (/ (log x) (log 3))))

(define (car_alt z)
  (if (not-div-by-3 z)
      (get-log-2-inv z)
      (car_alt (/ z 3))))

(define (cdr_alt z)
  (if (not-div-by-2 z)
      (get-log-3-inv z)
      (cdr_alt (/ z 2))))

(define x (cons_alt 7 11))
(car_alt x)
(cdr_alt x)
