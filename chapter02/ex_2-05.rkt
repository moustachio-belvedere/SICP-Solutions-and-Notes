#lang sicp

(define (not-div-by-y x y)
  (not (= 0
          (remainder x y))))

(define (get-log-y-inv x y)
  (inexact->exact (/ (log x) (log y))))

(define (cons_alt x y)
  (* (expt 2 x) (expt 3 y)))

(define (car_alt z)
  (if (not-div-by-y z 3)
      (get-log-y-inv z 2)
      (car_alt (/ z 3))))

(define (cdr_alt z)
  (if (not-div-by-y z 2)
      (get-log-y-inv z 3)
      (cdr_alt (/ z 2))))

(define x (cons_alt 7 11))
(car_alt x)
(cdr_alt x)
