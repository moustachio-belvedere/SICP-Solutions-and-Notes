#lang sicp

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment x0 y0 x1 y1)
  (cons (make-point x0 y0) (make-point x1 y1)))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment segment)
  (let ((sx (x-point (start-segment segment)))
        (sy (y-point (start-segment segment)))
        (ex (x-point (end-segment segment)))
        (ey (y-point (end-segment segment))))
    (make-point (average sx ex)
                (average sy ey))))

(define s (make-segment 0 0 4 6))

(display (x-point (midpoint-segment s)))
(display ", ")
(display (y-point (midpoint-segment s)))
(newline)

