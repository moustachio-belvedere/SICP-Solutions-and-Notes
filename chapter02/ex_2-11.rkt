#lang sicp

; 2 bounds for x, 2 bounds for y so
; total possibilities for sign of bounds
; is 2^4=16 (ignoring bound at exactly 0) but
; of those, 7 are impossible as they
; imply a positive lower bound and negative
; upper bound, so 9 in total as Ben said.

(define (make-interval a b) (cons a b))

(define (lower-bound i)
  (min (car i)
       (cdr i)))

(define (upper-bound i)
  (max (car i)
       (cdr i)))

(define (1111? x y)
  (and (>= (upper-bound x) 0)
       (>= (lower-bound x) 0)
       (>= (upper-bound y) 0)
       (>= (lower-bound y) 0)))

(define (1110? x y)
  (and (>= (upper-bound x) 0)
       (>= (lower-bound x) 0)
       (>= (upper-bound y) 0)
       (<= (lower-bound y) 0)))

(define (1011? x y)
  (and (>= (upper-bound x) 0)
       (<= (lower-bound x) 0)
       (>= (upper-bound y) 0)
       (>= (lower-bound y) 0)))

(define (1100? x y)
  (and (>= (upper-bound x) 0)
       (>= (lower-bound x) 0)
       (<= (upper-bound y) 0)
       (<= (lower-bound y) 0)))

(define (0011? x y)
  (and (<= (upper-bound x) 0)
       (<= (lower-bound x) 0)
       (>= (upper-bound y) 0)
       (>= (lower-bound y) 0)))

(define (1010? x y)
  (and (>= (upper-bound x) 0)
       (<= (lower-bound x) 0)
       (>= (upper-bound y) 0)
       (<= (lower-bound y) 0)))

(define (1000? x y)
  (and (>= (upper-bound x) 0)
       (<= (lower-bound x) 0)
       (<= (upper-bound y) 0)
       (<= (lower-bound y) 0)))

(define (0010? x y)
  (and (<= (upper-bound x) 0)
       (<= (lower-bound x) 0)
       (>= (upper-bound y) 0)
       (<= (lower-bound y) 0)))

(define (0000? x y)
  (and (<= (upper-bound x) 0)
       (<= (lower-bound x) 0)
       (<= (upper-bound y) 0)
       (<= (lower-bound y) 0)))

(define (handle-expensive-case x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (mul-interval x y)
  (cond ((1111? x y) (make-interval (* (lower-bound x) (lower-bound y))
                                    (* (upper-bound x) (upper-bound y))))
        ((1110? x y) (make-interval (* (upper-bound x) (lower-bound y))
                                    (* (upper-bound x) (upper-bound y))))
        ((1011? x y) (make-interval (* (lower-bound x) (upper-bound y))
                                    (* (upper-bound x) (upper-bound y))))
        ((1100? x y) (make-interval (* (upper-bound x) (lower-bound y))
                                    (* (lower-bound x) (upper-bound y))))
        ((0011? x y) (make-interval (* (lower-bound x) (upper-bound y))
                                    (* (upper-bound x) (lower-bound y))))
        ((1010? x y) (handle-expensive-case x y))
        ((1000? x y) (make-interval (* (upper-bound x) (lower-bound y))
                                    (* (lower-bound x) (lower-bound y))))
        ((0010? x y) (make-interval (* (lower-bound x) (upper-bound y))
                                    (* (lower-bound x) (lower-bound y))))
        ((0000? x y) (make-interval (* (upper-bound x) (upper-bound y))
                                    (* (lower-bound x) (lower-bound y))))))
