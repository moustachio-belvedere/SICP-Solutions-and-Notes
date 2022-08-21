#lang sicp

(define (make-interval a b) (cons a b))

(define (lower-bound i)
  (min (car i)
       (cdr i)))

(define (upper-bound i)
  (max (car i)
       (cdr i)))

(define (mul-interval x y)
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

(define (check-interval y)
  (if (and (>= (upper-bound y) 0)
           (<= (lower-bound y) 0))
      (error "Error, dividing by interval that spans 0")))

(define (div-interval x y)
  (check-interval y)
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y)))))

(define x (make-interval 1 2))
(define y (make-interval -1 1))

(define z (div-interval x y))
(display z)