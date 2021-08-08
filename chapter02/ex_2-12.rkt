#lang sicp

(define (make-interval a b) (cons a b))

(define (lower-bound i)
  (min (car i)
       (cdr i)))

(define (upper-bound i)
  (max (car i)
       (cdr i)))

(define (make-center-percent center pcnt)
  (let ((width (* pcnt center)))
    (make-interval (- center width) (+ center width))))

(define (percent interval)
  (let ((width (/ (- (upper-bound interval)
                     (lower-bound interval))
                  2))
        (center (/ (+ (upper-bound interval)
                      (lower-bound interval))
                   2)))
    (/ width center)))

(define x (make-center-percent 2 0.5))
(display (car x))
(newline)
(display (cdr x))
(newline)

(display (percent x))
