#lang sicp

; without using list or apply it's a bit messy

; some helper functions
(define (square x) (* x x))

(define (sumSquare x y) (+ (square x) (square y)))

(define (largest x y z)
        (if (>= x y)
            (if (>= x z) x z)
            (if (>= y z) y z)))

(define (isMiddle x y z)
        (or (and (<= y z)
                 (>= y x))
            (and (<= y x)
                 (>= y z))))

(define (secondLargest x y z)
        (cond ((isMiddle x y z) y)
              ((isMiddle z x y) x)
              ((isMiddle y z x) z)))

(define (maxSumSquare x y z) 
        (sumSquare (largest x y z)
                   (secondLargest x y z)))

(maxSumSquare 5 5 6) ; 61
