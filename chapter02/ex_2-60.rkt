#lang sicp

; element of set remains unchanged
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define eos1 (list 1 2 3 3 4))
(element-of-set? 3 eos1)
(element-of-set? 5 eos1)

; doesn't require check before adjoin
; O(1) instead of O(n)
(define (adjoin-set x set)
  (cons x set))

(adjoin-set 5 (adjoin-set 5 eos1))


; remains unchanged
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2)))
        (else (intersection-set (cdr set1) 
                                set2))))

(define ix (list `x `x `z 2 3 5))
(define iy (list `a `x `c 2 7))
(intersection-set ix iy)

; can skip element-of-set check
; so it becomes O(n) compared to O(n^2)
(define (union-set s1 s2)
  (if (null? s1)
       s2
       (union-set (cdr s1) (cons (car s1) s2))))

(define x (list `x `y `z 2 3 5))
(define y (list `a `b `c 2 7))
(union-set x y)
