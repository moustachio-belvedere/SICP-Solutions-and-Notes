#lang sicp

(define (square x)
  (* x x))

(define (square-list1 items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list1 (cdr items)))))

(define (square-list2 items)
  (map square items))

(define x (list 1 2 3 4 5))
(square-list1 x)
(square-list2 x)
