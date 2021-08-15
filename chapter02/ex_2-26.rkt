#lang sicp

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) 
                    list2))))

(define x (list 1 2 3))
(define y (list 4 5 6))

; guess: (1 2 3 4 5 6)
(append x y)
; correct

; guess ((1 2 3) (4 5 6)
(cons x y)
; NO. ((1 2 3) 4 5 6)

; guess ((1 2 3) (4 5 6))
(list x y)
; correct

; see ex_2-26.png for block diagrams + tree diagrams
