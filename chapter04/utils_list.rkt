#! /bin/racket
#lang sicp

(#%provide tail
           all-but-tail)

(define (tail lst)
  (if (pair? lst)
      (if (null? (cdr lst))
          (car lst)
          (tail (cdr lst)))
      #f))

(define (all-but-tail lst)
  (if (pair? lst)
      (if (null? (cdr lst))
          '()
          (cons (car lst) (all-but-tail (cdr lst))))
      #f))

; (tail (list 2 3 5 7))
; 7
;
; (all-but-tail (list 2 3 5 7))
; (2 3 5)
