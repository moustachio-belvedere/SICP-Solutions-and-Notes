#lang sicp
(#%require "utils_trees.rkt")

; From exercise 2.62 and example above it in-text, we already have O(n) implementations of `intersection-set` and `union-set` for ordered lists.
; From ex 2.63 and 2.64 we have O(n) implementations of tree -> list and list -> tree conversions. 
; Thus, all that is required is to glue them together.

(define (union-set-list s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else (let ((x1 (car s1)) (x2 (car s2)))
                (cond ((= x1 x2)
                       (cons x1 (union-set-list (cdr s1) (cdr s2))))
                      ((< x1 x2)
                       (cons x1 (union-set-list (cdr s1) s2)))
                      ((< x2 x1)
                       (cons x2 (union-set-list s1 (cdr s2)))))))))

(define (intersection-set-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-list 
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set-list 
                          (cdr set1) 
                          set2))
              ((< x2 x1) (intersection-set-list 
                          set1 
                          (cdr set2)))))))

(define (union-set s1 s2)
  (list->tree (union-set-list (tree->list-2 s1)
                              (tree->list-2 s2))))

(define (intersection-set s1 s2)
  (list->tree (intersection-set-list (tree->list-2 s1)
                                     (tree->list-2 s2))))
