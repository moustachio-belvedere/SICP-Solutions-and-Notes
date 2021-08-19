#lang sicp

(define (tree-map func tree)
  (if (pair? tree)
      (map square-tree tree)
      (func tree)))

(define (square x)
  (* x x))

(define (square-tree tree)
  (tree-map square tree))

(define x (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))

(square-tree x)
