#lang sicp

(define (square-tree tree)
  (if (pair? tree)
      (map square-tree tree)
      (* tree tree)))

(define x (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))

(square-tree x)
