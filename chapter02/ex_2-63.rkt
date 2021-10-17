#lang sicp
(#%require "utils_trees.rkt")

(define x (make-tree 7
                     (make-tree 3
                                (make-tree 1 '() '())
                                (make-tree 5 '() '()))
                     (make-tree 9
                                '()
                                (make-tree 11 '() '()))))

(define y (make-tree 3
                     (make-tree 1 '() '())
                     (make-tree 7
                                (make-tree 5 '() '())
                                (make-tree 9 '()
                                           (make-tree 11 '() '())))))

(define z (make-tree 5
                     (make-tree 3
                                (make-tree 1 '() '())
                                '())
                     (make-tree 9
                                (make-tree 7 '() '())
                                (make-tree 11 '() '()))))

(define s (make-tree 1
                     '()
                     (make-tree 2
                                '()
                                (make-tree 3
                                           '()
                                           (make-tree 4
                                                      '()
                                                      '())))))

(tree->list-1 x)
(tree->list-2 x)
(newline)
(tree->list-1 y)
(tree->list-2 y)
(newline)
(tree->list-1 z)
(tree->list-2 z)
(newline)
(tree->list-1 s)
(tree->list-2 s)

;; all list results are equivalent
