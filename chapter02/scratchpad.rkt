#lang sicp
(#%require "utils_sort.rkt")

(list? (list 1 2 3))
(list? 1)
(list? (cons 1 (cons 2 '())))

(sort (list 2 1 7 6 5 9 8 9) <)
(sort '(2 1 7 6 5 9 8 9) <)
