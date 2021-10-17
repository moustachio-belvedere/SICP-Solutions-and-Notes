#lang sicp
(#%require "utils_trees.rkt")

;(list->tree '(1 3 5 7 9 11))
;         5
;       /   \
;      1     9
;    / \    / \
;       3  7  11

; O(n) as each element in list is handled once
