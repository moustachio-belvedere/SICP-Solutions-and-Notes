#lang sicp
(#%require "utils_trees.rkt")

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (entry set-of-records))) (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup
          given-key 
          (left-branch set-of-records)))
        ((> given-key (key (entry set-of-records)))
         (lookup 
          given-key 
          (right-branch set-of-records)))))
