#lang sicp
(#%require "utils_huff.rkt")

(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define (successive-merge leaf-set)
  (cond ((null? (cdr leaf-set)) (car leaf-set))
        (else (successive-merge (adjoin-set (make-code-tree (car leaf-set)
                                                            (cadr leaf-set))
                                             (cddr leaf-set))))))

(define (generate-huffman-tree pairs)
  (successive-merge
    (make-leaf-set pairs)))

(define my-tree
  (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))

(define sample-message
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define decoded-message1 (decode sample-message sample-tree))
(define decoded-message2 (decode sample-message my-tree))
(display decoded-message1)
(newline)
(display decoded-message2)
(newline)
