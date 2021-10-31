#lang sicp
(#%require "utils_huff.rkt")

(define (successive-merge leaf-set)
  (cond ((null? (cdr leaf-set)) (car leaf-set))
        (else (successive-merge (adjoin-set (make-code-tree (car leaf-set)
                                                            (cadr leaf-set))
                                             (cddr leaf-set))))))

(define (generate-huffman-tree pairs)
  (successive-merge
    (make-leaf-set pairs)))

(define n5 '((a 1) (b 2) (c 4) (d 8) (e 16)))

(define n10 '((a 1) (b 2) (c 4) (d 8) (e 16)
              (f 32) (g 64) (h 128) (i 256) (j 512)))

(define n5tree (generate-huffman-tree n5))
(define n10tree (generate-huffman-tree n10))
; (display n10tree)
; (newline)
; (newline)
; (display (car n10tree))
; (newline)
; (newline)
; (display (car (car n10tree)))

; from display cmds and drawing,
; it's clear that we need 1 bit
; to encode the most frequent symbol
; and n-1 bits to encode the least
; frequent symbol
