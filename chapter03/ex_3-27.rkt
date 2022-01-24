#!/bin/racket
#lang sicp
(#%require "utils_table.rkt")

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result 
             (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize 
   (lambda (n)
     (display n)(newline)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else 
            (+ (memo-fib (- n 1))
               (memo-fib (- n 2))))))))

(display "Call 1:\n")
(memo-fib 10)

(display "\nCall 2:\n")
(memo-fib 11)

;; (memoize fib) is not sufficient because the table
;; is not checked at each recursive call, and no values
;; are stored in the table.

(memo-fib 3)

;; NOTE: post solution check quote from Eli's solutions,
;; "(memo-fib n) is O(n) in time... the tree is flattened into a linear list."
