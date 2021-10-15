#lang sicp
(#%require "utils_symsorted.rkt")

(define (adjoin-set x set)
  (define (set-iter acc setr)
    (cond ((null? setr) (append set (list x)))
          ((= x (car setr)) set)
          ((> x (car setr)) (set-iter (append acc (list (car setr))) (cdr setr)))
          ((< x (car setr)) (append acc (list x) setr))))
  (set-iter '() set))

(define s1 (list 2 3 5 11))

(adjoin-set 7 s1)
(adjoin-set 3 s1)
(adjoin-set 1 s1)
(adjoin-set 17 s1)
