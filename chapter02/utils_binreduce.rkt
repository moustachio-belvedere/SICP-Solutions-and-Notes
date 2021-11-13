#lang sicp
(#%provide binreduce)

(define (binreduce op x)
    (cond ((not (pair? x)) x)
          ((null? x) x)
          ((= (length x) 1) (car x))
          (else (op (car x) (binreduce op (cdr x))))))
