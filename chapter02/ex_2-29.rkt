#lang sicp

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) 
                    list2))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (flatten-once items)
  (define (flat-iter items acc)
    (cond ((null? items) acc)
          ((pair? (car items)) (flat-iter (cdr items) (append acc (car items))))
          (else (flat-iter (cdr items) (append acc (list (car items)))))))
  (flat-iter items nil))

(define (fringe items)
  (if (pair? items)
      (flatten-once (map fringe items))
      items))

(define (sum list) (apply + list))

(define (make-mobile left right)
  (list left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (make-branch length structure)
  (list length structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(sum (list 1 2 3 4 5))
