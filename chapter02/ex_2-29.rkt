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

(define (is-mobile? x)
  (pair? (car x)))

(define (is-branch? x)
  (not (pair? (car x))))

(define (has-weight? x)
  (not (pair? (branch-structure x))))

(define (total-weight x)
  (if (has-weight? x)
      (branch-structure x)
      (if (is-mobile? x)
          (+ (total-weight (left-branch x))
             (total-weight (right-branch x)))
          (total-weight (branch-structure x)))))

(define x (make-mobile (make-branch 5 10)
                       (make-branch 7 10)))

(define y (make-mobile (make-mobile (make-branch 5 10)
                                    (make-branch 3 9))
                       (make-branch 5 (make-mobile (make-branch 3 4)
                                                   (make-branch 2 5)))))

(total-weight x)
(total-weight y)
