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

(define (total-torque x offset)
  (if (has-weight? x)
      (* offset (branch-structure x))
      (if (is-mobile? x)
          (+ (total-torque (left-branch x) (- offset (branch-length (left-branch x))))
             (total-torque (right-branch x) (+ offset (branch-length (right-branch x)))))
          (total-torque (branch-structure x) offset))))

(define (is-balanced? mobile)
  (= (total-torque mobile 0) 0))

;;; torque tests
(define a (make-mobile (make-branch 5 10)
                       (make-branch 5 10)))

(define b (make-mobile (make-branch 5 10)
                       (make-branch 2 10)))

(define c (make-mobile (make-branch 3 (make-mobile (make-branch 3 2)
                                                   (make-branch 4 1)))
                       (make-branch 3 (make-mobile (make-branch 4 1)
                                                   (make-branch 3 2)))))

(define d (make-mobile (make-branch 3 (make-mobile (make-branch 3 2)
                                                   (make-branch 4 1)))
                       (make-branch 3 (make-mobile (make-branch 4 1)
                                                   (make-branch 3 3)))))
(is-balanced? a)
(is-balanced? b)
(is-balanced? c)
(is-balanced? d)

;;; weight tests
(define x (make-mobile (make-branch 5 10)
                       (make-branch 7 10)))

(define y (make-mobile (make-mobile (make-branch 5 10)
                                    (make-branch 3 9))
                       (make-branch 5 (make-mobile (make-branch 3 4)
                                                   (make-branch 2 5)))))

(total-weight x)
(total-weight y)
