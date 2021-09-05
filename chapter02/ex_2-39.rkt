#lang sicp

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) 
                    list2))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverseR sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverseL sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(define x (list 2 3 5 7 11 13 17 19 23))

(reverseR x)
(reverseL x)
