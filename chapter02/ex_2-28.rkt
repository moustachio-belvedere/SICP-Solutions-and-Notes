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

(define x (list (list 1 2 (list 8 9)) (list 3 4) (list 5 6) 7))
(define y (list (list 1 2) (list 3 4)))
(define z (list 1 2 3 (list 4 5)))

(fringe x)
(fringe y)
(fringe z)
