#lang sicp

(define (reverse items)
  (define (reverse-iter items acc)
    (if (null? items)
        acc
        (reverse-iter (cdr items) (cons (car items) acc))))
  (reverse-iter items nil))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (deep-reverse items)
  (if (pair? items)
      (let ((ritems (reverse items)))
        (map deep-reverse ritems))
      items))

(define x (list (list 1 2 (list 8 9)) (list 3 4) (list 5 6) 7))
(deep-reverse x)

(define y (list (list 1 2) (list 3 4)))
(deep-reverse y)
