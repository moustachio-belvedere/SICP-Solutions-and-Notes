#lang sicp

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (even? x)
  (if (= (remainder x 2) 0)
      1
      0))

(define (same-parity . lin)
  (let ((init-parity (even? (car lin))))
    (define (par-iter lbuild l-iter)
      (if (null? l-iter)
          lbuild
          (if (= (even? (car l-iter))
                 init-parity)
              (par-iter (append lbuild (cons (car l-iter) nil))
                        (cdr l-iter))
              (par-iter lbuild
                        (cdr l-iter)))))
    (par-iter (cons (car lin) nil) (cdr lin))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
(same-parity 5 19 22 23 24 31 32)
    

