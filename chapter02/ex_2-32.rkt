#lang sicp

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (if (pair? s)
                                          (append (list (car s)) x)
                                          (append (list s) x))) rest)))))

(define x (list 1 2 3))

(subsets x)
