#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
                       (cdr sequence)))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append 
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

(define (enum-t x)
  (map (lambda (x) (if (pair? x)
                       (map (lambda (y) 
                       (append (list (car y) (enum-t (cdr x))))
                       x))
       x))

(define (count-leaves t)
  (accumulate (lambda (x y) (+ 1 y))
              0
              (enumerate-tree t)))

; 10 leaves
(define x (list 9 5 2 (list 1 2 (list 5 3) 3 2) 1))

(display x)
(newline)
(enum-t x)
(enumerate-tree x)
(count-leaves x)
