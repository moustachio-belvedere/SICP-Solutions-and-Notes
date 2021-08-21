#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (map-u p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil
              sequence))

(define (append-u seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length-u sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(define x (list 1 2 3 4 5))
(define y (list 6 7 8 9))

(map-u (lambda (x) (* x x)) x)
(append-u x y)
(length-u x)
(length-u y)
