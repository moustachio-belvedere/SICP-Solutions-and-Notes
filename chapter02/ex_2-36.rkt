#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (accumulate-horizontal op init seqs)
  (if (null? seqs)
      nil
      (cons (accumulate op init (car seqs))
            (accumulate-horizontal op init (cdr seqs)))))

(define (accumulate-vertical op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-vertical op init (map cdr seqs)))))

(define x (list (list 1 2 3)
                (list 4 5 6)
                (list 7 8 9)
                (list 10 11 12)))

(display x)
(newline)
(display "accumulate-horizontal: ")
(accumulate-horizontal + 0 x)
(display "accumulate-vertical: ")
(accumulate-vertical + 0 x)
