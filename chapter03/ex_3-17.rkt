#!/bin/racket
#lang sicp

(define (in? x lst)
  (cond ((null? lst) #f)
        ((eq? x (car lst)) #t)
        (else (in? x (cdr lst)))))

(define (append! lst appendage)
  (if (null? (cdr lst))
      (set-cdr! lst appendage)
      (append! (cdr lst) appendage)))

(define (count-pairs x)
  (define visited (cons 0 '()))

  (define (scount-pairs x)
    (if (pair? x)
      (if (not (in? x visited))
        (begin (append! visited (list x))
               (+ (scount-pairs (car x))
                  (scount-pairs (cdr x))
                  1))
         (+ (scount-pairs (car x))
            (scount-pairs (cdr x))))
      0))

  (scount-pairs x))

(define ret-3 '(2 3 5)) 
(count-pairs ret-3)

(define ret-4 (cons (cons 2 '()) (cons 3 '())))
(set-cdr! (cdr ret-4) (car ret-4))
(count-pairs ret-4)

(define ret-5 (cons (cons 2 '()) (cons 3 '())))
(set-car! (cdr ret-5) (car ret-5))
(set-cdr! (cdr ret-5) (car ret-5))
(count-pairs ret-5)

(define ret-7 (cons (cons 2 '()) (cons 3 '())))
(set-car! (cdr ret-7) (car ret-7))
(set-cdr! (cdr ret-7) (car ret-7))
(set-car! ret-7 (cdr ret-7))
(count-pairs ret-7)

;; could also have a shadow data structure of bools perhaps
