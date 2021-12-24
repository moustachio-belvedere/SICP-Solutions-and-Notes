#!/bin/racket
#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

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

(set-car! (car ret-7) (cdr ret-7))
(set-cdr! (car ret-7) (cdr ret-7))

(count-pairs ret-7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commented out so script above can be run without ;;
;; entering infinite loop                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define ret-never (cons (cons 2 '()) (cons 3 '())))
;; (set-car! (cdr ret-never) ret-never)
;; (count-pairs ret-never)
