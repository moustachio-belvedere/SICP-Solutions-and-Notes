#!/bin/racket
#lang sicp

;; main idea of algorithm:
;; if the previous pair is a descendent of the current
;; pair, then there is a cyclical path in the graph.
;;
;; only previous pair needs to be stored so memory
;; usage is constant.

(define (iter-helper iters)
  (let ((iter-count 0))
    (lambda ()
      (set! iter-count (+ iter-count 1))
      (if (> iter-count iters)
          (error "too many iterations!")
          #t))))

(define (in? x lst)
  (define errcount (iter-helper 20))

  (define (in?-apply x lst)
;    (display "in?\n")
;    (display x)
;    (newline)
;    (display lst)
;    (newline)
;    (newline)
    (errcount)
    (cond ((null? lst) #f)
          ((null? x) #f)
          ((not (pair? lst)) #f)
          ((eq? x (car lst)) #t)
          ((eq? x (cdr lst)) #t)
          (else (or (in?-apply x (car lst))
                    (in?-apply x (cdr lst))))))

  (in?-apply x lst))

(define (is-cyclical? lst)
  (define previous '())

  ;; useful for debugging
  (define errcount (iter-helper 100))

  (define (cycle-find lst)
;    (display "CYCLE FIND\n")
;    (display lst)
;    (newline)
;    (newline)

    (errcount)

    (cond ((null? lst) #f)
          ((not (pair? lst)) #f)
          (else (if (in? previous lst)
                    #t
                    (begin (set! previous lst)
                           (or (cycle-find (car lst))
                               (cycle-find (cdr lst))))))))

  (cycle-find lst))

(define x (list 1))
(is-cyclical? x)
(set-cdr! x x)
(is-cyclical? x)

(define y (list 1 (list 2 (list 3))))
(is-cyclical? y)
(set-cdr! (cdadr y) y)
(is-cyclical? y)

(define z (cons (cons 2 '()) (cons 3 '())))
(is-cyclical? z)
(set-car! (cdr z) z)
(is-cyclical? z)

(define t1 (cons 'a 'b)) 
(define t2 (cons t1 t1))
(is-cyclical? t2)

;; this test doesn't work as it enters an
;; infinite loop inside the `in?` function
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define k '(d e f)) 
(set-cdr! (last-pair k) (cdr k))
; (is-cyclical? k)
