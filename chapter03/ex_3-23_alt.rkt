#!/bin/racket
#lang sicp

;; much cleaner implementation of the
;; doubly linked list approach

;; utils
(define (front-ptr q) (car q))
(define (rear-ptr q) (cdr q))
(define (set-front-ptr! q item) 
  (set-car! q item))
(define (set-rear-ptr! q item) 
  (set-cdr! q item))
(define (q-len-1 q)
  (eq? (front-ptr q) (rear-ptr q)))
(define (null-all q)
  (set-front-ptr! q '())
  (set-rear-ptr! q '()))
(define (sq q) ;; for pretty-printing.
  (map (lambda (v) (car v)) (front-ptr q)))

;; user interface
(define (make-deque) (cons '() '()))

(define (empty-deque? q)
  (null? (front-ptr q)))

(define (front-deque q)
  (if (empty-deque? q)
      (error "FRONT called with an 
              empty q" q)
      (caar (front-ptr q))))

(define (rear-deque q)
  (if (empty-deque? q)
      (error "REAR called with an 
              empty q" q)
      (caar (rear-ptr q))))

(define (front-insert-deque! q item)
  (let ((new-pair (cons (cons item '()) (front-ptr q))))
    (cond ((empty-deque? q)
           (set-front-ptr! q new-pair)
           (set-rear-ptr! q new-pair)
           q)
          (else (set-cdr! (car (front-ptr q)) new-pair)
                (set-front-ptr! q new-pair)
                q))))

(define (rear-insert-deque! q item)
  (let ((new-pair (cons (cons item (rear-ptr q)) '())))
    (cond ((empty-deque? q)
           (set-front-ptr! q new-pair)
           (set-rear-ptr! q new-pair)
           q)
           (else (set-cdr! (rear-ptr q) new-pair)
                 (set-rear-ptr! q new-pair)
                 q))))

(define (front-delete-deque! q)
  (cond ((empty-deque? q)
         (error "FRONT-DELETE! called with 
                 an empty q" q))
        ((q-len-1 q) (null-all q) q)
        (else (set-front-ptr! q (cdr (front-ptr q)))
              (set-cdr! (car (front-ptr q)) '())
               q)))

(define (rear-delete-deque! q)
  (cond ((empty-deque? q)
         (error "REAR-DELETE! called with 
                 an empty q" q))
        ((q-len-1 q) (null-all q) q)
        (else (set-rear-ptr! q (cdar (rear-ptr q)))
              (set-cdr! (rear-ptr q) '())
              q)))

;; test fixture
(define q1 (make-deque))

;; insert and front/rear getter tests
;(front-insert-deque! q1 'b)
;(front-insert-deque! q1 'a)
;(rear-insert-deque! q1 'c)
;(rear-insert-deque! q1 'd)
;(sq q1)
;(front-deque q1)
;(rear-deque q1)

;; deletion tests
;(front-insert-deque! q1 'd)
;(front-insert-deque! q1 'c)
;(front-insert-deque! q1 'b)
;(front-insert-deque! q1 'a)
;(rear-insert-deque! q1 'a)
;(rear-insert-deque! q1 'b)
;(rear-insert-deque! q1 'c)
;(rear-insert-deque! q1 'd)
;(sq q1)
;(front-delete-deque! q1)
;(sq q1)
;(front-delete-deque! q1)
;(sq q1)
;(front-delete-deque! q1)
;(sq q1)
;(front-delete-deque! q1)
;(sq q1)

;(front-insert-deque! q1 'd)
;(front-insert-deque! q1 'c)
;(front-insert-deque! q1 'b)
;(front-insert-deque! q1 'a)
(rear-insert-deque! q1 'a)
(rear-insert-deque! q1 'b)
(rear-insert-deque! q1 'c)
(rear-insert-deque! q1 'd)
(sq q1)
(rear-delete-deque! q1)
(sq q1)
(rear-delete-deque! q1)
(sq q1)
(rear-delete-deque! q1)
(sq q1)
(rear-delete-deque! q1)
(sq q1)
