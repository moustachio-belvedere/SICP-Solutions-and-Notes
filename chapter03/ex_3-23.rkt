#!/bin/racket
#lang sicp

;; O(1) deque implementation
;; requires something akin to a
;; doubly-linked list for reverse
;; direction traversal

;; lower level
;; q-part is queue, h-part is the wayback
(define (q-part q) (car q))
(define (h-part q) (cdr q))

(define (qfront-ptr q) (if (pair? (q-part q))
                          (car (q-part q))
                          '()))
(define (qrear-ptr q) (if (pair? (q-part q))
                         (cdr (q-part q))
                         '()))
(define (set-qfront-ptr! q item) 
  (set-car! (q-part q) item))
(define (set-qrear-ptr! q item) 
  (set-cdr! (q-part q) item))
(define (qnullify-rear! q)
  (set-cdr! (qrear-ptr q) '()))

(define (hfront-ptr q) (if (pair? (h-part q))
                          (car (h-part q))
                          '()))
(define (hrear-ptr q) (if (pair? (h-part q))
                         (cdr (h-part q))
                         '()))
(define (set-hfront-ptr! q item) 
  (set-car! (h-part q) item))
(define (set-hrear-ptr! q item) 
  (set-cdr! (h-part q) item))
(define (hnullify-rear! q)
  (set-cdr! (hrear-ptr q) '()))

(define (set-hist! q item)
  (set-hfront-ptr! q item))

(define (add-hfront! q item)
  (if (null? (hfront-ptr q))
    (let ((new-hist (cons item '())))
      (set-hfront-ptr! q new-hist)
      (set-hrear-ptr! q new-hist))
    (let ((new-hist (cons item (hfront-ptr q))))
      (set-hfront-ptr! q new-hist))))

(define (add-hrear! q item)
  ; handle init case
  (let ((new-hist (cons item '())))

  (if (null? (hfront-ptr q))
      (set-cdr! q (cons new-hist new-hist))
      (begin
        (set-cdr! (hrear-ptr q) (cons item '()))
        (set-hrear-ptr! q (cdr (hrear-ptr q)))))))

(define (pop-hist! q)
  (let ((most-recent (car (hfront-ptr q))))
    (set-hist! q (cdr (hfront-ptr q)))
    most-recent))

(define (qlend q)
  (eq? (qfront-ptr q) (qrear-ptr q)))

(define (null-all q)
  (set-hfront-ptr! q '())
  (set-hrear-ptr! q '())
  (set-qfront-ptr! q '())
  (set-qrear-ptr! q '()))

;; user interface
(define (make-deque) (cons (cons '() '()) (cons '() '())))

(define (empty-deque? q) 
  (null? (qfront-ptr (q-part q))))

(define (front-deque q)
  (if (empty-deque? q)
      (error "FRONT called with an 
              empty q" q)
      (car (qfront-ptr q))))

(define (rear-deque q)
  (if (empty-deque? q)
      (error "REAR called with an 
              empty q" q)
      (car (qrear-ptr q))))

(define (front-insert-deque! q item)
  (let ((new-pair (cons item '())))
    (let ((new-hist (cons new-pair '())))
    (cond ((empty-deque? q)
           (set-qfront-ptr! q new-pair)
           (set-qrear-ptr! q new-pair)
           q)
          (else (set-qfront-ptr! q (cons item (qfront-ptr q)))
                (add-hrear! q (qfront-ptr q))
                q)))))

(define (rear-insert-deque! q item)
  (let ((new-pair (cons item '())))
    (let ((new-hist (cons new-pair '())))
    (cond ((empty-deque? q)
           (set-qfront-ptr! q new-pair)
           (set-qrear-ptr! q new-pair)
           q)
           (else (add-hfront! q (qrear-ptr q))
                 (set-cdr! (qrear-ptr q)
                           new-pair)
                 (set-qrear-ptr! q new-pair)
                 q)))))

(define (front-delete-deque! q)
  (cond ((empty-deque? q)
         (error "FRONT-DELETE! called with 
                 an empty q" q))
        ((qlend q)
               (null-all q)
               q)
        (else (set-qfront-ptr! 
               q 
               (cdr (qfront-ptr q)))
              q)))

(define (rear-delete-deque! q)
  (cond ((empty-deque? q)
         (error "REAR-DELETE! called with 
                 an empty q" q))
        ((qlend q)
               (null-all q)
               q)
        (else (set-qrear-ptr!
               q
               (pop-hist! q))
              (qnullify-rear! q)
              q)))

(define (showq q)
  (car (q-part q)))

(define q1 (make-deque))
;(empty-deque? q1)

;; rear insert tests
;(rear-insert-deque! q1 'b)
;(front-deque q1)
;(rear-deque q1)
;(rear-insert-deque! q1 'c)
;(rear-insert-deque! q1 'd)
;(rear-insert-deque! q1 'e)
;(rear-delete-deque! q1)
;(rear-delete-deque! q1)
;(rear-delete-deque! q1)
;(rear-delete-deque! q1)
;(rear-insert-deque! q1 'c)
;(rear-insert-deque! q1 'd)
;(rear-insert-deque! q1 'e)

;; front insert tests
(front-insert-deque! q1 'd)
(front-insert-deque! q1 'c)
(front-insert-deque! q1 'b)
(front-insert-deque! q1 'a)
(rear-insert-deque! q1 'e)
(rear-insert-deque! q1 'f)
(front-delete-deque! q1)
(front-delete-deque! q1)
(front-delete-deque! q1)
(front-delete-deque! q1)
(front-delete-deque! q1)
(front-insert-deque! q1 'g)
(newline)

;;; front-delete tests
;(front-delete-deque! q1)
;(front-deque q1)
;(rear-deque q1)
;(front-delete-deque! q1)
;(front-deque q1)
;(rear-deque q1)
;(front-delete-deque! q1)
;(newline)
;
;;; rear-delete tests
;(rear-insert-deque! q1 'b)
;(rear-insert-deque! q1 'c)
;(front-insert-deque! q1 'a)
