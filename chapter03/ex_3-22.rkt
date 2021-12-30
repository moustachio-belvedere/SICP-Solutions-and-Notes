#!/bin/racket
#lang sicp

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define queue (cons '() '()))

    ;; intermediate getter and mutator functions
    (define (set-front-ptr! item) 
      (set-car! queue item))
    (define (set-rear-ptr! item) 
      (set-cdr! queue item))
    (define (front-ptr) (car queue))
    (define (rear-ptr) (cdr queue))

    ;; queue interface
    (define (empty-queue?)
      (null? (front-ptr)))

    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an
                  empty queue" queue)
          (car (front-ptr))))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               queue)
              (else (set-cdr! (rear-ptr)
                              new-pair)
                    (set-rear-ptr! new-pair)
                    queue))))

    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with 
                     an empty queue" queue))
            (else (set-front-ptr! (cdr (front-ptr)))
                  queue)))

    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            (else (error "Queue method not found!"))))

    dispatch))

(define (empty-queue? q) ((q 'empty-queue?)))
(define (front-queue q) ((q 'front-queue)))
(define (insert-queue! q item) ((q 'insert-queue!) item))
(define (delete-queue! q) ((q 'delete-queue!)))
 
(define q1 (make-queue))

(empty-queue? q1)
(insert-queue! q1 'a)
(empty-queue? q1)
(insert-queue! q1 'b)
(front-queue q1)
(delete-queue! q1)
(delete-queue! q1)
