#!/bin/racket
#lang sicp

(define (make-table)

  (define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) 
         (car records))
        (else (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))

    (define (lookup key)
      (let ((record (assoc key
                           (cdr local-table))))
        (if record (cdr record) false)))

    (define (insert! key value)
      (let ((record 
             (assoc key 
                    (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! 
             local-table
             (cons (cons key value)
                   (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))

(define x (make-table))
(define get (x 'lookup-proc))
(define put (x 'insert-proc!))

(put 'y "hiya")
(get 'y)
(get 'z)
