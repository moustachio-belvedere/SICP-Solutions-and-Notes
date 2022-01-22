#!/bin/racket
#lang sicp

(define (make-table same-key?)

  (define (assoc key records)
  (cond ((null? records) false)
        ((same-key? key (caar records)) 
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

; example of usage
(define (approx-key? sought provided)
  (cond ((and (number? sought) (number? provided))
         (< (abs (- sought provided)) 0.01))
        (else (equal? sought provided))))

(define x (make-table approx-key?))
(define get (x 'lookup-proc))
(define put (x 'insert-proc!))

; test symbol lookup behaves as before
(put 'y "hiya")
(get 'y)
(get 'z)

; test numeric key lookup
(put 0.1 "a number")
(get 0.1)
(get 0.11)
(get 0.12)
