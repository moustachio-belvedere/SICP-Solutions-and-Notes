#!/bin/racket
#lang sicp
(#%require "utils_tree.rkt")

;; a messy-but-quick implementation of insertion
;; and lookup of a table that uses a set for data
;; storage.
(define (make-table)

  (let ((local-table (make-tree '*table* '() '())))

    (define (lookup-internal key)
      (lookup key local-table))

    (define (insert! key value)
      (let ((record (lookup-internal key)))
        (if record
           (set-cdr! record value)
           (set! local-table
                 (adjoin-set (cons key value)
                   local-table))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) (lambda (key)
                                    (let ((result (lookup-internal key)))
                                      (if result
                                          (cdr result)
                                          result))))
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))

(define x (make-table))
(define get (x 'lookup-proc))
(define put (x 'insert-proc!))

(put "a" "ahiya")
;; (*table* ((a . ahiya) () ()) ())
(put "c" "chiya")
;; (*table* ((a . ahiya) () ((c . chiya) () ())) ())
(get "a")
(get "c")
(get "d")
(put "b" "bhiya")
;; (*table* ((a . ahiya) () ((c . chiya) ((b . bhiya) () ()) ())) ())
(get "b")
