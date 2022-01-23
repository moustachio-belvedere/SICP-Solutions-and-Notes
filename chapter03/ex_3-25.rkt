#!/bin/racket
#lang sicp

(define (is-table? record)
  (if (pair? record)
    (if (pair? (cdr record))
        (equal? (cadr record) '*table*)
        false)
    false))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) 
         (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))

    (define (lookup cur-table keys)
      (let ((record (assoc (car keys)
                           (cdr cur-table))))
        (if record
            (if (null? (cdr keys))
                record
                (if (is-table? record)
                    (lookup (cdr record) (cdr keys))
                    false))
            false)))

    ;; 4 paths
    ;; - on last key, so insert at current table dimension
    ;; - table path exists, enter recursively
    ;; - no table path exists, either 
    ;;      * entry exists but is not a table or
    ;;      * entry is non-existent
    (define (create-insert! cur-table keys value)
      (let ((node (assoc (car keys)
                         (cdr cur-table))))
      (cond ((null? (cdr keys))
               (set-cdr! cur-table
                 (cons (cons (car keys) value)
                       (cdr cur-table))))
            ((is-table? node)
               (create-insert! (cdr node) (cdr keys) value))
            ((and node (not (is-table? node)))
               (set-cdr! node (list '*table*))
               (create-insert! (cdr node) (cdr keys) value))
            ((not node)
               (set-cdr! cur-table
                 (cons (cons (car keys) (list '*table*))
                       (cdr cur-table)))
               (create-insert! cur-table keys value)))))

    (define (insert! cur-table keys value)
      (let ((record (lookup cur-table keys)))
        (if record
            (set-cdr! record value)
            (create-insert! cur-table keys value)))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) (lambda (keys)
                                    (cdr (lookup local-table keys))))
            ((eq? m 'insert-proc!) (lambda (keys value)
                                    (insert! local-table keys value)))
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))

(define x (make-table))
(define get (x 'lookup-proc))
(define put (x 'insert-proc!))

;; 3 test cases
;; put in nested node where no new tables need to be created.
;; put in nested node where new tables must be created.
;; replace existing leaf node (value record) with a table
;;         if nesting is required.
(put '(a) "hiya")
(get '(a))

(put '(a b) "holler")
(get '(a b))
(get '(a))

(put '(a c) "woah")
(get '(a c))

(put '(c) "nice")
(get '(c))

(put '(d e) "where")
(get '(d e))

(put '(d z a b) "nice")
(get '(d z a b))
