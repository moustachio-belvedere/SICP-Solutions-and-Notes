#! /bin/racket
#lang sicp

(define (cond? exp)
  (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause)
  (car clause))

(define (cond-actions clause)
  (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (recipe-clause? clause)
  (eq? (cadr clause) '=>))

(define (rconsequent clause)
  (cddr clause))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF" clauses))
            (make-if (cond-predicate first)
                     (if (recipe-clause? first)
                         (list (rconsequent first) (cond-predicate first))
                         (sequence->exp
                          (cond-actions first)))
                     (expand-clauses
                      rest))))))
