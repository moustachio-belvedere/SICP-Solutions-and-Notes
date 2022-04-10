#! /bin/racket
#lang sicp

;; a) the plan to test for applications first will fail mainly
;;    because special forms are not applications. E.g.
;;    - (define a 2) not request to call function `define` with two args
;;    - (lambda (x) x) not request to call function `lambda` with two operands
;;
;;    the current test `application?` cannot detect these special forms
;;    without help from the prior `if` clauses.

;; b) strategy can be achieved by explicitly denoting function application
;;    requests with a 'call keyword. Essentially, introducing a new special
;;    form? We just have to change the `application?` test and associated
;;    functions.

(define (application? exp) (tagged-list exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
