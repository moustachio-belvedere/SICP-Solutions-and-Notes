#!/bin/racket
#lang sicp
(#%require "utils_constraint.rkt")

(define a (make-connector))
(define b (make-connector))

(define square
  (lambda (x) (* x x)))

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: 
                    SQUARER" 
                   (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        ;; funciton only triggered on new val so
        ;; no check for (has-value? a) required
        (set-value! b (square (get-value a)) me)))

  (define (process-forget-value) 
    ;; no need for additional
    ;; (process-new-value) call
    ;; as there are only two,
    ;; directly connected nodes
    (forget-value! a me)
    (forget-value! b me))

  (define (me request) 
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: 
                        SQUARER" request))))

  (connect a me)
  (connect b me)
  me)

(probe "a" a)
(probe "b" b)
(squarer a b)

;; test forward
(set-value! a 2 'user) ;; b -> 4

;; test backward
(forget-value! a 'user)
(set-value! b 25 'user) ;; a -> 5
