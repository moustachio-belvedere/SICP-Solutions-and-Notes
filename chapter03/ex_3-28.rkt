#!/bin/racket
#lang sicp

(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal o1) 
                        (get-signal o2))))
      (after-delay 
       or-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure)
  'ok)

;; a simple 'logical-or' implementation
;; without any error handling like the
;; book's 'logical-not' function.
(define (logical-or x y)
  (cond ((= (x 1)) 1)
        ((= (y 1)) 1)
        (else 0)))
