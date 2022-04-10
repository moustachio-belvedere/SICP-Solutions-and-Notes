#! /bin/racket
#lang sicp

;; assuming set-up from ex_3-03.rkt
(install! 'and eval-and)
(install! 'or eval-or)

;; special-forms approach
(define (eval-end exp env)
  (define (and-iter ops prev)
    (if (no-operands? ops)
        prev
        (let ((cur (eval (first-operand ops) env)))
          (if (true? cur)
              (and-iter (rest-operands ops) cur)
              #f))))
  (and-iter (operands exp) env #t))

(define (eval-or exp env)
  (define (or-iter ops)
    (if (no-operands? ops)
        #f
        (if (eval (first-operand ops) env)
            #t
            (or-iter (rest-operands ops)))))
  (or-iter (operands exp)))

; ;; derived expressions?
; (define (make-if predicate consequent alternative)
;   (list 'if predicate consequent alternative))
;
; ;; assume 'true and 'false are implemented booleans
; (define (eval-and exp env)
;   (define (and-rec ops prev)
;     (make-if             prev
;              (make-if (first-operand ops)
;                       (and-rec (rest-operands
;   (make-if (
