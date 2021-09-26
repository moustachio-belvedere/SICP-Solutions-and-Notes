#lang sicp
(#%require "utils_symdiff.rkt")
(#%require "ex_2-58_utils.rkt")

(define (not-operator x)
  (not (or (eq? x `+)
           (eq? x `*))))

; simplest case, two operands, fully bracketed
; assumes there is at least one operation with two operands
; e.g. `(x + 5)
;(define (parse-l0 e)
  ;(define (e-iter prefix infix)
    ;(cond ((null? infix)
            ;prefix)
          ;((pair? (car infix))
           ;(e-iter (append prefix (parse-l0 (car infix)))
                   ;(cdr infix)))
          ;((not-operator (car infix))

(define (parse-l0 e)
  (list (cadr e)
        (if (pair? (car e))
            (parse-l0 (car e))
            (car e))
        (if (pair? (caddr e))
            (parse-l0 (caddr e))
            (caddr e))))

(define (parse-l1 e)
(parse-l0 `(x + (5 * 15)))
              
          

              


