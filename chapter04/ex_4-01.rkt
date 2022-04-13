#! /bin/racket
#lang sicp

;; a bit tricky to test properly, so doing a PoC first:
(define (list-of-values exps)
  (if (null? exps)
      '()
      (cons (tfun (car exps))
            (list-of-values (cdr exps)))))

;; convenience function for it's display side-effect
(define (tfun x)
  (display x)
  (display ", ")
  x)

(define test (list 1 2 3 4 5))
(define out (list-of-values test))
;; original is l2r as Racket is l2r
(newline)
(display out)
(newline)
(newline)

;; left-to-right enforced
(define (list-of-vals-l2r exps)
  (define (lv-iter acc rem)
    (if (null? rem)
        acc
        (lv-iter (append acc (list (tfun (car rem)))) (cdr rem))))
  (lv-iter '() exps))

(define l2r-out (list-of-vals-l2r test))
(newline)
(display l2r-out)
(newline)
(newline)

;; right-to-left enforced
(define (list-of-vals-r2l exps)
  (define (lv-iter acc rem)
    (if (null? rem)
        acc
        (lv-iter (cons (tfun (car rem)) acc) (cdr rem))))
  (lv-iter '() (reverse exps)))

(define r2l-out (list-of-vals-r2l test))
(newline)
(display r2l-out)
(newline)
(newline)

;; all three have same output
;; but second implementation enforces l2r evaluation
;; and third implementation enforces r2l evaluation
;;
;; applying this to the actual function, l2r:
; (define (list-of-values exps env)
;   (define (lv-iter acc rem)
;     (if (null? rem)
;         acc
;         (lv-iter (append acc (list (eval (first-operand rem) env)))
;                  (rest-operands rem))))
;   (lv-iter '() exps))
;
; ;; r2l, assumes some function `reverse-operands` exists
; ;; which reverses the order of the operands
; (define (list-of-values exps env)
;   (define (lv-iter acc rem)
;     (if (null? rem)
;         acc
;         (lv-iter (cons (eval (first-operand rem) env) acc)
;                  (rest-operands rem))))
;   (lv-iter '() (reverse-operands exps)))
