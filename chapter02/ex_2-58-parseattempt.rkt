#lang sicp
(#%require "utils_symdiff.rkt")

; alternative approach to 2.58,
; parse minimally bracketed infix
; to prefix operator notation

(define (operator? x)
  (or (eq? x `+)
      (eq? x `*)))

; turns infix pairs into prefix pairs
(define (parse-l0 e)
  (list (cadr e)
        (if (pair? (car e))
            (parse-l0 (car e))
            (car e))
        (if (pair? (caddr e))
            (parse-l0 (caddr e))
            (caddr e))))

(define (parse-l1 e)
  (define (naked? exp op)
    (let ((pseudo-op (cadr exp)))
      (and (not (number? pseudo-op))
           (eq? pseudo-op op))))

  (define (bracketise lhs rhs op1 op2 op)
    (append lhs
            (cons (list (if (pair? op1) (parse-l1 op1) op1)
                        op
                        (if (pair? op2) (parse-l1 op2) op2))
                        `())
            rhs))

  (define (op-parse e acc op)
    (cond ((null? e) acc)
          ((and (not (null? (cdr e))) (naked? e op))
            (parse-l1 (bracketise acc (cdddr e) (car e) (caddr e) (cadr e))))
          (else (if (null? (cdr e))
                  (op-parse (cdr e) (append acc (list (car e))) op)
                  (op-parse (cddr e) (append acc (list (car e) (cadr e))) op)))))

  (define (l1-iter e)
    (op-parse (op-parse e `() `*)
              `()
              `+))

  (if (= (length e) 3)
    e
    (l1-iter e)))

(define (l1-wrap e)
  (car (parse-l1 e)))

(define (parse e)
  (parse-l0 (l1-wrap e)))

(define test `(a + b + x * y * (j + k * l)))
;(parse `(a * (b + x) + 5 * z))
;(parse `((a + b) * x + z * 4))
