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
  ; groups stratified operations into pairs
  (define (naked? exp op)
    (let ((pseudo-op (cadr exp)))
      (and (not (number? pseudo-op))
           (eq? pseudo-op op))))

  ; by example:
  ; (bracketise (1 +) () 5 7 `*)
  ; (1 + 5 * 7) => (1 + (5 * 7))
  (define (bracketise lhs rhs op1 op2 op)
    (cond ((and (pair? op1) (pair? op2))
            (append lhs
                    (cons (list (parse-l1 op1) op (parse-l1 op2)) rhs)))
          ((pair? op1)
            (append lhs
                    (cons (list (parse-l1 op1) op op2) rhs)))
          ((pair? op2)
            (append lhs
                    (cons (list op1 op (parse-l1 op2)) rhs)))
          (else
            (append lhs (cons (list op1 op op2) rhs)))))

  (define (l1-iter e acc)
    (display acc)
    (newline)
    (cond ((null? e) acc)
          ;((operator? (car e))
          ;  (l1-iter (cdr e) (append acc (list (car e)))))
          ((and (not (null? (cdr e))) (naked? e `*))
            (parse-l1 (bracketise acc (cdddr e) (car e) (caddr e) (cadr e))))
          ;((and (not (null? (cdr e))) (naked? e `+))
          ;  (parse-l1 (bracketise acc (cdddr e) (car e) (caddr e) (cadr e))))
          ))

  (if (= (length e) 3)
    e
    (l1-iter e `())))

(define (parse e)
  (parse-l0 (parse-l1 e)))

(define test `(a + b + x * y * (j + k * l)))
(parse-l1 test)

