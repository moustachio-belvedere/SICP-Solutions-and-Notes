#lang sicp
(#%require "utils_symdiff.rkt")

; alternative approach to 2.58,
; parse minimally bracketed infix
; to prefix operator notation

(define (sum? x)
  (and (pair? x) (eq? (cadr x) `+)))

(define (addend s) (car s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) `*)))

(define (multiplier m) (car m))
(define (multiplicand m) (caddr m))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        (else (error "unknown expression 
                      type: DERIV" exp))))

(define (operator? x)
  (or (eq? x `+)
      (eq? x `*)))

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

(define (deriv-infix e var)
  (deriv (l1-wrap e) var))

(deriv-infix `(x + 3 * (x + y + 2)) `x)
(deriv-infix `(3 * (x + y + 2) + x) `x)
