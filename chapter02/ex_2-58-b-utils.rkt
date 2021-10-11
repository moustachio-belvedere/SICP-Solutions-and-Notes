#lang sicp
(#%require "utils_symdiff.rkt")

; my implementations of variadic make-sum and make-prod
; are slightly unwieldy looking, but they a handle a number
; of interesting edge cases.

; any numbers that can be accumulated, are.

; if any accumulated numeric mutliplier is equal to 1, it is ignored.
; if any accumulated numeric addition term is equal to 0, it is ignored.

(define (augend s)
  (let ((out (cddr s)))
    (if (= (length out) 1)
        (car out)
        (append (list `+) out))))

(define (make-sum . ax)
  (define (sum-iter terms symbols num-tally)
    (cond ((null? terms)
           (if (= num-tally 0)
               symbols
               (append symbols (list num-tally))))
          ((number? (car terms))
           (sum-iter (cdr terms)
                     symbols
                     (+ (car terms) num-tally)))
          ((pair? (car terms))
           (if (sum? (car terms))
               (sum-iter (cdr (append terms (cdr (car terms))))
                         symbols
                         num-tally)
               (sum-iter (cdr terms)
                         (append symbols (list (car terms)))
                         num-tally)))
          ((variable? (car terms))
           (sum-iter (cdr terms)
                     (append symbols (list (car terms)))
                     num-tally))))
  (let ((out (append (list `+) (sum-iter ax `() 0))))
    (if (= (length out) 2)
        (cadr out)
        out)))

(define (multiplicand p)
  (let ((out (cddr p)))
    (if (= (length out) 1)
        (car out)
        (append (list `*) out))))

(define (make-product . mx)
  (define (prod-iter terms symbols num-tally)
    (cond ((null? terms)
           (cond ((= num-tally 1)
                  (if (null? symbols)
                      (list 1)
                      symbols))
                 ((= num-tally 0) (list 0))
                 (else (append (list num-tally) symbols))))
          ((number? (car terms))
           (prod-iter (cdr terms)
                      symbols
                      (* (car terms) num-tally)))
          ((pair? (car terms))
           (if (product? (car terms))
               (prod-iter (cdr (append terms (cdr (car terms))))
                          symbols
                          num-tally)
               (prod-iter (cdr terms)
                          (append symbols (list (car terms)))
                          num-tally)))
          ((variable? (car terms))
           (prod-iter (cdr terms)
                      (append symbols (list (car terms)))
                      num-tally))))
  (let ((out (append (list `*) (prod-iter mx `() 1))))
    (if (= (length out) 2)
        (if (pair? (cdr out))
            (cadr out)
            out)
        out)))

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
