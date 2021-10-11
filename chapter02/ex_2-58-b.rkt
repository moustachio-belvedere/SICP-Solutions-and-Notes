#lang sicp
(#%require "utils_symdiff.rkt")

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

(define (sum? x)
  (and (pair? x) (eq? (cadr x) `+)))

(define (addend s) (car s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) `*)))

(define (multiplier m) (car m))
(define (multiplicand m) (caddr m))

(define tester `(x + (3 * (x + (y + 2)))))
(deriv tester `x)
