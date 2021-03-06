#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(newline)
(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(newline)
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))
(newline)

; for fold-right and fold-left to be guaranteed equal, the `op` must be commutable (and associative?)
