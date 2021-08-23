#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x) (if (pair? x)
                                 (count-leaves x)
                                 1))
                   t)))

; 10 leaves
(define x (list 9 5 2 (list 1 2 (list 5 3) 3 2) 1))

(display x)
(newline)
(count-leaves x)
