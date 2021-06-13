#lang sicp

; compute element r of row n on Pascal's Triangle
(define (pascal n r)
  (cond ((or (= r 0)
             (= n 0)
             (= n r))
         1)
  (else
  (+ (pascal (- n 1) (- r 1))
     (pascal (- n 1) r)))))

; print test, left aligned for simplicity
(define (print-row n)
  (define (print-call r)
    (display (pascal n r))
    (display ", ")
    (print-iter (+ r 1)))

  (define (print-iter r)
    (if (<= r n) (print-call r)
        (display "\n")))
        
  (print-iter 0))

; inspect output
(print-row 0)
(print-row 1)
(print-row 2)
(print-row 3)
(print-row 4)
(print-row 5)
(print-row 6)
(print-row 7)
