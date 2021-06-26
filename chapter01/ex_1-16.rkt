#lang sicp


; utility functions
(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
     (* x x))


; recursive implementation (from book)
(define (rec-exp b n)
  (cond ((= n 0) 1)
        ((even? n) (square (rec-exp b (/ n 2))))
        (else (* b (rec-exp b (- n 1))))))


; iterative implementation
(define (iter-exp b n)
  ; used frequently
  (define sb (square b))

  ; iterator func, a is a state variable
  (define (iterator b n a u)
    (cond ((= n 0) a)
          ((= a 1) (iterator b
                             (- n 1)
                             (* b a)
                             (+ u 1)))
          ((<= (* 2 u) (+ u n)) (iterator b
                                          (- n u)
                                          (square a)
                                          (* 2 u)))
          ((>= n 2) (iterator b
                              (- n 2)
                              (* a sb)
                              (+ u 2)))
          (else (iterator b
                          (- n 1)
                          (* a b)
                          (+ u 1)))))
                          

  ; initialise iterator
  (iterator b n 1 0))

; much simpler + more efficient solution seen online after above soln.
(define (fexp-iter b n)
  (define (fast-expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter (square b) (/ n 2) a))
          (else (fast-expt-iter b (- n 1) (* a b)))))

  (fast-expt-iter b n 1))

; tests
(display "\nRecursive tests:\n")
(rec-exp 2 9)
(rec-exp 2 10)
(rec-exp 2 11)
(rec-exp 2 12)
(rec-exp 2 13)
(rec-exp 2 14)
(rec-exp 2 15)
(rec-exp 2 16)
(rec-exp 5 9)
(rec-exp 5 10)
(rec-exp 5 11)
(rec-exp 5 12)
(rec-exp 5 13)
(rec-exp 5 14)
(rec-exp 5 15)
(rec-exp 5 16)

(display "\nIterative tests:\n")
(iter-exp 2 9)
(iter-exp 2 10)
(iter-exp 2 11)
(iter-exp 2 12)
(iter-exp 2 13)
(iter-exp 2 14)
(iter-exp 2 15)
(iter-exp 2 16)
(iter-exp 5 9)
(iter-exp 5 10)
(iter-exp 5 11)
(iter-exp 5 12)
(iter-exp 5 13)
(iter-exp 5 14)
(iter-exp 5 15)
(iter-exp 5 16)

(display "\nAlt method from online solutions:\n")
(fexp-iter 2 9)
(fexp-iter 2 10)
(fexp-iter 2 11)
(fexp-iter 2 12)
(fexp-iter 2 13)
(fexp-iter 2 14)
(fexp-iter 2 15)
(fexp-iter 2 16)
(fexp-iter 5 9)
(fexp-iter 5 10)
(fexp-iter 5 11)
(fexp-iter 5 12)
(fexp-iter 5 13)
(fexp-iter 5 14)
(fexp-iter 5 15)
(fexp-iter 5 16)

