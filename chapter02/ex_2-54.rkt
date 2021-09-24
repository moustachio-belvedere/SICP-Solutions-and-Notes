#lang sicp

(define (equal? i1 i2)
  (cond ((null? i1) #t)
        ((pair? (car i1)) (equal? (car i1) (car i2)))
        ((eq? (car i1) (car i2)) (equal? (cdr i1) (cdr i2)))
        (else #f)))

(equal? `(hey yo) `(hey yo)) ; #t
(equal? `(hello (why not NO) absolutely)
        `(hello (why not NO) absolutely)) ; #t
(equal? `(hello (why (see three) not NO) absolutely)
        `(hello (why (see three) not NO) absolutely)) ; #t
(equal? `(hello (why not YES) absolutely)
        `(hello (why not NO) absolutely)) ; #f
