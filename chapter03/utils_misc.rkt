#lang sicp
(#%provide flatten-list
           appendl)

;; added this just as a reminder of how it worked
(define (appendl l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (appendl (cdr l1) l2))))

;; racketlang implementation doesn't play nicely
;; with SICP-lang mutable pairs, so use this little
;; implementation where needed.
(define (flatten-list l)
  (cond ((null? l) '())
        ((pair? (car l)) (appendl (flatten-list (car l))
                                  (flatten-list (cdr l))))
        (else (cons (car l) (flatten-list (cdr l))))))
