#lang sicp

; values printed out automatically 
; by running `racket ./ex_1-1.rkt` from
; shell

10

(+ 5 3 4)

(- 9 1)

(/ 6 2)

(+ (* 2 4) (- 4 6))

(define a 3) ; no output

(define b (+ a 1)) ; no output

(+ a b (* a b))

(= a b) ; #f for false

(if (and (> b a) (< b (* a b))) ; true and true, so output b
    b
    a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

(+ 2 (if (> b a) b a))

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))


