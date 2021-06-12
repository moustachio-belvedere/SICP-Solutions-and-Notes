#lang sicp

(define (va a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

; let va = +
; va 3 7
; (inc (+ (dec 3) 7))
; (inc (+ 2 7))
; (inc (inc (+ (dec 2) 7)))
; (inc (inc (+ 1 7)))
; (inc (inc (inc (+ (dec 1) 7))))
; (inc (inc (inc (+ 0 7))))
; (inc (inc (inc 7)))
; (inc (inc 8))
; (inc 9)
; 10

; needs a stack, no state
; => recursive process


(define (vb a b)
  (if (= a 0)
      b
      (vb (dec a) (inc b))))
; let vb = +
; (+ 3 7)
; (+ (dec 3) (inc 7))
; (+ 2 8)
; (+ (dec 2) (inc 8))
; (+ 1 9)
; (+ (dec 1) (inc 9))
; (+ 0 10)
; 10

; stateful, no deferred operation chain
; => iterative process

; userful hint from main solutions wiki:
; The easiest way to spot that the first process is recursive (without writing out the substitution) is to note that the "+" procedure calls itself at the end while nested in another expression; the second calls itself, but as the top expression.

