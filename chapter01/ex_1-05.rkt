#lang sicp

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

; applicate order will enter infinite loop as it tries
; to evalute (p)
; normal order 'lazy' evaluation won't enter infinite loop
; as it never tries to evaluate (p) because the branch
; that would require that is not taken.

