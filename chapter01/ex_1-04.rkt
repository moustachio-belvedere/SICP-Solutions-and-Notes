#lang sicp

; operator is conditional to provide
; a short-cut to adding absolute value
; of b.

; if b negative, the property that
; '-(-b) == +b' is used.
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 2 3)  ; 5
(a-plus-abs-b 2 -3) ; 5

