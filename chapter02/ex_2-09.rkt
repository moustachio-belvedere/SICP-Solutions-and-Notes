#lang sicp

; w0 = (u0 - l0)/2
; w1 = (u1 - l1)/2
 
;   w_sum
; = ((u0 + u1) - (l0 + l1))/2
; = (u0 + u1)/2 - (l0 + l1)/2
; = (u0 - l0)/2 + (u1 - l1)/2
; = w0 + w1

; very similar derivation for subtraction

; an example of when it doesn't work for mutliplication (follow similar path for division proof)
;   w_prod
; = (u0*u1 - l0*l1)/2

; which cannot be rearranged to form w0 and w1 separately

