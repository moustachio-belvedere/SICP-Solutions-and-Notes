#lang sicp

; (fold-right / 1 (list 1 2 3)) =
; (((3/2)/1)/1) = 
; 3/2 

; (fold-left / 1 (list 1 2 3)) = 
; (((1/2)/3)/1) = 
; 1/6

; (fold-right list nil (list 1 2 3)) = 
; (list (list (list 3) 2) 1) =
; (((3) 2) 1)

; (fold-left list nil (list 1 2 3)) =
; (list (list (list 1) 2) 3) =
; (((1) 2) 3)

; for fold-right and fold-left to be guaranteed equal, the `op` must be commutable 
