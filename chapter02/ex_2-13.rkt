; Let interval
; ix = (x ± e_x)
; iy = (y ± e_y)
; where e__ is the width of error and is small (e__ << 1)
; therefore
; ix*iy = (x*y ± e_y*x ± e_x*y e_x*e_y)
; and the last term is small enough to ignore, so we just have:
; ix*iy = x*y ± e_y*x ± e_x*y
; which can be easily converted into a lisp function
