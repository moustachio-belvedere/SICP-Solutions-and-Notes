#lang sicp

(define (double func)
  (lambda (x)
    (func (func x))))

(define (inc x)
  (+ x 1))

(((double (double double)) inc) 5)

; ; inner bracket expansion
; (double double)
; (lambda (f) (double (double f)))
; 
; ; outer bracket expansion
; (double (double double))
; (double (lambda (f) (double (double f))))
; (lambda (fo) ((lambda (fi) (double (double fi)))
;               (lambda (fii) (fo (fo fii)))))
; 
; ; test the manual expansion matches the initial usage
; (((lambda (fo) ((lambda (fi) (double (double fi)))
;                 (lambda (fii) (fo (fo fii))))) inc) 5)

; difficult to expand manually...
