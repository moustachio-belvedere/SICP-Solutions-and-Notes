#lang sicp

(define (square x)
  (* x x))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (average x y)
  (/ (+ x y) 2))

(define (len segment)
  (sqrt (+ (square (- (x-point (start-segment segment)) 
                      (x-point (end-segment segment))))
           (square (- (y-point (start-segment segment))
                      (y-point (end-segment segment)))))))

(define (perimeter rect)
  (* 2 (+ (width-rec rect)
          (length-rec rect))))

(define (area rect)
  (* (width-rec rect)
     (length-rec rect)))

; implementation 1
; note: a rectangle is defined by two perpendicular lengths joined at one vertex
; no test of these two properties is implemented
; WORKS FOR ANY ORIENTATION OF RECTANGLE
;   (define (make-rect vert horiz)
;     (cons vert horiz))
;   
;   (define (vert-seg rect)
;     (car rect))
;   
;   (define (horiz-seg rect)
;     (cdr rect))
;   
;   (define (width-rec rect)
;     (len (horiz-seg rect)))
;   
;   (define (length-rec rect)
;     (len (vert-seg rect)))
;   
;   
;   (define x (make-rect (make-segment (make-point 0 0)
;                                      (make-point 4 0))
;                        (make-segment (make-point 0 0)
;                                      (make-point 0 5))))
;   
;   (perimeter x)
;   (area x)

; implementation 2
; define rectangle by midpoint, width and length
; assumes width and length are parallel to axes so less general than implementation 1
(define (make-alt-rect midpoint lenwid)
  (cons midpoint lenwid))

(define (width-rec rect)
  (car (cdr rect)))

(define (length-rec rect)
  (cdr (cdr rect)))

(define x (make-alt-rect (make-point 0 0) (cons 4 5)))
(perimeter x)
(area x)
