#lang sicp

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) 
         (start-segment segment))
        ((frame-coord-map frame) 
         (end-segment segment))))
     segment-list)))

(define (make-vect x y)
  (cons x y))

(define (make-segment start end)
  (cons start end))

; 2-49a
(define (draw-outline frame)
  ((segments->painter
    (list (make-segment (make-vect 0 0)
                        (make-vect 0 1))
          (make-segment (make-vect 0 1)
                        (make-vect 1 1))
          (make-segment (make-vect 1 1)
                        (make-vect 1 0))
          (make-segment (make-vect 1 0)
                        (make-vect 0 0))))
   frame))

; 2-49b
(define (draw-x frame)
  ((segments->painter
    (list (make-segment (make-vect 0 0)
                        (make-vect 1 1))
          (make-segment (make-vect 0 1)
                        (make-vect 1 0))))
   frame))

; 2-49c
(define (draw-diamond frame)
  ((segments->painter
    (list (make-segment (make-vect 0 0.5)
                        (make-vect 0.5 1))
          (make-segment (make-vect 0.5 1)
                        (make-vect 1 0.5))
          (make-segment (make-vect 1 0.5)
                        (make-vect 0.5 0))
          (make-segment (make-vect 0.5 0)
                        (make-vect 0 0.5))))
   frame))

; 2-49d
;(define wave
;  (list (make-segment (make-vect 0 0.4)
;                      (make-vect 
; would take a while to draw all the lines!
