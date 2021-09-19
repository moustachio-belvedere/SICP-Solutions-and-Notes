#lang sicp
(#%require sicp-pict)

(define (make-vect x y)
  (cons x y))

(define (rotate90 painter)
  (transform-painter
    painter
    (make-vect 1.0 0.0)
    (make-vect 1.0 1.0)
    (make-vect 0.0 0.0)))

(define (rotate270 painter)
  (transform-painter 
   painter
   (make-vect 0.0 1.0)  
   (make-vect 0.0 0.0)  
   (make-vect 1.0 1.0)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left  (transform-painter 
                        painter1
                        (make-vect 0.0 0.0)
                        split-point
                        (make-vect 0.0 1.0)))
          (paint-right (transform-painter
                        painter2
                        split-point
                        (make-vect 1.0 0.0)
                        (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below-direct painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-low (transform-painter 
                        painter1
                        (make-vect 0.0 0.0)
                        (make-vect 1.0 0.0)
                        split-point))
          (paint-hi  (transform-painter
                        painter2
                        split-point
                        (make-vect 1.0 0.5)
                        (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-low frame)
        (paint-hi frame)))))

(define (below-via painter1 painter2)
  (rotate90
    (beside (rotate270 painter1)
            (rotate270 painter2))))
