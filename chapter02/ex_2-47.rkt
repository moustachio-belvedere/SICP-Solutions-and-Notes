#lang sicp

(define (make-frame-list origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame-cons origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame-list frame)
  (caddr frame))

(define (edge2-frame-cons frame)
  (cddr frame))

(define frame-list (make-frame-list 2 3 5))
(origin-frame frame-list)
(edge1-frame frame-list)
(edge2-frame-list frame-list)

(define frame-cons (make-frame-cons 2 3 5))
(origin-frame frame-cons)
(edge1-frame frame-cons)
(edge2-frame-cons frame-cons)
