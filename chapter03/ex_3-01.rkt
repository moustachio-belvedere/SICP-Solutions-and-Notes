#lang sicp

(define (make-accumulator init)
  (let ((acc init))
    (lambda (to-add)
      (set! acc (+ acc to-add))
      acc)))

(define A (make-accumulator 1))

(A 1)
(A 5)
