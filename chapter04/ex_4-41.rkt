#! /bin/racket
#lang sicp

(#%require "utils_sicpuseful.rkt")

;; smith/fletcher, cooper/fletcher
(define (same-or-adjacent? x y)
  (<= (abs (- x y)) 1))

;; miller > c
(define (higher? x y)
  (> x y))

(define baker    (list 1 2 3 4 5))
(define cooper   (list 1 2 3 4 5))
(define fletcher (list 1 2 3 4 5))
(define miller   (list 1 2 3 4 5))
(define smith    (list 1 2 3 4 5))

(map (lambda (c)
       (filter (lambda (m) (higher? m c)) miller))
     cooper)
;; (letrec ((c (car cooper))
;;          (mp (

;; (define (big-boss b c f m s)
