#! /bin/racket
#lang sicp

(#%require "utils_sicpuseful.rkt")

;; smith/fletcher, cooper/fletcher
(define (same-or-adjacent? x y)
  (<= (abs (- x y)) 1))

;; miller > c
(define (higher? x y)
  (> x y))

;; with some of the 'manual' constraint work
;; pre-applied
(define baker    (list 1 2 3 4))
(define cooper   (list 2 4))
(define fletcher (list 2 3 4))
(define miller   (list 3 4 5))
(define smith    (list 1 2 4 5))

(map (lambda (c)
       (filter (lambda (m) (higher? m c)) miller))
     cooper)
;; (letrec ((c (car cooper))
;;          (mp (

;; (define (big-boss b c f m s)
