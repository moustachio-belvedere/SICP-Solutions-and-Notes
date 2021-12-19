#!/bin/racket
#lang racket
(require pict)
(require sdraw)

(define (save-pict the-pict name kind)
  (define bm (pict->bitmap the-pict))
  (send bm save-file name kind))

;; (define ltest (sdraw '(1 2) #:null-style '/ #:null-thickness 0.5))

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 
  (cons (list 'a 'b) (list 'a 'b)))

(define z1pic (sdraw z1 #:null-style '/ #:null-thickness 0.5))
(define z2pic (sdraw z2 #:null-style '/ #:null-thickness 0.5))

(save-pict z1pic "z1pic.png" 'png)
(save-pict z2pic "z2pic.png" 'png)
