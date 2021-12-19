#!/bin/racket
#lang racket
(require pict)
(require sdraw)

(define ltest (sdraw '(1 2) #:null-style '/ #:null-thickness 0.5))

(define (save-pict the-pict name kind)
  (define bm (pict->bitmap the-pict))
  (send bm save-file name kind))

(save-pict ltest "ltest.png" 'png)
