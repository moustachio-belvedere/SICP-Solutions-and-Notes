#lang sicp
(#%require sicp-pict)

(define (split-lambda s1 s2)
  ((lambda (x) (x x))
   (lambda (splitter)
     (lambda (painter n)
       (if (= n 0)
           painter
           (let ((smaller ((splitter splitter) painter 
                                    (- n 1))))
             (s1 painter 
                 (s2 smaller smaller))))))))

(define (split-regular s1 s2)
  (define (mod-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (mod-split painter 
                                 (- n 1))))
          (s1 painter 
                 (s2 smaller smaller)))))
  mod-split)

(define right-split-lambda (split-lambda beside below))
(define up-split-lambda (split-lambda below beside))

(paint (right-split-lambda einstein 10))
(paint (up-split-lambda einstein 10))

(define right-split-regular (split-regular beside below))
(define up-split-regular (split-regular below beside))

(paint (right-split-regular einstein 10))
(paint (up-split-regular einstein 10))