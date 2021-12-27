#!/bin/racket
#lang sicp

(define null '())

;; from user AntonKolobov, passes all tests by
;; Turtle & Hare implementation
;; http://community.schemewiki.org/?sicp-ex-3.19
(define (has-cycle? tree) 
 ;; Helpers 
 (define (iterator value idx) 
   (cons value idx)) 
 (define (update-iterator it value idx) 
   (set-car! it value) 
   (set-cdr! it idx)) 
 (define (iterator-id it) 
   (cdr it)) 
 (define (iterator-value it) 
   (car it)) 
 (define (iterator-same-pos? it1 it2) 
   (eq? (iterator-id it1) (iterator-id it2))) 
 (define (iterator-eq? it1 it2) 
   (and (iterator-same-pos? it1 it2) 
        (eq? (iterator-value it1) (iterator-value it2)))) 

 ;; slow-it - tracks each node (1, 2, 3, 4...) 
 ;; fast-it - tracks only even nodes (2, 4...) 
 (let ((slow-it (iterator tree 0)) 
       (fast-it (iterator '() 0)) 
       (clock-cnt 0)) 
   (define (dfs root) 
     (if (not (pair? root)) 
         false 
         (begin 
           (set! clock-cnt (+ clock-cnt 1)) 
           (if (and (even? clock-cnt) 
                    (iterator-same-pos? slow-it fast-it)) 
               (update-iterator slow-it root clock-cnt)) 
           (if (even? clock-cnt) 
               (update-iterator fast-it root 
                                (+ (iterator-id fast-it) 1))) 
           (if (iterator-eq? slow-it fast-it) 
               true 
               (or (dfs (car root)) 
                   (dfs (cdr root))))))) 
   (dfs tree))) 

(define x (list 1))
(has-cycle? x)
(set-cdr! x x)
(has-cycle? x)

(define y (list 1 (list 2 (list 3))))
(has-cycle? y)
(set-cdr! (cdadr y) y)
(has-cycle? y)

(define z (cons (cons 2 '()) (cons 3 '())))
(has-cycle? z)
(set-car! (cdr z) z)
(has-cycle? z)

(define t1 (cons 'a 'b)) 
(define t2 (cons t1 t1))
(has-cycle? t2)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define k '(d e f)) 
(set-cdr! (last-pair k) (cdr k))
(has-cycle? k)
