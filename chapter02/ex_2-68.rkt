#lang sicp
(#%require "utils_huff.rkt")

(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define decoded-message (decode sample-message sample-tree))

(define (encode-symbol symbol tree)
  (let ((caplist '()))
  (define (symfind symbol current travelled)
    (display travelled)
    (newline)
    (if (leaf? current)
        (if (eq? (symbol-leaf current) symbol)
            (append caplist travelled)
            (append caplist '()))
        (let ((llog (append travelled '(0)))
              (rlog (append travelled '(1))))
          (symfind symbol
                   (left-branch current)
                   llog)
          (symfind symbol
                   (right-branch current)
                   rlog))))

  (symfind symbol tree '())
  (display caplist)
  caplist))

  ;(define (symfind symbol travelled tree logged)
  ;  (if (leaf? tree)
  ;      (if (eq? (symbol-leaf tree) symbol)
  ;          travelled
  ;          '()))


(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

(display sample-tree)

(encode-symbol 'A sample-tree)

;(display "Encoded message: ")
;(display sample-message)
;(newline)
;(display "Decoded message: ")
;(display decoded-message)
;(newline)

