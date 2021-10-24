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

(define (encode-symbol symbol tree)
  (define (symfind symbol current travelled logger)
    (if (leaf? current)
        (if (eq? (symbol-leaf current) symbol)
            (append logger travelled)
            (append logger '()))
        (let ((llog (append travelled '(0)))
              (rlog (append travelled '(1))))
          (append logger (symfind symbol
                                  (left-branch current)
                                  llog
                                  logger)
                         (symfind symbol
                                  (right-branch current)
                                  rlog
                                  logger)))))

  (let ((result (symfind symbol tree '() '())))
    (cond ((null? result)
            (error "ERROR: Symbol not found in tree\n"))
          (else result))))

(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

; (encode-symbol 'A sample-tree)
; (encode-symbol 'B sample-tree)
; (encode-symbol 'C sample-tree)
; (encode-symbol 'D sample-tree)
; (encode-symbol 'E sample-tree) ;; raises error as expected

(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define decoded-message (decode sample-message sample-tree))

(display "Encoded message: ")
(display sample-message)
(newline)
(display "Recoded message: ")
(display (encode decoded-message sample-tree))
(newline)
