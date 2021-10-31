#lang sicp
(#%require "utils_huff.rkt")

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

(define (successive-merge leaf-set)
  (cond ((null? (cdr leaf-set)) (car leaf-set))
        (else (successive-merge (adjoin-set (make-code-tree (car leaf-set)
                                                            (cadr leaf-set))
                                             (cddr leaf-set))))))

(define (generate-huffman-tree pairs)
  (successive-merge
    (make-leaf-set pairs)))

(define lyrics-tree 
  (generate-huffman-tree '((A 2)   (BOOM 1) (GET 2) (JOB 2)
                           (NA 16) (SHA 3)  (YIP 9) (WAH 1))))

(define msg '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

(display "Bits required for message of length ")
(display (length msg))
(display " is: ")
(display (length (encode msg lyrics-tree))) ; 84 bits required for message of length 36
(newline)

; for fixed length code, need 3 bits ( log2(8) ) bits per symbol, so 108 bits would be required
