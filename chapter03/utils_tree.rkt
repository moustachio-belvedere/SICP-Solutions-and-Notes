#lang sicp
(#%provide entry
           left-branch
           right-branch
           make-tree
           element-of-set?
           adjoin-set
           tree->list-1
           tree->list-2
           list->tree
           partial-tree
           lookup)

(define (key entry) (car entry))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (t-op op x y)
  (op (car x) (car y)))
(define (t= x y) (t-op string=? x y))
(define (t< x y) (t-op string<? x y))
(define (t> x y) (t-op string>? x y))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((t= x (entry set)) true)
        ((t< x (entry set))
         (element-of-set? 
          x 
          (left-branch set)))
        ((t> x (entry set))
         (element-of-set? 
          x 
          (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((equal? (entry set) '*table*)
         (make-tree
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((t= x (entry set)) set)
        ((t< x (entry set))
         (make-tree 
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((t> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))

(define (lookup given-key set-of-records)
  (if (null? set-of-records) 
      false
  (let ((cur-entry (entry set-of-records)))
  (let ((not-header? (not (equal? cur-entry '*table*))))
  (cond (not-header?
          (cond ((string=? given-key (key cur-entry)) cur-entry)
                ((string<? given-key (key cur-entry))
                 (lookup
                  given-key 
                  (left-branch set-of-records)))
                ((string>? given-key (key cur-entry))
                 (lookup 
                  given-key 
                  (right-branch set-of-records)))))
        (else (let ((lsearch
                       (lookup
                        given-key 
                        (left-branch set-of-records)))
                     (rsearch
                       (lookup
                        given-key 
                        (right-branch set-of-records))))
                (if rsearch
                    rsearch
                    lsearch))))))))
