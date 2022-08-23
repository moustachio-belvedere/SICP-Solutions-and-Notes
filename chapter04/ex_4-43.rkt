;; load in functions using
;; cat ex_4-43.rkt - | ./utils_ambeval.rkt

;; note, by logical deduction, that
;; Mr Moore is father of Mary Ann Moore,
;; Colonel Downing owns the Melissa, named after Barnacle Hood's daughter
;; (and the trickier one, noted by absence) Dr Parker must own the Mary Ann

;; further, it can then be deduced that:
;; Mr Moore's daughter is Mary Ann, and he owns the Lorna
;; Col. Downing's daughter is Lorna, and he owns the Melissa
;; Mr Hall's daughter is Gabrielle, and he owns the Rosalind
;; Barnacle Hood's daughter is Melissa, and he owns the Gabrielle
;; Dr Parker's daughter is Rosalind, and he owns the Mary Ann

;; for a programmatic solution
(define (require p)
    (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

;; looks like we need two lists, one for
;; familial relation, and another for yacht
;; ownership, this is just for the last constraint.
;; leaving out G father named after D Parker
;; daughter constraint, there are 3 solutions

;; mary ann, lorna, gabrielle, melissa, rosalind
;; inefficient version
(define (yachts-and-daughters)
  (let ((dmoore (amb 1 2 3 4 5))
        (ddowning (amb 1 2 3 4 5))
        (dhall (amb 1 2 3 4 5))
        (dhood (amb 1 2 3 4 5))
        (dparker (amb 1 2 3 4 5))
        (ymoore 2)
        (ydowning 4)
        (yhall 5)
        (yhood 3)
        (yparker 1))

    (require (= dmoore 1)) ;; given by surname
    (require (not (= dparker 1))) ;; deduced by her absence in ownership listing
    (require (not (= dhood 3))) ;; Sir Barnacle’s yacht is the Gabrielle
    (require (not (= dmoore 2))) ;; Mr. Moore owns the Lorna
    (require (not (= dhall 5))) ;; Mr. Hall the Rosalind
    (require (not (= ddowning 4))) ;; The Melissa, owned by Colonel Downing
    (require (= dhood 4)) ;; is named after Sir Barnacle’s daughter.
    ;; Gabrielle’s father owns the yacht that is named after Dr. Parker’s daughter
    ;; don't need to check hood here, as he is fixed to 4.
    ;; Moore's check we could also remove, but keeping in for now as it makes
    ;; next part of question easier.
    (cond ((= dmoore 3) (require (= ymoore dparker)))
          ((= ddowning 3) (require (= ydowning dparker)))
          ((= dhall 3) (require (= yhall dparker)))
          ((= dparker 3) (require (= yparker dparker))))

    (require (distinct? (list dmoore ddowning dhall dhood dparker)))
    (list dmoore ddowning dhall dhood dparker)))

;; with moore requirement, there is one solution (1, 2, 3, 4, 5) as expected.

;; more efficient version
(define (yachts-and-daughters-efficient)
  (let ((ymoore 2) (ydowning 4) (yhall 5) (yhood 3) (yparker 1) (dmoore 1) (dhood 4))
  (let ((dparker (amb 2 3 4 5)))
        (if (= dparker 3) (require (= yparker dparker)))
  (let ((ddowning (amb 1 2 3 5)))
        (if (= ddowning 3) (require (= ydowning dparker)))
  (let ((dhall (amb 1 2 3 4)))
        (if (= dhall 3) (require (= yhall dparker)))
    (require (distinct? (list dmoore ddowning dhall dhood dparker)))
    (list dmoore ddowning dhall dhood dparker))))))

(define (more-yachts-and-daughters)
  (let ((dmoore (amb 1 2 3 4 5))
        (ddowning (amb 1 2 3 4 5))
        (dhall (amb 1 2 3 4 5))
        (dhood (amb 1 2 3 4 5))
        (dparker (amb 1 2 3 4 5))
        (ymoore 2)
        (ydowning 4)
        (yhall 5)
        (yhood 3)
        (yparker 1))

    (require (not (= dparker 1))) ;; deduced by her absence in ownership listing
    (require (not (= dhood 3))) ;; Sir Barnacle’s yacht is the Gabrielle
    (require (not (= dmoore 2))) ;; Mr. Moore owns the Lorna
    (require (not (= dhall 5))) ;; Mr. Hall the Rosalind
    (require (not (= ddowning 4))) ;; The Melissa, owned by Colonel Downing
    (require (= dhood 4)) ;; is named after Sir Barnacle’s daughter.
    ;; Gabrielle’s father owns the yacht that is named after Dr. Parker’s daughter
    ;; don't need to check hood here, as he is fixed to 4.
    ;; Moore's check we could also remove, but keeping in for now as it makes
    ;; next part of question easier.
    (cond ((= dmoore 3) (require (= ymoore dparker)))
          ((= ddowning 3) (require (= ydowning dparker)))
          ((= dhall 3) (require (= yhall dparker)))
          ((= dparker 3) (require (= yparker dparker))))

    (require (distinct? (list dmoore ddowning dhall dhood dparker)))
    (list dmoore ddowning dhall dhood dparker)))

;; with the restriction relaxed on knowing May Ann's surname,
;; there are two solutions
;; (1 2 3 4 5) the original and
;; (3 5 1 4 2)
