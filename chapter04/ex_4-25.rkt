;; Applicative order
;; -----------------
;; in applicative order, the unless-based factorial
;; will infinitely recurse.
(define (factorial n)
  (unless (= n 1)
    (* n (facorial (- n 1)))
    1))

;; the 'base case' of (= n 1) is meaningless as even
;; when n is equal to 1, the expression (factorial (- n 1))
;; will be applicatively evaluated, and will continue
;; to be evaluated in subsequent recursions with n<=0.

;; Normal order
;; ------------
;; Using the normal model of evaluation, the recursive call
;; (factorial (- n 1)) will only occur if the base case has
;; not been reached. This procedure would work as intended
;; in this case.



