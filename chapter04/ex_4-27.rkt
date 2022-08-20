#! /bin/racket
#lang sicp
(#%require "utils_table.rkt")

(#%provide eval
           driver-loop
           special-forms
           install!
           make-lambda
           sequence->exp
           make-if)

(define special-forms (make-table))
(define (install! key val) (insert! key val special-forms))
(define (special-form exp)
  (if (pair? exp)
      (lookup (car exp) special-forms)))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ;; begin data-directed section
        ((special-form exp)
         ((special-form exp) exp env))
        ;; end data-directed section
        ((application? exp)
         (metapply (actual-value (operator exp) env)
                (operands exp)
                env))
        ;;;; applicative version ;;;;
        ;;((application? exp)
        ;; (metapply (eval (operator exp) env)
        ;;        (list-of-values
        ;;         (operands exp)
        ;;         env)))
        ;;;; =================== ;;;;
        (else
         (error "Unknown expression
                 type: EVAL" exp))))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (metapply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values
           arguments
           env)))  ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args
            arguments
            env)   ; changed
           (procedure-environment procedure))))
        (else (error "Unknown procedure
                      type: APPLY"
                     procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value
             (first-operand exps)
             env)
            (list-of-arg-values
             (rest-operands exps)
             env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it
             (first-operand exps)
             env)
            (list-of-delayed-args
             (rest-operands exps)
             env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp)
                           env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-unless exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-alternative exp) env)
      (eval (if-consequent exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps)
                        env))))

(define (eval-assignment exp env)
  (set-variable-value!
   (assignment-variable exp)
   (eval (assignment-value exp) env)
   env)
  'ok)

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda
       (cdadr exp)   ; formal parameters
       (cddr exp)))) ; body

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate
                 consequent
                 alternative)
  (list 'if
        predicate
        consequent
        alternative))

(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp
                 (cond-actions first))
                (error "ELSE clause isn't
                        last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp
                      (cond-actions first))
                     (expand-clauses
                      rest))))))

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
  ;;(list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied"
                 vars
                 vals)
          (error "Too few arguments supplied"
                 vars
                 vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop
              (enclosing-environment env)))
            ((eq? var (car vars))
             (let ((val (car vals)))
               (if (eq? val '*unassigned*)
                   (error "Unassigned variable" var)
                   (car vals))))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop
              (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame!
              var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'display display)
        (list 'newline newline)
        (list '+ +)
        (list '* *)
        (list '- -)
        (list '= =)
        (list 'list list)
        (list 'apply metapply)
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc)
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment
          (primitive-procedure-names)
          (primitive-procedure-objects)
          the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment
  (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))


(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define input-prompt  ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value
                   input
                   the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline)
  (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display
       (list 'compound-procedure
             (procedure-parameters object)
             (procedure-body object)
             '<procedure-env>))
      (display object)))

;; install special forms in table
(install! 'quote (lambda (exp env) (text-of-quotation exp)))
(install! 'set! eval-assignment)
(install! 'define eval-definition)
(install! 'if eval-if)
(install! 'unless eval-unless)
(install! 'lambda
          (lambda (exp env) (make-procedure (lambda-parameters exp)
                                            (lambda-body exp)
                                            env)))
(install! 'begin
          (lambda (exp env) (eval-sequence (begin-actions exp) env)))
(install! 'cond
          (lambda (exp env) (eval (cond->if exp) env)))

;; let from ex_4-06
(define (let->combination exp)
  (let ((params (map car (cadr exp)))
        (args   (map cadr (cadr exp)))
        (body   (cddr exp)))
   (append (list (make-lambda params body)) args)))
(install! 'let (lambda (exp env) (eval (let->combination exp) env)))
;; let from ex_4-06

(define (scan-out-defines proc-body)
  ;; helper func for adding a list to the end
  ;; of another list otherwise untouched
  (define (add-to-end l1 item)
    (append l1 (list item)))

  ;; helper func for repetitive let add logic
  (define (new-let let-acc top-expr)
    (let ((lvar (definition-variable top-expr)))
      (list 'let (add-to-end (cadr let-acc)
                             (list lvar ''*unassigned*)))))

  ;; helper func for repetitive body modification logic
  (define (new-bod mod-body top-expr)
    (let ((lvar (definition-variable top-expr))
          (lval (definition-value top-expr)))
      (add-to-end mod-body
                  (list 'set! lvar lval))))

  (define (has-lets? let-acc)
    (not (null? (cadr let-acc))))

  ;; main logic
  (define (traverse-body let-acc mod-body orig-body)
    (if (null? orig-body)
        (if (has-lets? let-acc)
            (list (append let-acc mod-body))
            mod-body)
        (let ((top-expr (car orig-body)))
             (if (definition? top-expr)
                 (traverse-body (new-let let-acc top-expr)
                                (new-bod mod-body top-expr)
                                (cdr orig-body))
                 (traverse-body let-acc
                                (add-to-end mod-body top-expr)
                                (cdr orig-body))))))

  (traverse-body (list 'let '()) '() proc-body))

(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result
                (actual-value
                 (thunk-exp obj)
                 (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           ;; replace exp with its value:
           (set-car! (cdr obj) result)
           ;; forget unneeded env:
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(driver-loop)

;; (define count 0)
;; (define (id x) (set! count (+ count 1)) x)
;; (define w (id (id 10)))
;;
;; count
;; >> 1
;;
;; w
;; >> 10
;;
;; count
;; >> 2

;; the only surprsing result is the first one.
;; the definition of `w` appears to invoke `id` exactly
;; once. why?
;;
;; easier to understand with the slightly modified function:
;; (define (id x s) (set! count (+ count 1)) (display s) x)
;; (define w (id (id 10 "inner") "outer"))
;; >> outer

;; in defining `w`, the outer function call of `id` proceeds
;; through the two expressions with side-effects (`set!` and `display`)
;; but does not evaluate the "inner" call as it's result is not strictly
;; needed yet.


