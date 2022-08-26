;; load in functions using
;; cat ex_4-49.rkt - | ./utils_ambeval.rkt

(define (require p)
    (if (not p) (amb)))

(define nouns
  '(noun student professor cat class))

(define verbs
  '(verb studies lectures eats sleeps))

(define articles '(article the a))

(define adjectives
  '(adjective nice benign pleasant malevolent oleaginous hairy))

(define prepositions
  '(prep for to in by with))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb
     verb-phrase
     (maybe-extend
      (list 'verb-phrase
            verb-phrase
            (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-verb-phrase)))

(define (parse-simple-noun-phrase)
  (amb (list 'simple-noun-phrase
         (parse-word articles)
         (parse-word nouns))
       (list 'simple-noun-phrase
         (parse-word articles)
         (parse-word adjectives)
         (parse-word nouns))))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb
     noun-phrase
     (maybe-extend
      (list 'noun-phrase
            noun-phrase
            (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

;; to get random word from word-list
(define (nth lst i)
    (if (= i 0) (car lst)
            (nth (cdr lst) (- i 1))))

(define (random-element lst)
    (nth lst (random (length lst))))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (let ((found-word (random-element (cdr word-list))))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

;; ;;; Amb-Eval input:
;; (parse '(t t t t t t t t))
;;
;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; (sentence (simple-noun-phrase (article a) (adjective pleasant) (noun professor)) (verb-phrase (verb sleeps) (prep-phrase (prep in) (simple-noun-phrase (article the) (adjective oleaginous) (noun student)))))
;;
;; ;;; Amb-Eval input:
;; try-again
;;
;; ;;; Amb-Eval value:
;; (sentence (noun-phrase (simple-noun-phrase (article a) (adjective pleasant) (noun professor)) (prep-phrase (prep by) (simple-noun-phrase (article the) (adjective hairy) (noun professor)))) (verb sleeps))
;;
;; ;;; Amb-Eval input:
;; try-again
;;
;; ;;; There are no
;;                  more values of
;; (parse (quote (t t t t t t t t)))
