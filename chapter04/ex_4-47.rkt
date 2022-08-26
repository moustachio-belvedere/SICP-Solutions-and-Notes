;; load in functions using
;; cat ex_4-47.rkt - | ./utils_ambeval.rkt

(define (require p)
    (if (not p) (amb)))

(define nouns
  '(noun student professor cat class))

(define verbs
  '(verb studies lectures eats sleeps))

(define articles '(article the a))

(define prepositions
  '(prep for to in by with))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

;; gives the five parses of
;; the professor lectures to the student in the class with the cat
;; but fails on to halt search correctly on 6th `try-again`


;; switching order of `amb` arguments causes failure on first parse
;; attempt
(define (parse-verb-phrase)
  (amb (list
        'verb-phrase
        (parse-verb-phrase)
        (parse-prepositional-phrase))
        (parse-word verbs)))

(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-verb-phrase)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

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

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*)
                 (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))
