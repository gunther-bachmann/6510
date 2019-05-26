#lang racket

(require (for-syntax racket/match))

(define-syntax (LABEL stx)
  (syntax-case stx ()
    [(LABEL op ix)
     (match `(,(syntax->datum #'op) ,(syntax->datum #'ix))
       [(list "some" ':)
        #'(LABEL_s (string-append op (symbol->string (quote ix)) " line:" (number->string (syntax-line #'op))))]
       [(list "some" '+)
        #'(LABEL_s (string-append op (symbol->string (quote ix)) " and line:" (number->string (syntax-line #'op))))]
       [(list "some-other" _)
        #'(LABEL_s (string-append "plus" " line:" (number->string (syntax-line #'op))))])]))

(define (LABEL_s label) `(displayln ,label))

(LABEL "some" :)
(LABEL "some" +)
(LABEL "some-other" :)
