#lang racket
#|

 provides utilty functions useful for defining syntax macros

 |#

(module+ test
  (require rackunit))

(provide symbol-append discard-void-syntax-object make-id meta-info?)

(define (make-id stx id-template . ids)
  (let ([str (apply format id-template (map syntax->datum ids))])
    (datum->syntax stx (string->symbol str))))

;; create a new symbol with the given appendix
(define (symbol-append symbol appendix)
  (string->symbol (format "~a~a" (syntax->datum symbol) appendix)))

(module+ test #| symbol-append |#
  (check-match (symbol-append #'some '_or)
               'some_or))

;; return the one of the two that is neither void nor its syntax->datum is void
(define (discard-void-syntax-object a b)
  (if (or (void? a) (void? (syntax->datum a)))
      b
      a))

(module+ test #| discard-void-syntax-object |#
  (check-match (syntax->datum (discard-void-syntax-object (void) #'some))
               'some)

  (check-match (syntax->datum (discard-void-syntax-object #'some (void)))
               'some)

  (check-match (syntax->datum (with-syntax ([void-syn (void)]) (discard-void-syntax-object #'void-syn #'some)))
               'some)

  (check-match (syntax->datum (with-syntax ([void-syn (void)]) (discard-void-syntax-object #'some #'void-syn)))
               'some))

(define (meta-info? stx)
  (let ([datum (syntax->datum stx)])
    (or
     (and (list? datum)
        (equal? (car datum)
                '#:line))
     (and (list? datum)
        (equal? (car datum)
                'quote)
        (equal? (caadr datum)
                '#:line)))))


(module+ test #| meta-info? |#
  (check-true (meta-info? #'(#:line 1 #:org-cmd "some")))
  (check-true (meta-info? #'(quote (#:line 1 #:org-cmd "some")))))
