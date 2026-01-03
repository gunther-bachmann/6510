#lang racket/base

(provide
 regression-test   ;; only if state is defined (non void), execute the given tests
 )

#|

 utilities and or syntactic sugar useful for writing tests

 |#

(require (only-in racket/path file-name-from-path))

;; only if state is defined (non void), execute the given tests
;; useful if a debug session is run, the result is void,
;; which otherwise produces lots of error messages
(define-syntax-rule (regression-test state doc-str tests ...)
  (begin
    (displayln (string-append (path->string (file-name-from-path (syntax-source #'doc-str))) ": " doc-str))
    (cond [(void? state)
           (displayln (format "left debug session.\nignoring tests on this debug-state: ~a." (symbol->string 'state)))]
          [else
           tests ...])))

(module+ test
  (require rackunit)

  (define ignored-state (void))

  (regression-test
   ignored-state
   "ignore failing checks"
   (check-equal? #t #f)
   (check-equal? #t #f))

  (define executed-state 'id)

  (regression-test
   executed-state
   "execute checks"
   (check-equal? #t #t)
   (check-equal? #t #t)))
