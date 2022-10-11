#lang racket

;; use this instead of (require rackunit) to make use of skipping tests

(require ansi-color)
(require rackunit)

(provide skip)
(provide (all-from-out rackunit))
(provide (all-from-out ansi-color))

(define-syntax (skip stx)
  (syntax-case stx ()
    ([_ body]
     (datum->syntax
      stx
      (syntax->datum
       #'(check-true
          (begin
            (with-colors 'red
              (lambda () (color-displayln "test skipped.")))
            #t)))))))
