#lang racket

(provide
 6510-debugger--has-proc-display-cap
 6510-debugger--has-single-step-cap)

(define elisp-function-has-proc-display-cap "6510-debugger--has-proc-display-cap")
(define elisp-function-has-single-step-cap "6510-debugger--has-single-step-cap")

(define (6510-debugger--has-proc-display-cap)
  (-has-cap elisp-function-has-proc-display-cap))

(define (6510-debugger--has-single-step-cap)
  (-has-cap elisp-function-has-single-step-cap))

(define (-has-cap elisp-function)
  (string=?
   "t"
   (string-trim
    (with-output-to-string
      (lambda ()
        (system* (find-executable-path "emacsclient")
                 "-e"
                 (format "(~a)" elisp-function)))))))
