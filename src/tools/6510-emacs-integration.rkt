#lang racket

(provide
 6510-debugger--has-proc-display-cap
 6510-debugger--has-single-step-cap
 6510-debugger--has-assembly-disp-cap
 6510-debugger--has-output-cap
 6510-debugger--execute-elisp-expression
 nmil-debugger--has-stack-display-cap)

(define elisp-function-has-proc-display-cap "6510-debugger--has-proc-display-cap")
(define elisp-function-has-single-step-cap "6510-debugger--has-single-step-cap")
(define elisp-function-has-assembly-disp-cap "6510-debugger--has-assembly-disp-cap")
(define elisp-function-has-output-cap "6510-debugger--has-output-cap")
(define elisp-function-has-nmil-stack-cap "nmil-debugger--has-stack-display-cap")


(define (nmil-debugger--has-stack-display-cap)
  (-has-cap elisp-function-has-nmil-stack-cap))

(define (6510-debugger--has-proc-display-cap)
  (-has-cap elisp-function-has-proc-display-cap))

(define (6510-debugger--has-single-step-cap file-name)
  (-has-cap elisp-function-has-single-step-cap file-name))

(define (6510-debugger--has-assembly-disp-cap file-name)
  (-has-cap elisp-function-has-assembly-disp-cap file-name))

(define (6510-debugger--has-output-cap)
  (-has-cap elisp-function-has-output-cap))

(define (6510-debugger--execute-elisp-expression form)
  (string-trim
    (with-output-to-string
      (lambda ()
        (system* (find-executable-path "emacsclient")
                 "-e"
                 form
                 )))))

(define (-has-cap elisp-function (param1 '()))
  (string=?
   "t"
   (cond [(and (not (empty? param1)) (string? param1))
          (6510-debugger--execute-elisp-expression (format "(~a \"~a\")" elisp-function param1))]
         [else
          (6510-debugger--execute-elisp-expression (format "(~a)" elisp-function))])))
