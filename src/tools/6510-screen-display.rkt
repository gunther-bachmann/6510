#lang racket

#|

 allow display of processor status in emacs

|#

(require (rename-in  racket/contract [define/contract define/c]))
(require (only-in "./6510-emacs-integration.rkt" 6510-debugger--execute-elisp-expression))

(provide
 6510-debugger--print-string)

(define elisp-function-print-string "6510-debugger--print-string")

(define/c (6510-debugger--print-string str)
  (-> string? any/c)
  (6510-debugger--execute-elisp-expression
   (format "(~a \"~a\")"
           elisp-function-print-string
           str)))
