#lang racket

#|

 allow display of nmil stack status in emacs

|#

(require (rename-in racket/contract
                    [define/contract define/c])
         (only-in "../nmil/vm-inspector-utils.rkt"
                  vm-stack->strings)
         (only-in "./6510-emacs-integration.rkt"
                  6510-debugger--execute-elisp-expression)
         "6510-debugger-shared.rkt")

(provide
 nmil-debugger--stack-buffer-display
 nmil-debugger--stack-buffer-kill)

(define elisp-function-kill-proc-buffer "nmil-debugger--stack-buffer-kill")
(define elisp-function-open-n-display-proc-buffer "nmil-debugger--stack-buffer-display")

(define/c (nmil-debugger--stack-buffer-display d-state)
  (-> debug-state? any/c)
  (define status-string (string-join (vm-stack->strings (car (debug-state-states d-state))) "\n"))
  (6510-debugger--execute-elisp-expression
   (format "(~a \"~a\")"
           elisp-function-open-n-display-proc-buffer
           status-string)))

(define/c (nmil-debugger--stack-buffer-kill)
  (-> any/c)
  (6510-debugger--execute-elisp-expression
   (format "(~a)" elisp-function-kill-proc-buffer)))
