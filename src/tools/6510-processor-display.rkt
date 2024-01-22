#lang racket

#|

 allow display of processor status in emacs

|#

(require (rename-in  racket/contract [define/contract define/c]))
(require "6510-debugger-shared.rkt")
(require (only-in "6510-interpreter.rkt" state->string))
(require (only-in "./6510-emacs-integration.rkt" 6510-debugger--execute-elisp-expression))

(provide
 6510-debugger--proc-buffer-display
 6510-debugger--proc-buffer-kill)

(define elisp-function-kill-proc-buffer "6510-debugger--proc-buffer-kill")
(define elisp-function-open-n-display-proc-buffer "6510-debugger--proc-buffer-display")

(define/c (6510-debugger--proc-buffer-display d-state)
  (-> debug-state? any/c)
  (define proc-status-string (state->string (car (debug-state-states d-state))))
  (6510-debugger--execute-elisp-expression
   (format "(~a \"~a\")"
           elisp-function-open-n-display-proc-buffer
           proc-status-string)))

(define/c (6510-debugger--proc-buffer-kill)
  (-> any/c)
  (6510-debugger--execute-elisp-expression
   (format "(~a)" elisp-function-kill-proc-buffer)))
