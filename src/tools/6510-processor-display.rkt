#lang racket

#|

 allow display of processor status in emacs

|#

(require (rename-in  racket/contract [define/contract define/c]))
(require "6510-debugger-shared.rkt")
(require (only-in "6510-interpreter.rkt" state->string))

(provide
 6510-proc-buffer-display
 6510-proc-buffer-kill)

(define/c (6510-proc-buffer-display d-state)
  (-> debug-state? any/c)
  (define proc-status-string (state->string (car (debug-state-states d-state))))
  (parameterize ([current-output-port (open-output-nowhere)])
    (system* (find-executable-path "emacsclient") "-e" (format "(6510-proc-buffer-display \"~a\")" proc-status-string))))

(define/c (6510-proc-buffer-kill)
  (-> any/c)
  (system* (find-executable-path "emacsclient") "-e" "(6510-proc-buffer-kill)"))
