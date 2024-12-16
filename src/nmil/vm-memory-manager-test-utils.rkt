#lang racket/base

(require (only-in racket/port open-output-nowhere))
(require (only-in racket/list empty? drop))

(require "../6510.rkt" )

(require  "../6510-test-utils.rkt")
(require  "../util.rkt")

(require (only-in "../tools/6510-debugger.rkt" run-debugger-on))
(require (only-in "../ast/6510-command.rkt" ast-label-def-cmd? ast-label-def-cmd-label))
(require (only-in "../tools/6510-interpreter.rkt" peek-word-at-address cpu-state-clock-cycles peek))
(require (only-in "../tools/6510-interpreter.rkt"
                  6510-load-multiple
                  initialize-cpu
                  run-interpreter-on
                  ))
(require (only-in "../ast/6510-assembler.rkt"
                  assemble-to-code-list
                  ))

(provide run-code-in-test-on-code remove-labels-for)

(define (remove-labels-for code labels-to-remove (result (list)))
  (cond
    [(empty? code) (reverse result)]
    [else
     (define cmd (car code))
     (cond
       [(ast-label-def-cmd? cmd)
        (define label (ast-label-def-cmd-label cmd))
        (if (findf (lambda (label-cmd) (string=? label (ast-label-def-cmd-label label-cmd))) labels-to-remove)
            (remove-labels-for (cdr code) labels-to-remove result)
            (remove-labels-for (cdr code) labels-to-remove (cons cmd result)))]
       [else (remove-labels-for (cdr code) labels-to-remove (cons cmd result))])]))

(define (run-code-in-test-on-code wrapped-test-code (debug #f))
  (define state-before
    (6510-load-multiple (initialize-cpu)
                        (assemble-to-code-list wrapped-test-code)))
  (if debug
      (run-debugger-on state-before)
      (parameterize ([current-output-port (open-output-nowhere)])
        (run-interpreter-on state-before))))
