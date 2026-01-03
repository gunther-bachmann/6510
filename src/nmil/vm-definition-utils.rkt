#lang racket/base

(provide
 define-vm-function-wol  ;; define a function without label (has to be added), with local label extension of labels ending on "__"
 define-vm-function      ;; define a function with (starting) label, with local label extension of labels ending on "__"
 )

#|

 functions that are useful for the definition of virtual machine functions written in racket assembler

 |#

(require
 (only-in racket/list
          flatten)
 (only-in "../6510.rkt"
          label)
 (only-in "../ast/6510-resolver.rkt"
          add-label-suffix))

;; syntactic sugar to define a vm function, without label at start of impl.
(define-syntax-rule (define-vm-function-wol name code-list)
  (define name
    (add-label-suffix
     "__" (string-append "__" (symbol->string (quote name)))
     (flatten
      code-list)) ))

;; syntactic sugar to define a vm function
(define-syntax-rule (define-vm-function name code-list)
  (define-vm-function-wol name
    (append
      (list (label name))
      (flatten
       code-list))))
