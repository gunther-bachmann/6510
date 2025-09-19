#lang racket/base

#|

  byte code ast definitions (bc-ast) are used to enable vm byte code specific
  extensions to the ast definitions that are originally specified only for assembler.

|#

(require (for-syntax racket/base)
         (only-in racket/contract/base
                  struct-guard/c
                  listof)
         (only-in "../6510-utils.rkt"
                  ->string)
         (only-in "../ast/6510-command.rkt"
                  ast-bytes-cmd
                  ast-bytes-cmd?
                  ast-label-def-cmd?
                  ast-command?
                  ast-unresolved-bytes-cmd?))

(provide (struct-out bc-ast-rel-branch-reference)
         bc-cmd?
         bc-rel-ref)

;; is this ast command structure a valid byte code ast command?
(define (bc-cmd? cmd)
  (or (ast-bytes-cmd? cmd)               ;; reused for simple byte lists
     (ast-label-def-cmd? cmd)           ;; reused to define labels
     (bc-ast-rel-branch-reference? cmd) ;; relative byte code branch reference
     (ast-unresolved-bytes-cmd? cmd)))  ;; reused byte list with references

(struct bc-ast-rel-branch-reference ast-bytes-cmd
  (label-ref)
  #:transparent
  #:guard (struct-guard/c
           list?
           (listof byte?)
           string?))

;; a relative reference e.g. for byte code branch commands
(define-syntax (bc-rel-ref stx)
  (syntax-case stx ()
    ([_ str]
     #'(bc-ast-rel-branch-reference '() (list 0) (->string #'str)))))
