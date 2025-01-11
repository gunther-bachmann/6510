#lang racket/base

#|

byte code ast definitions (bc-ast) are used to enable vm byte code specific
extensions to the ast definitions that are originally specified only for assembler.

|#

(require (only-in racket/contract/base struct-guard/c listof))

(require (only-in "../ast/6510-command.rkt" ast-bytes-cmd  ast-bytes-cmd? ast-label-def-cmd? ast-command?))

(provide (struct-out bc-ast-rel-branch-reference)
         bc-cmd?)

(define (bc-cmd? cmd)
  (or (ast-bytes-cmd? cmd)
     (ast-label-def-cmd? cmd)
     (bc-ast-rel-branch-reference? cmd)))

(struct bc-ast-rel-branch-reference ast-bytes-cmd
  (label-ref)
  #:transparent
  #:guard (struct-guard/c
           list?
           (listof byte?)
           string?))
