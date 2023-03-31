#lang racket

(require "6510-command.rkt")
(require "6510-resolver.rkt")
(require "6510-relocator.rkt")
(require "6510-constants.rkt")

(require "../6510-utils.rkt")

(provide assemble)

(define/contract (assemble org program)
  (-> word/c (listof ast-command?) (listof byte/c))
  (define program-p1 (->resolved-decisions (label-instructions program) program))
  (define lsoffsets (label-string-offsets org program-p1))
  (define program-p2 (->resolve-labels org lsoffsets program-p1 '()))
  (define program-p3 (resolve-constants (constant-definitions-hash program-p1) program-p2))
  (resolved-program->bytes program-p3))
