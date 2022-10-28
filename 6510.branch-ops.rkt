#lang racket

(require "6510-addressing-utils.rkt")
(require "6510-addressing.rkt")

(require "6510-command.rkt")

(provide BCC BCS BEQ BMI BNE BPL BVC BVS) 

(module+ test
  (require "6510-test-utils.rkt"))

(define-opcode BCC ((relative . #x90)))
(define-opcode BCS ((relative . #xb0)))
(define-opcode BEQ ((relative . #xf0)))

(module+ test #| BEQ |#
  (check-equal? (BEQ $10)
                (ast-rel-opcode-cmd '(#xf0 #x10)))
  (check-equal? (BEQ some)
                (ast-unresolved-rel-opcode-cmd '(#xf0) (ast-resolve-byte-scmd "some" 'relative))))

(define-opcode BMI ((relative . #x30)))

(define-opcode BNE ((relative . #xd0)))

(define-opcode BPL ((relative . #x10)))

(define-opcode BVC ((relative . #x50)))

(define-opcode BVS ((relative . #x70)))
