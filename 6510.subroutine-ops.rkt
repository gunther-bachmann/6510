#lang racket

(require "6510-addressing.rkt")

(provide JMP JSR RTI RTS)

(module+ test
  (require "6510-command.rkt")
  (require "6510-test-utils.rkt"))

(define-opcode JMP
  ((absolute . #x4C)
   (indirect . #x6c)))

(module+ test #| JMP |#
  (check-equal? (JMP $FFD2)
                (ast-opcode-cmd '(#x4c #xd2 #xff)))
  (check-equal? (JMP some)
                (ast-unresolved-opcode-cmd '(#x4c) (ast-resolve-word-scmd "some")))
  (check-equal? (JMP ($FFD2))
                (ast-opcode-cmd '(#x6c #xd2 #xff)))
  (check-equal? (JMP (some))
                (ast-unresolved-opcode-cmd '(#x6c) (ast-resolve-word-scmd "some"))))

(define-opcode JSR ((absolute . #x20)))

(module+ test #| JSR |#
  (check-equal? (JSR $FFD2)
                (ast-opcode-cmd '(#x20 #xd2 #xff))))

(define-opcode RTI ((implicit . #x40)))

(define-opcode RTS ((implicit . #x60)))
