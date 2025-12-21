#lang racket
#|

 provide 6510 jump/return instructions:
  JMP
  JSR
  RTI
  RTS

 |#

(require "../scheme-asm/6510-addressing.rkt")

(provide JMP JSR RTI RTS)

(module+ test
  (require "../ast/6510-command.rkt")
  (require "../6510-test-utils.rkt"))

(define-opcode JMP
  ((absolute . #x4C)
   (indirect . #x6c)))

(module+ test #| JMP |#
  (check-match (JMP $FFD2)
                (ast-opcode-cmd _ '(#x4c #xd2 #xff)))
  (check-match (JMP some)
                (ast-unresolved-opcode-cmd _ '(#x4c) (ast-resolve-word-scmd "some")))
  (check-match (JMP ($FFD2))
                (ast-opcode-cmd _ '(#x6c #xd2 #xff)))
  (check-match (JMP (some))
                (ast-unresolved-opcode-cmd _ '(#x6c) (ast-resolve-word-scmd "some"))))

(define-opcode JSR ((absolute . #x20)))

(module+ test #| JSR |#
  (check-match (JSR $FFD2)
                (ast-opcode-cmd _ '(#x20 #xd2 #xff))))

(define-opcode RTI ((implicit . #x40)))

(define-opcode RTS ((implicit . #x60)))
