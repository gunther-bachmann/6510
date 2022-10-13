#lang racket

(require "6510-alt-utils.rkt")
(require "6510-alt-addressing.rkt")

(provide JMP JSR RTI RTS) 

(module+ test
  (require "6510-test-utils.rkt"))

(define-opcode JMP
  ((absolute . #x4C)
   (indirect . #x6c)))

(module+ test #| JMP |#
  (check-equal? (JMP $FFD2)
                '(opcode #x4c #xd2 #xff))
  (check-equal? (JMP some)
                '(opcode #x4c (resolve-word "some")))
  (check-equal? (JMP ($FFD2))
                '(opcode #x6c #xd2 #xff))
  (check-equal? (JMP (some))
                '(opcode #x6c (resolve-word "some"))))

(define-opcode JSR ((absolute . #x20)))

(module+ test #| JSR |#
  (check-equal? (JSR $FFD2)
                '(opcode #x20 #xd2 #xff)))

(define-opcode RTI ((implicit . #x40)))

(define-opcode RTS ((implicit . #x60)))
