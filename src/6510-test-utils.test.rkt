#lang racket/base

(require "./6510-test-utils.rkt")

(module+ tests
  (require rackunit))

(module+ test #| drop meta info |#
  (require (only-in "ast/6510-command.rkt" ast-opcode-cmd))

  (check-equal? (drop-meta-info (ast-opcode-cmd '() '(10 20)))
                (list 'struct:ast-opcode-cmd '(10 20)))

  (check-equal? (drop-meta-infos (list (ast-opcode-cmd '() '(10 20))))
                (list (list 'struct:ast-opcode-cmd '(10 20)))))
