#lang racket/base

(module+ test
  (require "./vm-interpreter-bc.test-utils.rkt")
  (require (only-in "./vm-interpreter-bc.misc.rkt" BC_BNOP))

  (define relevant-opcode-definitions (filtered-opcode-definitions
                                       (list "BC_BNOP")))

  (define (wrap-bytecode-for-test bc-to-wrap)
    (wrap-bytecode-for-bc-test
     bc-to-wrap
     relevant-opcode-definitions
     (list  BC_BNOP)))

  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (run-bc-wrapped-in-test- bc wrapped-code debug)))

(module+ test #| nop |#
  (define nop-state
    (run-bc-wrapped-in-test
     (list (bc BNOP)
           (bc BNOP))))

  (inform-check-equal? (cpu-state-clock-cycles nop-state)
                99))
