#lang racket/base

#|

test of bytecode implementation of push

|#

(module+ test
  (require "./test-utils.rkt")
  (require (only-in "./predicates.rkt"
                    BC_I_Z_P
                    BC_INT_P))

  (define relevant-opcode-definitions (filtered-opcode-definitions
                                       (list "BC_I_Z_P"
                                             "BC_INT_P")))

  (define (wrap-bytecode-for-test bc-to-wrap)
    (wrap-bytecode-for-bc-test
     bc-to-wrap
     relevant-opcode-definitions
     (list BC_I_Z_P
           BC_INT_P
           )))

  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (run-bc-wrapped-in-test- bc wrapped-code debug)))
