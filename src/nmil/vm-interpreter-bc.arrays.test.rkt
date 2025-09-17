#lang racket/base

#|

test of bytecode implementation of array commands

|#

(module+ test
  (require "./vm-interpreter-bc.test-utils.rkt")
  (require (only-in "./vm-interpreter-bc.arrays.rkt" BC_DEC_RBI_NZ_P_BRA))
  (require (only-in "./vm-interpreter-bc.branch.rkt" BC_T_P_BRA))
  (require (only-in "./vm-interpreter-bc.push_n_pop.rkt" BC_PUSH_B))

  (define relevant-opcode-definitions (filtered-opcode-definitions
                                       (list "BC_DEC_RBI_NZ_P_BRA"
                                             "BC_T_P_BRA"
                                             "BC_PUSH_B")))

  (define (wrap-bytecode-for-test bc-to-wrap)
    (wrap-bytecode-for-bc-test
     bc-to-wrap
     relevant-opcode-definitions
     (list  BC_DEC_RBI_NZ_P_BRA
            ;; ---
            BC_T_P_BRA
            BC_PUSH_B)))

  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (run-bc-wrapped-in-test- bc wrapped-code debug)))
