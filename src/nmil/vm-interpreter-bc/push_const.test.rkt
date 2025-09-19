#lang racket/base

#|

test of bytecode implementation of pushing constants

|#

(module+ test
  (require (only-in "./push_const.rkt"
                    BC_PUSH_CONST_NUM_SHORT)
           "./test-utils.rkt")

  (define relevant-opcode-definitions (filtered-opcode-definitions
                                       (list "BC_PUSH_INT0"
                                             "BC_PUSH_INT1"
                                             "BC_PUSH_INT2"
                                             "BC_PUSH_INTm1")))
  (define (wrap-bytecode-for-test bc-to-wrap)
    (wrap-bytecode-for-bc-test
     bc-to-wrap
     relevant-opcode-definitions
     (list  BC_PUSH_CONST_NUM_SHORT)))

  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (run-bc-wrapped-in-test- bc wrapped-code debug)))

(module+ test #| push const num |#
  (define use-case-push-num-s-state-after
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc PUSH_I1)
      (bc PUSH_I2)
      (bc PUSH_IM1)
      (bc BREAK))))

  (check-equal? (vm-stack->strings use-case-push-num-s-state-after)
                (list "stack holds 4 items"
                      "int $1fff  (rt)"
                      "int $0002"
                      "int $0001"
                      "int $0000")))
