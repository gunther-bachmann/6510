#lang racket/base

#|

test of bytecode implementation of native commands

|#
(module+ test
  (require (only-in "./native.rkt"
                    BC_POKE_B
                    BC_NATIVE
                    RETURN_TO_BC)
           (only-in "./push_n_pop.rkt"
                    BC_PUSH_B)
           "./test-utils.rkt")


  (define relevant-opcode-definitions (filtered-opcode-definitions
                                       (list "BC_POKE_B"
                                             "BC_PUSH_B"
                                             "BC_NATIVE"
                                             "BC_BREAK")))

(define (wrap-bytecode-for-test bc-to-wrap)
    (wrap-bytecode-for-bc-test
     bc-to-wrap
     relevant-opcode-definitions
     (list  BC_NATIVE
            BC_POKE_B
            RETURN_TO_BC
            ;; ---
            BC_PUSH_B)))

  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (run-bc-wrapped-in-test- bc wrapped-code debug)))


(module+ test #| bdec |#
  (define native-return-test
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_B) (byte #x14)
       (bc NATIVE)
       (INC ZP_RT+1)
       (JSR RETURN_TO_BC)
       (bc PUSH_B) (byte #x16)))
     ))

  (check-equal? (vm-stack->strings native-return-test)
                (list "stack holds 2 items"
                      "byte $16  (rt)"
                      "byte $15")
                "last element was increment from $14->$15 before another push was issue"))
