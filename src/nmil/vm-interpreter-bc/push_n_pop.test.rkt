#lang racket/base

#|

test of bytecode implementation of push

|#
(module+ test
  (require (only-in "./push_const.rkt"
                    BC_PUSH_CONST_NUM_SHORT)
           (only-in "./push_n_pop.rkt"
                    BC_PUSH_B
                    BC_POP
                    BC_PUSH_I)
           "./test-utils.rkt")


  (define relevant-opcode-definitions (filtered-opcode-definitions
                                       (list "BC_PUSH_B"
                                             "BC_POP"
                                             "BC_PUSH_INT0"
                                             "BC_PUSH_INT1"
                                             "BC_PUSH_INT2"
                                             "BC_PUSH_I"
                                             "BC_BREAK")))

  (define (wrap-bytecode-for-test bc-to-wrap)
    (wrap-bytecode-for-bc-test
     bc-to-wrap
     relevant-opcode-definitions
     (list BC_PUSH_B
           BC_POP
           ;;
           BC_PUSH_CONST_NUM_SHORT
           BC_PUSH_I
           )))

  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (run-bc-wrapped-in-test- bc wrapped-code debug)))

(module+ test #| push byte |#
  (define push-byte-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_B) (byte 0)
       (bc PUSH_B) (byte 1)
       (bc PUSH_B) (byte 10)))
     ))

  (check-equal? (vm-stack->strings push-byte-state)
                (list "stack holds 3 items"
                      "byte $0a  (rt)"
                      "byte $01"
                      "byte $00")))

(module+ test #| pop |#
  (define pop-0-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc POP))
     ))

  (check-equal? (vm-stack->strings pop-0-state)
                (list "stack is empty"))

  (define pop-1-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc PUSH_I1)
      (bc POP))))

  (check-equal? (vm-stack->strings pop-1-state)
                (list "stack holds 1 item"
                      "int $0000  (rt)"))
  (define pop-2-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc PUSH_I1)
      (bc PUSH_I2)
      (bc POP))
     ))

  (check-equal? (vm-stack->strings pop-2-state)
                (list "stack holds 2 items"
                      "int $0001  (rt)"
                      "int $0000")))

(module+ test #| VM_PUSH_CONST_INT |#
  (define use-case-push-int-state-after
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I) (byte #xf0 #x04)
      (bc BREAK))))

  (check-equal? (vm-stack->strings use-case-push-int-state-after)
                (list "stack holds 1 item"
                      "int $04f0  (rt)")))
