#lang racket/base

#|

  test of bytecode implementation of comparison commands

|#

(module+ test
  (require (only-in "./compare.rkt"
                    BC_B_GT_P
                    BC_B_LT_P
                    BC_B_GE_P
                    BC_I_GT_P
                    bc-compare-code)
           (only-in "./push_const.rkt"
                    BC_PUSH_CONST_NUM_SHORT)
           (only-in "./push_n_pop.rkt"
                    BC_PUSH_B)
           "./test-utils.rkt")

  (define relevant-opcode-definitions (filtered-opcode-definitions
                                       (list "BC_PUSH_B"
                                             "BC_I_GT_P"
                                             "BC_B_GE_P"
                                             "BC_B_LT_P"
                                             "BC_B_GT_P"
                                             "BC_PUSH_INT0"
                                             "BC_PUSH_INT1"
                                             "BC_PUSH_INT2"
                                             "BC_BREAK")))
(define (wrap-bytecode-for-test bc-to-wrap)
    (wrap-bytecode-for-bc-test
     bc-to-wrap
     relevant-opcode-definitions
     (list  BC_I_GT_P
            BC_B_GE_P
            BC_B_LT_P
            BC_B_GT_P
            ;; ---
            BC_PUSH_B
            BC_PUSH_CONST_NUM_SHORT)))

  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (run-bc-wrapped-in-test- bc wrapped-code debug)))

(module+ test #| BC_B_GT_P |#
  (define gt-01-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_B) (byte 10)
      (bc PUSH_B) (byte 20)
      (bc B_GT_P))))

  (check-equal? (vm-regt->string gt-01-state)
                "int $0001")

  (define gt-02-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_B) (byte 20)
      (bc PUSH_B) (byte 20)
      (bc B_GT_P))))

  (check-equal? (vm-regt->string gt-02-state)
                "int $0000")

  (define gt-03-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_B) (byte 20)
      (bc PUSH_B) (byte 21)
      (bc B_GT_P))))

  (check-equal? (vm-regt->string gt-03-state)
                "int $0001")

  (define gt-04-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_B) (byte 20)
      (bc PUSH_B) (byte 19)
      (bc B_GT_P))))

  (check-equal? (vm-regt->string gt-04-state)
                "int $0000"))

(module+ test #| BC_B_LT_P |#
  (define lt-01-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_B) (byte 20)
      (bc PUSH_B) (byte 10)
      (bc B_LT_P))))

  (check-equal? (vm-regt->string lt-01-state)
                "int $0001")

  (define lt-02-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_B) (byte 20)
      (bc PUSH_B) (byte 20)
      (bc B_LT_P))))

  (check-equal? (vm-regt->string lt-02-state)
                "int $0000")

  (define lt-03-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_B) (byte 20)
      (bc PUSH_B) (byte 21)
      (bc B_LT_P))))

  (check-equal? (vm-regt->string lt-03-state)
                "int $0000")

  (define lt-04-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_B) (byte 20)
      (bc PUSH_B) (byte 19)
      (bc B_LT_P))))

  (check-equal? (vm-regt->string lt-04-state)
                "int $0001"))


(module+ test #| INT GREATER? |#
  (define int-greater-0>-1-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_IM1)
      (bc PUSH_I0)
      (bc I_GT_P))
     ))

  (skip (check-equal? (vm-regt->string int-greater-0>-1-state)
                      "int $0001"
                      "comparison of negative with positive number (failing currently)"))

  (define int-greater-0>0-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc PUSH_I0)
      (bc I_GT_P))))

  (check-equal? (vm-regt->string int-greater-0>0-state)
                "int $0000")

  (define int-greater-1>1-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I1)
      (bc PUSH_I1)
      (bc I_GT_P))))

  (check-equal? (vm-regt->string int-greater-1>1-state)
                "int $0000")

  (define int-greater-2>2-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I2)
      (bc PUSH_I2)
      (bc I_GT_P))))

  (check-equal? (vm-regt->string int-greater-2>2-state)
                "int $0000")

  (define int-greater-2>1-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I1)
      (bc PUSH_I2)
      (bc I_GT_P))))

  (check-equal? (vm-regt->string int-greater-2>1-state)
                "int $0001")

  (define int-greater-1>0-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc PUSH_I1)
      (bc I_GT_P))))

  (check-equal? (vm-regt->string int-greater-1>0-state)
                "int $0001")

  (define int-greater-0>1-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I1)
      (bc PUSH_I0)
      (bc I_GT_P))))

  (check-equal? (vm-regt->string int-greater-0>1-state)
                "int $0000"))

(module+ test #| code len |#
  (inform-check-equal? (code-len bc-compare-code)
                       69
                       "compare code len"))
