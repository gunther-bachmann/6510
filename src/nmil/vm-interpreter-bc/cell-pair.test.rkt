#lang racket/base

(module+ test
  (require "./test-utils.rkt")

  (require (only-in "./cell-pair.rkt"
                    BC_CxxR
                    BC_PUSH_NIL
                    BC_CONS
                    BC_COONS
                    BC_NIL_P
                    BC_CAR
                    BC_CDR))
  (require (only-in "./push_const.rkt"
                    BC_PUSH_CONST_NUM_SHORT))


  (define relevant-opcode-definitions (filtered-opcode-definitions
                                       (list "BC_CONS"
                                             "BC_COONS"
                                             "BC_CxxR"
                                             "BC_PUSH_NIL"
                                             "BC_NIL_P"
                                             "BC_CAR"
                                             "BC_CDR"

                                             "BC_PUSH_INT0"
                                             "BC_PUSH_INT1"
                                             "BC_PUSH_INT2")))

  (define (wrap-bytecode-for-test bc-to-wrap)
    (wrap-bytecode-for-bc-test
     bc-to-wrap
     relevant-opcode-definitions
     (list  BC_CxxR
            BC_PUSH_NIL
            BC_CONS
            BC_COONS
            BC_NIL_P
            BC_CAR
            BC_CDR
            ;; ---
            BC_PUSH_CONST_NUM_SHORT)))

  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (run-bc-wrapped-in-test- bc wrapped-code debug)))


(module+ test #| cxxr |#
  (define cxxr-0-state
    (run-bc-wrapped-in-test
      (list
         (bc PUSH_NIL)
         (bc PUSH_I2)
         (bc PUSH_I1)
         (bc CONS)
         (bc CONS)
         (bc CAAR))
      ))
  (check-equal? (vm-stack->strings cxxr-0-state)
                (list "stack holds 1 item"
                      "int $0001  (rt)"))

  (define cxxr-1-state
    (run-bc-wrapped-in-test
      (list
         (bc PUSH_NIL)
         (bc PUSH_I2)
         (bc PUSH_I1)
         (bc CONS)
         (bc CONS)
         (bc CDAR))
      ))
  (check-equal? (vm-stack->strings cxxr-1-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)"))

  (define cxxr-2-state
    (run-bc-wrapped-in-test
      (list
         (bc PUSH_I2)
         (bc PUSH_I1)
         (bc CONS)
         (bc PUSH_NIL)
         (bc CONS)
         (bc CADR))
      ))
  (check-equal? (vm-stack->strings cxxr-2-state)
                (list "stack holds 1 item"
                      "int $0001  (rt)"))

  (define cxxr-3-state
    (run-bc-wrapped-in-test
      (list
         (bc PUSH_I2)
         (bc PUSH_I1)
         (bc CONS)
         (bc PUSH_NIL)
         (bc CONS)
         (bc CDDR))
      ))
  (check-equal? (vm-stack->strings cxxr-3-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)")))

(module+ test #| bc-push-const-nil |#
  (define bc-push-const-nil-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_NIL)
      (bc BREAK))))

  (check-equal? (vm-stack->strings bc-push-const-nil-state)
                (list "stack holds 1 item"
                      "pair-ptr NIL  (rt)")))

(module+ test #| bc-cons |#
   (define bc-cons-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_NIL)
      (bc PUSH_I0)
      (bc CONS)
      (bc BREAK))
     ))

   (check-equal? (vm-stack->strings bc-cons-state)
                   (list "stack holds 1 item"
                         (format "pair-ptr[1] $~a05  (rt)" (format-hex-byte PAGE_AVAIL_0))))
   (check-equal? (vm-deref-cell-pair-w->string bc-cons-state (+ PAGE_AVAIL_0_W #x05))
                    "(int $0000 . pair-ptr NIL)"))

(module+ test #| bc-nil-p |#
  (define bc-nil-p-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_NIL)
      (bc NIL_P)
      (bc BREAK))))

  (check-equal? (vm-stack->strings bc-nil-p-state)
                (list "stack holds 1 item"
                      "int $0001  (rt)"))

  (define bc-nil-p-2-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_NIL)
      (bc PUSH_I2)
      (bc CONS)
      (bc NIL_P)
      (bc BREAK))))

  (check-equal? (vm-deref-cell-pair-w->string bc-nil-p-2-state (+ PAGE_AVAIL_0_W #x05))
                "(empty . pair-ptr NIL)")
  (check-equal? (vm-stack->strings bc-nil-p-2-state)
                (list "stack holds 1 item"
                      "int $0000  (rt)")))

(module+ test #| bc-car |#
   (define bc-car-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_NIL)
      (bc PUSH_I2)
      (bc CONS)
      (bc CAR)
      (bc BREAK))))

   (check-equal? (vm-stack->strings bc-car-state)
                 (list "stack holds 1 item"
                       "int $0002  (rt)")))

(module+ test #| bc-cdr |#
   (define bc-cdr-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_NIL)
      (bc PUSH_I2)
      (bc CONS)
      (bc CDR)
      (bc BREAK))))

   (check-equal? (vm-stack->strings bc-cdr-state)
                 (list "stack holds 1 item"
                       "pair-ptr NIL  (rt)")))
