#lang racket/base

(module+ test
  (require (only-in "./cell-pair.rkt"
                    BC_CxxR
                    BC_PUSH_NIL
                    BC_CONS
                    BC_COONS
                    BC_NIL_P
                    BC_CAR
                    BC_CDR)
           (only-in "./push_const.rkt"
                    BC_PUSH_CONST_NUM_SHORT)
           "./test-utils.rkt")

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
                                             "BC_PUSH_INT2"
                                             "BC_BREAK")))

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
                (list "stack holds 2 items"
                      "int $0001  (rt)"
                      "ptr NIL"))

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
                (list "stack holds 2 items"
                      "int $0002  (rt)"
                      "ptr NIL"))

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
                (list "stack holds 2 items"
                      "int $0001  (rt)"
                      "ptr NIL"))

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
                (list "stack holds 2 items"
                      "int $0002  (rt)"
                      "ptr NIL")))

(module+ test #| bc-push-const-nil |#
  (define bc-push-const-nil-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_NIL)
      (bc BREAK))))

  (check-equal? (vm-stack->strings bc-push-const-nil-state)
                (list "stack holds 2 items"
                      "ptr NIL  (rt)"
                      "ptr NIL")))

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
                   (list "stack holds 2 items"
                         (format "ptr[1] $~a02  (rt)" (byte->hex-string PAGE_AVAIL_0))
                         "ptr NIL"))
   (check-equal? (vm-deref-cell-pair-w->string bc-cons-state (+ PAGE_AVAIL_0_W #x02))
                    "(int $0000 . ptr NIL)"))

(module+ test #| bc-nil-p |#
  (define bc-nil-p-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_NIL)
      (bc NIL_P))))

  (check-equal? (vm-stack->strings bc-nil-p-state)
                (list "stack holds 2 items"
                      "int $0001  (rt)"
                      "ptr NIL"))

  (define bc-nil-p-2-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_NIL)
      (bc PUSH_I2)
      (bc CONS)
      (bc NIL_P))
     ))

  (check-equal? (vm-deref-cell-pair-w->string bc-nil-p-2-state (+ PAGE_AVAIL_0_W #x02))
                "(int $0002 . ptr NIL)")
  (check-equal? (vm-stack->strings bc-nil-p-2-state)
                (list "stack holds 2 items"
                      "int $0000  (rt)"
                      "ptr NIL")))

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
                 (list "stack holds 2 items"
                       "int $0002  (rt)"
                       "ptr NIL")))

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
                 (list "stack holds 2 items"
                       "ptr NIL  (rt)"
                       "ptr NIL")))
