#lang racket/base

#|
   this file is implementing of the fibonacci function

|#

(require "../../6510.rkt"
         "../vm-bc-ast.rkt"
         (only-in "../vm-bc-opcode-definitions.rkt"
                  bc
                  fetch-opcode-list)
         (only-in "../vm-bc-resolver.rkt"
                  bc-resolve
                  bc-bytes)
         (only-in "../vm-interpreter.rkt"
                  vm-interpreter))

(module+ test #|  |#
  (require (only-in "../test-utils.rkt"
                    regression-test)
           "./test-utils.rkt"))

(define FIB ;;  -> point struct
  (bc-resolve
   (list
    (label FIB)
           (byte 1)
           (bc WRITE_TO_L0)
           (bc PUSH_I) (byte 3) (byte 0)
           (bc I_GT_P)
           (bc T_P_BRA) (bc-rel-ref FIB_R1)
           (bc PUSH_L0)
           (bc IDEC)
           (bc CALL) (word-ref FIB)
           (bc PUSH_L0)
           (bc IDEC)
           (bc IDEC)
           (bc CALL) (word-ref FIB)
           (bc IADD)
           (bc RET)

    (label FIB_R1)
           (bc PUSH_I1)
           (bc RET))))

(module+ test #| fibonacci |#
  (define fib8-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I) (byte 8) (byte 0)
       (bc CALL) (word-ref FIB)
       (bc BREAK))
      FIB)
     ))
  (regression-test
   fib8-state
   "fib8"
   (check-equal? (vm-stack->strings fib8-state)
                 (list "stack holds 2 items"
                       (format "int $0015  (rt)")
                       "ptr NIL"))))
