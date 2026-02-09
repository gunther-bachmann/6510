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
                  vm-interpreter)
         (only-in "../vm-bc-opcode-definitions.rkt"
                  mark-bc-breakpoint))

(module+ test #|  |#
  (require (only-in "../test-utils.rkt"
                    regression-test)
           "./test-utils.rkt"))

(define RECURSE ;;  -> point struct
  (bc-resolve
   (list
    (label RECURSE)
           (byte 6) ;; occupy some space, even though not needed
           (bc WRITE_TO_L0)
           (bc I_Z_P)
           (bc T_P_BRA) (bc-rel-ref RET_RECURSE)
           (bc PUSH_L0)
           (bc IDEC)
           (bc CALL) (word-ref RECURSE)
           (bc POP)
           (bc PUSH_L0)
           (bc RET) ;; explicitly no tail call

    (label RET_RECURSE)
           (bc PUSH_I1)
           (bc RET))))

(module+ test #| recurse |#
  (define recurse-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I) (byte 41) (byte 0)
       (bc CALL) (word-ref RECURSE)
       (bc BREAK))
      RECURSE)
     #f))

  (regression-test
   recurse-state
   "recurse"
   (check-equal? (vm-stack->strings recurse-state)
                 (list "stack holds 2 items"
                       (format "int $0029  (rt)")
                       "ptr NIL"))))

(module+ test #| recurse |#
  (define recurse-fail-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I) (byte 42) (byte 0)
       (bc CALL) (word-ref RECURSE)
       (bc BREAK))
      RECURSE)
     ))

  (regression-test
   recurse-fail-state
   "this will result in a new page for locals, return does not restore the correct (previous page)"
   (check-equal? (vm-stack->strings recurse-fail-state)
                 (list "stack holds 2 items"
                       (format "int $002a  (rt)")
                       "ptr NIL"))))
