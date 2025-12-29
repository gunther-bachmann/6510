#lang racket/base

(require (only-in racket/list flatten)
         "../../6510.rkt"
         (only-in "../../tools/6510-interpreter.rkt"
                    cpu-state-clock-cycles)
         (only-in "../vm-bc-opcode-definitions.rkt"
                  bc)
         (only-in "../vm-interpreter.rkt"
                  vm-interpreter))

(module+ test #|  |#
  (require "../../6510-test-utils.rkt"
           (only-in "../vm-inspector-utils.rkt"
                    vm-stack-n->strings)
           (only-in "./test-utils.rkt"
                    wrap-bytecode-for-full-bc-test)
           (only-in "../vm-interpreter-test-utils.rkt"
                    run-bc-wrapped-in-test-
                    vm-list->strings))

  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-full-bc-test bc))
    (run-bc-wrapped-in-test- bc wrapped-code debug)))

(define BC_ADD_NATIVE
  (list
   (label BC_ADD_NATIVE)
          (byte 1) ;; locals
          (bc POP_TO_L0)
          (bc NATIVE)

          (CLC)
          (LDY !$00) ;; local 0
          (LDA (ZP_LOCALS_HB_PTR),y)
          (ADC ZP_RT+1) ;; TOS
          (STA ZP_RT+1)
          (JSR RETURN_TO_BC)    ;; uses address on the stack (=> jsr) to calculate next bc to execute

          (bc RET)              ;; bc interpretation continues here
          ))

(module+ test #| bc_add_native |#
  (define add-native-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc BNOP)
       (bc PUSH_B) (byte 4)
       (bc PUSH_B) (byte 6)
       (bc CALL) (word-ref BC_ADD_NATIVE)
       (bc PUSH_I1)
       (bc BREAK))
      (list (org #x8700))
      BC_ADD_NATIVE)
     ))
  (inform-check-equal? (cpu-state-clock-cycles add-native-state)
                       793)
  (check-equal? (vm-stack-n->strings add-native-state)
                (list "stack holds 3 items"
                      "int $0001  (rt)"
                      "byte $0a"
                      "ptr NIL")
                "native adding bytes 4 + 6 = 10 => 0a"))

(define BC_ADD_NATIVE_2
  (list
   (label BC_ADD_NATIVE_2)
          (byte 1) ;; locals
          (bc POP_TO_L0)
          (bc NATIVE) ;; jump off to native code

          (CLC)
          (LDY !$00) ;; local 0
          (LDA (ZP_LOCALS_HB_PTR),y)
          (ADC ZP_RT+1) ;; TOS
          (STA ZP_RT+1)

          (JMP BC_RET) ;; does execute BC_RET and returns to bc interpreter
   ))

(module+ test #| bc_add_native |#
  (define add-native-state-2
    (run-bc-wrapped-in-test
     (append
      (list
       (bc BNOP)
       (bc PUSH_B) (byte 4)
       (bc PUSH_B) (byte 6)
       (bc CALL) (word-ref BC_ADD_NATIVE_2)
       (bc PUSH_I2)
       (bc BREAK))
      (list (org #x8700))
      BC_ADD_NATIVE_2)
     ))
  (inform-check-equal? (cpu-state-clock-cycles add-native-state-2)
                       760)
  (check-equal? (vm-stack-n->strings add-native-state-2)
                (list "stack holds 3 items"
                      "int $0002  (rt)"
                      "byte $0a"
                      "ptr NIL")
                "native adding bytes 4 + 6 = 10 => 0a"))
