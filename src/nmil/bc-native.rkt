#lang racket/base

(require (only-in racket/list flatten))
(require "./bc-ast.rkt")
(require (only-in "./bc-resolver.rkt" bc-resolve bc-bytes))

(require (only-in "../cisc-vm/stack-virtual-machine.rkt"
                  CONS
                  CAR
                  CDR
                  GOTO
                  RET
                  BYTE+
                  INT+
                  INT-
                  BRA
                  CALL
                  NIL?
                  TAIL_CALL

                  PUSH_NIL
                  PUSH_LOCAL
                  PUSH_GLOBAL
                  PUSH_STRUCT_FIELD

                  POP_TO_LOCAL
                  POP_TO_GLOBAL))

(require [only-in "./vm-interpreter.rkt"
                  vm-interpreter
                  bc
                  PUSH_I
                  PUSH_B
                  CELL_EQ
                  EXT
                  CAAR
                  CADR
                  CDAR
                  CDDR
                  COONS
                  POP
                  DUP
                  BNOP
                  INT_0_P
                  INC_INT
                  MAX_INT
                  F_P_BRA
                  T_P_BRA
                  INT_GREATER_P
                  CONS_PAIR_P
                  T_P_RET
                  F_P_RET
                  NIL?_RET_LOCAL_0_POP_1
                  INT_P
                  SWAP
                  POP_TO_LOCAL_0
                  POP_TO_LOCAL_1
                  POP_TO_LOCAL_2
                  POP_TO_LOCAL_3
                  WRITE_TO_LOCAL_0
                  WRITE_TO_LOCAL_1
                  WRITE_TO_LOCAL_2
                  WRITE_TO_LOCAL_3
                  PUSH_LOCAL_0
                  PUSH_LOCAL_1
                  PUSH_LOCAL_2
                  PUSH_LOCAL_3
                  PUSH_LOCAL_0_CAR
                  PUSH_LOCAL_1_CAR
                  PUSH_LOCAL_2_CAR
                  PUSH_LOCAL_3_CAR
                  PUSH_LOCAL_0_CDR
                  PUSH_LOCAL_1_CDR
                  PUSH_LOCAL_2_CDR
                  PUSH_LOCAL_3_CDR
                  PUSH_I0
                  PUSH_I1
                  PUSH_I2
                  PUSH_IM1
                  WRITE_FROM_LOCAL_0
                  WRITE_FROM_LOCAL_1
                  WRITE_FROM_LOCAL_2
                  WRITE_FROM_LOCAL_3
                  NATIVE])
(require (only-in "./vm-memory-manager.rkt" ZP_VM_PC shorten-cell-strings shorten-cell-string))

(require "../6510.rkt")
(require (only-in "../tools/6510-interpreter.rkt" memory-list))

(module+ test #|  |#
  (require "../6510-test-utils.rkt")

  (require (only-in "./vm-interpreter-test-utils.rkt" run-bc-wrapped-in-test- vm-list->strings))
  (require (only-in "../cisc-vm/stack-virtual-machine.rkt" BRK))
  (require (only-in "../tools/6510-interpreter.rkt" cpu-state-clock-cycles))

  (require (only-in "./vm-memory-manager.rkt"
                    vm-cell-at-nil?
                    vm-page->strings
                    vm-stack->strings
                    vm-regt->string
                    vm-cell-at->string
                    vm-cell->string
                    vm-deref-cell-pair-w->string))
  (require (only-in "../util.rkt" bytes->int format-hex-byte format-hex-word))


  (define PAGE_AVAIL_0 #x97)
  (define PAGE_AVAIL_0_W #x9700)
  (define PAGE_AVAIL_1 #x96)
  (define PAGE_AVAIL_1_W #x9600)

  (define (wrap-bytecode-for-test bc)
    (append (list (org #x7000)
                  (JSR VM_INITIALIZE_MEMORY_MANAGER)
                  (JSR VM_INITIALIZE_CALL_FRAME)
                  (JSR VM_INTERPRETER_INIT)
                  (JMP VM_INTERPRETER))
            (list (org #x8000))
            (flatten bc)
            (list (org #xa000))
            vm-interpreter))

  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (run-bc-wrapped-in-test- bc wrapped-code debug)))


(define BC_ADD_NATIVE
  (list
   (label BC_ADD_NATIVE)
          (byte 1) ;; locals
          (bc POP_TO_LOCAL_0)
          (bc NATIVE)

          (CLC)
          (LDY !$00) ;; local 0
          (LDA (ZP_LOCALS_HB_PTR),y)
          (ADC ZP_RT+1) ;; TOS
          (STA ZP_RT+1)
          (JSR RETURN_TO_BC)    ;; uses address on the stack (=> jsr) to calculate next bc to execute

          (bc RET)
          ))

(module+ test #| bc_add_native |#
  (define add-native-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_B) (byte 4)
       (bc PUSH_B) (byte 6)
       (bc CALL) (word-ref BC_ADD_NATIVE)
       (bc PUSH_I1)
       (bc BRK))
      (list (org #x8F00))
      BC_ADD_NATIVE)
     ))
  (check-equal? (vm-stack->strings add-native-state)
                (list "stack holds 2 items"
                      "int $0001  (rt)"
                      "byte $0a")
                "native adding bytes 4 + 6 = 10 => 0a"))

(define BC_ADD_NATIVE_2
  (list
   (label BC_ADD_NATIVE_2)
          (byte 1) ;; locals
          (bc POP_TO_LOCAL_0)
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
       (bc PUSH_B) (byte 4)
       (bc PUSH_B) (byte 6)
       (bc CALL) (word-ref BC_ADD_NATIVE_2)
       (bc PUSH_I2)
       (bc BRK))
      (list (org #x8F00))
      BC_ADD_NATIVE_2)
     ))
  (check-equal? (vm-stack->strings add-native-state-2)
                (list "stack holds 2 items"
                      "int $0002  (rt)"
                      "byte $0a")
                "native adding bytes 4 + 6 = 10 => 0a"))
