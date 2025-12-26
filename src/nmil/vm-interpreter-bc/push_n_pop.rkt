#lang racket/base

#|

Bytecode implementation of comparison commands

TODO: get tests (still in vm-interpreter) into this file

|#


(require (only-in racket/list
                  flatten)
         "../../6510.rkt"
         (only-in "../../ast/6510-resolver.rkt"
                  add-label-suffix)
         (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC
                  VM_INTERPRETER_INC_PC_2_TIMES)
         (only-in "../vm-interpreter-loop.rkt"
                  ZP_VM_PC)
         (only-in "../vm-runtime/vm-cell-stack.rkt"
                  PUSH_XA_TO_EVLSTK
                  PUSH_INT_TO_EVLSTK
                  PUSH_RT_TO_EVLSTK)
         (only-in "../vm-runtime/vm-memory-map.rkt"
                  ZP_RT
                  ZP_RP
                  ZP_CELL_STACK_TOS
                  ZP_CELL_STACK_LB_PTR
                  ZP_CELL_STACK_HB_PTR
                  TAG_BYTE_BYTE_CELL)
         (only-in "../vm-runtime/vm-m1-slots-n.rkt"
                  DEC_REFCNT_M1_SLOT_RT_N
                  INC_REFCNT_M1_SLOT_RT_N))

(provide BC_PUSH_B
         BC_DUP
         BC_SWAP
         BC_POP
         BC_POP_TO_RA
         BC_POP_TO_RB
         BC_PUSH_I)

(define BC_PUSH_B
  (list
   (label BC_PUSH_B)
          (LDY !$01)
          (LDA (ZP_VM_PC),y)
          (LDX !TAG_BYTE_BYTE_CELL)
          (JSR PUSH_XA_TO_EVLSTK)
          (JMP VM_INTERPRETER_INC_PC_2_TIMES)))

(define BC_DUP
  (list
   (label BC_DUP)
          (JSR INC_REFCNT_M1_SLOT_RT_N)
          (JSR PUSH_RT_TO_EVLSTK)
          (JMP VM_INTERPRETER_INC_PC)))

(define BC_SWAP
  (list
   (label BC_SWAP)
          (LDY ZP_CELL_STACK_TOS)
          (LDA (ZP_CELL_STACK_LB_PTR),y)
          (TAX)
          (LDA ZP_RT)
          (STA (ZP_CELL_STACK_LB_PTR),y)
          (STX ZP_RT)
          (LDA (ZP_CELL_STACK_HB_PTR),y)
          (TAX)
          (LDA ZP_RT+1)
          (STA (ZP_CELL_STACK_HB_PTR),y)
          (STX ZP_RT+1)
          (JMP VM_INTERPRETER_INC_PC)))

(define BC_POP_TO_RB '())
(define BC_POP_TO_RA '())
(define BC_POP
  (list
   (label BC_POP_TO_RB)
          (LDA !$00)
          (STA ZP_RBI)          ;; initialize index to 0
          (JSR CP_RT_TO_RB)     ;; copy tos to rb
          (JMP VM_POP_EVLSTK_AND_INC_PC)

   (label BC_POP_TO_RA)
          (LDA !$00)
          (STA ZP_RAI)          ;; initialize index to 0
          (JSR CP_RT_TO_RA)     ;; copy tos to ra
          (JMP VM_POP_EVLSTK_AND_INC_PC)

   (label BC_POP) ;;--------------------------------------------------------------------------------
          (JSR DEC_REFCNT_M1_SLOT_RT_N) ;; no dec, since ra is refcounted too
          (JMP VM_POP_EVLSTK_AND_INC_PC)))

(define BC_PUSH_I
  (list
   (label BC_PUSH_I)
          (LDY !$02)                             ;; index 1 past the byte code itself
          (LDA (ZP_VM_PC),y)                     ;; load high byte of int (not encoded)
          (TAX)                                  ;; -> X
          (DEY)                                  ;; index 2 past the byte code
          (LDA (ZP_VM_PC),y)                     ;; load low byte of int  -> A
          (JSR PUSH_INT_TO_EVLSTK)         ;; push A/X as int onto stack
          (LDA !$03)                             ;; increment program counter by 3 (bytecode + int)
          (JMP VM_INTERPRETER_INC_PC_A_TIMES)))  ;; interpreter loop
