#lang racket/base

(provide BC_PUSH_B
         BC_DUP
         BC_SWAP
         BC_POP
         BC_POP_TO_RA
         BC_POP_TO_RB
         BC_PUSH_I)

#|

 Bytecode implementation of comparison commands

 |#


(require "../../6510.rkt"
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function
                  define-vm-function-wol)
         (only-in "../vm-interpreter-loop.rkt"
                  ZP_VM_PC)
         (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC
                  VM_INTERPRETER_INC_PC_2_TIMES)
         (only-in "../vm-runtime/vm-cell-stack.rkt"
                  PUSH_XA_TO_EVLSTK
                  PUSH_INT_TO_EVLSTK
                  PUSH_RT_TO_EVLSTK_TAIL)
         (only-in "../vm-runtime/vm-m1-slots.rkt"
                  DEC_REFCNT_M1_SLOT_RT__IF_PTR
                  INC_REFCNT_M1_SLOT_RT)
         (only-in "../vm-runtime/vm-memory-map.rkt"
                  ZP_RT
                  ZP_RP
                  ZP_CELL_STACK_TOS
                  ZP_CELL_STACK_LB_PTR
                  ZP_CELL_STACK_HB_PTR
                  TAG_BYTE_BYTE_CELL))

(define-vm-function BC_PUSH_B
  (list
          (LDY !$01)
          (LDA (ZP_VM_PC),y)
          (LDX !TAG_BYTE_BYTE_CELL)
          (JSR PUSH_XA_TO_EVLSTK)
          (JMP VM_INTERPRETER_INC_PC_2_TIMES)))

(define-vm-function BC_DUP
  (list
          (JSR INC_REFCNT_M1_SLOT_RT__IF_PTR)
          (JSR PUSH_RT_TO_EVLSTK_TAIL)
          (JMP VM_INTERPRETER_INC_PC)))

(define-vm-function BC_SWAP
  (list
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
(define-vm-function-wol BC_POP
  (list
   (label BC_POP_TO_RB)
          (LDA !$00)
          (STA ZP_RBI)                    ;; initialize index to 0
          (JSR CP_RT_TO_RB)               ;; copy tos to rb
          (JMP VM_POP_EVLSTK_AND_INC_PC)  ;; no dec, since rb is refcounted too

   (label BC_POP_TO_RA)
          (LDA !$00)
          (STA ZP_RAI)                    ;; initialize index to 0
          (JSR CP_RT_TO_RA)               ;; copy tos to ra
          (JMP VM_POP_EVLSTK_AND_INC_PC)  ;; no dec, since ra is refcounted too

   (label BC_POP) ;;--------------------------------------------------------------------------------
          (JSR DEC_REFCNT_M1_SLOT_RT__IF_PTR)
          (JMP VM_POP_EVLSTK_AND_INC_PC)))

(define-vm-function BC_PUSH_I
  (list
          (LDY !$02)                             ;; index 1 past the byte code itself
          (LDA (ZP_VM_PC),y)                     ;; load high byte of int (not encoded)
          (TAX)                                  ;; -> X
          (DEY)                                  ;; index 2 past the byte code
          (LDA (ZP_VM_PC),y)                     ;; load low byte of int  -> A
          (JSR PUSH_INT_TO_EVLSTK)               ;; push A/X as int onto stack
          (LDA !$03)                             ;; increment program counter by 3 (bytecode + int)
          (JMP VM_INTERPRETER_INC_PC_A_TIMES)))  ;; interpreter loop
