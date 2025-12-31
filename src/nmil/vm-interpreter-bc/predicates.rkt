#lang racket/base

(require "../../6510.rkt"
         (only-in "../../ast/6510-resolver.rkt"
                  add-label-suffix)
         (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC
                  VM_INTERPRETER_INC_PC_2_TIMES)
         (only-in "../vm-runtime/vm-m1-slots.rkt"
                  DEC_REFCNT_M1_SLOT_RT__IF_PTR
                  DEC_REFCNT_M1_SLOT_RZ__IF_PTR)
         (only-in "../vm-runtime/vm-memory-map.rkt"
                  ZP_RT
                  ZP_RZ
                  ZP_CELL_STACK_TOS
                  ZP_CELL_STACK_HB_PTR
                  ZP_CELL_STACK_LB_PTR)
         (only-in "../vm-runtime/vm-register-functions.rkt"
                  WRITE_INT1_TO_RT
                  WRITE_INT0_TO_RT)
         (only-in "../vm-runtime/vm-register-functions.rkt"
                  CP_RT_TO_RZ))

(provide BC_I_Z_P
         BC_INT_P
         BC_CELL_EQ_P
         BC_CONS_PAIR_P)

(define BC_I_Z_P
  (add-label-suffix
   "__" "__I_Z_P"
  (list
   (label BC_I_Z_P)
          (LDA ZP_RT+1)
          (BNE IS_NOT_ZERO__)
          (LDA ZP_RT)
          (CMP !$03)
          (BEQ IS_ZERO__)

   (label IS_NOT_ZERO__)
          (LDA !$00)
          (STA ZP_RT+1)
          (LDA !$03)
          (STA ZP_RT)
          (JMP VM_INTERPRETER_INC_PC)

   (label IS_ZERO__)
          (LDA !$01)
          (STA ZP_RT+1)
          (JMP VM_INTERPRETER_INC_PC))))

(define BC_INT_P
  (add-label-suffix
   "__" "__INT_P"
  (list
   (label BC_INT_P)
          (LDA ZP_RT)
          (LDX !$01)
          (AND !$03)
          (CMP !$03)
          (BEQ IS_INT__)
          (JSR DEC_REFCNT_M1_SLOT_RT__IF_PTR)
          (LDA !TAGGED_INT_0_LB)
          (LDX !$00)
   (label IS_INT__)
          (STA ZP_RT)
          (STX ZP_RT+1)
          (JMP VM_INTERPRETER_INC_PC))))

(define BC_CONS_PAIR_P
  (list
   (label BC_CONS_PAIR_P)
          (JSR CP_RT_TO_RZ) ;; A = content of ZP_RT (lowbyte)

          (BEQ IS_NO_PAIR_SINCE_NIL__BC_CONS_PAIR_P)
          (LSR)
          (BCS IS_NO_PAIR_SINCE_ATOM__BC_CONS_PAIR_P)
          ;; no check slot to be cell-array with len 2
          (LDY !$01)
          (LDA (ZP_RT),y)
          (CMP !$02) ;; code for cell array with len 2
          (BEQ storey__BC_CONS_PAIR_P) ;; is pair -> store y=1 as int

   (label IS_NO_PAIR_SINCE_NIL__BC_CONS_PAIR_P)
   (label IS_NO_PAIR_SINCE_ATOM__BC_CONS_PAIR_P)
          (LDY !$00)
   (label storey__BC_CONS_PAIR_P)
          (STY ZP_RT+1) ;; is pair, store y=1 into int
          (LDY !TAGGED_INT_0_LB)
          (STY ZP_RT) ;; is pair store int tag
          (JSR DEC_REFCNT_M1_SLOT_RZ__IF_PTR)
          (JMP VM_INTERPRETER_INC_PC)
          ))

(define BC_CELL_EQ_P
  (add-label-suffix
   "__" "__CELL_EQ_P"
  (list
   (label BC_CELL_EQ_P)
          (LDY ZP_CELL_STACK_TOS)
          (LDA (ZP_CELL_STACK_HB_PTR),y)
          (STA ZP_RZ+1)
          (CMP ZP_RT+1)
          (BNE NE_LB__)
          (LDA (ZP_CELL_STACK_LB_PTR),y)
          (STA ZP_RZ)
          (CMP ZP_RT)
          (BNE NE__)

          (JSR DEC_REFCNT_M1_SLOT_RT__IF_PTR)
          (JSR DEC_REFCNT_M1_SLOT_RZ__IF_PTR)
          (DEC ZP_CELL_STACK_TOS)
          (JSR WRITE_INT1_TO_RT)
          (JMP VM_INTERPRETER_INC_PC)

   (label NE_LB__)
          (LDA (ZP_CELL_STACK_LB_PTR),y)
          (STA ZP_RZ)
   (label NE__)
          (JSR DEC_REFCNT_M1_SLOT_RT__IF_PTR)
          (JSR DEC_REFCNT_M1_SLOT_RZ__IF_PTR)
          (DEC ZP_CELL_STACK_TOS)
          (JSR WRITE_INT0_TO_RT)
          (JMP VM_INTERPRETER_INC_PC))))
