#lang racket/base

(require "../../6510.rkt"
         (only-in "../../ast/6510-resolver.rkt"
                  add-label-suffix)
         (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC
                  VM_INTERPRETER_INC_PC_2_TIMES)
         (only-in "../vm-runtime/vm-memory-manager.rkt"
                  DEC_REFCNT_RT
                  DEC_REFCNT_RZ)
         (only-in "../vm-runtime/vm-memory-map.rkt"
                  ZP_RT
                  ZP_RZ
                  ZP_CELL_STACK_TOS
                  ZP_CELL_STACK_HB_PTR
                  ZP_CELL_STACK_LB_PTR)
         (only-in "../vm-runtime/vm-register-functions.rkt"
                  WRITE_INT1_TO_RT
                  WRITE_INT0_TO_RT))

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
          (AND !$83)
          (CMP !$03)
          (BEQ IS_INT__)
          (JSR DEC_REFCNT_RT)
          (LDA !$03)
          (LDX !$00)
   (label IS_INT__)
          (STA ZP_RT)
          (STX ZP_RT+1)
          (JMP VM_INTERPRETER_INC_PC))))

(define BC_CONS_PAIR_P
  (list
   (label BC_CONS_PAIR_P)
          (JSR CP_RT_TO_RZ)

          (LDX !$03) ;; low byte of int (for bool)
          (STX ZP_RT)
          (CMP !$01)
          (BEQ IS_NO_PAIR_SINCE_NIL__BC_CONS_PAIR_P)
          (AND !$03)
          (CMP !$01)
          (BEQ IS_PAIR__BC_CONS_PAIR_P)
   (label IS_NO_PAIR_SINCE_NIL__BC_CONS_PAIR_P)
          (LDA !$00)
   (label IS_PAIR__BC_CONS_PAIR_P)
          (STA ZP_RT+1)
          (JSR DEC_REFCNT_RZ)
          (JMP VM_INTERPRETER_INC_PC)))

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

          (JSR DEC_REFCNT_RT)
          (JSR DEC_REFCNT_RZ)
          (DEC ZP_CELL_STACK_TOS)
          (JSR WRITE_INT1_TO_RT)
          (JMP VM_INTERPRETER_INC_PC)

   (label NE_LB__)
          (LDA (ZP_CELL_STACK_LB_PTR),y)
          (STA ZP_RZ)
   (label NE__)
          (JSR DEC_REFCNT_RT)
          (JSR DEC_REFCNT_RZ)
          (DEC ZP_CELL_STACK_TOS)
          (JSR WRITE_INT0_TO_RT)
          (JMP VM_INTERPRETER_INC_PC))))
