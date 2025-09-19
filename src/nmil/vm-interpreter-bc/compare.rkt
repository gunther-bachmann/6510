#lang racket/base

#|

  Bytecode implementation of comparison commands

|#


(require (only-in racket/list
                  flatten)
         "../../6510.rkt"
         (only-in "../../ast/6510-resolver.rkt"
                  add-label-suffix)
         (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC)
         (only-in "../vm-memory-map.rkt"
                  ZP_RT
                  ZP_RP)
         (only-in "../vm-cell-stack.rkt"
                  POP_CELL_EVLSTK_TO_RP)
         (only-in "../vm-register-functions.rkt"
                  WRITE_INT0_TO_RT
                  WRITE_INT1_TO_RT)
         (only-in "./push_n_pop.rkt"
                  BC_PUSH_B))

(provide BC_B_GT_P
         BC_B_LT_P
         BC_B_GE_P
         BC_I_GT_P)

(define BC_B_GT_P
  (add-label-suffix
   "__" "__BC_B_GT_P"
   (list
    (label BC_B_GT_P)
           (JSR POP_CELL_EVLSTK_TO_RP)
           (LDA ZP_RP+1)
           (CMP ZP_RT+1)
           (BMI GREATER__)
           (JSR WRITE_INT0_TO_RT)
           (JMP VM_INTERPRETER_INC_PC)
    (label GREATER__)
           (JSR WRITE_INT1_TO_RT)
           (JMP VM_INTERPRETER_INC_PC))))

(define BC_B_LT_P
  (add-label-suffix
   "__" "__BC_B_LT_P"
   (list
    (label BC_B_LT_P)
           (JSR POP_CELL_EVLSTK_TO_RP)
           (LDA ZP_RT+1)
           (CMP ZP_RP+1)
           (BPL GREATER_OR_EQUAL__)
           (JSR WRITE_INT1_TO_RT)
           (JMP VM_INTERPRETER_INC_PC)
    (label GREATER_OR_EQUAL__)
           (JSR WRITE_INT0_TO_RT)
           (JMP VM_INTERPRETER_INC_PC))))

(define BC_B_GE_P
  (add-label-suffix
   "__" "__BC_B_GE_P"
   (list
    (label BC_B_GE_P)
           (JSR POP_CELL_EVLSTK_TO_RP)
           (LDA ZP_RP+1)
           (CMP ZP_RT+1)
           (BPL GREATER_OR_EQUAL__)
           (JSR WRITE_INT1_TO_RT)
           (JMP VM_INTERPRETER_INC_PC)
    (label GREATER_OR_EQUAL__)
           (JSR WRITE_INT0_TO_RT)
           (JMP VM_INTERPRETER_INC_PC))))

(define BC_I_GT_P
  (add-label-suffix
   "__" "__I_GT_P"
   (list
    (label BC_I_GT_P)
           (LDA ZP_RT)                  ;; TODO: optimize by using POP_CELL_EVLSTK_TO_RP, but take care to change branch commands accordingly
           (STA ZP_RP)
           (LDA ZP_RT+1)
           (STA ZP_RP+1)
           (JSR POP_CELL_EVLSTK_TO_RT)
           (LDA ZP_RT)
           (CMP ZP_RP)
           (BMI GREATER__)
           (BNE LESS_OR_EQUAL__)
           (LDA ZP_RT+1)
           (CMP ZP_RP+1)
           (BMI GREATER__)
    (label LESS_OR_EQUAL__)
           (JSR WRITE_INT0_TO_RT)
           (JMP VM_INTERPRETER_INC_PC)
    (label GREATER__)
           (JSR WRITE_INT1_TO_RT)
           (JMP VM_INTERPRETER_INC_PC))))
