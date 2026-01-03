#lang racket/base

(provide BC_B_GT_P
         BC_B_LT_P
         BC_B_GE_P
         BC_I_GT_P

         bc-compare-code)

#|

  Bytecode implementation of comparison commands

 |#


(require "../../6510.rkt"
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function)
         (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC)
         (only-in "../vm-runtime/vm-cell-stack.rkt"
                  POP_CELL_EVLSTK_TO_RP)
         (only-in "../vm-runtime/vm-memory-map.rkt"
                  ZP_RT
                  ZP_RP)
         (only-in "../vm-runtime/vm-register-functions.rkt"
                  WRITE_INT0_TO_RT
                  WRITE_INT1_TO_RT)
         (only-in "./push_n_pop.rkt"
                  BC_PUSH_B))

(define-vm-function BC_B_GT_P
   (list
           (JSR POP_CELL_EVLSTK_TO_RP)
           (LDA ZP_RP+1)
           (CMP ZP_RT+1)

    (label BPL_GREATER_WRITE_INT0)
           (BPL GREATER__)
    (label LESSOE__BC_B_GT_P)
           (JSR WRITE_INT1_TO_RT)
           (JMP VM_INTERPRETER_INC_PC)
    (label GREATER__BC_B_GT_P)
           (JSR WRITE_INT0_TO_RT)
           (JMP VM_INTERPRETER_INC_PC)))

(define-vm-function BC_B_LT_P
   (list
           (JSR POP_CELL_EVLSTK_TO_RP)
           (LDA ZP_RT+1)
           (CMP ZP_RP+1)
           (JMP BPL_GREATER_WRITE_INT0)))

(define-vm-function BC_B_GE_P
   (list
           (JSR POP_CELL_EVLSTK_TO_RP)
           (LDA ZP_RP+1)
           (CMP ZP_RT+1)
           (JMP BPL_GREATER_WRITE_INT0)))

(define-vm-function BC_I_GT_P
   (list
           (JSR POP_CELL_EVLSTK_TO_RP)
           (LDA ZP_RP)
           (CMP ZP_RT)
           (BMI GREATER__BC_B_GT_P)
           (BNE LESSOE__BC_B_GT_P)
           (LDA ZP_RP+1)
           (CMP ZP_RT+1)
           (JMP BPL_GREATER_WRITE_INT0)))


(define bc-compare-code
  (append
   BC_B_GT_P
   BC_B_LT_P
   BC_B_GE_P
   BC_I_GT_P))
