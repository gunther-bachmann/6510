#lang racket/base

#|

Bytecode implementation of comparison commands

TODO: get tests (still in vm-interpreter) into this file

|#


(require "../../6510.rkt")
(require (only-in "../../ast/6510-resolver.rkt" add-label-suffix))
(require (only-in racket/list flatten))
(require (only-in "../vm-memory-map.rkt"
                  ZP_RT
                  ZP_RP))
(require (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC))
(require (only-in "../vm-mm-cell-stack.rkt"
                  POP_CELL_EVLSTK_TO_RP))
(require (only-in "../vm-mm-register-functions.rkt"
                  WRITE_INT0_TO_RT
                  WRITE_INT1_TO_RT))
(require (only-in "./push_n_pop.rkt" BC_PUSH_B))

(provide BC_B_GT_P
         BC_B_LT_P
         BC_B_GE_P
         BC_I_GT_P)

;; @DC-B: B_GT_P, group: predicates
(define B_GT_P #x48)
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

;; @DC-B: B_LT_P, group: predicates
(define B_LT_P #xcc)
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

;; @DC-B: B_GE_P, group: predicates
(define B_GE_P #x4c)
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

;; @DC-B: I_GT_P, group: predicates
(define I_GT_P #xc8) ;; *I*​nt *G*​reater *T*​han *P*​redicates
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
