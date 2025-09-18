#lang racket/base

#|

Bytecode implementation of comparison commands

TODO: get tests (still in vm-interpreter) into this file

|#


(require "../6510.rkt")
(require (only-in "../ast/6510-resolver.rkt" add-label-suffix))
(require (only-in racket/list flatten))
(require (only-in "./vm-memory-map.rkt"
                  ZP_RT
                  ZP_RP
                  ZP_VM_PC
                  ZP_CELL_STACK_TOS
                  ZP_CELL_STACK_LB_PTR
                  ZP_CELL_STACK_HB_PTR))
(require (only-in "./vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC
                  VM_INTERPRETER_INC_PC_2_TIMES))
(require (only-in "./vm-mm-cell-stack.rkt"
                  PUSH_XA_TO_EVLSTK
                  PUSH_RT_TO_EVLSTK_IF_NONEMPTY))


(provide BC_PUSH_B
         BC_DUP
         BC_SWAP)

(define BC_PUSH_B
  (list
   (label BC_PUSH_B)
          (LDY !$01)
          (LDA (ZP_VM_PC),y)
          (LDX !$ff)
          (JSR PUSH_XA_TO_EVLSTK)
          (JMP VM_INTERPRETER_INC_PC_2_TIMES)))

(define BC_DUP
  (list
   (label BC_DUP)
          (JSR INC_REFCNT_RT)
          (JSR PUSH_RT_TO_EVLSTK_IF_NONEMPTY)
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
