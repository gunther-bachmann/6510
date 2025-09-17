#lang racket/base

(require "../6510.rkt")
(require (only-in "./vm-memory-map.rkt"
                  ZP_RT
                  ZP_RP))
(require (only-in "./vm-interpreter-loop.rkt" VM_INTERPRETER_INC_PC))
(require (only-in "./vm-mm-cell-stack.rkt" POP_CELL_EVLSTK_TO_RP))

(provide BC_BINC
         BC_BDEC
         BC_BADD)

(define BC_BINC
  (list
   (label BC_BINC)
          (INC ZP_RT+1)
          (JMP VM_INTERPRETER_INC_PC)))

(define BC_BDEC
  (list
   (label BC_BDEC)
          (DEC ZP_RT+1)
          (JMP VM_INTERPRETER_INC_PC)))

(define BC_BADD
  (list
   (label BC_BADD)
          (JSR POP_CELL_EVLSTK_TO_RP)
          (CLC)
          (LDA ZP_RT+1)
          (ADC ZP_RP+1)
          (STA ZP_RT+1)
          (JMP VM_INTERPRETER_INC_PC)))
