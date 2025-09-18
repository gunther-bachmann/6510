#lang racket/base

(require "../../6510.rkt")

(require (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC
                  VM_INTERPRETER_INC_PC_2_TIMES))

(provide BC_BNOP
         BC_BREAK
         BC_GC_FL)

(define BC_BNOP
  (list
   (label BC_BNOP)
          (JSR $0100)
          (JMP VM_INTERPRETER_INC_PC)))

(define BC_BREAK
  (list
   (label BC_BREAK)
          (BRK)))

(define BC_GC_FL
  (list
   (label BC_GC_FL)
          (JSR GC_ALL)
          (JMP VM_INTERPRETER_INC_PC_2_TIMES)))
