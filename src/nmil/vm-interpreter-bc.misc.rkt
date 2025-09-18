#lang racket/base

(require "../6510.rkt")

(require (only-in "./vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC))

(provide BC_BNOP)

(define BC_BNOP
  (list
   (label BC_BNOP)
          (JSR $0100)
          (JMP VM_INTERPRETER_INC_PC)))
