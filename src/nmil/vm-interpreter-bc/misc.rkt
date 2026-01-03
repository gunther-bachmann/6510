#lang racket/base

(provide BC_BNOP
         BC_BREAK
         BC_GC_FL)

(require "../../6510.rkt"
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function)
         (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC
                  VM_INTERPRETER_INC_PC_2_TIMES))

(define-vm-function BC_BNOP
  (list
          (JSR $0100) ;; special jump address interpreted as cpu cycle reset
                      ;; MUST BE REMOVED WHEN DEPLOYED TO C64
          (JMP VM_INTERPRETER_INC_PC)))

(define-vm-function BC_BREAK
  (list
          (BRK)))

(define-vm-function BC_GC_FL
  (list
          (JSR GC_ALL)
          (JMP VM_INTERPRETER_INC_PC_2_TIMES)))
