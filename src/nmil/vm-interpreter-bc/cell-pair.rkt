#lang racket/base

(require "../../6510.rkt")
(require (only-in "../../ast/6510-resolver.rkt" add-label-suffix))
(require (only-in racket/list flatten))

(require (only-in "../vm-memory-map.rkt"
                  ZP_RT
                  ZP_RZ))
(require (only-in "../vm-memory-manager.rkt"
                  INC_REFCNT_RT
                  DEC_REFCNT_RZ))
(require (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC))
(require (only-in "../vm-lists.rkt"
                  VM_CxxR
                  VM_CONS__REFCNTD
                  VM_CAR
                  VM_CDR))
(require (only-in "../vm-mm-cell-stack.rkt"
                  PUSH_NIL_TO_EVLSTK))
(require (only-in "../vm-mm-register-functions.rkt"
                  CP_RT_TO_RZ))

(provide BC_CxxR
         BC_PUSH_NIL
         BC_CONS
         BC_COONS
         BC_NIL_P
         BC_CDR
         BC_CAR)

(define BC_CONS
  (list
   (label BC_CONS)
          (JSR VM_CONS__REFCNTD)
          (JMP VM_INTERPRETER_INC_PC)))

(define BC_CxxR
  (list
   (label BC_CxxR)
          (LDX ZP_RT)
          (STX ZP_RZ)
          (LDX ZP_RT+1)
          (STX ZP_RZ+1)
          ;; prepared offset for branch in VM_CxxR call ($00 = CAAR, $06 = CADR, $0c = CDAR, $12 = CDDR)
          (JSR VM_CxxR)
          (JSR INC_REFCNT_RT)
          (JSR DEC_REFCNT_RZ)
          (JMP VM_INTERPRETER_INC_PC)))

(define BC_PUSH_NIL
  (list
   (label BC_PUSH_NIL)
          (JSR PUSH_NIL_TO_EVLSTK)        ;; push NIL on the stack
          (JMP VM_INTERPRETER_INC_PC)))         ;; interpreter loop

(define BC_COONS
  (list
   (label BC_COONS)
          (JSR VM_CONS__REFCNTD)
          (JSR VM_CONS__REFCNTD)
          (JMP VM_INTERPRETER_INC_PC)))

(define BC_NIL_P
  (list
   (label BC_NIL_P)
          (JSR CP_RT_TO_RZ)             ;; keep for dec-refcnt
          (JSR VM_NIL_P)                      ;; if rt is NIL replace with true (int 1) else replace with false (int 0)
          (JSR DEC_REFCNT_RZ)
          (JMP VM_INTERPRETER_INC_PC)))         ;; interpreter loop

(define BC_CDR
  (list
   (label BC_CDR)
          (JSR CP_RT_TO_RZ)
          (JSR VM_CDR)
          (JSR INC_REFCNT_RT)
          (JSR DEC_REFCNT_RZ)
          (JMP VM_INTERPRETER_INC_PC)))

(define BC_CAR
  (list
   (label BC_CAR)
          (JSR CP_RT_TO_RZ)
          (JSR VM_CAR)
          (JSR INC_REFCNT_RT)
          (JSR DEC_REFCNT_RZ)
          (JMP VM_INTERPRETER_INC_PC)))
