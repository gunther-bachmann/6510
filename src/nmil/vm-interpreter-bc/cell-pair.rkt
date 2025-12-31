#lang racket/base

(require (only-in racket/list
                  flatten)
         "../../6510.rkt"
         (only-in "../../ast/6510-resolver.rkt"
                  add-label-suffix)
         (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC)
         (only-in "../vm-runtime/vm-lists.rkt"
                  VM_CxxR
                  VM_CONS__REFCNTD
                  VM_CAR
                  VM_CDR)
         (only-in "../vm-runtime/vm-m1-slots.rkt"
                  INC_REFCNT_M1_SLOT_RT
                  DEC_REFCNT_M1_SLOT_RZ__IF_PTR)
         (only-in "../vm-runtime/vm-memory-map.rkt"
                  ZP_RT
                  ZP_RZ)
         (only-in "../vm-runtime/vm-cell-stack.rkt"
                  PUSH_NIL_TO_EVLSTK)
         (only-in "../vm-runtime/vm-register-functions.rkt"
                  CP_RT_TO_RZ))

(provide BC_CxxR
         BC_PUSH_NIL
         BC_CONS
         BC_COONS
         BC_NIL_P
         BC_CDR
         BC_CAR

         bc-cell-pair-code)

(define BC_CONS
  (list
   (label BC_CONS)
          (JSR VM_CONS__REFCNTD)
          (JMP VM_INTERPRETER_INC_PC)))

(define BC_CxxR
  (list
   (label BC_CxxR)
          (JSR CP_RT_TO_RZ)             ;; keep for dec-refcnt
          ;; prepared offset for branch in VM_CxxR call ($00 = CAAR, $06 = CADR, $0c = CDAR, $12 = CDDR)
          (JSR VM_CxxR)
          (JSR INC_REFCNT_M1_SLOT_RT__IF_PTR)
          (JSR DEC_REFCNT_M1_SLOT_RZ) ;; rz must be a pair ptr otherwise cxxr would have failed
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
          (JSR DEC_REFCNT_M1_SLOT_RZ__IF_PTR)
          (JMP VM_INTERPRETER_INC_PC)))         ;; interpreter loop

(define BC_CDR
  (list
   (label BC_CDR)
          (JSR CP_RT_TO_RZ)
          (JSR VM_CDR)
          (JSR INC_REFCNT_M1_SLOT_RT__IF_PTR)
          (JSR DEC_REFCNT_M1_SLOT_RZ)           ;; must be a ptr else cdr would have failed
          (JMP VM_INTERPRETER_INC_PC)))

(define BC_CAR
  (list
   (label BC_CAR)
          (JSR CP_RT_TO_RZ)
          (JSR VM_CAR)
          (JSR INC_REFCNT_M1_SLOT_RT__IF_PTR)
          (JSR DEC_REFCNT_M1_SLOT_RZ)           ;; must be a ptr else car would have failed
          (JMP VM_INTERPRETER_INC_PC)))


(define bc-cell-pair-code
  (append
   BC_CxxR
   BC_PUSH_NIL
   BC_CONS
   BC_COONS
   BC_NIL_P
   BC_CDR
   BC_CAR))
