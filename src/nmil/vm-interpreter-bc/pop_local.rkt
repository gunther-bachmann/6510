#lang racket/base

(require (only-in racket/list
                  flatten)
         "../../6510.rkt"
         (only-in "../../ast/6510-resolver.rkt"
                  add-label-suffix)
         (only-in "../vm-runtime/vm-m1-slots-n.rkt"
                  DEC_REFCNT_M1_SLOT_RZ__IF_PTR_N
                  INC_REFCNT_M1_SLOT_RT_N))

(provide BC_POP_TO_LOCAL_SHORT
         BC_WRITE_TO_LOCAL_SHORT)

(define BC_WRITE_TO_LOCAL_SHORT '())
(define BC_POP_TO_LOCAL_SHORT
  (add-label-suffix
   "__" "__BC_POP_TO_LOCAL_SHORT"
  (flatten
   (list
    (label BC_POP_TO_LOCAL_SHORT)
           (LSR)
           (AND !$03)
           (PHA)
           (TAY)                                ;; index -> Y
           ;; decrement old local
           (LDA (ZP_LOCALS_LB_PTR),y)
           (BEQ POP_NO_GC__)
           (STA ZP_RZ)
           (LDA (ZP_LOCALS_HB_PTR),y)
           (STA ZP_RZ+1)
           (JSR DEC_REFCNT_M1_SLOT_RZ__IF_PTR_N)
    (label POP_NO_GC__)
           (PLA)
           (TAY)                                ;; index -> Y
           (LDA ZP_RT)
           (STA (ZP_LOCALS_LB_PTR),y)           ;; store low byte of local at index
           (LDA ZP_RT+1)
           (STA (ZP_LOCALS_HB_PTR),y)           ;; store high byte of local at index -> A
           (JMP VM_POP_EVLSTK_AND_INC_PC)          ;; fill RT with next tos
           ;; no increment, since pop removes it from stack
           ;; next bc

    ;; write to local
   (label  BC_WRITE_TO_LOCAL_SHORT)
           (AND !$06)
           (LSR)
           (PHA)
           (TAY)                                ;; index -> Y

           ;; decrement old local
           (LDA (ZP_LOCALS_LB_PTR),y)
           (BEQ WRITE_NO_GC__)
           (STA ZP_RZ)
           (LDA (ZP_LOCALS_HB_PTR),y)
           (STA ZP_RZ+1)
           (JSR DEC_REFCNT_M1_SLOT_RZ__IF_PTR_N)
    (label WRITE_NO_GC__)
           (PLA)
           (TAY)                                ;; index -> Y
           (LDA ZP_RT)
           (STA (ZP_LOCALS_LB_PTR),y)           ;; store low byte of local at index
           (LDA ZP_RT+1)
           (STA (ZP_LOCALS_HB_PTR),y)           ;; store high byte of local at index -> A
           ;; increment, since it is now in locals and on stack
           (JSR INC_REFCNT_M1_SLOT_RT_N)
           (JMP VM_INTERPRETER_INC_PC)          ;; next bc
))))
