#lang racket/base

(provide BC_POP_TO_LOCAL_SHORT
         BC_WRITE_TO_LOCAL_SHORT)

(require "../../6510.rkt"
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function-wol)
         (only-in "../vm-runtime/vm-m1-slots.rkt"
                  DEC_REFCNT_M1_SLOT_RZ__IF_PTR
                  DEC_REFCNT_M1_SLOT_RZ
                  INC_REFCNT_M1_SLOT_RT))

(define BC_WRITE_TO_LOCAL_SHORT '())
(define-vm-function-wol BC_POP_TO_LOCAL_SHORT
   (list
    (label PREP_LOCAL__)
           (LSR)
           (AND !$03)
           (PHA)
           (TAY)                                ;; index -> Y
           ;; decrement old local (if necessary)
           (LDA (ZP_LOCALS_LB_PTR),y)
           (BEQ POP_NO_GC__)
           (ROR)
           (BCS POP_NO_GC__)
           (ROL)
           (STA ZP_RZ)
           (LDA (ZP_LOCALS_HB_PTR),y)
           (STA ZP_RZ+1)
           (JSR DEC_REFCNT_M1_SLOT_RZ) ;; must be a pointer (checked before)
    (label POP_NO_GC__)
           (PLA)
           (TAY)                                ;; index -> Y
           (LDA ZP_RT)
           (STA (ZP_LOCALS_LB_PTR),y)           ;; store low byte of local at index
           (LDA ZP_RT+1)
           (STA (ZP_LOCALS_HB_PTR),y)           ;; store high byte of local at index -> A
           (RTS)

    (label BC_POP_TO_LOCAL_SHORT)
           (JSR PREP_LOCAL__)
           (JMP VM_POP_EVLSTK_AND_INC_PC)          ;; fill RT with next tos
           ;; no increment, since pop removes it from stack
           ;; next bc

    ;; write to local
   (label  BC_WRITE_TO_LOCAL_SHORT)
           (JSR PREP_LOCAL__)
           ;; increment, since it is now in locals and on stack
           (JSR INC_REFCNT_M1_SLOT_RT__IF_PTR)
           (JMP VM_INTERPRETER_INC_PC)          ;; next bc
))
