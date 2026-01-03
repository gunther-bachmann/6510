#lang racket/base

(provide BC_WRITE_LOCAL_SHORT           ;; write a local into the tos (rt)
         BC_PUSH_LOCAL_SHORT            ;; push the local onto the eval stack
         BC_PUSH_LOCAL_CXR              ;; push local 0-3 and then car
         PUSH_RT_WRITE_LOCAL_bc_enc)    ;; push rt, then write local (encoded w/i bc) into rt, no refcnt!

#|

  implement bc push/write local commands

 |#

(require "../../6510.rkt"
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function
                  define-vm-function-wol)
         (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC)
         (only-in "../vm-runtime/vm-cell-array.rkt"
                  WRITE_ARR_AT0_RT_TO_RT
                  WRITE_ARR_AT1_RT_TO_RT)
         (only-in "../vm-runtime/vm-cell-stack.rkt"
                  PUSH_RT_TO_EVLSTK)
         (only-in "../vm-runtime/vm-m1-slots.rkt"
                  INC_REFCNT_M1_SLOT_RT
                  DEC_REFCNT_M1_SLOT_RT__IF_PTR)
         (only-in "../vm-runtime/vm-memory-map.rkt"
                  ZP_LOCALS_HB_PTR
                  ZP_LOCALS_LB_PTR
                  ZP_RT))

(define BC_WRITE_LOCAL_SHORT '())
(define-vm-function BC_PUSH_LOCAL_SHORT
   (list
    ;; push local
           (JSR PUSH_RT_WRITE_LOCAL_bc_enc)
           (JSR INC_REFCNT_M1_SLOT_RT__IF_PTR)
           (JMP VM_INTERPRETER_INC_PC)

    (label BC_WRITE_LOCAL_SHORT)
           (LSR)
           (AND !$03)
           (PHA)
           (JSR DEC_REFCNT_M1_SLOT_RT__IF_PTR)
           (PLA)
           (TAY)                                ;; index -> Y
           (LDA (ZP_LOCALS_LB_PTR),y)           ;; load low byte of local at index
           (STA ZP_RT)                          ;;
           (LDA (ZP_LOCALS_HB_PTR),y)           ;; load high byte of local at index
           (STA ZP_RT+1)                        ;;
           (JSR INC_REFCNT_M1_SLOT_RT__IF_PTR)
           (JMP VM_INTERPRETER_INC_PC)          ;; next bc
           ))

;; no refcnt
(define-vm-function PUSH_RT_WRITE_LOCAL_bc_enc
   (list
           (LSR)
           (AND !$03)
           (PHA)
           (JSR PUSH_RT_TO_EVLSTK)
           (PLA)
           (TAY)                                ;; index -> Y
           (LDA (ZP_LOCALS_LB_PTR),y)           ;; load low byte of local at index
           (STA ZP_RT)                                ;; low byte -> X
           (LDA (ZP_LOCALS_HB_PTR),y)           ;; load high byte of local at index -> A
           (STA ZP_RT+1)
           (RTS)))

(define-vm-function-wol BC_PUSH_LOCAL_CXR
   (list
    (label BC_PUSH_LX_CAR)
           (JSR PUSH_RT_WRITE_LOCAL_bc_enc)
           (JSR WRITE_ARR_AT0_RT_TO_RT)
           (JSR INC_REFCNT_M1_SLOT_RT__IF_PTR)
           (JMP VM_INTERPRETER_INC_PC)

    (label BC_PUSH_LX_CDR)
           (JSR PUSH_RT_WRITE_LOCAL_bc_enc)
           (JSR WRITE_ARR_AT1_RT_TO_RT)
           (JSR INC_REFCNT_M1_SLOT_RT__IF_PTR)
           (JMP VM_INTERPRETER_INC_PC)))
