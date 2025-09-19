#lang racket/base

#|

  implement bc push/write local commands

|#

(require (only-in racket/list
                  flatten)
         "../../6510.rkt"
         (only-in "../../ast/6510-resolver.rkt"
                  add-label-suffix)
         (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC)
         (only-in "../vm-memory-manager.rkt"
                  INC_REFCNT_RT
                  DEC_REFCNT_RT)
         (only-in "../vm-memory-map.rkt"
                  ZP_LOCALS_HB_PTR
                  ZP_LOCALS_LB_PTR
                  ZP_RT)
         (only-in "../vm-cell-stack.rkt"
                  PUSH_RT_TO_EVLSTK_IF_NONEMPTY))

(provide BC_WRITE_LOCAL_SHORT           ;; write a local into the tos (rt)
         BC_PUSH_LOCAL_SHORT            ;; push the local onto the eval stack
         BC_PUSH_LOCAL_CXR              ;; push local 0-3 and then car
         PUSH_RT_WRITE_LOCAL_bc_enc)    ;; push rt, then write local (encoded w/i bc) into rt, no refcnt!

(define BC_WRITE_LOCAL_SHORT #t)
(define BC_PUSH_LOCAL_SHORT
  (add-label-suffix
   "__" "__BC_PUSH_LOCAL_SHORT"
  (flatten
   (list
    (label BC_PUSH_LOCAL_SHORT)
    ;; push local
           (JSR PUSH_RT_WRITE_LOCAL_bc_enc)
           (JSR INC_REFCNT_RT)
           (JMP VM_INTERPRETER_INC_PC)

    (label BC_WRITE_LOCAL_SHORT)
           (LSR)
           (AND !$03)
           (PHA)
           (JSR DEC_REFCNT_RT)
           (PLA)
           (TAY)                                ;; index -> Y
           (LDA (ZP_LOCALS_LB_PTR),y)           ;; load low byte of local at index
           (STA ZP_RT)                          ;;
           (LDA (ZP_LOCALS_HB_PTR),y)           ;; load high byte of local at index
           (STA ZP_RT+1)                        ;;
           (JSR INC_REFCNT_RT)
           (JMP VM_INTERPRETER_INC_PC)          ;; next bc
           ))))

(define PUSH_RT_WRITE_LOCAL_bc_enc
  (add-label-suffix
   "__" "__PUSH_RT_WRITE_LOCAL_bc_enc"
  (flatten
   (list
    (label PUSH_RT_WRITE_LOCAL_bc_enc)
           (LSR)
           (AND !$03)
           (PHA)
           (JSR PUSH_RT_TO_EVLSTK_IF_NONEMPTY)
           (PLA)
           (TAY)                                ;; index -> Y
           (LDA (ZP_LOCALS_LB_PTR),y)           ;; load low byte of local at index
           (STA ZP_RT)                                ;; low byte -> X
           (LDA (ZP_LOCALS_HB_PTR),y)           ;; load high byte of local at index -> A
           (STA ZP_RT+1)
           (RTS)))))

(define BC_PUSH_LOCAL_CXR
  (add-label-suffix
   "__" "__BC_PUSH_LOCAL_CXR"
  (flatten
   (list
    (label BC_PUSH_LX_CAR)
           (JSR PUSH_RT_WRITE_LOCAL_bc_enc)
           (JSR WRITE_CELLPAIR_RT_CELL0_TO_RT)
           (JSR INC_REFCNT_RT)
           (JMP VM_INTERPRETER_INC_PC)

    (label BC_PUSH_LX_CDR)
           (JSR PUSH_RT_WRITE_LOCAL_bc_enc)
           (JSR WRITE_CELLPAIR_RT_CELL1_TO_RT)
           (JSR INC_REFCNT_RT)
           (JMP VM_INTERPRETER_INC_PC)))))
