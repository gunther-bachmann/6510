#lang racket/base

(require "../6510.rkt")
(require (only-in "../ast/6510-resolver.rkt" add-label-suffix))
(require (only-in racket/list flatten))


(provide BC_WRITE_LOCAL_SHORT
         BC_PUSH_LOCAL_SHORT)

;;                       @DC-B: PUSH_L0, group: stack
(define PUSH_L0 #x00) ;; *PUSH* *L*​ocal *0* on evlstk
;;                       @DC-B: PUSH_L1, group: stack
(define PUSH_L1 #x02) ;; *PUSH* *L*​ocal *1* on evlstk
;;                       @DC-B: PUSH_L2, group: stack
(define PUSH_L2 #x04) ;; *PUSH* *L*​ocal *2* on evlstk
;;                       @DC-B: PUSH_L3, group: stack
(define PUSH_L3 #x06) ;; *PUSH* *L*​ocal *3* on evlstk

;;                        @DC-B: WRITE_L0, group: stack
(define WRITE_L0 #x10) ;; *WRITE* *L*​ocal *0* into rt
;;                        @DC-B: WRITE_L1, group: stack
(define WRITE_L1 #x12) ;; *WRITE* *L*​ocal *1* into rt
;;                        @DC-B: WRITE_L2, group: stack
(define WRITE_L2 #x14) ;; *WRITE* *L*​ocal *2* into rt
;;                        @DC-B: WRITE_L3, group: stack
(define WRITE_L3 #x16) ;; *WRITE* *L*​ocal *3* into rt
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
