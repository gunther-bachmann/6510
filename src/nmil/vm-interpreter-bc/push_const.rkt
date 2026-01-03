#lang racket/base

(provide BC_PUSH_CONST_NUM_SHORT
         BC_PUSH_INT0
         BC_PUSH_INT1
         BC_PUSH_INT2
         BC_PUSH_INTm1)

(require "../../6510.rkt"
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function-wol)
         (only-in "../vm-runtime/vm-memory-map.rkt"
                  TAGGED_INT_0_LB))

(define BC_PUSH_INT0 '())
(define BC_PUSH_INT1 '())
(define BC_PUSH_INT2 '())
(define BC_PUSH_INTm1 '())
(define-vm-function-wol BC_PUSH_CONST_NUM_SHORT
   (list
    (label BC_PUSH_INT0)
    (label BC_PUSH_INT1)
    (label BC_PUSH_INT2)
           (LSR)
           (AND !$03)
           (LDX !TAGGED_INT_0_LB)
           (JSR PUSH_XA_TO_EVLSTK)
           (JMP VM_INTERPRETER_INC_PC)

    (label BC_PUSH_INTm1)
           (JSR PUSH_INT_m1_TO_EVLSTK)  ;;
           (JMP VM_INTERPRETER_INC_PC) ;; interpreter loop
           ))
