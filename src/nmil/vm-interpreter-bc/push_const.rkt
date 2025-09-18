#lang racket/base

(require "../../6510.rkt")
(require (only-in "../../ast/6510-resolver.rkt" add-label-suffix))
(require (only-in racket/list flatten))

(provide BC_PUSH_CONST_NUM_SHORT
         BC_PUSH_INT0
         BC_PUSH_INT1
         BC_PUSH_INT2
         BC_PUSH_INTm1)

(define BC_PUSH_INT0 #t)
(define BC_PUSH_INT1 #t)
(define BC_PUSH_INT2 #t)
(define BC_PUSH_INTm1 #t)
(define BC_PUSH_CONST_NUM_SHORT
  (add-label-suffix
   "__" "__BC_PUSH_CONST_NUM_SHORT"
  (flatten
   (list
    (label BC_PUSH_INT0)
    (label BC_PUSH_INT1)
    (label BC_PUSH_INT2)
           (LSR)
           (AND !$03)
           (LDX !$03)
           (JSR PUSH_XA_TO_EVLSTK)
           (JMP VM_INTERPRETER_INC_PC)

    (label BC_PUSH_INTm1)
           (JSR PUSH_INT_m1_TO_EVLSTK)  ;;
           (JMP VM_INTERPRETER_INC_PC) ;; interpreter loop
           ))))
