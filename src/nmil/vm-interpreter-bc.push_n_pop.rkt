#lang racket/base

#|

Bytecode implementation of comparison commands

TODO: get tests (still in vm-interpreter) into this file

|#


(require "../6510.rkt")
(require (only-in "../ast/6510-resolver.rkt" add-label-suffix))
(require (only-in racket/list flatten))
(require (only-in "./vm-memory-map.rkt"
                  ZP_RT
                  ZP_RP))
(require (only-in "./vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC))
(require (only-in "./vm-mm-cell-stack.rkt"
                  POP_CELL_EVLSTK_TO_RP))
(require (only-in "./vm-mm-register-functions.rkt"
                  WRITE_INT0_TO_RT
                  WRITE_INT1_TO_RT))

(provide BC_PUSH_B)

;; @DC-B: PUSH_B
;; *PUSH* *B*​yte, following the instruction
;; len: 2
(define PUSH_B #x2e)
(define BC_PUSH_B
  (list
   (label BC_PUSH_B)
          (LDY !$01)
          (LDA (ZP_VM_PC),y)
          (LDX !$ff)
          (JSR PUSH_XA_TO_EVLSTK)
          (JMP VM_INTERPRETER_INC_PC_2_TIMES)))

