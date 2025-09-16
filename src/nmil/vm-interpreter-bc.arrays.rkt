#lang racket/base

(require "../6510.rkt")
(require (only-in "../ast/6510-resolver.rkt" add-label-suffix))
(require (only-in racket/list flatten))

(require (only-in "./vm-memory-map.rkt"
                  ZP_RBI))
(require (only-in "./vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC_2_TIMES))
(require (only-in "./vm-interpreter-bc.branch.rkt" BRANCH_BY_NEXT_BYTE__NO_POP))

(provide BC_DEC_RBI_NZ_P_BRA)

(define BC_DEC_RBI_NZ_P_BRA
  (add-label-suffix
   "__" "__BC_DEC_RBI_NZ_P_BRA"
   (flatten
    (list
     (label BC_DEC_RBI_NZ_P_BRA)
            (DEC ZP_RBI)
            (BEQ NO_BRA__) ;; == 0 => no branch
            (JMP BRANCH_BY_NEXT_BYTE__NO_POP)
     (label NO_BRA__)
            (JMP VM_INTERPRETER_INC_PC_2_TIMES)))))
