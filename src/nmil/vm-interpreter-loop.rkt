#lang racket/base

#|

define the interpreter loop and its entry points
and the bc operation jump table

|#


(require "../6510.rkt")
(require (only-in racket/list flatten))
(require (only-in "./vm-memory-map.rkt"
                  ZP_VM_PC))
(require (only-in "./vm-mm-cell-stack.rkt"
                  POP_CELL_EVLSTK_TO_RT))

(provide VM_INTERPRETER
         VM_INTERPRETER_OPTABLE
         VM_INTERPRETER_INC_PC
         VM_POP_EVLSTK_AND_INC_PC
         VM_INTERPRETER_INC_PC_2_TIMES
         VM_INTERPRETER_INC_PC_A_TIMES)

(define VM_INTERPRETER_INC_PC_2_TIMES #t)
(define VM_INTERPRETER_INC_PC_A_TIMES #t)
(define VM_POP_EVLSTK_AND_INC_PC #t)
(define VM_INTERPRETER_INC_PC #t)
;; interpreter loop without short commands
;; each byte command must have lowest bit set to 0 to be aligned to the jump table
(define VM_INTERPRETER
  (list
   (label VM_INTERPRETER_INC_PC_2_TIMES)
          (LDA !$02)
   (label VM_INTERPRETER_INC_PC_A_TIMES)
          (CLC)                                 ;; clear for add
          (ADC ZP_VM_PC)                        ;; PC = PC + A
          (STA ZP_VM_PC)
          (BCC VM_INTERPRETER)                  ;; same page -> no further things to do
          (BCS VM_INTERPRETER_NEXT_PAGE)

   (label VM_POP_EVLSTK_AND_INC_PC)
          (JSR POP_CELL_EVLSTK_TO_RT)

   (label VM_INTERPRETER_INC_PC)                ;; inc by one (regular case)
   ;; (label BC_NOP)                               ;; is equivalent to NOP
          (INC ZP_VM_PC)
          (BNE VM_INTERPRETER)                  ;; same page -> no further things to do
   (label VM_INTERPRETER_NEXT_PAGE)
          (INC ZP_VM_PC+1)                      ;; increment high byte of pc (into next page)

    ;; ----------------------------------------
   (label VM_INTERPRETER)
          (LDY !$00)                            ;; use 0 offset to ZP_VM_PV
   (label VM_INTERPRETERy)
          (LDA (ZP_VM_PC),y)                    ;; load byte code
          ;; normal bytecode command
   (label OPERAND__VM_INTERPRETER)
          (STA JMPOP__VM_INTERPRETER+1)         ;; lowbyte of the table
   (label JMPOP__VM_INTERPRETER)
          (JMP (VM_INTERPRETER_OPTABLE))        ;; jump by table
))

;; the jump table is filled by codes defined here
;; must be page aligned!
(define VM_INTERPRETER_OPTABLE
  (flatten ;; necessary because word ref creates a list of ast-byte-codes ...
   (list
    (label VM_INTERPRETER_OPTABLE)                ;; code
           (build-list 128 (lambda (_n) (word-ref VM_INTERPRETER_INC_PC))))))
