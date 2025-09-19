#lang racket/base

(require (only-in racket/list
                  flatten)
         "../../6510.rkt"
         (only-in "../../ast/6510-resolver.rkt"
                  add-label-suffix)
         (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC_A_TIMES
                  VM_INTERPRETER_INC_PC)
         (only-in "../vm-memory-map.rkt"
                  ZP_VM_PC
                  ZP_RT
                  ZP_RP)
         (only-in "../vm-cell-stack.rkt"
                  POP_CELL_EVLSTK_TO_RT))

(provide BC_POKE_B
         BC_NATIVE
         RETURN_TO_BC)

(define BC_POKE_B
  (flatten
   (list
    (label BC_POKE_B)
           (LDY !$02)
           (LDA (ZP_VM_PC),y)   ;; [PC+2] = highbyte of poke target
           (STA ZP_RP+1)
           (DEY)
           (LDA (ZP_VM_PC),y)   ;; [PC+1] = lowbyte of poke target
           (STA ZP_RP)
           (LDA ZP_RT+1)        ;; byte to poke (is in RT+1
           (DEY)
           (STA (ZP_RP),y)
           (JSR POP_CELL_EVLSTK_TO_RT)
           (LDA !$03)
           (JMP VM_INTERPRETER_INC_PC_A_TIMES))))

;; @DC-B: NATIVE, group: misc
;; following bytes are native 6510 commands, JSR RETURN_TO_BC ends this sequence
;; len: 1
(define BC_NATIVE
  (list
   (label BC_NATIVE)
          ;; (INC ZP_VM_PC)
          ;; (BNE CONT__BC_NATIVE)
          ;; (INC ZP_VM_PC+1)
   (label CONT__BC_NATIVE)
          (JMP (ZP_VM_PC)))) ;; this jump actually jumps onto the current bytecode command,
                             ;; but since NATIVE is 4a (which is 6510 LSR), this can be done without the incr.

(define RETURN_TO_BC
  (list
   (label RETURN_TO_BC)
          (PLA)
          (STA ZP_VM_PC)
          (PLA)
          (STA ZP_VM_PC+1)
          (JMP VM_INTERPRETER_INC_PC)))
