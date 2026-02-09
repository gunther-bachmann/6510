#lang racket/base

(provide BC_BINC        ;; increment byte (tos)
         BC_BDEC        ;; decrement
         BC_BADD        ;; add two topmost bytes
         BC_IMAX        ;; get max of two topmost integers
         BC_IINC        ;; increment integer (tos)
         BC_IDEC        ;; decrement integer (tos)
         BC_IADD        ;; add two topmost integer
         BC_BSHR        ;; shift tos byte one bit to the right
         BC_ISUB

         bc-atom-num-code)

(require "../../6510.rkt"
         (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC
                  VM_INTERPRETER_INC_PC_2_TIMES)
         (only-in "../vm-runtime/vm-memory-map.rkt"
                  ZP_RT
                  ZP_RP
                  ZP_EVAL_STACK_TAIL_TOP
                  ZP_EVAL_STACK_TAIL_LB_PTR
                  ZP_EVAL_STACK_TAIL_HB_PTR)
         (only-in "../vm-runtime/vm-cell-stack.rkt"
                  POP_EVLSTK_TAIL_TO_RP
                  POP_EVLSTK_TAIL_TO_RT)
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function-wol
                  define-vm-function))       ;; subtract two topmost integers

(define-vm-function BC_BINC
  (list
          (INC ZP_RT+1)
          (JMP VM_INTERPRETER_INC_PC)))

(define-vm-function BC_BDEC
  (list
          (DEC ZP_RT+1)
          (JMP VM_INTERPRETER_INC_PC)))

(define-vm-function BC_BADD
  (list
          (JSR POP_EVLSTK_TAIL_TO_RP)
          (CLC)
          (LDA ZP_RT+1)
          (ADC ZP_RP+1)
          (STA ZP_RT+1)
          (JMP VM_INTERPRETER_INC_PC)

;; ;; alternatively (speed optimized) : 23 bytes long
;; ;; does inline pop of eval stack
;;           (LDY ZP_EVAL_STACK_TAIL_TOP)
;;           (CMP !$01)
;;           (BEQ SWITCH_TO_PREV_CELL_STK__BC_ADD)
;;           ;; no stack check
;;    (label DO_ADD__BC_ADD)
;;           (LDA (ZP_EVAL_STACK_TAIL_HB_PTR),y) ;; high byte = payload of byte-cell
;;           (CLC)
;;           (ADC ZP_RT+1)
;;           (STA ZP_RT+1)
;;           (DEC ZP_EVAL_STACK_TAIL_TOP)
;;           (JMP VM_INTERPRETER_INC_PC)
;;    (label SWITCH_TO_PREV_CELL_STK__BC_ADD)
;;           (JSR SWITCH_TO_PREV_CELL_STK)
;;           (BNE DO_ADD__BC_ADD)
))

(define-vm-function BC_IMAX
  (list
          (LDY ZP_EVAL_STACK_TAIL_TOP)

          ;; compare high byte of int (which is lb)
          (LDA (ZP_EVAL_STACK_TAIL_LB_PTR),y)
          (CMP ZP_RT)
          (BNE NO_OTHER_COMPARE__) ;; already different => no need to compare low byte

          ;; compare low byte of int (which is hb)
          (LDA (ZP_EVAL_STACK_TAIL_HB_PTR),y)
          (CMP ZP_RT+1)

   (label NO_OTHER_COMPARE__)
          (BMI KEEP_RT__)

          (JSR POP_EVLSTK_TAIL_TO_RT)     ;; pop RT and move TOS into RT
          (JMP VM_INTERPRETER_INC_PC)

    (label KEEP_RT__)
          (DEC ZP_EVAL_STACK_TAIL_TOP) ;; just pop but keep RT, since INT no GC necessary
          (JMP VM_INTERPRETER_INC_PC)))

(define-vm-function BC_IINC
  (list
          (INC ZP_RT+1)
          (BNE DONE__)
          (LDA ZP_RT)
          (CLC)
          (ADC !$01)
          (ORA !$03)
          (STA ZP_RT)
   (label DONE__)
          (JMP VM_INTERPRETER_INC_PC)))

(define-vm-function BC_IDEC
  (list
          (LDY ZP_RT+1)
          (DEY)
          (STY ZP_RT+1)
          (CPY !$ff)
          (BNE DONE__)
          (LDA ZP_RT)
          (SEC)
          (SBC !$04)
          (ORA !$03)
          (STA ZP_RT)
   (label DONE__)
          (JMP VM_INTERPRETER_INC_PC)))

(define-vm-function BC_IADD
  (list
          (LDY ZP_EVAL_STACK_TAIL_TOP)               ;; get current index to tagged byte
          (LDA (ZP_EVAL_STACK_TAIL_HB_PTR),y)        ;; A = untagged lowbyte of int (stored in high byte)
          (CLC)                                 ;; for addition the carry flags needs to be clear
          (ADC ZP_RT+1)                         ;; A = A + stack value (int low byte)
          (STA ZP_RT+1)                         ;; RT untagged lowbyte = result

          (LDA (ZP_EVAL_STACK_TAIL_LB_PTR),y)       ;; A = tagged high byte of int (stored in low byte)
          (AND !$fc)                            ;; mask out lower two bits
          (BCC NO_INC_HIGH__)        ;; if previous addition had no overflow, skip inc
          (CLC)                                 ;; clear for addition
          (ADC !$04)                            ;; increment int (adding 4 into the enoded int starting at bit 2)

    (label NO_INC_HIGH__)
          (ADC ZP_RT)                           ;; A = A + stack value (int high byte)
          ;; (AND !$7f)                            ;; since ZP_RT has the lower two bits set, just mask out the highest bit
          (STA ZP_RT)                           ;; RT tagged high byte = result

          (DEC ZP_EVAL_STACK_TAIL_TOP)               ;; pop value from cell-stack (leave result in RT as tos)
          (JMP VM_INTERPRETER_INC_PC)))         ;; interpreter loop


(define-vm-function BC_BSHR
   (list
           (LDA ZP_RT+1)
           (LSR)
           (STA ZP_RT+1)
           (JMP VM_INTERPRETER_INC_PC)))

(define-vm-function BC_ISUB
  (list
          (LDY ZP_EVAL_STACK_TAIL_TOP)               ;; get current index to tagged byte
          (SEC)                                 ;; for subtraction carry needs to be set
          (LDA ZP_RT+1)                         ;; A = untagged lowbyte of int (stored in high byte)
          (SBC (ZP_EVAL_STACK_TAIL_HB_PTR),y)      ;; A = A - stack value (int low byte)
          (STA ZP_RT+1)                         ;; RT untagged lowbyte = result

          (LDA ZP_RT)                           ;; A = tagged highbyte of int (stored in low byte)
          (BCS NO_DEC_HIGH__)       ;; if carry is set from subtraction of lower bits, no subtraction carry over necessary
          (SEC)                                 ;; for subtraction carry needs to be set
          (SBC !$04)                            ;; subtract 1 in the masked int highbyte (starting at bit2) => 4
          (SEC)

   (label NO_DEC_HIGH__)
          (SBC (ZP_EVAL_STACK_TAIL_LB_PTR),y)      ;; A = A - stack value (int high byte)
          ;; (AND !$fc)                            ;; mask out under/overflow (lower two bits and high bit)
          (ORA !$03)                            ;; set lower two bits to tag it as integer value
          (STA ZP_RT)                           ;; RT tagged high byte = result

   (label DONE__)
          (DEC ZP_EVAL_STACK_TAIL_TOP)               ;; pop value from cell-stack (leave result in rt untouched)
          (JMP VM_INTERPRETER_INC_PC)))

(define bc-atom-num-code
  (append BC_BINC
          BC_BDEC
          BC_BADD
          BC_IMAX
          BC_IINC
          BC_IDEC
          BC_IADD
          BC_BSHR
          BC_ISUB))
