#lang racket/base

(require "../../6510.rkt"
         (only-in "../../ast/6510-resolver.rkt"
                  add-label-suffix)
         (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC
                  VM_INTERPRETER_INC_PC_2_TIMES)
         (only-in "../vm-runtime/vm-memory-map.rkt"
                  ZP_RT
                  ZP_RP
                  ZP_CELL_STACK_TOS
                  ZP_CELL_STACK_LB_PTR
                  ZP_CELL_STACK_HB_PTR)
         (only-in "../vm-runtime/vm-cell-stack.rkt"
                  POP_CELL_EVLSTK_TO_RP
                  POP_CELL_EVLSTK_TO_RT))

(provide BC_BINC
         BC_BDEC
         BC_BADD
         BC_IMAX
         BC_IINC
         BC_IADD
         BC_BSHR
         BC_ISUB)

(define BC_BINC
  (list
   (label BC_BINC)
          (INC ZP_RT+1)
          (JMP VM_INTERPRETER_INC_PC)))

(define BC_BDEC
  (list
   (label BC_BDEC)
          (DEC ZP_RT+1)
          (JMP VM_INTERPRETER_INC_PC)))

(define BC_BADD
  (list
   (label BC_BADD)
          (JSR POP_CELL_EVLSTK_TO_RP)
          (CLC)
          (LDA ZP_RT+1)
          (ADC ZP_RP+1)
          (STA ZP_RT+1)
          (JMP VM_INTERPRETER_INC_PC)))

(define BC_IMAX
  (add-label-suffix
   "__" "__IMAX"
  (list
   (label BC_IMAX)
          (LDY ZP_CELL_STACK_TOS)

          ;; compare high byte of int (which is lb)
          (LDA (ZP_CELL_STACK_LB_PTR),y)
          (CMP ZP_RT)
          (BNE NO_OTHER_COMPARE__) ;; already different => no need to compare low byte

          ;; compare low byte of int (which is hb)
          (LDA (ZP_CELL_STACK_HB_PTR),y)
          (CMP ZP_RT+1)

   (label NO_OTHER_COMPARE__)
          (BMI KEEP_RT__)

          (JSR POP_CELL_EVLSTK_TO_RT)     ;; pop RT and move TOS into RT
          (JMP VM_INTERPRETER_INC_PC_2_TIMES)

    (label KEEP_RT__)
          (DEC ZP_CELL_STACK_TOS) ;; just pop but keep RT, since INT no GC necessary
          (JMP VM_INTERPRETER_INC_PC_2_TIMES))))

(define BC_IINC
  (add-label-suffix
   "__" "__IINC"
  (list
   (label BC_IINC)
          (INC ZP_RT+1)
          (BNE DONE__)
          (INC ZP_RT)
          (LDA ZP_RT)
          (ORA !$03)
          (AND !$7f)
          (STA ZP_RT)
   (label DONE__)
          (JMP VM_INTERPRETER_INC_PC_2_TIMES))))

(define BC_IADD
  (add-label-suffix
   "__" "__IADD"
  (list
   (label BC_IADD)
          (LDY ZP_CELL_STACK_TOS)               ;; get current index to tagged byte
          (LDA (ZP_CELL_STACK_HB_PTR),y)        ;; A = untagged lowbyte of int (stored in high byte)
          (CLC)                                 ;; for addition the carry flags needs to be clear
          (ADC ZP_RT+1)                         ;; A = A + stack value (int low byte)
          (STA ZP_RT+1)                         ;; RT untagged lowbyte = result

          (LDA (ZP_CELL_STACK_LB_PTR),y)       ;; A = tagged high byte of int (stored in low byte)
          (AND !$7c)                            ;; mask out lower two and highest bit
          (BCC NO_INC_HIGH__)        ;; if previous addition had no overflow, skip inc
          (CLC)                                 ;; clear for addition
          (ADC !$04)                            ;; increment int (adding 4 into the enoded int starting at bit 2)

    (label NO_INC_HIGH__)
          (ADC ZP_RT)                           ;; A = A + stack value (int high byte)
          (AND !$7f)                            ;; since ZP_RT has the lower two bits set, just mask out the highest bit
          (STA ZP_RT)                           ;; RT tagged high byte = result

          (DEC ZP_CELL_STACK_TOS)               ;; pop value from cell-stack (leave result in RT as tos)
          (JMP VM_INTERPRETER_INC_PC))))         ;; interpreter loop


(define BC_BSHR
  (add-label-suffix
   "__" "__BC_BSHR"
   (list
    (label BC_BSHR)
           (LDA ZP_RT+1)
           (LSR)
           (STA ZP_RT+1)
           (JMP VM_INTERPRETER_INC_PC))))

(define BC_ISUB
  (add-label-suffix
   "__" "__ISUB"
  (list
   (label BC_ISUB)
          (LDY ZP_CELL_STACK_TOS)               ;; get current index to tagged byte
          (SEC)                                 ;; for subtraction carry needs to be set
          (LDA ZP_RT+1)                         ;; A = untagged lowbyte of int (stored in high byte)
          (SBC (ZP_CELL_STACK_HB_PTR),y)      ;; A = A - stack value (int low byte)
          (STA ZP_RT+1)                         ;; RT untagged lowbyte = result

          (LDA ZP_RT)                           ;; A = tagged highbyte of int (stored in low byte)
          (BCS NO_DEC_HIGH__)       ;; if carry is set from subtraction of lower bits, no subtraction carry over necessary
          (SEC)                                 ;; for subtraction carry needs to be set
          (SBC !$04)                            ;; subtract 1 in the masked int highbyte (starting at bit2) => 4

   (label NO_DEC_HIGH__)
          (SBC (ZP_CELL_STACK_LB_PTR),y)      ;; A = A - stack value (int high byte)
          (AND !$7c)                            ;; mask out under/overflow (lower two bits and high bit)
          (ORA !$03)                            ;; set lower two bits to tag it as integer value
          (STA ZP_RT)                           ;; RT tagged high byte = result

   (label DONE__)
          (DEC ZP_CELL_STACK_TOS)               ;; pop value from cell-stack (leave result in rt untouched)
          (JMP VM_INTERPRETER_INC_PC))))
