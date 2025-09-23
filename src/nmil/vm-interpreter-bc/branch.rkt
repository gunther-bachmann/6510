#lang racket/base

(require (only-in racket/list
                  flatten)
         "../../6510.rkt"
         (only-in "../../ast/6510-resolver.rkt"
                  add-label-suffix)
         (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER
                  ZP_VM_PC
                  VM_INTERPRETER_INC_PC_2_TIMES)
         (only-in "../vm-runtime/vm-memory-map.rkt"
                  ZP_RT
                  TAG_BYTE_BYTE_CELL))

(provide BC_Z_P_BRA                     ;; branch by next byte if tos is zero (byte or int), pop if branching
         BC_NZ_P_BRA                    ;; branch by next byte if tos is NOT zero (byte or int), pop if not branching
         BC_T_P_BRA                     ;; branch by next byte if tos is true (actually anything != 0), always pop
         BC_F_P_BRA                     ;; branch by next byte if tos is false (actually = 0), always pop
         BC_GOTO                        ;; unconditional branch

         CONTINUE_AFTER_BRA             ;; pc = pc + 2 + A (number of bytes to jump forward)
         POP_AND_CONTINUE_AFTER_BRA     ;; pop eval stack and then CONTINUE_AFTER_BRA
         BRANCH_BY_NEXT_BYTE            ;; pop eval stack and then pc += [vm+1] (+2 if forward), signed add
         BRANCH_BY_NEXT_BYTE__NO_POP)   ;; pc += [vm+1] (+2 if forward), signed add, NO POP

;; pop (0) if branching
(define BC_Z_P_BRA
  (add-label-suffix
   "__" "__BC_Z_P_BRA"
   (flatten
    (list
     (label BC_Z_P_BRA)
            (LDX ZP_RT+1)
            (BNE NO_BRA__)              ;; byte !=0 or lowbyte of int != 0 => no branch
            (LDX ZP_RT)
            (CPX !$03)                  ;; high byte is only int tag => int = 0 => branch
            (BEQ BRA__)
            (CPX !TAG_BYTE_BYTE_CELL)   ;; high byte == byte tag => it is a 0 byte
            (BNE NO_BRA__)              ;; != => no branch
     (label BRA__)
            (JSR POP_CELL_EVLSTK_TO_RT)
            (JMP BRANCH_BY_NEXT_BYTE)
     (label NO_BRA__)
            (JMP VM_INTERPRETER_INC_PC_2_TIMES)))))

;; pop (0) if not branching
(define BC_NZ_P_BRA
  (add-label-suffix
   "__" "__BC_NZ_P_BRA"
   (flatten
    (list
     (label BC_NZ_P_BRA)
            (LDX ZP_RT+1)
            (BNE BRA__) ;; != 0 => branch before even looking at anything else
            (LDX ZP_RT)
            (CPX !$03)
            (BEQ NO_BRA__) ;; lowbyte = 03 (zero int)  => definitely no branch
            (CPX !TAG_BYTE_BYTE_CELL)
            (BEQ NO_BRA__)
     (label BRA__)
            (JMP BRANCH_BY_NEXT_BYTE__NO_POP)
     (label NO_BRA__)
            (JSR POP_CELL_EVLSTK_TO_RT)
            (JMP VM_INTERPRETER_INC_PC_2_TIMES)))))

(define CONTINUE_AFTER_BRA #t)
(define POP_AND_CONTINUE_AFTER_BRA #t)
(define BRANCH_BY_NEXT_BYTE #t)
(define BRANCH_BY_NEXT_BYTE__NO_POP #t)
(define BC_T_P_BRA
  (add-label-suffix
   "__" "__T_P_BRA"
  (list
   (label BC_T_P_BRA)
          ;; (CLC)
          (LDA ZP_RT+1)
          (BEQ POP_AND_CONTINUE_AFTER_BRA) ;; when false (A = 0), just continue, no branch

   (label BRANCH_BY_NEXT_BYTE)
   ;; branch by adding second byte code
          (LDY !$01)
          (LDA (ZP_VM_PC),y)
          (BMI NEGATIVE_BRANCH__)

   (label POP_AND_CONTINUE_AFTER_BRA)
          (TAX)
          (JSR POP_CELL_EVLSTK_TO_RT)
          (TXA)
   (label CONTINUE_AFTER_BRA)
          (CLC)
          (ADC !$02) ;; this cannot incur any carry, since the jump forward can only be < 128 => result < 130 => no carry, yet
          (ADC ZP_VM_PC)
          (STA ZP_VM_PC)
          (BCC NO_PAGE_CHANGE__)
          (INC ZP_VM_PC+1)
   (label NO_PAGE_CHANGE__)
          (JMP VM_INTERPRETER)

   (label NEGATIVE_BRANCH__)
          (TAX)
          (JSR POP_CELL_EVLSTK_TO_RT)
          (TXA)
   (label NEGATIVE_BRANCH_NO_POP__)
          (CLC)
          [ADC ZP_VM_PC]
          (STA ZP_VM_PC)
          (BCS NO_PAGE_CHANGE_ON_BACK__)
          (DEC ZP_VM_PC+1)
   (label NO_PAGE_CHANGE_ON_BACK__)
          (JMP VM_INTERPRETER)

   (label BRANCH_BY_NEXT_BYTE__NO_POP)
          (LDY !$01)
          (LDA (ZP_VM_PC),y)
          (BMI NEGATIVE_BRANCH_NO_POP__)
          (JMP CONTINUE_AFTER_BRA))))

(define BC_F_P_BRA
  (list
   (label BC_F_P_BRA)
          (CLC)
          (LDA ZP_RT+1)
          (BEQ BRANCH_BY_NEXT_BYTE)
          (LDA !$00)
          (BEQ POP_AND_CONTINUE_AFTER_BRA)))

(define BC_GOTO
  (add-label-suffix
   "__" "__GOTO"
  (list
   (label BC_GOTO)
          (CLC)
          (LDY !$01)
          (LDA (ZP_VM_PC),y)
          (BMI JUMP_BACK__)

          (ADC !$02)
          (JMP VM_INTERPRETER_INC_PC_A_TIMES)

   (label JUMP_BACK__)
          (ADC ZP_VM_PC)
          (STA ZP_VM_PC)
          (BCS NO_PAGE_CHANGE_ON_BACK__)
          (DEC ZP_VM_PC+1)
   (label NO_PAGE_CHANGE_ON_BACK__)
          (JMP VM_INTERPRETER))))
