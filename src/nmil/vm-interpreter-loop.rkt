#lang racket/base

(provide VM_INTERPRETER                         ;; fetch op at (VM_PC),y=0 and interpret that byte code
         VM_INTERPRETER_ZP                      ;; interpreter loop implemented in zero page (shaving off some cpu cycles)
         VM_INTERPRETER_OPTABLE                 ;; 256 byte holding the function vector for 128 byte codes (low, high each)
         VM_INTERPRETER_INC_PC                  ;; increment VM_PC (16 bit inc) then move on to VM_INTERPRETER
         VM_POP_EVLSTK_AND_INC_PC               ;; pop a value from the eval stack, then move on to VM_INTERPRETER_INC_PC
         VM_INTERPRETER_INC_PC_2_TIMES          ;; increment VM_PC 2 times then move on to VM_INTERPRETER
         VM_INTERPRETER_INC_PC_A_TIMES          ;; increment VM_PC by content of register A then move on to VM_INTERPRETER
         VM_INTERPRETER_INIT                    ;; init interpreter (VM_ZP and VM_FUNC_PTR) #x8000
         VM_INTERPRETER_INIT_AX                 ;; init interpreter (VM_ZP and VM_FUNC_PTR) A-lowbyte X=highbyte
         ZP_VM_PC                               ;; pc of the virtual byte code machine
         interpreter-loop-label                 ;; label of the central interpreter loop (used by debugger)
         )

#|

define the interpreter loop and its entry points
and the bc operation jump table

|#

(require (only-in racket/list
                  flatten)
         "../6510.rkt"
         (only-in "vm-runtime/vm-cell-stack.rkt"
                  POP_EVLSTK_TAIL_TO_RT)
         (only-in "vm-runtime/vm-memory-map.rkt"
                  ;; ZP_VM_PC
                  ZP_FUNC_PTR))

;; initialize PC to $8000
(define VM_INTERPRETER_INIT_AX '())
(define VM_INTERPRETER_INIT
  (list
   (label VM_INTERPRETER_INIT)
          (LDA !$00)
          (LDX !$08)                            ;; bc start at $0800
   (label VM_INTERPRETER_INIT_AX)
          (STA ZP_VM_PC)
          (STA ZP_FUNC_PTR)
          (STX ZP_VM_PC+1)
          (STX ZP_FUNC_PTR+1)                ;; mark func-ptr $0800
          (RTS)))

;; interpreter loop without short commands
;; each byte command must have lowest bit set to 0 to be aligned to the jump table
(define VM_INTERPRETER_INC_PC_2_TIMES '())
(define VM_INTERPRETER_INC_PC_A_TIMES '())
(define VM_POP_EVLSTK_AND_INC_PC '())
(define VM_INTERPRETER
  (list
                                               ;; for this shortcut to work, the interpreter must be initialized accordingly
   (label VM_INTERPRETER_RTS_TARGET)           ;; if the bc command ends with RTS, I want to end up here
          (LDA !>VM_INTERPRETER_RTS_TARGET-1)  ;; restore return target for next RTS command
          (PHA)                                ;; high byte first
          (LDA !<VM_INTERPRETER_RTS_TARGET-1)  ;; then
          (PHA)                                ;; low byte
          (JMP VM_INTERPRETER_INC_PC)
          ;; (BNE VM_INTERPRETER_INC_PC_OLD)      ;; now do regular inc pc and fetch next command
          ;; alternatively jump to zero page (JMP VM_INTERPRETER_ZP_INC_PC)

   (label VM_INTERPRETER_INC_PC_2_TIMES)
          (LDA !$02)
   (label VM_INTERPRETER_INC_PC_A_TIMES)
          (CLC)                                 ;; clear for add
          (ADC ZP_VM_PC)                        ;; PC = PC + A
          (STA ZP_VM_PC)
          (BCS VM_INTERPRETER_NEXT_PAGE_OLD)
          ;; (BCC VM_INTERPRETER_OLD)              ;; same page -> no further things to do
          (JMP VM_INTERPRETER)
          ;; alternatively jump to zero page (JMP VM_INTZP__INC_PAGE)

   (label VM_POP_EVLSTK_AND_INC_PC)
          (JSR POP_EVLSTK_TAIL_TO_RT)
          (JMP VM_INTERPRETER_INC_PC)

   ;; (label VM_INTERPRETER_INC_PC_OLD)            ;; inc by one (regular case)
   ;; ;; (label BC_NOP)                            ;; is equivalent to NOP
   ;;        (INC ZP_VM_PC)
   ;;        (BEQ VM_INTERPRETER_NEXT_PAGE_OLD)    ;; other page -> inc page

    ;; ----------------------------------------
   ;; (label VM_INTERPRETER_OLD)
   ;;        (JMP VM_INTERPRETER)
   ;;        (LDY !$00)                            ;; use 0 offset to ZP_VM_PC
   ;; (label VM_INTERPRETERy)
   ;;        (LDA (ZP_VM_PC),y)                    ;; load byte code
   ;;        ;; normal bytecode command
   ;; (label OPERAND__VM_INTERPRETER)
   ;;        (STA JMPOP__VM_INTERPRETER+1)         ;; lowbyte of the table
   ;; (label JMPOP__VM_INTERPRETER)
   ;;        (JMP (VM_INTERPRETER_OPTABLE))        ;; jump by table
   (label VM_INTERPRETER_NEXT_PAGE_OLD)
          (INC ZP_VM_PC+1)                      ;; increment high byte of pc (into next page)
          ;;(BNE VM_INTERPRETER_OLD)
          (JMP VM_INTERPRETER)
          ;; jmp to zero page?
))

(define interpreter-loop-label "VM_INTERPRETER")


;; interpreter loop completely in the zero page
;; the program counter (two bytes) is kept at VM_PCM1+1 and VM_PCM1+2!
;; usage 16 bytes (including VM_PC => 14 additional bytes used in zero page)
;; cycle count: 19 normal, 27 on page change
;; compared to non zero page impl: 23 normal, 31 on page change
(define ZP_VM_PC #x85)
(define VM_INTERPRETER_INC_PC '())
(define VM_INTERPRETER_ZP
  (list
   (org #x0080)

   (byte-const ZP_VM_PC #x85) ;; #x80 + 5
   (byte-const JUMP_LOWBYTE #x8a) ;; #x80 + 10

    ;; optional (3 additional bytes) <-- may save a lot elsewhere
    ;; (label VM_INTERPRETER_ZP_POP_EVLSTK_AND_INC_PC)
    ;;        (JSR POP_EVLSTK_TAIL_TO_RT)
   (label VM_INTERPRETER_INC_PC)
           (INC ZP_VM_PC)                      ;; increment the lowbyte of the program counter (part of this code)
           (BEQ VM_INTZP__INC_PAGE)            ;; in the rare case of page increments jump off
    (label VM_INTZP__LOAD_BC)
    (label VM_INTERPRETER)
           (ast-bytes-cmd '() (list (car (ast-opcode-cmd-bytes (LDA $8000)))))
    (label ZP_VM_PCXXX)
           (ast-bytes-cmd '() (list #x00 #x00))  ;; load the byte pointed to by the program counter (which is held exactly here)!
           ;; (STA VM_INTZP_JUMP+1)            ;; store as lowbyte for the jumptable jump
           (STA JUMP_LOWBYTE)
    (label VM_INTZP_JUMP)
           (JMP (VM_INTERPRETER_OPTABLE))      ;; do the indirect jump via the jump table
    (label VM_INTZP__INC_PAGE)
           (INC ZP_VM_PC+1)                    ;; do the page increment
           (BNE VM_INTZP__LOAD_BC)))           ;; go back to routine to load byte code

;; (module+ test
;;   (require (only-in "../ast/6510-assembler.rkt" new-assemble-to-code-list))
;;   (new-assemble-to-code-list (append (list (org #x0080) (label VM_INTERPRETER_OPTABLE)) VM_INTERPRETER_ZP)))

;; the jump table is filled by codes defined in the bx opcode definitions file
;; must be page aligned! since only lowbyte is modified in indirect call => optable needs to be exactly within one page!
(define VM_INTERPRETER_OPTABLE
  (flatten ;; necessary because word ref creates a list of ast-byte-codes ...
   (list
    (label VM_INTERPRETER_OPTABLE)              ;; code
    (build-list 128 (lambda (_n) (word-ref VM_INTERPRETER_INC_PC))))))
