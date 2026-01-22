#lang racket/base

(provide BC_CALL
         BC_Z_P_RET_POP_N
         BC_NZ_P_RET_POP_N
         BC_NIL_P_RET_L0_POP_N
         BC_TAIL_CALL
         BC_F_P_RET_F
         BC_F_P_RET
         BC_T_P_RET
         BC_RET

         bc-call-ret-code)

(require "../../6510.rkt"
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function
                  define-vm-function-wol)
         (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC
                  ZP_VM_PC)
         (only-in "../vm-runtime/vm-call-frame.rkt"
                  VM_PUSH_CALL_FRAME
                  VM_POP_CALL_FRAME)
         (only-in "../vm-runtime/vm-m1-slots.rkt"
                  DEC_REFCNT_M1_SLOT_RT__IF_PTR
                  DEC_REFCNT_M1_SLOT_RZ__IF_PTR)
         (only-in "../vm-runtime/vm-memory-map.rkt"
                  ZP_FUNC_PTR
                  ZP_RP
                  TAGGED_INT_0
                  TAG_BYTE_BYTE_CELL))

(define-vm-function BC_CALL
  (list
          ;; load the two bytes following into ZP_RP (ptr to function descriptor)
          (LDY !$01)
          (LDA (ZP_VM_PC),y)                    ;; load lowbyte of call target, right behind byte-code
          (STA ZP_RP)                           ;; -> RA
          (INY)
          (LDA (ZP_VM_PC),y)                    ;; load highbyte of call target, behind lowbyte
          (STA ZP_RP+1)                         ;; -> RA
          ;; RA now holds the call target function address

          ;; put return to adress into zp_vm_pc (for save)
          (LDA !$03)                            ;; call is 3 bytes long (bc + address)
          (CLC)
          (ADC ZP_VM_PC)
          (STA ZP_VM_PC)                        ;; write into program counter
          (BCC DONE_INC_PC__)
          (INC ZP_VM_PC+1)                      ;; inc page of program counter
          ;; zp_vm_pc holds follow bc after this call
   (label DONE_INC_PC__)

   (label VM_CALL_NO_PUSH_FUN_IN_RA)
          ;; ZP_RP holds pointer to function descriptor
          (LDY !$00)                            ;; index to number of locals (0)
          (LDA (ZP_RP),y)                       ;; A = #locals
          (TAX)
          (JSR VM_PUSH_CALL_FRAME)
          ;; (LDY !$00)                            ;; index to number of locals (0)
          ;; (LDA (ZP_RP),y)                       ;; A = #locals
          ;; (AND !$0f)                            ;; mask out the number of locals
          ;; (JSR VM_ALLOC_LOCALS)                 ;; even if A=0 will set the top_mark and the locals appropriately

          ;; load zp_vm_pc with address of function bytecode
          (LDA ZP_RP)
          (STA ZP_VM_PC)
          (STA ZP_FUNC_PTR)
          (LDA ZP_RP+1)
          (STA ZP_VM_PC+1)
          (STA ZP_FUNC_PTR+1)

          (JMP VM_INTERPRETER_INC_PC))) ;; function starts at function descriptor + 1

(define BC_NZ_P_RET_POP_N '())
(define-vm-function BC_Z_P_RET_POP_N
   (list
           (LSR)                        ;; number to pop
           (AND !$03)
           (LDX ZP_RT+1)
           (BNE DONE__)                 ;; tos != byte 0 => do nothing
           (LDX ZP_RT)
           (CPX !>TAGGED_INT_0)
           (BEQ POP_N_RET__)            ;; tos = int 0 => pop and return
           (CPX !TAG_BYTE_BYTE_CELL)
           (BNE DONE__)                 ;; tos is neither int nor byte => do nothing

    (label POP_N_RET__)
           (CMP !$00)                   ;;
           (BEQ RET__)                  ;; nothing to pop -> just return
           (STA COUNT__)                ;; keep count to pop
    (label POP_LOOP__)
           (JSR DEC_REFCNT_M1_SLOT_RT__IF_PTR)
           (JSR POP_EVLSTK_TAIL_TO_RT)
           (DEC COUNT__)
           (BNE POP_LOOP__)
    (label RET__)
           ;; (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)
           (JSR VM_POP_CALL_FRAME)                     ;; now pop the call frame

    (label DONE__)
           (JMP VM_INTERPRETER_INC_PC)

    (label BC_NZ_P_RET_POP_N)
           (LSR)                        ;; number to pop
           (AND !$03)
           (LDX ZP_RT+1)
           (BEQ DONE__)                 ;; tos (hb) = 0 => do nothing, cannot be byte 0 nor int 0
           (LDX ZP_RT)
           (CPX !TAG_BYTE_BYTE_CELL)
           (BEQ POP_N_RET__)            ;; is a byte cell (and high byte !=0) -> do pop and return
           (CPX !>TAGGED_INT_0)
           (BNE POP_N_RET__)            ;; tos = int != 0 => pop and return
           (JMP VM_INTERPRETER_INC_PC)  ;; tos is int 0 => do nothing

    ;; type specialized method (byte)
    ;; (label BC_BZ_P_RET_POP_N)
    ;;        (LSR)                        ;; number to pop
    ;;        (AND !$03)
    ;;        (LDX ZP_RT+1)
    ;;        (BEQ POP_N_RET__)
    ;;        (JMP VM_INTERPRETER_INC_PC)

    ;; (label BC_BNZ_P_RET_POP_N)
    ;;        (LSR)                        ;; number to pop
    ;;        (AND !$03)
    ;;        (LDX ZP_RT+1)
    ;;        (BNE POP_N_RET__)
    ;;        (JMP VM_INTERPRETER_INC_PC)

    ;; ;; type specialized method (int)
    ;; (label BC_IZ_P_RET_POP_N)
    ;;        (LSR)                        ;; number to pop
    ;;        (AND !$03)
    ;;        (LDX ZP_RT+1)
    ;;        (BNE DONE__)
    ;;        (LDX ZP_RT)
    ;;        (CMP !>TAGGED_INT_0)
    ;;        (BEQ POP_N_RET__)
    ;;        (JMP VM_INTERPRETER_INC_PC)

    ;; (label BC_INZ_P_RET_POP_N)
    ;;        (LSR)                        ;; number to pop
    ;;        (AND !$03)
    ;;        (LDX ZP_RT+1)
    ;;        (BNE POP_N_RET__)
    ;;        (LDX ZP_RT)
    ;;        (CPX !>TAGGED_INT_0)
    ;;        (BNE POP_N_RET__)
    ;;        (JMP VM_INTERPRETER_INC_PC)

    (label COUNT__)
           (byte 0)))

(define-vm-function BC_NIL_P_RET_L0_POP_N
  (list
          (LDX ZP_RT)
          (CPX !<TAGGED_NIL)                 ;; lowbyte = tagged_nil lowbyte
          (BEQ RETURN__)                     ;; is nil => return param or local
          (JMP VM_INTERPRETER_INC_PC)        ;; next instruction

   (label RETURN__)
          (LSR)                              ;;
          (AND !$03)
          (BEQ DONE__)

          (CLC)
          (ADC !$01)
          (STA COUNT__)                ;; keep count to pop

   (label POP_LOOP__)
          (JSR DEC_REFCNT_M1_SLOT_RT__IF_PTR)
          (JSR POP_EVLSTK_TAIL_TO_RT)
          (DEC COUNT__)
          (BNE POP_LOOP__)

   (label DONE__)
          (LDY !$00)
          (LDA (ZP_LOCALS_LB_PTR),y)          ;; load low byte from local
          (STA ZP_RT)                         ;; -> RT
          (LDA (ZP_LOCALS_HB_PTR),y)          ;; load high byte from local
          (STA ZP_RT+1)                       ;; -> RT

          ;; (LDA !$00)
          (TYA)
          (STA (ZP_LOCALS_LB_PTR),y)          ;; clear low byte from local
          (STA (ZP_LOCALS_HB_PTR),y)          ;; clear high byte from local
          ;; (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)
          (JSR VM_POP_CALL_FRAME)           ;; now pop the call frame

          (JMP VM_INTERPRETER)                ;; and continue

   (label SHORTCMD__)
          ;; open for other shortcut command
   (label ERROR_EMPTY_STACK__)
          (BRK)

   (label COUNT__)
          (byte 0)))

(define-vm-function BC_TAIL_CALL
  (list
          (LDA ZP_FUNC_PTR)
          (STA ZP_VM_PC)
          (LDA ZP_FUNC_PTR+1)
          (STA ZP_VM_PC+1)

          ;; adjust pc to start executing function ptr +1
          (JMP VM_INTERPRETER_INC_PC)))

(define-vm-function BC_RET
  (list
          ;; load # locals = 0 skip this step
          (LDA ZP_RA)
          (BEQ NO_RA__)
          (JSR VM_REFCOUNT_DECR_ARRAY_REGS)
   (label NO_RA__)
          ;; (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)
          (JSR VM_POP_CALL_FRAME)             ;; maybe move the respective code into here, (save jsr)
          (JMP VM_INTERPRETER)))

(define-vm-function BC_F_P_RET_F
  (list
          (LDA ZP_RT+1)
          (BNE IS_TRUE__)
          ;; don't pop false value, return it!
          ;; (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)
          (JSR VM_POP_CALL_FRAME)             ;; now pop the call frame
          (JMP VM_INTERPRETER)
   (label IS_TRUE__)
          (JMP VM_POP_EVLSTK_AND_INC_PC)))

(define-vm-function BC_F_P_RET
  (list
          (LDA ZP_RT+1)
          (BNE IS_TRUE__)
          (JSR POP_EVLSTK_TAIL_TO_RT)
          ;; (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)
          (JSR VM_POP_CALL_FRAME)             ;; now pop the call frame
          (JMP VM_INTERPRETER)
   (label IS_TRUE__)
          (JMP VM_POP_EVLSTK_AND_INC_PC)))

(define-vm-function BC_T_P_RET
  (list
          (LDA ZP_RT+1)
          (BEQ IS_FALSE__)
          (JSR POP_EVLSTK_TAIL_TO_RT)
          ;; (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)
          (JSR VM_POP_CALL_FRAME)             ;; now pop the call frame
          (JMP VM_INTERPRETER)
   (label IS_FALSE__)
          (JMP VM_POP_EVLSTK_AND_INC_PC)))

(define bc-call-ret-code
  (append
   BC_CALL
   BC_Z_P_RET_POP_N
   BC_NZ_P_RET_POP_N
   BC_NIL_P_RET_L0_POP_N
   BC_TAIL_CALL
   BC_F_P_RET_F
   BC_F_P_RET
   BC_T_P_RET
   BC_RET))
