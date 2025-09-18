#lang racket/base

(require "../../6510.rkt")
(require (only-in "../../ast/6510-resolver.rkt" add-label-suffix))
(require (only-in racket/list flatten))

(require (only-in "../vm-memory-map.rkt"
                  ZP_VM_PC
                  ZP_VM_FUNC_PTR
                  ZP_RP
                  TAGGED_INT_0
                  TAG_BYTE_BYTE_CELL))
(require (only-in "../vm-interpreter-loop.rkt" VM_INTERPRETER_INC_PC))
(require (only-in "../vm-call-frame.rkt"
                  VM_PUSH_CALL_FRAME_N
                  VM_ALLOC_LOCALS
                  VM_REFCOUNT_DECR_CURRENT_LOCALS))
(require (only-in "../vm-memory-manager.rkt"
                  DEC_REFCNT_RT))

(provide BC_CALL
         BC_Z_P_RET_POP_N
         BC_NZ_P_RET_POP_N
         BC_NIL_P_RET_L0_POP_N
         BC_TAIL_CALL
         BC_F_P_RET_F
         BC_F_P_RET
         BC_T_P_RET
         BC_RET)

(define BC_CALL
  (add-label-suffix
   "__" "__CALL"
  (list
   (label BC_CALL)
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
          (JSR VM_PUSH_CALL_FRAME_N)
          (LDY !$00)                            ;; index to number of locals (0)
          (LDA (ZP_RP),y)                       ;; A = #locals
          (AND !$0f)                            ;; mask out the number of locals
          (JSR VM_ALLOC_LOCALS)                 ;; even if A=0 will set the top_mark and the locals appropriately

          ;; load zp_vm_pc with address of function bytecode
          (LDA ZP_RP)
          (STA ZP_VM_PC)
          (STA ZP_VM_FUNC_PTR)
          (LDA ZP_RP+1)
          (STA ZP_VM_PC+1)
          (STA ZP_VM_FUNC_PTR+1)

          (JMP VM_INTERPRETER_INC_PC)))) ;; function starts at function descriptor + 1

(define BC_NZ_P_RET_POP_N '())
(define BC_Z_P_RET_POP_N
  (add-label-suffix
   "__" "BC_Z_P_RET_POP_N"
   (list
    (label BC_Z_P_RET_POP_N)
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
           (JSR DEC_REFCNT_RT)
           (JSR POP_CELL_EVLSTK_TO_RT)
           (DEC COUNT__)
           (BNE POP_LOOP__)
    (label RET__)
           (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)
           (JSR VM_POP_CALL_FRAME_N)                     ;; now pop the call frame

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
           (byte 0))))

(define BC_NIL_P_RET_L0_POP_N
  (add-label-suffix
   "__" "__BC_NIL_P_RET_L0_POP_N"
  (list
   (label BC_NIL_P_RET_L0_POP_N)
          (LDX ZP_RT)
          (CPX !<TAGGED_NIL)                 ;; lowbyte = tagged_nil lowbyte
          (BEQ RETURN__)                     ;; is nil => return param or local
          (JMP VM_INTERPRETER_INC_PC)        ;; next instruction

   (label RETURN__)
          (LSR)                              ;;
          (AND !$03)
          (BEQ DONE__)
          (TAX)

   (label LOOP_POP__)
          (DEC ZP_CELL_STACK_TOS)
          (LDY ZP_CELL_STACK_TOS)
          (LDA (ZP_CELL_STACK_LB_PTR),y)
          (STA ZP_RZ)
          (LDA (ZP_CELL_STACK_HB_PTR),y)
          (STA ZP_RZ+1)
          (STX ZP_RP)
          (JSR DEC_REFCNT_RZ)
          (LDX ZP_RP)
          (LDY ZP_CELL_STACK_TOS)
          (CPY !$01)
          (BEQ STACK_DEPLETED__)
          (DEX)
          (BNE LOOP_POP__)

          (STY ZP_CELL_STACK_TOS)             ;; store new tos marker

   (label DONE__)
          (LDY !$00)
          (LDA (ZP_LOCALS_LB_PTR),y)          ;; load low byte from local
          (STA ZP_RT)                         ;; -> RT
          (LDA (ZP_LOCALS_HB_PTR),y)          ;; load high byte from local
          (STA ZP_RT+1)                       ;; -> RT

          (LDA !$00)
          (STA (ZP_LOCALS_LB_PTR),y)          ;; clear low byte from local
          (STA (ZP_LOCALS_HB_PTR),y)          ;; clear high byte from local
          (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)
          (JSR VM_POP_CALL_FRAME_N)           ;; now pop the call frame

          (JMP VM_INTERPRETER)                ;; and continue

   (label STACK_DEPLETED__)
          ;; (LDY !$01)                       ;; Y already is 01 when entering here
          (LDA (ZP_CELL_STACK_LB_PTR),y)      ;; get previous lb page
          (BEQ ERROR_EMPTY_STACK__)           ;; = 0 => stack ran empty

          (STA ZP_CELL_STACK_LB_PTR+1)        ;; store previous lb page to lb ptr
          (LDA (ZP_CELL_STACK_HB_PTR),y)      ;; get previous hb page
          (STA ZP_CELL_STACK_HB_PTR+1)        ;; store previous hb page into hb ptr
          (LDY !$ff)                          ;; assume $ff as new cell_stack_tos
          (BNE LOOP_POP__)                    ;; always jump


   (label SHORTCMD__)
          ;; open for other shortcut command
   (label ERROR_EMPTY_STACK__)
          (BRK))))

(define BC_TAIL_CALL
  (list
   (label BC_TAIL_CALL)
          (LDA ZP_VM_FUNC_PTR)
          (STA ZP_VM_PC)
          (LDA ZP_VM_FUNC_PTR+1)
          (STA ZP_VM_PC+1)

          ;; adjust pc to start executing function ptr +1
          (JMP VM_INTERPRETER_INC_PC)))

(define BC_RET
  (add-label-suffix
   "__" "__BC_RET"
  (list
   (label BC_RET)
          ;; load # locals = 0 skip this step
          (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)
          (LDA ZP_RA)
          (BEQ NO_RA__)
          (JSR VM_REFCOUNT_DECR_ARRAY_REGS)
   (label NO_RA__)
          (JSR VM_POP_CALL_FRAME_N)             ;; maybe move the respective code into here, (save jsr)
          (JMP VM_INTERPRETER))))

(define BC_F_P_RET_F
  (add-label-suffix
   "__" "__F_P_RET_F"
  (list
   (label BC_F_P_RET_F)
          (LDA ZP_RT+1)
          (BNE IS_TRUE__)
          ;; don't pop false value, return it!
          (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)
          (JSR VM_POP_CALL_FRAME_N)             ;; now pop the call frame
          (JMP VM_INTERPRETER)
   (label IS_TRUE__)
          (JMP VM_POP_EVLSTK_AND_INC_PC))))

(define BC_F_P_RET
  (add-label-suffix
   "__" "__F_P_RET"
  (list
   (label BC_F_P_RET)
          (LDA ZP_RT+1)
          (BNE IS_TRUE__)
          (JSR POP_CELL_EVLSTK_TO_RT)
          (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)
          (JSR VM_POP_CALL_FRAME_N)             ;; now pop the call frame
          (JMP VM_INTERPRETER)
   (label IS_TRUE__)
          (JMP VM_POP_EVLSTK_AND_INC_PC))))

(define BC_T_P_RET
  (add-label-suffix
   "__" "__BC_T_P_RET"
  (list
   (label BC_T_P_RET)
          (LDA ZP_RT+1)
          (BEQ IS_FALSE__)
          (JSR POP_CELL_EVLSTK_TO_RT)
          (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)
          (JSR VM_POP_CALL_FRAME_N)             ;; now pop the call frame
          (JMP VM_INTERPRETER)
   (label IS_FALSE__)
          (JMP VM_POP_EVLSTK_AND_INC_PC))))
