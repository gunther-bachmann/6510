#lang racket/base

(require "../6510.rkt")
(require (only-in "../ast/6510-resolver.rkt" add-label-suffix))
(require (only-in racket/list flatten))

(require (only-in "./vm-memory-map.rkt"
                  ZP_VM_PC
                  ZP_VM_FUNC_PTR
                  ZP_RP
                  TAGGED_INT_0
                  TAG_BYTE_BYTE_CELL))
(require (only-in "./vm-interpreter-loop.rkt" VM_INTERPRETER_INC_PC))
(require (only-in "./vm-call-frame.rkt"
                  VM_PUSH_CALL_FRAME_N
                  VM_ALLOC_LOCALS
                  VM_REFCOUNT_DECR_CURRENT_LOCALS))
(require (only-in "./vm-memory-manager.rkt"
                  DEC_REFCNT_RT))

(provide BC_CALL
         BC_Z_P_RET_POP_N
         BC_NZ_P_RET_POP_N)

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

(define BC_NZ_P_RET_POP_N #t)
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
