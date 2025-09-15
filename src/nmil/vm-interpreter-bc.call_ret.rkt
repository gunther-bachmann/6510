#lang racket/base

(require "../6510.rkt")
(require (only-in "../ast/6510-resolver.rkt" add-label-suffix))
(require (only-in racket/list flatten))

(provide BC_CALL)

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
