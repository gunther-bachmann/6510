#lang racket/base

(provide BC_BIOS)

(require "../../6510.rkt"
         (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC_2_TIMES)
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function-wol
                  define-vm-function)
         (only-in "../vm-runtime/vm-bios.rkt"
                  BIOS_PUT_STRING_AT))

(define-vm-function BC_BIOS
  (list
          (LDY !1)
          (LDA (ZP_VM_PC),y)
          (ASL A)
          (TAY)
          (LDA JUMP_TABLE__,y)
          (STA JUMP_CMD__+1)
          (INY)
          (LDA JUMP_TABLE__,y)
          (STA JUMP_CMD__+2)
   (label JUMP_CMD__)
          (JSR DONE__)
          (JMP VM_INTERPRETER_INC_PC_2_TIMES)
   (label DONE__)
          (RTS) ;; dummy for unmodified jump

   (label JUMP_TABLE__)
   #| 00 |#(ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd (symbol->string 'BIOS_PUT_STRING_AT)))
          ))

