#lang racket/base

(provide BC_BENCH)

(require "../../6510.rkt"
         (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC_2_TIMES)
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function-wol
                  define-vm-function))

;; execute benchmark function indexed by next byte
(define-vm-function BC_BENCH
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
          (ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd "BM_WAIT_FOR_KEYPRESS"))
          (ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd "BM_START_TIMER"))
          (ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd "BM_STOP_TIMER"))
          (ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd "BM_REPORT_TIMER"))
          (ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd "BM_RESET"))
          (ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd "BM_FILL_SCREEN_W_NUMBERS"))
          (ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd "BM_SCROLL_RIGHT_40"))
          ))
