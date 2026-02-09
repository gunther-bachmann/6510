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
          #| 00 |#(ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd "BM_WAIT_FOR_KEYPRESS"))
          #| 01 |#(ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd "BM_START_TIMER"))
          #| 02 |#(ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd "BM_STOP_TIMER"))
          #| 03 |#(ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd "BM_REPORT_TIMER"))
          #| 04 |#(ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd "BM_RESET"))
          #| 05 |#(ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd "BM_FILL_SCREEN_W_NUMBERS"))
          #| 06 |#(ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd "BM_SCROLL_RIGHT_40"))
          #| 07 |#(ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd "BM_SCROLL_DOWN_25"))
          #| 08 |#(ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd "BM_SCROLL_UP_25"))
          #| 09 |#(ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd "BM_SCROLL_LEFT_40"))
          #| 0a |#(ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd "BM_TEXT_WINDOW"))
          ))
