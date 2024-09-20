#lang racket/base

(require "../6510.rkt")
(require (only-in "../ast/6510-assembler.rkt" assemble assemble-to-code-list translate-code-list-for-basic-loader))
(require (only-in racket/list flatten take))

(require (only-in "./vm-memory-manager.rkt" vm-memory-manager))
(require (only-in "./vm-lists.rkt" vm-lists))

(module+ test
  (require "../6510-test-utils.rkt")
  (require (only-in racket/port open-output-nowhere))
  (require (only-in "../tools/6510-disassembler.rkt" disassemble-bytes))
  (require (only-in "../tools/6510-debugger.rkt" run-debugger-on)))

(require (only-in "../tools/6510-interpreter.rkt" 6510-load 6510-load-multiple initialize-cpu run-interpreter run-interpreter-on memory-list cpu-state-accumulator))

(provide vm-interpreter)

(define VM_INTERPRETER_VARIABLES
  (list
   (byte-const VM_PC #x14)) ;; program counter 14..15
  )

;; initialize PC to $8000
(define VM_INTERPRETER_INIT
  (list
   (label VM_INTERPRETER_INIT)
          (LDA !$00)
          (STA VM_PC)
          (LDA !$80)
          (STA VM_PC+1)
          (RTS)))

(define VM_BRK
  (list
   (label VM_BRK)
          (BRK)))

(define VM_PUSH_CONST_NUM_SHORT
  (list
   (label VM_PUSH_CONST_NUM_SHORT)
          (JMP VM_INTERPRETER_NEXT)))

(define VM_INT_PLUS
  (list
   (label VM_INT_PLUS)
          (JMP VM_INTERPRETER_NEXT)))

(define VM_INT_MINUS
  (list
   (label VM_INT_MINUS)
          (JMP VM_INTERPRETER_NEXT)))

(define VM_PUSH_PARAM_OR_LIST
  (list
   (label VM_PUSH_PARAM_OR_LIST)
          (JMP VM_INTERPRETER_NEXT)))

;; must be page aligned!
(define VM_INTERPRETER_OPTABLE
  (flatten ;; necessary because word ref creates a list of ast-byte-codes ...
   (list
    (label VM_INTERPRETER_OPTABLE)
           (word-ref VM_PUSH_CONST_NUM_SHORT) ;; 80..87 -> 00 INT|BYTE
           (word-ref VM_INTERPRETER_NEXT)     ;; 02 effectively NOP
           (word-ref VM_BRK)                  ;; 04 break into debugger/exit program
           (word-ref VM_CONS)                 ;; 06
           (word-ref VM_CAR)                  ;; 08
           (word-ref VM_CDR)                  ;; 0a
           (word-ref VM_NIL_P)                ;; 0c
           (word-ref VM_INT_PLUS)             ;; 0e
           (word-ref VM_PUSH_PARAM_OR_LIST)   ;; 88..8F -> 10 LOCAL|PARAM
           (word-ref VM_INT_MINUS)            ;; 12
           ;; ...
    )))

(define VM_INTERPRETER
  (list
   (label VM_INTERPRETER)
          (LDY !$ff)
   (label VM_INTERPRETER_NEXT)
          (INY)
          (LDA (VM_PC),y)
          (ASL A)
          (BCC OPERAND__VM_INTERPRETER)
          (AND !$F0)
   (label OPERAND__VM_INTERPRETER)
          (STA JMPOP__VM_INTERPRETER+1) ;; lowbyte of the table
   (label JMPOP__VM_INTERPRETER)
          (JMP (VM_INTERPRETER_OPTABLE))))

(module+ test #| vm_interpreter |#
  (define use-case-brk
    (list
     (byte 02))) ;; byte code for VM_BRK

  (define use-case-brk-code
    (append (list (org #x7000)
                  (JSR VM_INTERPRETER_INIT)
                  (JMP VM_INTERPRETER))
            (list (org #x8000))
            use-case-brk
            (list (org #xc000))
            vm-interpreter))

  (define use-case-brk-state-before (6510-load-multiple (initialize-cpu) (assemble-to-code-list use-case-brk-code)))
  ;; (run-debugger-on use-case-brk-state-before)
  (define use-case-brk-state-after  (parameterize ([current-output-port (open-output-nowhere)]) (run-interpreter-on use-case-brk-state-before)))
)

(define vm-interpreter
  (append VM_INTERPRETER_VARIABLES
          VM_INTERPRETER_INIT
          VM_PUSH_CONST_NUM_SHORT
          VM_INTERPRETER
          VM_BRK
          VM_INT_PLUS
          VM_INT_MINUS
          VM_PUSH_PARAM_OR_LIST
          (list (org-align #x100)) ;; align to next page
          VM_INTERPRETER_OPTABLE
          vm-lists))
