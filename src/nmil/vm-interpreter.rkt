#lang racket/base

(require "../6510.rkt")
(require (only-in "../ast/6510-assembler.rkt" assemble assemble-to-code-list translate-code-list-for-basic-loader))
(require (only-in racket/list flatten take))

(require (only-in "./vm-memory-manager.rkt" vm-memory-manager))
(require (only-in "./vm-lists.rkt" vm-lists))

(module+ test
  (require "../6510-test-utils.rkt")
  (require (only-in "../6510-utils.rkt" absolute))
  (require (only-in racket/port open-output-nowhere))
  (require (only-in "../tools/6510-disassembler.rkt" disassemble-bytes))
  (require (only-in "../tools/6510-debugger.rkt" run-debugger-on)))

(require (only-in "../tools/6510-interpreter.rkt" 6510-load 6510-load-multiple initialize-cpu run-interpreter run-interpreter-on memory-list cpu-state-accumulator cpu-state-program-counter peek))

(provide vm-interpreter)


;; TODO: implement CALL, PUSH_LOCAL,
;; TODO: supporting functions like create local frame


(define VM_INTERPRETER_VARIABLES
  (list
   (byte-const ZP_VM_PC #x14)) ;; program counter 14..15
  )

;; initialize PC to $8000
(define VM_INTERPRETER_INIT
  (list
   (label VM_INTERPRETER_INIT)
          (LDA !$00)
          (STA ZP_VM_PC)
          (LDA !$80)
          (STA ZP_VM_PC+1)
          (RTS)))

(define VM_BRK
  (list
   (label VM_BRK)
          (BRK)))

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
  (define use-case-brk-current-vm-pc
    (absolute (peek use-case-brk-state-after #x15)
              (peek use-case-brk-state-after #x14)))

  (check-equal? (memory-list use-case-brk-state-after
                             use-case-brk-current-vm-pc use-case-brk-current-vm-pc)
                (list #x02)
                "stopped at byte code brk"))

(define VM_PUSH_CONST_NUM_SHORT
  (flatten
   (list
    (label VM_PUSH_CONST_NUM_SHORT)
           (LDA (ZP_VM_PC),y)
           (AND !$07) ;; lower three bits are encoded into the short command
           (ASL A) ;; * 2
           (TAY)
           (LDA VM_PUSH_CONST_NUM_SHORT__JUMP_REFS,y)
           (STA VM_PUSH_CONST_NUM_SHORT__JSR_TARGET+1)
           (INY)
           (LDA VM_PUSH_CONST_NUM_SHORT__JUMP_REFS,y)
           (STA VM_PUSH_CONST_NUM_SHORT__JSR_TARGET+2)
    (label VM_PUSH_CONST_NUM_SHORT__JSR_TARGET)
           (JSR VM_CELL_STACK_PUSH_INT_0)
           (JMP VM_INTERPRETER_INC_PC)

    (label VM_PUSH_CONST_NUM_SHORT__JUMP_REFS)
           (word-ref VM_CELL_STACK_PUSH_INT_0)
           (word-ref VM_CELL_STACK_PUSH_INT_1)
           (word-ref VM_CELL_STACK_PUSH_INT_2)
           (word-ref VM_CELL_STACK_PUSH_INT_m1)
           ;; (word-ref VM_CELL_STACK_PUSH_BYTE_0)
           ;; (word-ref VM_CELL_STACK_PUSH_BYTE_1)
           ;; (word-ref VM_CELL_STACK_PUSH_BYTE_2)
           ;; (word-ref VM_CELL_STACK_PUSH_BYTE_m1)
           )))

(module+ test #| vm_interpreter |#
  (define use-case-push-num-s
    (list
     (byte #x80)     ;; byte code for PUSH_INT_0
     (byte #x81)     ;; byte code for PUSH_INT_1
     (byte #x82)     ;; byte code for PUSH_INT_2
     (byte #x83)     ;; byte code for PUSH_INT_m1
     (byte #x02)))   ;; brk

  (define use-case-push-num-s-code
    (append (list (org #x7000)
                  (JSR VM_INITIALIZE_MEMORY_MANAGER)
                  (JSR VM_INTERPRETER_INIT)
                  (JMP VM_INTERPRETER))
            (list (org #x8000))
            use-case-push-num-s
            (list (org #xc000))
            vm-interpreter))

  (define use-case-push-num-s-state-before (6510-load-multiple (initialize-cpu) (assemble-to-code-list use-case-push-num-s-code)))
  ;; (run-debugger-on use-case-push-num-s-state-before)
  (define use-case-push-num-s-state-after  (parameterize ([current-output-port (open-output-nowhere)]) (run-interpreter-on use-case-push-num-s-state-before)))

  (check-equal? (memory-list use-case-push-num-s-state-after #xd9 #xe5)
                (list #x0a              ;; stack contains 4 elements = 4*3-2
                      #x00 #x00 #x00
                      #x00 #x00 #x01
                      #x00 #x00 #x02
                      #x7c #x7c #xff)
                "4 elements on the stack of int 0, int 1, int 2, int -1"))

(define VM_PUSH_CONST_INT
  (list
   (label VM_PUSH_CONST_INT)
          (LDY !$01)
          (LDA (ZP_VM_PC),y)
          (TAX)
          (INY)
          (LDA (ZP_VM_PC),y)
          (TAY)
          (TXA)
          (JSR VM_CELL_STACK_PUSH_INT)
          (LDA !$03)
          (JMP VM_INTERPRETER_INC_PC_A_TIMES)))

(module+ test #| VM_PUSH_CONST_INT |#
  (define use-case-push-int
    (list
     (byte #x0a)     ;; byte code for PUSH_INT
     (byte #x04)     ;; highbyte int
     (byte #xf0)     ;; lowbyte int
     (byte #x02)))   ;; brk

  (define use-case-push-int-code
    (append (list (org #x7000)
                  (JSR VM_INITIALIZE_MEMORY_MANAGER)
                  (JSR VM_INTERPRETER_INIT)
                  (JMP VM_INTERPRETER))
            (list (org #x8000))
            use-case-push-int
            (list (org #xc000))
            vm-interpreter))

  (define use-case-push-int-state-before (6510-load-multiple (initialize-cpu) (assemble-to-code-list use-case-push-int-code)))
  ;; (run-debugger-on use-case-push-int-state-before)
  (define use-case-push-int-state-after  (parameterize ([current-output-port (open-output-nowhere)]) (run-interpreter-on use-case-push-int-state-before)))

  (check-equal? (memory-list use-case-push-int-state-after #xd9 #xdc)
                (list #x01              ;; stack contains 1 elements = 1*3-2
                      #x10 #x10 #xf0)
                "one element on the stack"))

(define VM_INT_PLUS
  (list
   (label VM_INT_PLUS)
          (LDX ZP_CELL_TOS)
          (LDA ZP_CELL0+1,x)
          (ADC ZP_CELL0-2,x)
          (STA ZP_CELL0-2,x) ;; low byte

          (LDA ZP_CELL0,x)
          (BCC VM_INT_PLUS__NO_INC_HIGH)
          (CLC)
          (ADC !$04)

   (label VM_INT_PLUS__NO_INC_HIGH)
          (ADC ZP_CELL0-3,x)
          (AND !$7c)
          (STA ZP_CELL0-3,x) ;; high byte
          (STA ZP_CELL0-4,x) ;; high byte into tagged byte

   (label VM_INT_PLUS__DONE)
          (DEX)
          (DEX)
          (DEX)
          (STX ZP_CELL_TOS)
          (JMP VM_INTERPRETER_INC_PC)))

(module+ test #| vm_interpreter |#
  (define use-case-int-plus
    (list
     (byte #x81)            ;; byte code for PUSH_INT_1
     (byte #x82)            ;; byte code for PUSH_INT_2
     (byte #x07)            ;; byte code for INT_PLUS = 3
     (byte #x0a #x04 #xf0)  ;; push int #x4f0 (1264)
     (byte #x0a #x01 #x1f)  ;; push int #x11f (287)
     (byte #x07)     ;; byte code for INT_PLUS (+ #x04f0 #x011f) (1551 = #x060f)
     (byte #x81)            ;; byte code for PUSH_INT_1
     (byte #x83)            ;; byte code for PUSH_INT_m1
     (byte #x07)            ;; byte code for INT_PLUS = 0
     (byte #x02)))   ;; brk

  (define use-case-int-plus-code
    (append (list (org #x7000)
                  (JSR VM_INITIALIZE_MEMORY_MANAGER)
                  (JSR VM_INTERPRETER_INIT)
                  (JMP VM_INTERPRETER))
            (list (org #x8000))
            use-case-int-plus
            (list (org #xc000))
            vm-interpreter))

  (define use-case-int-plus-state-before (6510-load-multiple (initialize-cpu) (assemble-to-code-list use-case-int-plus-code)))
  ;; (run-debugger-on use-case-int-plus-state-before)
  (define use-case-int-plus-state-after  (parameterize ([current-output-port (open-output-nowhere)]) (run-interpreter-on use-case-int-plus-state-before)))

  (check-equal? (memory-list use-case-int-plus-state-after #xd9 #xe2)
                (list #x07              ;; stack contains 1 elements = 1*3-2
                      #x00 #x00 #x03
                      #x18 #x18 #x0f   ;; (1551 = #x060f) encoded #x06 << 2 = #x18, #x0f = #x0f
                      #x00 #x00 #x00)  ;; -1 + 1 = 0
                "three elements on the stack, int 3, int 1551, int 0"))

(define VM_INT_MINUS
  (list
   (label VM_INT_MINUS)
          (LDX ZP_CELL_TOS)

          (SEC)
          (LDA ZP_CELL0+1,x)
          (SBC ZP_CELL0-2,x)
          (STA ZP_CELL0-2,x) ;; low byte

          (LDA ZP_CELL0,x)
          (BCS VM_INT_MINUS__NO_DEC_HIGH)
          (SEC)
          (SBC !$04)

   (label VM_INT_MINUS__NO_DEC_HIGH)
          (SBC ZP_CELL0-3,x)
          (AND !$7c)
          (STA ZP_CELL0-3,x) ;; high byte
          (STA ZP_CELL0-4,x) ;; high byte into tagged byte

   (label VM_INT_MINUS__DONE)
          (DEX)
          (DEX)
          (DEX)
          (STX ZP_CELL_TOS)
          (JMP VM_INTERPRETER_INC_PC)))

(module+ test #| vm_interpreter |#
  (define use-case-int-minus
    (list
     (byte #x81)            ;; byte code for PUSH_INT_1
     (byte #x82)            ;; byte code for PUSH_INT_2
     (byte #x09)            ;; byte code for INT_MINUS = 2 - 1 = 1
     (byte #x0a #x04 #xf0)  ;; push int #x4f0 (1264)
     (byte #x0a #x01 #x1f)  ;; push int #x11f (287)
     (byte #x09)            ;; byte code for INT_MINUS (- #x011f #x04f0) ( -977 = #x0c2f -> encoded #x302f)
     (byte #x81)            ;; byte code for PUSH_INT_1
     (byte #x80)            ;; byte code for PUSH_INT_0
     (byte #x09)            ;; byte code for INT_MINUS => -1
     (byte #x02)))   ;; brk

  (define use-case-int-minus-code
    (append (list (org #x7000)
                  (JSR VM_INITIALIZE_MEMORY_MANAGER)
                  (JSR VM_INTERPRETER_INIT)
                  (JMP VM_INTERPRETER))
            (list (org #x8000))
            use-case-int-minus
            (list (org #xc000))
            vm-interpreter))

  (define use-case-int-minus-state-before (6510-load-multiple (initialize-cpu) (assemble-to-code-list use-case-int-minus-code)))
  ;; (run-debugger-on use-case-int-minus-state-before)
  (define use-case-int-minus-state-after  (parameterize ([current-output-port (open-output-nowhere)]) (run-interpreter-on use-case-int-minus-state-before)))

  (check-equal? (memory-list use-case-int-minus-state-after #xd9 #xe2)
                (list #x07              ;; stack contains 1 elements = 1*3-2
                      #x00 #x00 #x01
                      #x70 #x70 #x2f   ;;
                      #x7c #x7c #xff)  ;; 0 - 1 = -1
                "three elements on the stack, int 3, int 1551, int 0"))

(define VM_PUSH_PARAM_OR_LOCAL
  (list
   (label VM_PUSH_PARAM_OR_LOCAL)
          (JMP VM_INTERPRETER_INC_PC)))

;; must be page aligned!
(define VM_INTERPRETER_OPTABLE
  (flatten ;; necessary because word ref creates a list of ast-byte-codes ...
   (list
    (label VM_INTERPRETER_OPTABLE)
           (word-ref VM_PUSH_CONST_NUM_SHORT) ;; 80..87 -> 00 INT|BYTE
           (word-ref VM_INTERPRETER_INC_PC)   ;; 01 effectively NOP
           (word-ref VM_BRK)                  ;; 02 break into debugger/exit program
           (word-ref VM_CONS)                 ;; 03
           (word-ref VM_CAR)                  ;; 04
           (word-ref VM_CDR)                  ;; 05
           (word-ref VM_NIL_P)                ;; 06
           (word-ref VM_INT_PLUS)             ;; 07
           (word-ref VM_PUSH_PARAM_OR_LOCAL)   ;; 88..8F -> 10 LOCAL|PARAM
           (word-ref VM_INT_MINUS)            ;; 09
           (word-ref VM_PUSH_CONST_INT)       ;; 0a
           ;; ...
    )))

(define VM_INTERPRETER
  (list
   (label VM_INTERPRETER_INC_PC_A_TIMES)
          (CLC)
          (ADC ZP_VM_PC)
          (STA ZP_VM_PC)
          (BCC VM_INTERPRETER)
          (BCS VM_INTERPRETER__NEXT_PAGE)

   (label VM_INTERPRETER_INC_PC)            ;; inc by one
          (INC ZP_VM_PC)
          (BNE VM_INTERPRETER)
   (label VM_INTERPRETER__NEXT_PAGE)
          (INC ZP_VM_PC+1)

    ;; ----------------------------------------
   (label VM_INTERPRETER)
          (LDY !$00)                        ;; use 0 offset to ZP_VM_PV
   (label VM_INTERPRETERy)
          (LDA (ZP_VM_PC),y)
          (ASL A)                           ;; *2
          (BCC OPERAND__VM_INTERPRETER)     ;; bit7 was not set => normal command
          ;; short command
          (AND !$F0)
   (label OPERAND__VM_INTERPRETER)
          (STA JMPOP__VM_INTERPRETER+1)     ;; lowbyte of the table
   (label JMPOP__VM_INTERPRETER)
          (JMP (VM_INTERPRETER_OPTABLE))))  ;; jump by table

(define vm-interpreter
  (append VM_INTERPRETER_VARIABLES
          VM_INTERPRETER_INIT
          VM_PUSH_CONST_NUM_SHORT
          VM_PUSH_CONST_INT
          VM_INTERPRETER
          VM_BRK
          VM_INT_PLUS
          VM_INT_MINUS
          VM_PUSH_PARAM_OR_LOCAL
          (list (org-align #x100)) ;; align to next page
          VM_INTERPRETER_OPTABLE
          vm-lists))
