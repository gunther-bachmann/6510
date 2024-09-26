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
  (require (only-in "../tools/6510-debugger.rkt" run-debugger-on))

  (require (only-in "../cisc-vm/stack-virtual-machine.rkt"
                    CONS
                    CAR
                    CDR
                    GOTO
                    RET
                    BYTE+
                    INT+
                    INT-
                    BRA
                    CALL
                    BRK
                    NIL?
                    TAIL_CALL

                    PUSH_INT
                    PUSH_ARRAY_FIELD
                    PUSH_BYTE
                    PUSH_NIL
                    PUSH_LOCAL
                    PUSH_GLOBAL
                    PUSH_STRUCT_FIELD
                    PUSH_PARAM

                    POP_TO_PARAM
                    POP_TO_LOCAL
                    POP_TO_GLOBAL))

  (define (bc code)
    (ast-bytes-cmd '()  (list code))))

(require (only-in "../tools/6510-interpreter.rkt" 6510-load 6510-load-multiple initialize-cpu run-interpreter run-interpreter-on memory-list cpu-state-accumulator cpu-state-program-counter peek))

(provide vm-interpreter)


;; TODO: implement CALL, PUSH_LOCAL,
;; TODO: supporting functions like create local frame
;; TODO: implement some functions to make status of the interpreter more accessible (e.g. print cell-stack, zp_ptrX, disassemble command, step through byte code ...)


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

(define BC_BRK
  (list
   (label BC_BRK)
          (BRK)))

(module+ test #| vm_interpreter |#
  (define use-case-brk
    (list
     (bc BRK))) ;; byte code for VM_BRK

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
                (list BRK)
                "stopped at byte code brk"))

;; TODO: implement
(define BC_PUSH_LOCAL_OR_BYTE_SHORT
  (flatten
   (list
    (label BC_PUSH_LOCAL_OR_BYTE_SHORT)
           (JMP VM_INTERPRETER_INC_PC))))

(define BC_PUSH_CONST_NUM_SHORT
  (flatten
   (list
    (label BC_PUSH_CONST_NUM_SHORT)
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
     (byte #xb8)     ;; byte code for PUSH_INT_0
     (byte #xb9)     ;; byte code for PUSH_INT_1
     (byte #xba)     ;; byte code for PUSH_INT_2
     (byte #xbb)     ;; byte code for PUSH_INT_m1
     (bc BRK)
     ;; (byte #x02)
     ))   ;; brk

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

(define BC_PUSH_CONST_INT
  (list
   (label BC_PUSH_CONST_INT)
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
     (bc PUSH_INT)
     (byte #x04)     ;; highbyte int
     (byte #xf0)     ;; lowbyte int
     (bc BRK)))   ;; brk

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

(define BC_INT_PLUS
  (list
   (label BC_INT_PLUS)
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
     (byte #xb9)            ;; byte code for PUSH_INT_1
     (byte #xba)            ;; byte code for PUSH_INT_2
     (bc INT+)            ;; byte code for INT_PLUS = 3
     (bc PUSH_INT) (byte #x04 #xf0)  ;; push int #x4f0 (1264)
     (bc PUSH_INT) (byte #x01 #x1f)  ;; push int #x11f (287)
     (bc INT+)            ;; byte code for INT_PLUS (+ #x04f0 #x011f) (1551 = #x060f)
     (byte #xb9)            ;; byte code for PUSH_INT_1
     (byte #xbb)            ;; byte code for PUSH_INT_m1
     (bc INT+)            ;; byte code for INT_PLUS = 0
     (bc BRK)))   ;; brk

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

(define BC_INT_MINUS
  (list
   (label BC_INT_MINUS)
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
     (byte #xb9)            ;; byte code for PUSH_INT_1
     (byte #xba)            ;; byte code for PUSH_INT_2
     (bc INT-)            ;; byte code for INT_MINUS = 2 - 1 = 1
     (bc PUSH_INT) (byte  #x04 #xf0)  ;; push int #x4f0 (1264)
     (bc PUSH_INT) (byte #x01 #x1f)  ;; push int #x11f (287)
     (bc INT-)            ;; byte code for INT_MINUS (- #x011f #x04f0) ( -977 = #x0c2f -> encoded #x302f)
     (byte #xb9)            ;; byte code for PUSH_INT_1
     (byte #xb8)            ;; byte code for PUSH_INT_0
     (bc INT-)            ;; byte code for INT_MINUS => -1
     (bc BRK)))   ;; brk

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

;; TODO: implement
(define BC_PUSH_CONST_BYTE
  (list
   (label BC_PUSH_CONST_BYTE)
          (JMP VM_INTERPRETER_INC_PC)))

;; TODO: test
(define BC_NIL_P
  (list
   (label BC_NIL_P)
          (JSR VM_NIL_P)
          (JMP VM_INTERPRETER_INC_PC)))

;; TODO: test
(define BC_PUSH_CONST_NIL
  (list
   (label BC_PUSH_CONST_NIL)
          (JSR VM_CELL_STACK_PUSH_NIL)
          (JMP VM_INTERPRETER_INC_PC)))

;; TODO: implment
(define BC_PUSH_PARAM_OR_GLOBAL
  (list
   (label BC_PUSH_PARAM_OR_GLOBAL)
          (JMP VM_INTERPRETER_INC_PC)))

;; must be page aligned!
(define VM_INTERPRETER_OPTABLE
  (flatten ;; necessary because word ref creates a list of ast-byte-codes ...
   (list
    (label VM_INTERPRETER_OPTABLE)
           (word-ref BC_PUSH_PARAM_OR_GLOBAL)     ;; 00  <-  80..87 -> 00
           (word-ref VM_INTERPRETER_INC_PC)       ;; 02  <-  01 effectively NOP
           (word-ref BC_BRK)                      ;; 04  <-  02 break into debugger/exit program
           (word-ref VM_INTERPRETER_INC_PC)       ;; 06  <-  03 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 08  <-  04 reserved
           (word-ref BC_PUSH_CONST_BYTE)          ;; 0a  <-  05 reserved
           (word-ref BC_PUSH_CONST_INT)           ;; 0c  <-  06
           (word-ref VM_INTERPRETER_INC_PC)       ;; 0e  <-  07 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 10  <-  08 reserved
           (word-ref BC_PUSH_CONST_NIL)           ;; 12  <-  09 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 14  <-  0a reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 16  <-  0b reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 18  <-  0c reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 1a  <-  0d reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 1c  <-  0e reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 1e  <-  0f reserved
           (word-ref BC_PUSH_LOCAL_OR_BYTE_SHORT) ;; 20  <-  88..8F -> 10
           (word-ref VM_INTERPRETER_INC_PC)       ;; 22  <-  11 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 24  <-  12 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 26  <-  13 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 28  <-  14 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 2a  <-  15 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 2c  <-  16 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 2f  <-  17 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 30  <-  18 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 32  <-  19 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 34  <-  1a reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 36  <-  1b reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 38  <-  1c reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 3a  <-  1d reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 3c  <-  1e reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 3e  <-  1f reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 40  <-  90..97 -> 20 reserved
           (word-ref BC_NIL_P)                    ;; 42  <-  21
           (word-ref VM_INTERPRETER_INC_PC)       ;; 44  <-  22 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 46  <-  23 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 48  <-  24 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 4a  <-  25 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 4c  <-  26 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 4e  <-  27 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 50  <-  28 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 52  <-  29 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 54  <-  2a reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 56  <-  2b reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 58  <-  2c reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 5a  <-  2d reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 5c  <-  2e reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 5e  <-  2f reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 60  <-  98..9f -> 30 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 62  <-  31 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 64  <-  32 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 66  <-  33 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 68  <-  34 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 6a  <-  35 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 6c  <-  36 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 6e  <-  37 reserved
           (word-ref BC_PUSH_CONST_NUM_SHORT)     ;; 70  <-  b8..bf -> 70 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 72  <-  39 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 74  <-  3a reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 76  <-  3b reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 78  <-  3c reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 7a  <-  3d reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 7c  <-  3e reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 7e  <-  3f reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 80  <-  a0..a7 -> 40 reserved
           (word-ref VM_CDR)                      ;; 82  <-  41 reserved
           (word-ref VM_CONS)                     ;; 84  <-  42 reserved
           (word-ref VM_CAR)                      ;; 86  <-  43 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 88  <-  44 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 8a  <-  45 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 8c  <-  46 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 8e  <-  47 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 90  <-  48 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 92  <-  49 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 94  <-  4a reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 96  <-  4b reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 98  <-  4c reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 9a  <-  4d reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 9c  <-  4e reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 9e  <-  4f reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; a0  <-  a8..af -> 50 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; a2  <-  51 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; a4  <-  52 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; a6  <-  53 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; a8  <-  54 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; aa  <-  55 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; ac  <-  56 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; ae  <-  57 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; b0  <-  58 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; b2  <-  59 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; b4  <-  5a reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; b6  <-  5b reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; b8  <-  5c reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; ba  <-  5d reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; bc  <-  5e reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; be  <-  5f reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; c0  <-  b0..b7 -> 60 reserved
           (word-ref BC_INT_MINUS)                ;; c2  <-  61
           (word-ref BC_INT_PLUS)                 ;; c4  <-  62
           (word-ref VM_INTERPRETER_INC_PC)       ;; c6  <-  63 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; c8  <-  64 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; ca  <-  65 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; cc  <-  66 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; ce  <-  67 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; d0  <-  68 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; d2  <-  69 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; d4  <-  6a reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; d6  <-  6b reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; d8  <-  6c reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; da  <-  6d reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; dc  <-  6e reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; de  <-  6f reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; e0  <-  70 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; e2  <-  71 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; e4  <-  72 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; e6  <-  73 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; e8  <-  74 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; ea  <-  75 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; ec  <-  76 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; ee  <-  77 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; f0  <-  78 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; f2  <-  79 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; f4  <-  7a reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; f6  <-  7b reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; f8  <-  7c reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; fa  <-  7d reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; fc  <-  7e reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; fe  <-  7f reserved
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
          BC_PUSH_LOCAL_OR_BYTE_SHORT
          BC_PUSH_CONST_NUM_SHORT
          BC_PUSH_CONST_INT
          BC_PUSH_CONST_BYTE
          BC_PUSH_CONST_NIL
          BC_NIL_P
          VM_INTERPRETER
          BC_BRK
          BC_INT_PLUS
          BC_INT_MINUS
          BC_PUSH_PARAM_OR_GLOBAL
          (list (org-align #x100)) ;; align to next page
          VM_INTERPRETER_OPTABLE
          vm-lists))
