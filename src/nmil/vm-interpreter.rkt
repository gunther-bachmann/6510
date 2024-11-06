#lang racket/base


;; TODO: allow byte-code level debugging (with the possibility to step into 6510 code)?
;;       idea: establish break point in interpreter loop,
;;             print bc interpreter status (additionally)
;; TODO: implement ~/repo/+1/6510/mil.readlist.org::*what part of the 6510 vm design should be implement w/ racket to validate design?

(require "../6510.rkt")
(require (only-in "../ast/6510-assembler.rkt" assemble assemble-to-code-list translate-code-list-for-basic-loader))
(require (only-in racket/list flatten take))

(require (only-in "./vm-memory-manager.rkt"
                  vm-memory-manager
                  vm-stack->strings
                  ast-const-get
                  ZP_VM_PC
                  ZP_LOCALS_PTR
                  ZP_PARAMS_PTR
                  ZP_CELL_STACK_TOS
                  ZP_CELL_STACK_BASE_PTR))
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
                    POP_TO_GLOBAL

                    sPUSH_PARAMc
                    sNIL?-RET-PARAMc))

  (define (bc code)
    (ast-bytes-cmd '()  (list code)))

  (define (wrap-bytecode-for-test bc)
    (append (list (org #x7000)
                  (JSR VM_INITIALIZE_MEMORY_MANAGER)
                  (JSR VM_INTERPRETER_INIT)
                  (JMP VM_INTERPRETER))
            (list (org #x8000))
            bc
            (list (org #xc000))
            vm-interpreter))

  (define (run-bc-wrapped-in-test bc)
    (define wrapped-code (wrap-bytecode-for-test bc))
    (define state-before
      (6510-load-multiple (initialize-cpu)
                          (assemble-to-code-list wrapped-code)))
    ;; (run-debugger-on state-before)
    (parameterize ([current-output-port (open-output-nowhere)])
      (run-interpreter-on state-before)))

  (define (vm-pc state)
    (absolute (peek state (add1 ZP_VM_PC))
              (peek state ZP_VM_PC)))

  (define (vm-next-instruction-bytes state (n 1))
    (memory-list state
                 (vm-pc state)
                 (sub1 (+ n (vm-pc state))))))

(require (only-in "../tools/6510-interpreter.rkt" 6510-load 6510-load-multiple initialize-cpu run-interpreter run-interpreter-on memory-list cpu-state-accumulator cpu-state-program-counter peek))

(provide vm-interpreter)


;; TODO: implement CALL, PUSH_LOCAL, PUSH_PARAM_OR_LOCAL, TAIL_CALL, CONS, CAR, CDR, NIL?-RET-PARAM
;; TODO: supporting functions like create local frame
;; TODO: implement some functions to make status of the interpreter more accessible (e.g. print cell-stack, zp_ptrX, disassemble command, step through byte code ...)


(define VM_INTERPRETER_VARIABLES
  (list
   ;; avail:
   ;; $0b..0e
   ;; $14..15
   ;; $0f..11
   ;; $18..25
   ))

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


;; return id of this function (id = 16 bit ptr to function = zp_vm_pc to set when called)
(define (load-function state byte-code param-no locals-no name)
  ;; allocate code page to hold len(byte-code) + byte (param-no) + byte (locals-no) + byte (name-len) + len(name)
  ;; mem layout:
  ;;        00: # params
  ;;        01: # locals
  ;; id ->  02: first byte code
  ;;            ...
  ;;        01+len(byte-code) : last byte code
  ;;        02+len(bc): name
  ;;        02+len(bc)+len(name): len of name
  ;;        03+len(bc)+len(name): len of this datarecord = 03+len(bc)+len(name)
  ;;        --------
  ;;        00+len(datarecord): next free record
  '())

(module+ test #| vm_interpreter |#
  (define use-case-brk-state-after
    (run-bc-wrapped-in-test
     (list
      (bc BRK))))

  (check-equal? (vm-next-instruction-bytes use-case-brk-state-after)
                (list BRK)
                "stopped at byte code brk"))

(define BC_PUSH_PARAM_OR_LOCAL_SHORT
  (flatten
   (list
    (label BC_PUSH_PARAM_OR_LOCAL_SHORT)
           (LDY !$00)
           (LDA (ZP_VM_PC),y)
           (AND !$07) ;; lower three bits are encoded into the short command
           (LSR)
           (BCS PUSH_PARAM__BC_PUSH_PARAM_OR_LOCAL_SHORT)

           ;; push local
           (ASL A) ;; * 2
           (TAY)
           (LDA (ZP_LOCALS_PTR),y)
           (TAX)
           (INY)
           (LDA (ZP_LOCALS_PTR),y)
           (TAY)
           (TXA)
           (JSR VM_CELL_STACK_PUSH)
           (JMP VM_INTERPRETER_INC_PC)

           ;; push param
   (label  PUSH_PARAM__BC_PUSH_PARAM_OR_LOCAL_SHORT)
           (ASL A) ;; * 2
           (TAY)
           (LDA (ZP_PARAMS_PTR),y)
           (TAX)
           (INY)
           (LDA (ZP_PARAMS_PTR),y)
           (TAY) ;; y = highbyte
           (TXA) ;; a = lowbyte
           (JSR VM_CELL_STACK_PUSH)
           (JMP VM_INTERPRETER_INC_PC)
           )))

(define PUSH_LOCAL_0 #x80)

(module+ test #| BC_PUSH_PARAM_OR_LOCAL_SHORT |#
  (define use-case-push-param-or-local-state-after
    (run-bc-wrapped-in-test
     (list
      ;; todo: setup local0 to be a cell-int $0333
      (bc PUSH_LOCAL_0)
      (bc BRK))))

  (skip (check-equal? (vm-stack->strings use-case-push-param-or-local-state-after)
                      (list "stack holds 1 item"
                            "cell-int $0333"      ;; tos = 0
                            ))))

(define PUSH_INT_0 #xb8)
(define PUSH_INT_1 #xb9)
(define PUSH_INT_2 #xba)
(define PUSH_INT_m1 #xbb)

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
  (define use-case-push-num-s-state-after
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_INT_0)
      (bc PUSH_INT_1)
      (bc PUSH_INT_2)
      (bc PUSH_INT_m1)
      (bc BRK))))

  (check-equal? (vm-stack->strings use-case-push-num-s-state-after)
                (list "stack holds 4 items"
                      "cell-int $1fff"      ;; tos = -1
                      "cell-int $0002"
                      "cell-int $0001"
                      "cell-int $0000")))

(define BC_PUSH_CONST_INT
  (list
   (label BC_PUSH_CONST_INT)
          (LDY !$01)
          (LDA (ZP_VM_PC),y)
          (TAX)
          (INY)
          (LDA (ZP_VM_PC),y)
          (JSR VM_CELL_STACK_PUSH_INT)
          (LDA !$03)
          (JMP VM_INTERPRETER_INC_PC_A_TIMES)))

(module+ test #| VM_PUSH_CONST_INT |#
  (define use-case-push-int-state-after
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_INT) (byte #x04 #xf0)
      (bc BRK))))

  (check-equal? (vm-stack->strings use-case-push-int-state-after)
                (list "stack holds 1 item"
                      "cell-int $04f0")))

(define BC_INT_PLUS
  (list
   (label BC_INT_PLUS)
          (LDY ZP_CELL_STACK_TOS)
          (DEY)
          (LDA (ZP_CELL_STACK_BASE_PTR),y)
          (DEY)
          (DEY)
          (CLC)
          (ADC (ZP_CELL_STACK_BASE_PTR),y)
          (STA (ZP_CELL_STACK_BASE_PTR),y)

          (INY)
          (LDA (ZP_CELL_STACK_BASE_PTR),y)
          (BCC VM_INT_PLUS__NO_INC_HIGH)
          (CLC)
          (ADC !$04)

   (label VM_INT_PLUS__NO_INC_HIGH)
          (INY)
          (INY)
          (ADC (ZP_CELL_STACK_BASE_PTR),y)
          (AND !$7c)

          (DEY)
          (DEY)
          (STA (ZP_CELL_STACK_BASE_PTR),y)
          (STY ZP_CELL_STACK_TOS)
          (JMP VM_INTERPRETER_INC_PC)))

(module+ test #| vm_interpreter |#
  (define use-case-int-plus-state-after
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_INT_1)
      (bc PUSH_INT_2)
      (bc INT+)                      ;; byte code for INT_PLUS = 3
      (bc PUSH_INT) (byte #x04 #xf0) ;; push int #x4f0 (1264)
      (bc PUSH_INT) (byte #x01 #x1f) ;; push int #x11f (287)
      (bc INT+)                      ;; byte code for INT_PLUS (+ #x04f0 #x011f) (1551 = #x060f)
      (bc PUSH_INT_1)
      (bc PUSH_INT_m1)
      (bc INT+)                      ;; byte code for INT_PLUS = 0
      (bc BRK))))

  (check-equal? (vm-stack->strings use-case-int-plus-state-after)
                (list "stack holds 3 items"
                      "cell-int $0000"      ;; tos
                      "cell-int $060f"
                      "cell-int $0003")))

(define BC_INT_MINUS
  (list
   (label BC_INT_MINUS)
          (LDY ZP_CELL_STACK_TOS)
          (DEY)
          (SEC)
          (LDA (ZP_CELL_STACK_BASE_PTR),y)
          (DEY)
          (DEY)
          (SBC (ZP_CELL_STACK_BASE_PTR),y)
          (STA (ZP_CELL_STACK_BASE_PTR),y)

          (LDY ZP_CELL_STACK_TOS)
          (LDA (ZP_CELL_STACK_BASE_PTR),y)
          (BCS VM_INT_MINUS__NO_DEC_HIGH)
          (SEC)
          (SBC !$04)

   (label VM_INT_MINUS__NO_DEC_HIGH)
          (DEY)
          (DEY)
          (SBC (ZP_CELL_STACK_BASE_PTR),y)
          (AND !$7c)
          (STA (ZP_CELL_STACK_BASE_PTR),y)

   (label VM_INT_MINUS__DONE)
          (STY ZP_CELL_STACK_TOS)
          (JMP VM_INTERPRETER_INC_PC)))

(module+ test #| vm_interpreter |#
  (define use-case-int-minus-state-after
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_INT_1)
      (bc PUSH_INT_2)
      (bc INT-)                      ;; byte code for INT_MINUS = 2 - 1 = 1
      (bc PUSH_INT) (byte #x04 #xf0) ;; push int #x4f0 (1264)
      (bc PUSH_INT) (byte #x01 #x1f) ;; push int #x11f (287)
      (bc INT-)                      ;; byte code for INT_MINUS (- #x011f #x04f0) ( -977 = #x1c2f -> encoded #x602f)
      (bc PUSH_INT_1)
      (bc PUSH_INT_0)
      (bc INT-)                      ;; byte code for INT_MINUS => -1
      (bc BRK))))                    ;; brk

  (check-equal? (vm-stack->strings use-case-int-minus-state-after)
                (list "stack holds 3 items"
                      "cell-int $1fff"      ;; tos
                      "cell-int $1c2f"
                      "cell-int $0001")))

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

;; must be page aligned!
(define VM_INTERPRETER_OPTABLE
  (flatten ;; necessary because word ref creates a list of ast-byte-codes ...
   (list
    (label VM_INTERPRETER_OPTABLE)
           (word-ref BC_PUSH_PARAM_OR_LOCAL_SHORT);; 00  <-  80..87 -> 00
           (word-ref VM_INTERPRETER_INC_PC)       ;; 02  <-  01 effectively NOP
           (word-ref BC_BRK)                      ;; 04  <-  02 break into debugger/exit program
           (word-ref VM_INTERPRETER_INC_PC)       ;; 06  <-  03 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 08  <-  04 reserved
           (word-ref BC_PUSH_CONST_BYTE)          ;; 0a  <-  05 reserved
           (word-ref BC_PUSH_CONST_INT)           ;; 0c  <-  06
           (word-ref VM_INTERPRETER_INC_PC)       ;; 0e  <-  07 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 10  <-  88..8F
           (word-ref BC_PUSH_CONST_NIL)           ;; 12  <-  09 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 14  <-  0a reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 16  <-  0b reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 18  <-  0c reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 1a  <-  0d reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 1c  <-  0e reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 1e  <-  0f reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 20  <-  90..97 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 22  <-  11 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 24  <-  12 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 26  <-  13 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 28  <-  14 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 2a  <-  15 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 2c  <-  16 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 2f  <-  17 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 30  <-  98..9f reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 32  <-  19 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 34  <-  1a reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 36  <-  1b reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 38  <-  1c reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 3a  <-  1d reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 3c  <-  1e reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 3e  <-  1f reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 40  <-  a0..a7
           (word-ref BC_NIL_P)                    ;; 42  <-  21
           (word-ref VM_INTERPRETER_INC_PC)       ;; 44  <-  22 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 46  <-  23 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 48  <-  24 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 4a  <-  25 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 4c  <-  26 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 4e  <-  27 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 50  <-  a8..af reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 52  <-  29 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 54  <-  2a reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 56  <-  2b reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 58  <-  2c reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 5a  <-  2d reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 5c  <-  2e reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 5e  <-  2f reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 60  <-  b0..b7 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 62  <-  31 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 64  <-  32 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 66  <-  33 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 68  <-  34 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 6a  <-  35 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 6c  <-  36 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 6e  <-  37 reserved
           (word-ref BC_PUSH_CONST_NUM_SHORT)     ;; 70  <-  b8..bf reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 72  <-  39 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 74  <-  3a reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 76  <-  3b reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 78  <-  3c reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 7a  <-  3d reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 7c  <-  3e reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 7e  <-  3f reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 80  <-  c0..a7 reserved
           (word-ref VM_CDR)                      ;; 82  <-  41 reserved
           (word-ref VM_CONS)                     ;; 84  <-  42 reserved
           (word-ref VM_CAR)                      ;; 86  <-  43 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 88  <-  44 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 8a  <-  45 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 8c  <-  46 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 8e  <-  47 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 90  <-  c8..af reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 92  <-  49 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 94  <-  4a reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 96  <-  4b reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 98  <-  4c reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 9a  <-  4d reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 9c  <-  4e reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 9e  <-  4f reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; a0  <-  d0..d7 eserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; a2  <-  51 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; a4  <-  52 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; a6  <-  53 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; a8  <-  54 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; aa  <-  55 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; ac  <-  56 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; ae  <-  57 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; b0  <-  d8..df reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; b2  <-  59 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; b4  <-  5a reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; b6  <-  5b reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; b8  <-  5c reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; ba  <-  5d reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; bc  <-  5e reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; be  <-  5f reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; c0  <-  e0..e7 reserved
           (word-ref BC_INT_MINUS)                ;; c2  <-  61
           (word-ref BC_INT_PLUS)                 ;; c4  <-  62
           (word-ref VM_INTERPRETER_INC_PC)       ;; c6  <-  63 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; c8  <-  64 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; ca  <-  65 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; cc  <-  66 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; ce  <-  67 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; d0  <-  e8..ef reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; d2  <-  69 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; d4  <-  6a reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; d6  <-  6b reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; d8  <-  6c reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; da  <-  6d reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; dc  <-  6e reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; de  <-  6f reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; e0  <-  f0..f7 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; e2  <-  71 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; e4  <-  72 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; e6  <-  73 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; e8  <-  74 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; ea  <-  75 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; ec  <-  76 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; ee  <-  77 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; f0  <-  f8..ff reserved
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
          BC_PUSH_PARAM_OR_LOCAL_SHORT
          BC_PUSH_CONST_NUM_SHORT
          BC_PUSH_CONST_INT
          BC_PUSH_CONST_BYTE
          BC_PUSH_CONST_NIL
          BC_NIL_P
          VM_INTERPRETER
          BC_BRK
          BC_INT_PLUS
          BC_INT_MINUS
          (list (org-align #x100)) ;; align to next page
          VM_INTERPRETER_OPTABLE
          vm-lists))
