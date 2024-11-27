#lang racket/base


;; TODO: allow byte-code level debugging (with the possibility to step into 6510 code)?
;;       idea: establish break point in interpreter loop,
;;             print bc interpreter status (additionally)
;; TODO: implement ~/repo/+1/6510/mil.readlist.org::*what part of the 6510 vm design should be implement w/ racket to validate design?

(require "../6510.rkt")
(require (only-in "../ast/6510-assembler.rkt" assemble assemble-to-code-list translate-code-list-for-basic-loader))
(require (only-in racket/list flatten take))
(require (only-in "../6510-utils.rkt" word->hex-string high-byte low-byte))
(require (only-in "../tools/6510-interpreter.rkt" cpu-state-clock-cycles))

(require (only-in "./vm-memory-manager.rkt"
                  vm-memory-manager
                  vm-call-frame->strings
                  vm-stack->strings
                  vm-regt->string
                  vm-cell-at->string
                  vm-deref-cell-pair-w->string
                  ast-const-get
                  ZP_RT
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

  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (define state-before
      (6510-load-multiple (initialize-cpu)
                          (assemble-to-code-list wrapped-code)))
    (if debug
        (run-debugger-on state-before)
        (parameterize ([current-output-port (open-output-nowhere)])
          (run-interpreter-on state-before))))

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
          (LDA !$80)         ;; bc start at $8000
          (STA ZP_VM_PC+1)
          (RTS)))


(define BC_CALL
  (list
   (label BC_CALL)
          ;; load the two bytes following into ZP_RA (ptr to function descriptor)
          (LDY !$01)
          (LDA (ZP_VM_PC),y)
          (STA ZP_RA)
          (INY)
          (LDA (ZP_VM_PC),y)
          (STA ZP_RA+1)

          ;; put return to adress into zp_vm_pc (for save)
          (LDA !$03)
          (CLC)
          (ADC ZP_VM_PC)
          (STA ZP_VM_PC)
          (BCC DONE_INC_PC__BC_CALL)
          (INC ZP_VM_PC+1)
   (label DONE_INC_PC__BC_CALL)

   (label VM_CALL_PUSH_RT_FUN_IN_RA)
          (JSR VM_CELL_STACK_JUST_PUSH_RT)
          (LDA !$00)
          (STA ZP_RT) ;; mark rt as empty

   (label VM_CALL_NO_PUSH_FUN_IN_RA)
          ;; ZP_RA holds pointer to function descriptor          
          (LDY !$01)                            ;; index to number of locals 
          (LDA (ZP_RA),y)                       ;; A = #locals
          (ASL A)                               ;; A = 2*#locals
          (CLC)       
          (ADC !40)                             ;; A = 32+8+2*#locals

          ;; A = 32 (possible stack size: 16 cells) + 8 (old pointers etc.) + 2*#locals
          (JSR VM_ALLOC_CALL_FRAME)

          ;; A = number of parameters on stack to be used
          ;; X = number of locals
          ;; Carry: 1 = NO RT PUSH! 0 = push RT before call
          (LDY !$01)                            ;; index to number of locals 
          (LDA (ZP_RA),y)
          (TAX)                                 ;; X = number of locals
          (DEY)
          (LDA (ZP_RA),y)                       ;; A = number of parameters
          (SEC)
          (JSR VM_SAVE_EXEC_STATE_TO_CALL_FRAME)

          ;; load zp_vm_pc with address of function bytecode
          (LDA ZP_RA)
          (STA ZP_VM_PC)
          (LDA ZP_RA+1)
          (STA ZP_VM_PC+1)

          (LDA !$02)                            ;; byte code starts at zp_ra + 2
          (JMP VM_INTERPRETER_INC_PC_A_TIMES)))

(module+ test #| bc_call |#
  (define test-bc-before-call-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_INT_0)
             (bc BRK))
     ))

  (check-equal? (vm-call-frame->strings test-bc-before-call-state)
                (list "call-frame:       $cd02"
                      "program-counter:  $8001"
                      "params start@:    $cd02"
                      "locals start@:    $cd0a"
                      "cell-stack start: $cd0a"))
  (check-equal? (vm-stack->strings test-bc-before-call-state)
                (list "stack holds 1 item"
                      "cell-int $0000  (rt)")
                "stack holds just the pushed int")

  (define test-bc-call-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_INT_0)
             (bc CALL) (byte 00) (byte $8f)

             (org #x8F00)
      (label TEST_FUN)
             (byte 0)            ;; number of parameters
             (byte 0)            ;; number of locals
             (bc PUSH_INT_1)     ;; value to return
             (bc BRK))
     ))

  (check-equal? (vm-call-frame->strings test-bc-call-state)
                (list "call-frame:       $cd0c"
                      "program-counter:  $8f03"
                      "params start@:    $cd0c"
                      "locals start@:    $cd14"
                      "cell-stack start: $cd14"))
  (check-equal? (vm-stack->strings test-bc-call-state)
                (list "stack holds 1 item"
                      "cell-int $0001  (rt)")
                "stack holds just the pushed int, nothing that was there before the call")

  (define test-bc-call-wp-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_INT_0)
             (bc PUSH_INT_m1)
             (bc CALL) (byte 00) (byte $8f)

             (org #x8F00)
      (label TEST_FUN)
             (byte 2)            ;; number of parameters
             (byte 0)            ;; number of locals
             (bc PUSH_INT_1)     ;; value to return
             (bc BRK))
     ))

  (check-equal? (vm-call-frame->strings test-bc-call-wp-state)
                (list "call-frame:       $cd0e"
                      "program-counter:  $8f03"
                      "params start@:    $cd0a"
                      "locals start@:    $cd16"
                      "cell-stack start: $cd16"))
  (check-equal? (memory-list  test-bc-call-wp-state #xcd0a #xcd0d)
                (list #x00 #x03 #xff #x7f)
                "cell int 0, cell int -1 in reverse order (since on stack)")
  (check-equal? (vm-stack->strings test-bc-call-wp-state)
                (list "stack holds 1 item"
                      "cell-int $0001  (rt)")
                "stack holds just the pushed int, nothing that was there before the call")

  (define test-bc-call-wl-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_INT_0)
             (bc PUSH_INT_m1)
             (bc CALL) (byte 00) (byte $8f)

             (org #x8F00)
      (label TEST_FUN)
             (byte 0)            ;; number of parameters
             (byte 2)            ;; number of locals
             (bc PUSH_INT_1)     ;; value to return
             (bc BRK))))

  (check-equal? (vm-call-frame->strings test-bc-call-wl-state)
                (list "call-frame:       $cd0e"
                      "program-counter:  $8f03"
                      "params start@:    $cd0e"
                      "locals start@:    $cd16"
                      "cell-stack start: $cd1a"))
  (check-equal? (vm-stack->strings test-bc-call-wl-state)
                (list "stack holds 1 item"
                      "cell-int $0001  (rt)")
                "stack holds just the pushed int, nothing that was there before the call")

  (define test-bc-call-wpnl-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_INT_0)
             (bc PUSH_INT_m1)
             (bc CALL) (byte 00) (byte $8f)

             (org #x8F00)
      (label TEST_FUN)
             (byte 1)            ;; number of parameters
             (byte 2)            ;; number of locals
             (bc PUSH_INT_1)     ;; value to return
             (bc BRK))))

  (check-equal? (memory-list  test-bc-call-wpnl-state #xcd0c #xcd0d)
                (list #xff #x7f)
                "cell int -1 in reverse order (since on stack)")
  (check-equal? (vm-call-frame->strings test-bc-call-wpnl-state)
                (list "call-frame:       $cd0e"
                      "program-counter:  $8f03"
                      "params start@:    $cd0c"
                      "locals start@:    $cd16"
                      "cell-stack start: $cd1a"))
  (check-equal? (vm-stack->strings test-bc-call-wpnl-state)
                (list "stack holds 1 item"
                      "cell-int $0001  (rt)")
                "stack holds just the pushed int, nothing that was there before the call"))

(define BC_RET
  (list
   (label BC_RET)
          ;; restore from previous call frame, keep RT as result
          (JSR VM_POP_CALL_FRAME)
          (JMP VM_INTERPRETER)))

(module+ test #| bc_call |#
  (define test-bc-ret-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_INT_0)
             (bc CALL) (byte 00) (byte $8f)
             (bc BRK)

             (org #x8F00)
      (label TEST_FUN)
             (byte 0)            ;; number of parameters
             (byte 0)            ;; number of locals
             (bc PUSH_INT_1)     ;; value to return
             (bc RET))))
  
  (check-equal? (vm-call-frame->strings test-bc-ret-state)
                (list "call-frame:       $cd02"
                      "program-counter:  $8004"
                      "params start@:    $cd02"
                      "locals start@:    $cd0a"
                      "cell-stack start: $cd0a"))
  (check-equal? (vm-stack->strings test-bc-ret-state)
                  (list "stack holds 2 items"
                        "cell-int $0001  (rt)"
                        "cell-int $0000")
                  "previous value on the stack is restored to be there + returned value (in rt)")

  (define test-bc-ret-wpnl-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_INT_0)
             (bc PUSH_INT_m1)
             (bc CALL) (byte 00) (byte $8f)
             (bc BRK)

             (org #x8F00)
      (label TEST_FUN)
             (byte 1)            ;; number of parameters
             (byte 2)            ;; number of locals
             (bc PUSH_INT_1)     ;; value to return
             (bc RET))))

  (check-equal? (vm-stack->strings test-bc-ret-wpnl-state)
                (list "stack holds 2 items"
                      "cell-int $0001  (rt)"
                      "cell-int $0000")
                "the value returned and the original stack with its parameters (1) removed")
  (check-equal? (vm-call-frame->strings test-bc-ret-wpnl-state)
                (list "call-frame:       $cd02"
                      "program-counter:  $8005"
                      "params start@:    $cd02"
                      "locals start@:    $cd0a"
                      "cell-stack start: $cd0a")))

(define BC_BRK
  (list
   (label BC_BRK)
          (BRK)))

(module+ test #| bc_brk |#
  (define use-case-brk-state-after
    (run-bc-wrapped-in-test
     (list
      (bc BRK))))

  (check-equal? (vm-next-instruction-bytes use-case-brk-state-after)
                (list BRK)
                "stopped at byte code brk"))

;; return id of this function (id = 16 bit ptr to function = zp_vm_pc to set when called)
;; e.g. (register-function '() (list INT+ BRK) 2 3 "hello")
(define (register-function state byte-code param-no locals-no name)  
  (list
   (ast-bytes-cmd '() (list param-no locals-no))
   (ast-label-def-cmd '() name)
   (ast-bytes-cmd '() byte-code)
   (ast-bytes-cmd '() (bytes->list (string->bytes/locale name)))
   (ast-bytes-cmd '() (list (string-length name))))
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
  )

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
           (JSR VM_CELL_STACK_PUSH_R)
           (JMP VM_INTERPRETER_INC_PC)

           ;; push param
   (label  PUSH_PARAM__BC_PUSH_PARAM_OR_LOCAL_SHORT)
           (ASL A) ;; * 2
           (TAY)
           (LDA (ZP_PARAMS_PTR),y)
           (TAX)
           (INY)
           (LDA (ZP_PARAMS_PTR),y)
           (JSR VM_CELL_STACK_PUSH_R)
           (JMP VM_INTERPRETER_INC_PC)
           )))

(define PUSH_LOCAL_0 #x80)

;; TODO implement test
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
           (LDA (ZP_VM_PC),y)                             ;; load bytecode itself (y must be 0)
           (AND !$07)                                     ;; lower three bits are encoded into the short command
           (ASL A)                                        ;; * 2 (for 2 byte index into jump_refs)!
           (TAY)                                          ;; -> Y
           (LDA VM_PUSH_CONST_NUM_SHORT__JUMP_REFS,y)     ;; get lowbyte of jumpref
           (STA VM_PUSH_CONST_NUM_SHORT__JSR_TARGET+1)    ;; store into lowbyte of jsr command
           (LDA VM_PUSH_CONST_NUM_SHORT__JUMP_REFS+1,y)   ;; load highbyte of jumpref
           (STA VM_PUSH_CONST_NUM_SHORT__JSR_TARGET+2)    ;; store into highbyte of jsr command
    (label VM_PUSH_CONST_NUM_SHORT__JSR_TARGET)
           (JSR VM_CELL_STACK_PUSH_INT_0_R)               ;; execute (modified) jsr 
           (JMP VM_INTERPRETER_INC_PC)                    ;; interpreter loop

    (label VM_PUSH_CONST_NUM_SHORT__JUMP_REFS)
           (word-ref VM_CELL_STACK_PUSH_INT_0_R)
           (word-ref VM_CELL_STACK_PUSH_INT_1_R)
           (word-ref VM_CELL_STACK_PUSH_INT_2_R)
           (word-ref VM_CELL_STACK_PUSH_INT_m1_R)
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
                      "cell-int $1fff  (rt)"
                      "cell-int $0002"
                      "cell-int $0001"
                      "cell-int $0000")))

(define BC_PUSH_CONST_INT
  (list
   (label BC_PUSH_CONST_INT)
          (LDY !$01)                             ;; index 1 past the byte code itself
          (LDA (ZP_VM_PC),y)                     ;; load high byte of int (not encoded)
          (TAX)                                  ;; -> X
          (INY)                                  ;; index 2 past the byte code
          (LDA (ZP_VM_PC),y)                     ;; load low byte of int  -> A
          (JSR VM_CELL_STACK_PUSH_INT_R)         ;; push A/X as int onto stack
          (LDA !$03)                             ;; increment program counter by 3 (bytecode + int)
          (JMP VM_INTERPRETER_INC_PC_A_TIMES)))  ;; interpreter loop

(module+ test #| VM_PUSH_CONST_INT |#
  (define use-case-push-int-state-after
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_INT) (byte #x04 #xf0)
      (bc BRK))))

  (check-equal? (memory-list use-case-push-int-state-after ZP_RT (add1 ZP_RT))
                (list #x13 #xf0))
  (check-equal? (vm-stack->strings use-case-push-int-state-after)
                (list "stack holds 1 item"
                      "cell-int $04f0  (rt)")))

(define BC_INT_PLUS
  (list
   (label BC_INT_PLUS)
          (LDY ZP_CELL_STACK_TOS)               ;; get current index to tagged byte
          (DEY)                                 ;; index to high byte
          (LDA (ZP_CELL_STACK_BASE_PTR),y)      ;; A = untagged lowbyte of int (stored in high byte)
          (CLC)                                 ;; for addition the carry flags needs to be clear
          (ADC ZP_RT+1)                         ;; A = A + stack value (int low byte)
          (STA ZP_RT+1)                         ;; RT untagged lowbyte = result

          (INY)                                 ;; index to tagged low byte (bit0 and 1 set, bit 7 clear)
          (LDA (ZP_CELL_STACK_BASE_PTR),y)      ;; A = tagged high byte of int (stored in low byte)
          (AND !$7c)                            ;; mask out lower two and highest bit
          (BCC VM_INT_PLUS__NO_INC_HIGH)        ;; if previous addition had no overflow, skip inc
          (CLC)                                 ;; clear for addition
          (ADC !$04)                            ;; increment int (adding 4 into the enoded int starting at bit 2)

    (label VM_INT_PLUS__NO_INC_HIGH)
          (ADC ZP_RT)                           ;; A = A + stack value (int high byte)
          (AND !$7f)                            ;; since ZP_RT hat the lower two bits set, just mask out the highest bit
          (STA ZP_RT)                           ;; RT tagged high byte = result

          (DEY)
          (DEY)
          (STY ZP_CELL_STACK_TOS)               ;; pop value from cell-stack (leave result in RT as tos)
          (JMP VM_INTERPRETER_INC_PC)))         ;; interpreter loop

(module+ test #| vm_interpreter |#
  (define (bc-int-plus-state a b)
    (define ra (if (< a 0) (+ #x2000 a) a))
    (define rb (if (< b 0) (+ #x2000 b) b))
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_INT) (ast-bytes-cmd '() (list (high-byte ra) (low-byte ra)))
      (bc PUSH_INT) (ast-bytes-cmd '() (list (high-byte rb) (low-byte rb)))
      (bc INT+)                     
      (bc BRK))))

  (define (bc-int-plus-expectation state c)
    (check-equal? (vm-stack->strings state)
                (list "stack holds 1 item"
                      (format  "cell-int $~a  (rt)" (word->hex-string (if (< c 0) (+ #x2000 c) c))))))
 
  ;; (define _run-bc-int-plus-tests
  ;;   (for/list ([j '(-4096 -4095 -256 -255 -10 -5 -1 0 1 5 10 255 256 4095)])
  ;;     (for/list ([i '(-4096 -4095 -256 -255 -10 -5 -1 0 1 5 10 255 256 4095)])
  ;;       (bc-int-plus-expectation (bc-int-plus-state i j) (+ i j)))))

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

  (check-equal? (cpu-state-clock-cycles use-case-int-plus-state-after)
                1148)
  (check-equal? (vm-stack->strings use-case-int-plus-state-after)
                (list "stack holds 3 items"
                      "cell-int $0000  (rt)"
                      "cell-int $060f"
                      "cell-int $0003"
                      )))

(define BC_INT_MINUS
  (list
   (label BC_INT_MINUS)
          (LDY ZP_CELL_STACK_TOS)               ;; get current index to tagged byte
          (DEY)                                 ;; index to high byte
          (SEC)                                 ;; for subtraction carry needs to be set
          (LDA ZP_RT+1)                         ;; A = untagged lowbyte of int (stored in high byte)
          (SBC (ZP_CELL_STACK_BASE_PTR),y)      ;; A = A - stack value (int low byte)
          (STA ZP_RT+1)                         ;; RT untagged lowbyte = result

          (INY)                                 ;; index to tagged low byte (bit0 and 1 set, bit 7 clear)
          (LDA ZP_RT)                           ;; A = tagged highbyte of int (stored in low byte)
          (BCS VM_INT_MINUS__NO_DEC_HIGH)       ;; if carry is set from subtraction of lower bits, no subtraction carry over necessary
          (SEC)                                 ;; for subtraction carry needs to be set
          (SBC !$04)                            ;; subtract 1 in the masked int highbyte (starting at bit2) => 4

   (label VM_INT_MINUS__NO_DEC_HIGH)
          (SBC (ZP_CELL_STACK_BASE_PTR),y)      ;; A = A - stack value (int high byte)
          (AND !$7c)                            ;; mask out under/overflow (lower two bits and high bit)
          (ORA !$03)                            ;; set lower two bits to tag it as integer value
          (STA ZP_RT)                           ;; RT tagged high byte = result
          
   (label VM_INT_MINUS__DONE)
          (DEY)
          (DEY)
          (STY ZP_CELL_STACK_TOS)               ;; pop value from cell-stack (leave result in rt untouched)
          (JMP VM_INTERPRETER_INC_PC)))         ;; interpreter loop

(module+ test #| vm_interpreter |#
  (define (bc-int-minus-state a b)
    (define ra (if (< a 0) (+ #x2000 a) a))
    (define rb (if (< b 0) (+ #x2000 b) b))
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_INT) (ast-bytes-cmd '() (list (high-byte ra) (low-byte ra)))
      (bc PUSH_INT) (ast-bytes-cmd '() (list (high-byte rb) (low-byte rb)))
      (bc INT-)
      (bc BRK))))

  (define (bc-int-minus-expectation state c)
    (check-equal? (vm-stack->strings state)
                  (list "stack holds 1 item"
                        (format  "cell-int $~a  (rt)" (word->hex-string (if (< c 0) (+ #x2000 c) c))))))

  ;; (define _run-bc-int-minus-tests
  ;;   (for/list ([j '(-4096 -4095 -256 -255 -10 -5 -1 0 1 5 10 255 256 4095)])
  ;;     (for/list ([i '(-4096 -4095 -256 -255 -10 -5 -1 0 1 5 10 255 256 4095)])
  ;;       (bc-int-minus-expectation (bc-int-minus-state i j) (- j i)))))


  (define use-case-int-minus-state-after
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_INT_1)
      (bc PUSH_INT_2)
      (bc INT-)                      ;; byte code for INT_MINUS = 2 - 1 = 1
      (bc PUSH_INT) (byte #x04 #xf0) ;; push int #x4f0 (1264)
      (bc PUSH_INT) (byte #x01 #x1f) ;; push int #x11f (287)
      (bc INT-)                      ;; byte code for INT_MINUS (287 - 1264 = -977 = #x1c2f)
      (bc PUSH_INT_1)
      (bc PUSH_INT_0)
      (bc INT-)                      ;; byte code for INT_MINUS => -1
      (bc BRK))))                    ;; brk


  (check-equal? (cpu-state-clock-cycles use-case-int-minus-state-after)
                1148)
  (check-equal? (vm-stack->strings use-case-int-minus-state-after)
                (list "stack holds 3 items"
                      "cell-int $1fff  (rt)"
                      "cell-int $1c2f"
                      "cell-int $0001")))

;; TODO: implement
(define BC_PUSH_CONST_BYTE
  (list
   (label BC_PUSH_CONST_BYTE)
          (JMP VM_INTERPRETER_INC_PC)))

(define BC_NIL_P
  (list
   (label BC_NIL_P)
          (JSR VM_NIL_P_R)                      ;; if rt is NIL replace with true (int 1) else replace with false (int 0)
          (JMP VM_INTERPRETER_INC_PC)))         ;; interpreter loop

(module+ test #| bc-nil-p |#
  (define bc-nil-p-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_NIL)
      (bc NIL?)
      (bc BRK))))

  (check-equal? (vm-stack->strings bc-nil-p-state)
                (list "stack holds 1 item"
                      "cell-int $0001  (rt)"))

  (define bc-nil-p-2-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_INT_0)
      (bc PUSH_NIL)
      (bc CONS)
      (bc NIL?)
      (bc BRK))))

  (check-equal? (vm-deref-cell-pair-w->string bc-nil-p-2-state #xcc05)
                "(cell-int $0000 . cell-pair-ptr NIL)")
  (check-equal? (vm-stack->strings bc-nil-p-2-state)
                (list "stack holds 1 item"
                      "cell-int $0000  (rt)")))

(define BC_CONS
  (list
   (label BC_CONS)
          (JSR VM_CONS_R)
          (JMP VM_INTERPRETER_INC_PC)))

(module+ test #| bc-cons |#
   (define bc-cons-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_INT_0)
      (bc PUSH_NIL)
      (bc CONS)
      (bc BRK))))

  (check-equal? (vm-stack->strings bc-cons-state)
                (list "stack holds 1 item"
                      "cell-pair-ptr $cc05  (rt)"))
  (check-equal? (vm-deref-cell-pair-w->string bc-cons-state #xcc05)
                "(cell-int $0000 . cell-pair-ptr NIL)"))

(define BC_CAR
  (list
   (label BC_CAR)
          (JSR VM_CAR_R)
          (JMP VM_INTERPRETER_INC_PC)))

(module+ test #| bc-car |#
   (define bc-car-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_INT_0)
      (bc PUSH_NIL)
      (bc CONS)
      (bc CAR)
      (bc BRK))))

  (check-equal? (vm-stack->strings bc-car-state)
                (list "stack holds 1 item"
                      "cell-int $0000  (rt)")))

(define BC_CDR
  (list
   (label BC_CDR)
          (JSR VM_CDR_R)
          (JMP VM_INTERPRETER_INC_PC)))

(module+ test #| bc-cdr |#
   (define bc-cdr-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_INT_0)
      (bc PUSH_NIL)
      (bc CONS)
      (bc CDR)
      (bc BRK))))

  (check-equal? (vm-stack->strings bc-cdr-state)
                (list "stack holds 1 item"
                      "cell-pair-ptr NIL  (rt)")))

(define BC_PUSH_CONST_NIL
  (list
   (label BC_PUSH_CONST_NIL)    
          (JSR VM_CELL_STACK_PUSH_NIL_R)        ;; push NIL on the stack
          (JMP VM_INTERPRETER_INC_PC)))         ;; interpreter loop

(module+ test #| bc-push-const-nil |#
  (define bc-push-const-nil-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_NIL)
      (bc BRK))))

  (check-equal? (vm-stack->strings bc-push-const-nil-state)
                (list "stack holds 1 item"
                      "cell-pair-ptr NIL  (rt)")))

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
           (word-ref BC_RET)                      ;; 66  <-  33 reserved
           (word-ref BC_CALL)                     ;; 68  <-  34 reserved
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
           (word-ref BC_CDR)                      ;; 82  <-  41 reserved
           (word-ref BC_CONS)                     ;; 84  <-  42 reserved
           (word-ref BC_CAR)                      ;; 86  <-  43 reserved
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
          (CLC)                                 ;; clear for add
          (ADC ZP_VM_PC)                        ;; PC = PC + A
          (STA ZP_VM_PC)                        
          (BCC VM_INTERPRETER)                  ;; same page -> no further things to do
          (BCS VM_INTERPRETER__NEXT_PAGE)       ;; always branch to pc now on next page

   (label VM_INTERPRETER_INC_PC)                ;; inc by one (regular case)
          (INC ZP_VM_PC)                    
          (BNE VM_INTERPRETER)                  ;; same page -> no further things to do
   (label VM_INTERPRETER__NEXT_PAGE)
          (INC ZP_VM_PC+1)                      ;; increment high byte of pc (into next page)

    ;; ----------------------------------------
   (label VM_INTERPRETER)
          (LDY !$00)                            ;; use 0 offset to ZP_VM_PV
   (label VM_INTERPRETERy)
          (LDA (ZP_VM_PC),y)                    ;; load byte code
          (ASL A)                               ;; *2 (for jump table)
          (BCC OPERAND__VM_INTERPRETER)         ;; bit7 was not set => normal command
          ;; short command
          (AND !$F0)                            ;; only top 4 bits are used for the opcode dispatch!
   (label OPERAND__VM_INTERPRETER)
          (STA JMPOP__VM_INTERPRETER+1)         ;; lowbyte of the table
   (label JMPOP__VM_INTERPRETER)
          (JMP (VM_INTERPRETER_OPTABLE))))      ;; jump by table

(define vm-interpreter
  (append VM_INTERPRETER_VARIABLES
          VM_INTERPRETER_INIT
          BC_PUSH_PARAM_OR_LOCAL_SHORT
          BC_PUSH_CONST_NUM_SHORT
          BC_PUSH_CONST_INT
          BC_PUSH_CONST_BYTE
          BC_PUSH_CONST_NIL
          BC_NIL_P
          BC_CONS
          BC_CAR
          BC_CDR
          BC_CALL
          BC_RET
          VM_INTERPRETER
          BC_BRK
          BC_INT_PLUS
          BC_INT_MINUS
          (list (org-align #x100)) ;; align to next page
          VM_INTERPRETER_OPTABLE
          vm-lists))
