#lang racket/base

#|

implementation of a byte code interpreter completely in 6510 assembler
this is a proof of concept and used to identify problems in the architecture of the overall implementation.
if something cannot be elegantly implemented using 6510 assembler, some redesign has to take place.

|#


;; TODO: allow byte-code level debugging (with the possibility to step into 6510 code)?
;;       idea: establish break point in interpreter loop, (tell debugger to break at resolved-label)
;;             run own routine at break point,
;;             allow custom commands for this debugger mode
;;               e.g. pretty print next byte code commands (disassembler)
;;                    inspect and manipulate memory, call stack, locals, parameters
;;                    allow setting breakpoints on bytecode (at a certain address)
;;             allow in place execution of bytecode
;;             allow to switch debugger to byte code debugger and vice versa
;;             print bc interpreter status (additionally)
;; TODO: implement ~/repo/+1/6510/mil.readlist.org::*what part of the 6510 vm design should be implement w/ racket to validate design?
;; TODO: numbering of parameter/locals may still be different compared to stack-virtual-machein and svm-compiler/generator!
;; TODO: implement refcount gc for reverse list implementation/interpretation
;; TODO: implement structure access, allocation, deallocation
;; TODO: implement array access, allocation, deallocation (native arrays, regular arrays)
;; TODO: implement constant pool
;; TODO: implement structure creation
;; TODO: implement strings
;; TODO: implement string-operations and output
;; IDEA: implement exact numbers (as list of bcd digits e.g. 3 bcds in 16 bit?)

(require "../6510.rkt")
(require (only-in "../ast/6510-assembler.rkt" assemble assemble-to-code-list translate-code-list-for-basic-loader org-for-code-seq))
(require (only-in racket/list flatten take))
(require (only-in "../6510-utils.rkt" word->hex-string high-byte low-byte ))
(require (only-in "../util.rkt" bytes->int))
(require (only-in "../tools/6510-interpreter.rkt" cpu-state-clock-cycles peek-word-at-address))
(require (only-in "../ast/6510-relocator.rkt" label-string-offsets))

(require (only-in "./vm-memory-manager.rkt"
                  vm-memory-manager
                  vm-call-frame->strings
                  vm-cell-at-nil?
                  vm-page->strings
                  vm-stack->strings
                  vm-regt->string
                  vm-cell-at->string
                  vm-deref-cell-pair-w->string

                  VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE

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
    (define org-code-start (org-for-code-seq wrapped-code))
    (define label->offset (label-string-offsets org-code-start wrapped-code))
    (define interpreter-loop (hash-ref label->offset "VM_INTERPRETER"))
    (define interpreter-code-start (hash-ref label->offset "VM_INTERPRETER_INIT"))
    (define last-routine-of-memory-man (hash-ref label->offset "END__MEMORY_MANAGER"))
    (define end-of-interpreter-code (hash-ref label->offset "END__INTERPRETER"))
    (define end-of-interpreter-data (hash-ref label->offset "END__INTERPRETER_DATA"))
    (define state-before
      (6510-load-multiple (initialize-cpu)
                          (assemble-to-code-list wrapped-code)))
    (if debug
        (begin
          (display (format "code starts      $~a\n" (number->string org-code-start 16)))
          (display (format "interpreter-code $~a\n" (number->string interpreter-code-start 16)))
          (display (format "interpreter-loop $~a\n" (number->string interpreter-loop 16)))
          (display (format "inter code end   $~a\n" (number->string end-of-interpreter-code 16)))
          (display (format "interpreter end  $~a\n" (number->string end-of-interpreter-data 16)))
          (display (format "last mem man     $~a\n" (number->string last-routine-of-memory-man 16)))
          (run-debugger-on state-before))
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
          (STA ZP_VM_FUNC_PTR)
          (STA ZP_VM_FUNC_PTR+1)                ;; mark func-ptr $0000 as initial
          (LDA !$80)                            ;; bc start at $8000
          (STA ZP_VM_PC+1)          
          (RTS)))

(define BC_NIL_P_RET_PARAM_OR_LOCAL
  (list
   (label BC_NIL_P_RET_PARAM_OR_LOCAL)
          (LDA ZP_RT)
          (CMP !<TAGGED_NIL)                            ;; lowbyte = tagged_nil lowbyte
          (BEQ RETURN__BC_NIL_P_RET_PARAM_OR_LOCAL)     ;; is nil => return param or local
          (JMP VM_INTERPRETER_INC_PC)                   ;; next instruction

   (label RETURN__BC_NIL_P_RET_PARAM_OR_LOCAL)
          (LDY !$00)                                    ;; get byte code
          (LDA (ZP_VM_PC),y)
          (AND !$07)                                    ;; mask out lower 3 bits
          (LSR)                                         ;; lowest bit set = param, 0 = local
          (BCS RETURN_PARAM__BC_NIL_P_RET_PARAM_OR_LOCAL)

          ;; localxxx into rt and retrun
          (ASL A)                                       ;; * 2 to get index into locals
          (TAY)
          (LDA (ZP_LOCALS_PTR),y)                       ;; load low byte from local
          (STA ZP_RT)                                   ;; -> RT
          (INY)
          (LDA (ZP_LOCALS_PTR),y)                       ;; load high byte from local
          (STA ZP_RT+1)                                 ;; -> RT
          (JSR VM_POP_CALL_FRAME)                       ;; now pop the call frame
          (JMP VM_INTERPRETER)                          ;; and continue 

   (label RETURN_PARAM__BC_NIL_P_RET_PARAM_OR_LOCAL)
          (ASL A)                                       ;; * 2 to get index into params
          (TAY)                                                                       
          (LDA (ZP_PARAMS_PTR),y)                       ;; load high byte from param (organized high/low, since part of the calling functions' stack)
          (STA ZP_RT+1)                                 ;; -> RT                        
          (INY)                                                                       
          (LDA (ZP_PARAMS_PTR),y)                       ;; load low byte from param   
          (STA ZP_RT)                                   ;; -> RT                        
          (JSR VM_POP_CALL_FRAME)                       ;; now pop the call frame      
          (JMP VM_INTERPRETER)                          ;; and continue
          ))

(define NIL?_RET_LOCAL_0 #x98)
(define NIL?_RET_LOCAL_1 #x9a)
(define NIL?_RET_LOCAL_2 #x9c)
(define NIL?_RET_LOCAL_3 #x9e)

(define NIL?_RET_PARAM_0 #x99)
(define NIL?_RET_PARAM_1 #x9b)
(define NIL?_RET_PARAM_2 #x9d)
(define NIL?_RET_PARAM_3 #x9f)

(module+ test #| bc-nil-ret |#
  (define bc-nil-ret-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_INT_1)
             (bc PUSH_NIL)
             (bc CALL) (byte 00) (byte $8f)
             (bc BRK)

             (org #x8F00)
      (label TEST_FUN)
             (byte 2)            ;; number of parameters
             (byte 0)            ;; number of locals
             (bc PUSH_PARAM_1)
             (bc NIL?_RET_PARAM_0)     ;; return param0 if nil
             (bc BRK))))

  (check-equal? (vm-stack->strings bc-nil-ret-state)
                (list "stack holds 1 item"
                      "cell-int $0001  (rt)"))
  (check-equal? (vm-call-frame->strings bc-nil-ret-state)
                (list "call-frame:       $cd02"
                      "program-counter:  $8005"
                      "function-ptr:     $0000"
                      "params start@:    $cd02"
                      "locals start@:    $cd0a"
                      "cell-stack start: $cd0a"))

  (define bc-nil-ret-local-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_INT_1)
             (bc PUSH_NIL)
             (bc CALL) (byte 00) (byte $8f)
             (bc BRK)

             (org #x8F00)
      (label TEST_FUN)
             (byte 2)            ;; number of parameters
             (byte 1)            ;; number of locals
             (bc PUSH_PARAM_0)
             (bc POP_TO_LOCAL_0)
             (bc PUSH_PARAM_1)
             (bc NIL?_RET_LOCAL_0)     ;; return param0 if nil
             (bc BRK))))

  (check-equal? (vm-stack->strings bc-nil-ret-local-state)
                (list "stack holds 1 item"
                      "cell-int $0001  (rt)"))
  (check-equal? (vm-call-frame->strings bc-nil-ret-local-state)
                (list "call-frame:       $cd02"
                      "program-counter:  $8005"
                      "function-ptr:     $0000"
                      "params start@:    $cd02"
                      "locals start@:    $cd0a"
                      "cell-stack start: $cd0a")))

(define BC_TAIL_CALL
  (list
   (label BC_TAIL_CALL)
          ;; pop stack values into parameters (number?)
          (LDY !$00)
          (LDA (ZP_VM_FUNC_PTR),y)                      ;; number of parameters
          (BEQ DONE_PARAM_COPY____BC_TAIL_CALL)
          ;; get A elements from the cell-stack into params
          (ASL A)
          (TAY)
   (label PARAM_SET_LOOP__BC_TAIL_CALL)
          (DEY)
          (STY COUNT__BC_TAIL_CALL)
          (LDA ZP_RT)                                   ;; put in params in reverse low/high order!
          (STA (ZP_PARAMS_PTR),y)                       ;; load low byte of param at index
          (DEY)                                         ;;
          (LDA ZP_RT+1)
          (STA (ZP_PARAMS_PTR),y)                       ;; load high byte of param at index -> A
          (JSR VM_CELL_STACK_POP_R)                     ;; fill RT with next tos
          (LDY COUNT__BC_TAIL_CALL)
          (DEY)
          (BNE PARAM_SET_LOOP__BC_TAIL_CALL)

   (label DONE_PARAM_COPY____BC_TAIL_CALL)
          (STY ZP_RT)                                   ;; clear RT
          (DEY)
          (STY ZP_CELL_STACK_TOS)                       ;; set stack tos ptr to $ff (empty)


          ;; copy function ptr to pc
          (LDA ZP_VM_FUNC_PTR)
          (STA ZP_VM_PC)
          (LDA ZP_VM_FUNC_PTR+1)
          (STA ZP_VM_PC+1)

          ;; adjust pc to start executing function ptr +2
          (LDA !$02)
          (JMP VM_INTERPRETER_INC_PC_A_TIMES)

  (label  COUNT__BC_TAIL_CALL)
          (byte 2)))

(module+ test #| bc-tail-call |#
  (define bc-tail-call-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_NIL)
             (bc PUSH_INT_0)
             (bc CONS)
             (bc CALL) (byte 00) (byte $8f)
             (bc BRK)

             (org #x8F00)
      (label TEST_FUN)
             (byte 1)            ;; number of parameters
             (byte 0)            ;; number of locals
             (bc PUSH_PARAM_0)
             (bc NIL?_RET_PARAM_0)     ;; return param0 if nil
             (bc PUSH_NIL)       ;; value to use with tail call             
             (bc TAIL_CALL)
             (bc BRK))))

  (check-equal? (vm-stack->strings bc-tail-call-state)
                (list "stack holds 1 item"
                      "cell-pair-ptr NIL  (rt)"))
  (check-equal? (vm-call-frame->strings bc-tail-call-state)
                (list "call-frame:       $cd02"
                      "program-counter:  $8006"
                      "function-ptr:     $0000"
                      "params start@:    $cd02"
                      "locals start@:    $cd0a"
                      "cell-stack start: $cd0a"))

  ;; convert the list given by cell-pair-ptr (address) as a list of strings
  (define (vm-list->strings state address (string-list '()))   
    (unless (= (bitwise-and #x03 address) #x01)
      (raise-user-error "address is not a cell-pair-ptr"))
    (define cell-cdr (peek-word-at-address state (+ address 2)))
    (unless (= (bitwise-and #x03 cell-cdr) #x01)
      (raise-user-error "cdr cell is not a cell-pair-ptr => this is no list" ))
    (if (vm-cell-at-nil? state address)
        string-list
        (vm-list->strings state
                         cell-cdr
                         (cons (vm-cell-at->string state address)
                               string-list))))

  (define bc-tail-call-reverse-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_NIL)
             (bc PUSH_INT_0)
             (bc CONS)
             (bc PUSH_INT_1)
             (bc CONS)
             (bc PUSH_INT_2)
             (bc CONS) ;; list to reverse (param0)
             (bc PUSH_NIL)
             (bc PUSH_NIL)
             (bc CONS) ;; target list (param1)
             (bc CALL) (byte 00) (byte $8f)
             (bc BRK)

             (org #x8F00)
      (label TEST_FUN)
             (byte 2)                   ;; number of parameters
             (byte 0)                   ;; number of locals
             (bc PUSH_PARAM_0) 
             (bc NIL?_RET_PARAM_1)      ;; return param0 if nil
             (bc PUSH_PARAM_0)
             (bc CDR)                   ;; shrinking original list
             (bc PUSH_PARAM_1)
             (bc PUSH_PARAM_0)
             (bc CAR) 
             (bc CONS)                  ;; growing reverse list
             (bc TAIL_CALL)
             (bc BRK))))

  (check-equal? (memory-list bc-tail-call-reverse-state VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE (add1 VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE))
                (list #x00 #x00))
  (check-equal? (vm-page->strings bc-tail-call-reverse-state #xcc)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     7"
                      "next free slot: $55"))
  (check-equal? (cpu-state-clock-cycles bc-tail-call-reverse-state)
                7002) ;; 3575 offset
  (check-equal? (vm-list->strings bc-tail-call-reverse-state (peek-word-at-address bc-tail-call-reverse-state ZP_RT))
                (list "cell-int $0002"
                      "cell-int $0001"
                      "cell-int $0000")
                "list got reversed")
  (check-equal? (vm-stack->strings bc-tail-call-reverse-state)
                (list "stack holds 1 item"
                      "cell-pair-ptr $cc51  (rt)"))
  (check-equal? (vm-call-frame->strings bc-tail-call-reverse-state)
                (list "call-frame:       $cd02"
                      "program-counter:  $800d"
                      "function-ptr:     $0000"
                      "params start@:    $cd02"
                      "locals start@:    $cd0a"
                      "cell-stack start: $cd0a")))

(define BC_CALL
  (list
   (label BC_CALL)
          ;; load the two bytes following into ZP_RA (ptr to function descriptor)
          (LDY !$01)
          (LDA (ZP_VM_PC),y)                    ;; load lowbyte of call target, right behind byte-code
          (STA ZP_RA)                           ;; -> RA
          (INY)
          (LDA (ZP_VM_PC),y)                    ;; load highbyte of call target, behind lowbyte
          (STA ZP_RA+1)                         ;; -> RA
          ;; RA now holds the call target function address

          ;; put return to adress into zp_vm_pc (for save)
          (LDA !$03)                            ;; call is 3 bytes long (bc + address)
          (CLC)
          (ADC ZP_VM_PC)
          (STA ZP_VM_PC)                        ;; write into program counter
          (BCC DONE_INC_PC__BC_CALL)
          (INC ZP_VM_PC+1)                      ;; inc page of program counter
          ;; zp_vm_pc holds follow bc after this call
   (label DONE_INC_PC__BC_CALL)          

   (label VM_CALL_PUSH_RT_FUN_IN_RA)
          ;; push RT into pre call stack
          (JSR VM_CELL_STACK_JUST_PUSH_RT)      ;; push rt onto (current = pre call) the cell-stack
          (LDA !$00)
          (STA ZP_RT)                           ;; mark rt as empty
          ;; RT is pushed and empty

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
          (STA ZP_VM_FUNC_PTR)
          (LDA ZP_RA+1)
          (STA ZP_VM_PC+1)
          (STA ZP_VM_FUNC_PTR+1)

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
                      "function-ptr:     $0000"
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
                      "function-ptr:     $8f00"
                      "params start@:    $cd0c"
                      "locals start@:    $cd16"
                      "cell-stack start: $cd16"))
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
                      "function-ptr:     $8f00"
                      "params start@:    $cd0a"
                      "locals start@:    $cd18"
                      "cell-stack start: $cd18"))
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
                      "function-ptr:     $8f00"
                      "params start@:    $cd0e"
                      "locals start@:    $cd18"
                      "cell-stack start: $cd1c"))
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
                      "function-ptr:     $8f00"
                      "params start@:    $cd0c"
                      "locals start@:    $cd18"
                      "cell-stack start: $cd1c"))
  (check-equal? (vm-stack->strings test-bc-call-wpnl-state)
                (list "stack holds 1 item"
                      "cell-int $0001  (rt)")
                "stack holds just the pushed int, nothing that was there before the call"))

(define BC_RET
  (list
   (label BC_RET)
          ;; restore from previous call frame, keep RT as result
          (JSR VM_POP_CALL_FRAME)             ;; maybe move the respective code into here, (save jsr)
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
                      "function-ptr:     $0000"
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
                      "function-ptr:     $0000"
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
           (LDA (ZP_VM_PC),y)                   ;; load bytecode
           (AND !$07)                           ;; lower three bits are encoded into the short command
           (LSR)                                ;; encoding is ---- xxxp (p=1 parameter, p=0 local)
           (BCS PUSH_PARAM__BC_PUSH_PARAM_OR_LOCAL_SHORT)

           ;; push local
           (ASL A)                              ;; A = 0000 xxx0
           (TAY)                                ;; index -> Y
           (LDA (ZP_LOCALS_PTR),y)              ;; load low byte of local at index
           (TAX)                                ;; low byte -> X
           (INY)                                ;; 
           (LDA (ZP_LOCALS_PTR),y)              ;; load high byte of local at index -> A
           (JSR VM_CELL_STACK_PUSH_R)           ;; push A/X on stack
           (JMP VM_INTERPRETER_INC_PC)          ;; next bc

           ;; push param
   (label  PUSH_PARAM__BC_PUSH_PARAM_OR_LOCAL_SHORT)
           (ASL A)                              ;; A = 0000 xxx0                         
           (TAY)                                ;; index -> Y               
           (INY)                                ;; params are in low/high reversed order (stack)
           (LDA (ZP_PARAMS_PTR),y)              ;; load low byte of param at index       
           (TAX)                                ;; low byte -> X                          
           (DEY)                                ;;                                       
           (LDA (ZP_PARAMS_PTR),y)              ;; load high byte of param at index -> A  
           (JSR VM_CELL_STACK_PUSH_R)           ;; push A/X on stack                     
           (JMP VM_INTERPRETER_INC_PC)          ;; next bc                               
           )))

(define PUSH_LOCAL_0 #x80)
(define PUSH_LOCAL_1 #x82)
(define PUSH_LOCAL_2 #x84)
(define PUSH_LOCAL_3 #x86)

(define PUSH_PARAM_0 #x81)
(define PUSH_PARAM_1 #x83)
(define PUSH_PARAM_2 #x85)
(define PUSH_PARAM_3 #x87)

(define BC_POP_TO_PARAM_OR_LOCAL_SHORT
  (flatten
   (list
    (label BC_POP_TO_PARAM_OR_LOCAL_SHORT)
           (LDY !$00)
           (LDA (ZP_VM_PC),y)                   ;; load bytecode
           (AND !$07)                           ;; lower three bits are encoded into the short command
           (LSR)                                ;; encoding is ---- xxxp (p=1 parameter, p=0 local)
           (BCS POP_TO_PARAM__BC_POP_TO_PARAM_OR_LOCAL_SHORT)

           ;; pop to local
           (ASL A)                              ;; A = 0000 xxx0
           (TAY)                                ;; index -> Y
           (LDA ZP_RT)
           (STA (ZP_LOCALS_PTR),y)              ;; load low byte of local at index           
           (INY)                                ;;
           (LDA ZP_RT+1)
           (STA (ZP_LOCALS_PTR),y)              ;; load high byte of local at index -> A
           (JSR VM_CELL_STACK_POP_R)            ;; fill RT with next tos
           (JMP VM_INTERPRETER_INC_PC)          ;; next bc

           ;; pop to param
   (label  POP_TO_PARAM__BC_POP_TO_PARAM_OR_LOCAL_SHORT)
           (ASL A)                              ;; A = 0000 xxx0
           (TAY)                                ;; index -> Y
           (LDA ZP_RT+1)                        ;; put in params in reverse low/high order!
           (STA (ZP_PARAMS_PTR),y)              ;; load low byte of param at index           
           (INY)                                ;;
           (LDA ZP_RT)
           (STA (ZP_PARAMS_PTR),y)              ;; load high byte of param at index -> A
           (JSR VM_CELL_STACK_POP_R)            ;; fill RT with next tos
           (JMP VM_INTERPRETER_INC_PC)          ;; next bc
           )))

(define POP_TO_LOCAL_0 #x90)
(define POP_TO_LOCAL_1 #x92)
(define POP_TO_LOCAL_2 #x94)
(define POP_TO_LOCAL_3 #x96)

(define POP_TO_PARAM_0 #x91)
(define POP_TO_PARAM_1 #x93)
(define POP_TO_PARAM_2 #x95)
(define POP_TO_PARAM_3 #x97)

(module+ test #| BC_PUSH_PARAM_OR_LOCAL_SHORT |#
  (define test-bc-pop-to-l-state
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
             (bc POP_TO_LOCAL_0) ;;
             (bc BRK))))

  (check-equal? (vm-stack->strings test-bc-pop-to-l-state)
                (list "stack is empty"))
  (check-equal? (vm-cell-at->string test-bc-pop-to-l-state #xcd18)
                "cell-int $0001")
  (check-equal? (vm-call-frame->strings test-bc-pop-to-l-state)
                (list "call-frame:       $cd0e"
                      "program-counter:  $8f04"
                      "function-ptr:     $8f00"
                      "params start@:    $cd0c"
                      "locals start@:    $cd18"
                      "cell-stack start: $cd1c"))

(define test-bc-pop-to-p-state
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
             (bc POP_TO_PARAM_0) ;; overwrites -1
             (bc BRK))))

  (check-equal? (vm-stack->strings test-bc-pop-to-p-state)
                (list "stack is empty"))
  (check-equal? (memory-list test-bc-pop-to-p-state #xcd0c #xcd0d)
                (list #x01 #x03))
  (check-equal? (vm-call-frame->strings test-bc-pop-to-p-state)
                (list "call-frame:       $cd0e"
                      "program-counter:  $8f04"
                      "function-ptr:     $8f00"
                      "params start@:    $cd0c"
                      "locals start@:    $cd18"
                      "cell-stack start: $cd1c"))

  (define test-bc-push-l-state
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
             (bc POP_TO_LOCAL_0) ;;
             (bc PUSH_INT_0)
             (bc PUSH_LOCAL_0)
             (bc BRK))))

  (check-equal? (vm-stack->strings test-bc-push-l-state)
                (list "stack holds 2 items"
                      "cell-int $0001  (rt)"
                      "cell-int $0000")
                "int 1 was pushed from local")
  (check-equal? (vm-cell-at->string test-bc-push-l-state #xcd18)
                "cell-int $0001")
  (check-equal? (vm-call-frame->strings test-bc-push-l-state)
                (list "call-frame:       $cd0e"
                      "program-counter:  $8f06"
                      "function-ptr:     $8f00"
                      "params start@:    $cd0c"
                      "locals start@:    $cd18"
                      "cell-stack start: $cd1c"))

  (define test-bc-push-p-state
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
             (bc PUSH_PARAM_0)
             (bc BRK))))

  (check-equal? (vm-stack->strings test-bc-push-p-state)
                (list "stack holds 2 items"
                      "cell-int $1fff  (rt)"
                      "cell-int $0001")
                "int -1 was pushed from parameter")
  (check-equal? (vm-call-frame->strings test-bc-push-p-state)
                (list "call-frame:       $cd0e"
                      "program-counter:  $8f04"
                      "function-ptr:     $8f00"
                      "params start@:    $cd0c"
                      "locals start@:    $cd18"
                      "cell-stack start: $cd1c"))

  (define test-bc-pop-push-to-p-state
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
             (bc POP_TO_PARAM_0) ;; overwrites -1
             (bc PUSH_PARAM_0)
             (bc BRK))))

  (check-equal? (vm-stack->strings test-bc-pop-push-to-p-state)
                (list "stack holds 1 item"
                      "cell-int $0001  (rt)"))
  (check-equal? (memory-list test-bc-pop-push-to-p-state #xcd0c #xcd0d)
                (list #x01 #x03))
  (check-equal? (vm-call-frame->strings test-bc-pop-push-to-p-state)
                (list "call-frame:       $cd0e"
                      "program-counter:  $8f05"
                      "function-ptr:     $8f00"
                      "params start@:    $cd0c"
                      "locals start@:    $cd18"
                      "cell-stack start: $cd1c")))

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
                1154)
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
                1154)
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
      (bc PUSH_NIL)
      (bc PUSH_INT_0)
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
      (bc PUSH_NIL)
      (bc PUSH_INT_0)
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
      (bc PUSH_NIL)
      (bc PUSH_INT_0)
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
      (bc PUSH_NIL)
      (bc PUSH_INT_0)
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
           (word-ref BC_POP_TO_PARAM_OR_LOCAL_SHORT) ;; 20  <-  90..97 -> 20
           (word-ref VM_INTERPRETER_INC_PC)       ;; 22  <-  11 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 24  <-  12 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 26  <-  13 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 28  <-  14 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 2a  <-  15 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 2c  <-  16 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 2f  <-  17 reserved
           (word-ref BC_NIL_P_RET_PARAM_OR_LOCAL) ;; 30  <-  98..9f reserved
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
           (word-ref BC_TAIL_CALL)                ;; 6a  <-  35 reserved
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
          BC_POP_TO_PARAM_OR_LOCAL_SHORT
          BC_PUSH_CONST_NUM_SHORT
          BC_PUSH_CONST_INT
          BC_PUSH_CONST_BYTE
          BC_PUSH_CONST_NIL
          BC_NIL_P
          BC_NIL_P_RET_PARAM_OR_LOCAL
          BC_CONS
          BC_CAR
          BC_CDR
          BC_CALL
          BC_RET
          BC_BRK
          BC_INT_PLUS
          BC_INT_MINUS
          BC_TAIL_CALL
          VM_INTERPRETER
          (list (label END__INTERPRETER))
          (list (org-align #x100)) ;; align to next page
          VM_INTERPRETER_OPTABLE
          (list (label END__INTERPRETER_DATA))
          vm-lists))
