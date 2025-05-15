#lang racket/base

#|

implementation of a byte code interpreter completely in 6510 assembler
this is a proof of concept and used to identify problems in the architecture of the overall implementation.
if something cannot be elegantly implemented using 6510 assembler, some redesign has to take place.

|#


;; TODO: implement ~/repo/+1/6510/mil.readlist.org::*what part of the 6510 vm design should be implement w/ racket to validate design?
;; TODO: implement structure access, allocation, deallocation
;; TODO: implement array access, allocation, deallocation (native arrays, regular arrays)
;; TODO: implement constant pool
;; TODO: implement structure creation
;; TODO: implement strings
;; TODO: implement string-operations and output

;; PLANNED: harmonize virtual byte code machine with this implementation?

;; IDEA: implement exact numbers (as list of bcd digits e.g. 3 bcds in 16 bit?)
;; IDEA allow to switch debugger to byte code debugger and vice versa

#|
  Byte code command list and description
  opcode                 len       options                   description
  -----------------------------------------------------------------------------------
  ALLOC_A              1  14                             allocate cell-ptr to cell-array onto stack
  BRK                      1  01                             break (stop)
  CALL                     3  34                             statically call function pointed to be following two bytes
  CAR                      1  43                             replace tos with car
  CDR                      1  41                             replace tos with cdr
  CONS                     1  42                             pop car and cdr and push cons of car cdr
  INT_MINUS                1  61                             pop two integers and push the subtraction of them
  INT_PLUS                 1  62                             pop two integers and push the sum of them
  NIL?                     1  21                             replace tos with 0 (false) or 1 (true) if tos was nil
  NIL?_RET_LOCAL_0_POP_n   1  98+  n=1..4                     if tos is nil, pop n from eval-stack and return local0 as result (on tos)
  POP_TO_AF       1  16                             pop the cell at tos-2 into array (tos-1) at index (tos)
  POP_TO_LOCAL_n           1  90+  n=0..3                     pop tos into local#n
  PUSH_AF         1  15                             push field of the array (tos-1) at index (tos) onto eval-stack
  PUSH_B byte              2  17   byte=0..255 -128..+127     push a byte constant onto the eval-stack
  PUSH_I int               3  06   int=0..8191, -4096..4095   push integer constant onto eval-stack
  PUSH_Ii                  1  b8+  i=0,1,2,-1(m1)            push constant 0,1,2,-1 onto eval-stack
  PUSH_Ln                  1  80+  n=0..3                     push local#n onto eval-stack
  PUSH_NIL                 1  09                             push nil onto eval-stack
  RET                      1  33                             return from function
  TAIL_CALL                1  35                             tail call same function
  WRITE_FROM_LOCAL_n       1  81+  n=0..3                     write local#n into tos (overwriting old tos)
  WRITE_TO_LOCAL_n         1  91+  n=0..3                     write tos into local#n (without popping)


  (not implemented yet)
  PUSH_B byte           2  05   byte=0..255, -128..127      push byte constant onto eval-stack
  POP_n                    1       n=1..4                     pop top n values
  SET_CAR                  1                                 set car element to tos (of car-cdr-pair-ptr behind tos) and pop 2 values
  SET_CDR                  1                                 set cdr element to tos (of car-cdr-pair-ptr behind tos) and pop 2 values
|#

(require "../6510.rkt")
(require (only-in racket/list flatten))
(require (only-in "../ast/6510-assembler.rkt" assemble assemble-to-code-list translate-code-list-for-basic-loader org-for-code-seq))
(require (only-in "../6510-utils.rkt" word->hex-string high-byte low-byte ))
(require (only-in "../util.rkt" bytes->int format-hex-byte format-hex-word))
(require (only-in "../tools/6510-interpreter.rkt" cpu-state-clock-cycles peek-word-at-address))
(require (only-in "../ast/6510-relocator.rkt" label-string-offsets command-len))

(require (only-in racket/list flatten take empty? range))

(require (only-in "./vm-memory-manager.rkt"
                  WRITE_CELLPAIR_RT_CELL0_TO_RT
                  vm-memory-manager
                  vm-cell-at-nil?
                  vm-page->strings
                  vm-stack->strings
                  vm-regt->string
                  vm-cell-at->string
                  vm-cell->string
                  vm-deref-cell-pair-w->string
                  GLOBAL_CELLPAIR_FREE_LIST
                  ALLOC_CELLARR_TO_RA
                  POP_CELL_EVLSTK_TO_RT

                  ast-const-get
                  ZP_RT
                  ZP_VM_PC
                  ZP_LOCALS_LB_PTR
                  ZP_LOCALS_HB_PTR
                  ZP_VM_FUNC_PTR
                  ZP_CALL_FRAME
                  ZP_CELL_STACK_TOS
                  ZP_CELL_STACK_LB_PTR
                  ZP_CELL_STACK_HB_PTR))
(require (only-in "./vm-lists.rkt" vm-lists))
(require (only-in "./vm-call-frame.rkt"
                  vm-call-frame->strings
                  VM_POP_CALL_FRAME_N))

(module+ test
  (require "../6510-test-utils.rkt")
  (require (only-in "./vm-interpreter-test-utils.rkt" run-bc-wrapped-in-test- vm-next-instruction-bytes))

  (require (only-in "../cisc-vm/stack-virtual-machine.rkt"
                    BRK))

  (define (wrap-bytecode-for-test bc-to-wrap)
    (append (list (org #x7000)
                  (JSR VM_INITIALIZE_MEMORY_MANAGER)
                  (JSR VM_INITIALIZE_CALL_FRAME)
                  (JSR VM_INTERPRETER_INIT)
                  (JMP VM_INTERPRETER))
            (list (org #x8000))
            bc-to-wrap
            (list (bc BRK))
            (list (org #xa000))
            vm-interpreter))


  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (run-bc-wrapped-in-test- bc wrapped-code debug)))

(module+ test #| after mem init |#
  (define PAGE_CALL_FRAME #x9a)
  (define PAGE_LOCALS_LB #x98)
  (define PAGE_LOCALS_LB_W #x9800)
  (define PAGE_LOCALS_HB #x99)
  (define PAGE_LOCALS_HB_W #x9900)
  (define PAGE_AVAIL_0 #x97)
  (define PAGE_AVAIL_0_W #x9700)
  (define PAGE_AVAIL_1 #x96)
  (define PAGE_AVAIL_1_W #x9600))

(require (only-in "../tools/6510-interpreter.rkt" 6510-load 6510-load-multiple initialize-cpu run-interpreter run-interpreter-on memory-list cpu-state-accumulator cpu-state-program-counter peek))

(provide vm-interpreter
         bc
         TAIL_CALL
         CAR
         CDR
         GOTO
         RET
         CONS
         NIL_P
         CALL
         ISUB
         PUSH_I
         PUSH_B
         PUSH_NIL
         ALLOC_A
         F_P_RET_F
         GET_ARRAY_FIELD_0
         GET_ARRAY_FIELD_1
         GET_ARRAY_FIELD_2
         GET_ARRAY_FIELD_3
         SET_ARRAY_FIELD_0
         SET_ARRAY_FIELD_1
         SET_ARRAY_FIELD_2
         SET_ARRAY_FIELD_3
         GC_FL
         CELL_EQ
         CAAR
         CADR
         CDAR
         CDDR
         COONS
         DUP
         POP
         BNOP
         I0_P
         EXT
         MAX_INT
         INC_INT
         T_P_BRA
         F_P_BRA
         I_GT_P
         PUSH_I0
         PUSH_I1
         PUSH_I2
         PUSH_IM1
         CONS_PAIR_P
         T_P_RET
         F_P_RET
         NIL?_RET_LOCAL_0_POP_1
         NIL?_RET_LOCAL_0_POP_2
         NIL?_RET_LOCAL_0_POP_3
         NIL?_RET_LOCAL_0_POP_4
         INT_P
         SWAP
         POP_TO_LOCAL_0
         POP_TO_LOCAL_1
         POP_TO_LOCAL_2
         POP_TO_LOCAL_3
         WRITE_TO_LOCAL_0
         WRITE_TO_LOCAL_1
         WRITE_TO_LOCAL_2
         WRITE_TO_LOCAL_3
         PUSH_L0
         PUSH_L1
         PUSH_L2
         PUSH_L3
         PUSH_L0_CAR
         PUSH_L1_CAR
         PUSH_L2_CAR
         PUSH_L3_CAR
         PUSH_L0_CDR
         PUSH_L1_CDR
         PUSH_L2_CDR
         PUSH_L3_CDR
         WRITE_FROM_LOCAL_0
         WRITE_FROM_LOCAL_1
         WRITE_FROM_LOCAL_2
         WRITE_FROM_LOCAL_3
         NATIVE)

(define (bc code)
    (ast-bytes-cmd '()  (list code)))

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
          (LDA !$80)                            ;; bc start at $8000
          (STA ZP_VM_PC+1)          
          (STA ZP_VM_FUNC_PTR+1)                ;; mark func-ptr $8000 
          (RTS)))

(define BC_NIL_P_RET_LOCAL_N_POP
  (list
   (label BC_NIL_P_RET_LOCAL_N_POP)
          (LDX ZP_RT)
          (CPX !<TAGGED_NIL)                            ;; lowbyte = tagged_nil lowbyte
          (BEQ RETURN__BC_NIL_P_RET_LOCAL_N_POP)        ;; is nil => return param or local
          (JMP VM_INTERPRETER_INC_PC)                   ;; next instruction

   (label RETURN__BC_NIL_P_RET_LOCAL_N_POP)
          (LSR)                                         ;; lowest bit decides 0 = LOCAL_0, 1 = some other short command
          (TAX)
          (BCS SHORTCMD__BC_NIL_P_RET_LOCAL_N_POP)

   (label LOCAL_0_POP__BC_NIL_P_RET_LOCAL_N_POP)     
          ;; local 0 is written into tos (which is one pop already)          
          ;; now pop the rest (0..3 times additionally)
   (label NOW_POP__BC_NIL_P_RET_LOCAL_N_POP)
          (TXA)
          (BEQ DONE__BC_NIL_P_RET_LOCAL_N_POP)
   (label LOOP_POP__BC_NIL_P_RET_LOCAL_N_POP)
          (DEC ZP_CELL_STACK_TOS)
          (LDY ZP_CELL_STACK_TOS)
          (LDA (ZP_CELL_STACK_LB_PTR),y)
          (STA ZP_RA)
          (LDA (ZP_CELL_STACK_HB_PTR),y)
          (STA ZP_RA+1)
          (TXA)
          (PHA)
          (JSR DEC_REFCNT_RA)
          (PLA)
          (TAX)
          (LDY ZP_CELL_STACK_TOS)
          (CPY !$01)
          (BEQ STACK_DEPLETED__BC_NIL_P_RET_LOCAL_N_POP)
          (DEX)
          (BNE LOOP_POP__BC_NIL_P_RET_LOCAL_N_POP)

          (STY ZP_CELL_STACK_TOS)                       ;; store new tos marker

   (label DONE__BC_NIL_P_RET_LOCAL_N_POP)
          (LDY !$00)
          (LDA (ZP_LOCALS_LB_PTR),y)                    ;; load low byte from local
          (STA ZP_RT)                                   ;; -> RT
          (LDA (ZP_LOCALS_HB_PTR),y)                    ;; load high byte from local
          (STA ZP_RT+1)                                 ;; -> RT

          (LDA !$00)
          (STA (ZP_LOCALS_LB_PTR),y)                    ;; clear low byte from local
          (STA (ZP_LOCALS_HB_PTR),y)                    ;; clear high byte from local
          (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)
          (JSR VM_POP_CALL_FRAME_N)                     ;; now pop the call frame

          (JMP VM_INTERPRETER)                          ;; and continue 

   (label STACK_DEPLETED__BC_NIL_P_RET_LOCAL_N_POP)
          ;; (LDY !$01)                                 ;; Y already is 01 when entering here
          (LDA (ZP_CELL_STACK_LB_PTR),y)               ;; get previous lb page
          (BEQ ERROR_EMPTY_STACK__BC_NIL_P_RET_LOCAL_N_POP) ;; = 0 => stack ran empty

          (STA ZP_CELL_STACK_LB_PTR+1)                 ;; store previous lb page to lb ptr
          (LDA (ZP_CELL_STACK_HB_PTR),y)               ;; get previous hb page  
          (STA ZP_CELL_STACK_HB_PTR+1)                 ;; store previous hb page into hb ptr
          (LDY !$ff)                                   ;; assume $ff as new cell_stack_tos
          (BNE LOOP_POP__BC_NIL_P_RET_LOCAL_N_POP)     ;; always jump


   (label SHORTCMD__BC_NIL_P_RET_LOCAL_N_POP)
          ;; open for other shortcut command
   (label ERROR_EMPTY_STACK__BC_NIL_P_RET_LOCAL_N_POP)
          (BRK)))

(define NIL?_RET_LOCAL_0_POP_1 #x98)
(define NIL?_RET_LOCAL_0_POP_2 #x9a)
(define NIL?_RET_LOCAL_0_POP_3 #x9c)
(define NIL?_RET_LOCAL_0_POP_4 #x9e)
;; (define ZERO?_RET_LOCAL0_POP_1 #x99)
;; (define ZERO?_RET_LOCAL0_POP_2 #x9b)
;; (define ZERO?_RET_LOCAL0_POP_3 #x9d)
;; (define ZERO?_RET_LOCAL0_POP_4 #x9f)

(module+ test #| bc-nil-ret |#
  (define bc-nil-ret-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_NIL)
             (bc PUSH_I1)
             (bc CALL) (byte 00) (byte $8f)
             (bc BRK)

             (org #x8F00)
      (label TEST_FUN)
             (byte 1)                     ;; number of locals
             (bc POP_TO_LOCAL_0)          ;; pop tos into local 0 (now int 1)
             (bc NIL?_RET_LOCAL_0_POP_1)  ;; return local 0  if tos = nil (which it is)
             (bc BRK))
     ))

 (check-equal? (vm-stack->strings bc-nil-ret-state)
                  (list "stack holds 1 item"
                        "int $0001  (rt)"))
 (check-equal? (vm-call-frame->strings bc-nil-ret-state)
               (list (format "call-frame-ptr:   $~a03, topmark: 03" (format-hex-byte PAGE_CALL_FRAME))
                     "program-counter:  $8005"
                     "function-ptr:     $8000"
                     (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 03"
                             (format-hex-byte PAGE_LOCALS_LB)
                             (format-hex-byte PAGE_LOCALS_HB))))

  (define bc-nil-ret-local-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_I1)
             (bc PUSH_NIL)
             (bc CALL) (byte 00) (byte $8f)
             (bc BRK)

             (org #x8F00)
      (label TEST_FUN)    
             (byte 2)            ;; number of locals
             (bc POP_TO_LOCAL_1)
             (bc POP_TO_LOCAL_0)
             (bc PUSH_L1)
             (bc NIL?_RET_LOCAL_0_POP_1)     ;; return local 0 (int 1) if nil
             (bc BRK))
     ))

  (check-equal? (vm-stack->strings bc-nil-ret-local-state)
                   (list "stack holds 1 item"
                         "int $0001  (rt)"))
  (check-equal? (vm-call-frame->strings bc-nil-ret-local-state)
                (list (format "call-frame-ptr:   $~a03, topmark: 03" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $8005"
                         "function-ptr:     $8000"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 03"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB)))))

(define TAIL_CALL           #x35) ;; stack [new-paramN .. new-param0, ..., original-paramN ... original-param0] -> [new-paramN .. new-param0]
(define BC_TAIL_CALL
  (list
   (label BC_TAIL_CALL)
          (LDA ZP_VM_FUNC_PTR)
          (STA ZP_VM_PC)
          (LDA ZP_VM_FUNC_PTR+1)
          (STA ZP_VM_PC+1)

          ;; adjust pc to start executing function ptr +1
          (JMP VM_INTERPRETER_INC_PC)))

(module+ test #| bc-tail-call |#
  (define bc-tail-call-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_NIL)
             (bc PUSH_I0)
             (bc CONS)
             (bc CALL) (byte 00) (byte $8f)
             (bc BRK)

             (org #x8F00)
      (label TEST_FUN)
             (byte 1)            ;; number of locals
             (bc POP_TO_LOCAL_0)
             (bc PUSH_L0)
             (bc NIL?_RET_LOCAL_0_POP_1)    ;; return param0 if nil
             (bc POP_TO_LOCAL_0)
             (bc PUSH_NIL)       ;; value to use with tail call
             (bc TAIL_CALL)
             (bc BRK))))

   (check-equal? (vm-stack->strings bc-tail-call-state)
                   (list "stack holds 1 item"
                         "pair-ptr NIL  (rt)"))
   (check-equal? (vm-call-frame->strings bc-tail-call-state)
                 (list (format "call-frame-ptr:   $~a03, topmark: 03" (format-hex-byte PAGE_CALL_FRAME))
                          "program-counter:  $8006"
                          "function-ptr:     $8000"
                          (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 03"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))))

  ;; convert the list given by cell-pair-ptr (addresss) as a list of strings
  (define (vm-list->strings state address (string-list '()))
    (cond [(= address #x0001) ;; this is the nil ptr           
           (reverse string-list)]
          [else
           (unless (= (bitwise-and #x03 address) #x01)
             (raise-user-error (format "address is not a cell-pair-ptr ~a" (format-hex-word address))))
           (define cell-cdr (peek-word-at-address state (+ address 2)))
           (unless (= (bitwise-and #x03 cell-cdr) #x01)
             (raise-user-error (format "cdr cell is not a cell-pair-ptr => this is no list ~a" (format-hex-word cell-cdr)) ))
           (if (vm-cell-at-nil? state address)
               (reverse string-list)
               (vm-list->strings state
                                cell-cdr
                                (cons (vm-cell-at->string state address)
                                      string-list)))]))

  (define bc-tail-call-reverse-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_NIL)
             (bc PUSH_I0)
             (bc CONS)                  ;; (add ref to this cell) does allocate a cell
             (bc PUSH_I1)
             (bc CONS)                  ;; (add ref to this cell) does allocate a cell (removes a cell-ref from stack and adds a ref in the pair cell)
             (bc PUSH_I2)
             (bc CONS)                  ;; (add ref to this cell) does allocate a cell (removes a cell-ref from stack and adds a ref in the pair cell)
             (bc PUSH_NIL)
             (bc BNOP)
             (bc CALL) (byte 00) (byte $8f)
             (bc BRK)                   ;; << to make debugger stop/exit

             (org #x8F00)
      (label TEST_FUN)
             (byte 2)                   ;; number of locals
             (bc POP_TO_LOCAL_0)        ;; b-list (#refs stay)
             (bc WRITE_TO_LOCAL_1)      ;; a-list (#refs increase)
             (bc NIL?_RET_LOCAL_0_POP_1);; return b-list if a-list is nil (if popping, #refs decrease)
             (bc CDR)                   ;; shrinking original list (ref to cdr cell increases, ref of original cell decreases, order!)
             (bc PUSH_L0)          ;; (ref to local0 cell increases)
             (bc PUSH_L1_CAR)      ;; (ref to local1 cell increases)
             (bc CONS)                  ;; growing reverse list (ref to this cell set to 1), refs to cells consed, stay the same)
             (bc TAIL_CALL)
             (bc BRK))                  ;; just in case to make debugger stop/exit
     ))

  (check-equal? (memory-list bc-tail-call-reverse-state GLOBAL_CELLPAIR_FREE_LIST (add1 GLOBAL_CELLPAIR_FREE_LIST))
                   (list #x05 PAGE_AVAIL_0))
  (check-equal? (vm-page->strings bc-tail-call-reverse-state PAGE_AVAIL_0)
                   (list "page-type:      cell-pair page"
                         "previous page:  $00"
                         "slots used:     4"
                         "next free slot: $49"))
  (inform-check-equal? (cpu-state-clock-cycles bc-tail-call-reverse-state)
                4996)
  (check-equal? (vm-list->strings bc-tail-call-reverse-state (peek-word-at-address bc-tail-call-reverse-state ZP_RT))
                   (list "int $0000"
                         "int $0001"
                         "int $0002")
                   "list got reversed")
  (check-equal? (vm-stack->strings bc-tail-call-reverse-state)
                   (list "stack holds 1 item"
                         (format "pair-ptr[1] $~a09  (rt)" (format-hex-byte PAGE_AVAIL_0))))
  (check-equal? (vm-call-frame->strings bc-tail-call-reverse-state)
                   (list (format "call-frame-ptr:   $~a03, topmark: 03" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $800c"
                         "function-ptr:     $8000"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 03"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB)))))

(define CALL                #x34) ;; stack [int-cell: function index, cell paramN, ... cell param1, cell param0] -> [cell paramN, ... cell param1, cell param0]
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

   (label VM_CALL_NO_PUSH_FUN_IN_RA)
          ;; ZP_RA holds pointer to function descriptor          
          (LDY !$00)                            ;; index to number of locals (0)
          (LDA (ZP_RA),y)                       ;; A = #locals                    
          (TAX)
          (JSR VM_PUSH_CALL_FRAME_N)
          (LDY !$00)                            ;; index to number of locals (0)
          (LDA (ZP_RA),y)                       ;; A = #locals
          (AND !$0f)                            ;; mask out the number of locals
          (JSR VM_ALLOC_LOCALS)                 ;; even if A=0 will set the top_mark and the locals appropriately

          ;; load zp_vm_pc with address of function bytecode
          (LDA ZP_RA)
          (STA ZP_VM_PC)
          (STA ZP_VM_FUNC_PTR)
          (LDA ZP_RA+1)
          (STA ZP_VM_PC+1)
          (STA ZP_VM_FUNC_PTR+1)

          (JMP VM_INTERPRETER_INC_PC))) ;; function starts at function descriptor + 1

(module+ test #| bc_call |#
  (define test-bc-before-call-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_I0)
             (bc BRK))
     ))

  (check-equal? (vm-call-frame->strings test-bc-before-call-state)
                (list (format "call-frame-ptr:   $~a03, topmark: 03" (format-hex-byte PAGE_CALL_FRAME))
                      "program-counter:  $8001"
                      "function-ptr:     $8000"
                      (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 03"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))))
   (check-equal? (vm-stack->strings test-bc-before-call-state)
                 (list "stack holds 1 item"
                       "int $0000  (rt)")
                 "stack holds just the pushed int")

  (define test-bc-call-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_I0)
             (bc CALL) (byte 00) (byte $8f)
             (bc BRK)

             (org #x8F00)
      (label TEST_FUN)     
             (byte 0)            ;; number of locals
             (bc PUSH_I1)     ;; value to return
             (bc BRK))
     ))

   (check-equal? (vm-call-frame->strings test-bc-call-state)
                   (list (format "call-frame-ptr:   $~a03, topmark: 07" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $8f02"
                          "function-ptr:     $8f00"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 03"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))
                         (format "fast-frame ($~a03..$~a06)" (format-hex-byte PAGE_CALL_FRAME) (format-hex-byte PAGE_CALL_FRAME))
                         "return-pc:           $8004"
                         "return-function-ptr: $8000"
                         (format "return-locals-ptr:   $~a03, $~a03 (lb,hb)"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))))
   (check-equal? (vm-stack->strings test-bc-call-state)
                    (list "stack holds 2 items"
                          "int $0001  (rt)"
                          "int $0000")
                    "stack holds the pushed int and the parameter passed")

  (define test-bc-call-wp-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_I0)
             (bc PUSH_IM1)
             (bc CALL) (byte 00) (byte $8f)
             (bc BRK) 

             (org #x8F00)
      (label TEST_FUN)      
             (byte 0)            ;; number of locals
             (bc PUSH_I1)     ;; value to return
             (bc BRK))
     ))

  (check-equal? (vm-call-frame->strings test-bc-call-wp-state)
                   (list (format "call-frame-ptr:   $~a03, topmark: 07" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $8f02"
                         "function-ptr:     $8f00"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 03"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))
                         (format "fast-frame ($~a03..$~a06)" (format-hex-byte PAGE_CALL_FRAME) (format-hex-byte PAGE_CALL_FRAME))
                         "return-pc:           $8005"
                         "return-function-ptr: $8000"
                         (format "return-locals-ptr:   $~a03, $~a03 (lb,hb)"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))))
  (check-equal? (vm-stack->strings test-bc-call-wp-state)
                   (list "stack holds 3 items"
                         "int $0001  (rt)"
                         "int $1fff"
                         "int $0000")
                   "stack holds the pushed int, and all parameters")

  (define test-bc-call-wl-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_I0)
             (bc PUSH_IM1)
             (bc CALL) (byte 00) (byte $8f)
             (bc BRK)

             (org #x8F00)
      (label TEST_FUN)    
             (byte 2)            ;; number of locals
             (bc PUSH_I1)     ;; value to return
             (bc BRK))))

  (check-equal? (vm-call-frame->strings test-bc-call-wl-state)
                   (list (format "call-frame-ptr:   $~a03, topmark: 07" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $8f02"
                         "function-ptr:     $8f00"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 05"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))
                         (format "fast-frame ($~a03..$~a06)" (format-hex-byte PAGE_CALL_FRAME) (format-hex-byte PAGE_CALL_FRAME))
                         "return-pc:           $8005"
                         "return-function-ptr: $8000"
                         (format "return-locals-ptr:   $~a03, $~a03 (lb,hb)"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))))
  (check-equal? (vm-stack->strings test-bc-call-wl-state)
                   (list "stack holds 3 items"
                         "int $0001  (rt)"
                         "int $1fff"
                         "int $0000")
                   "stack holds the pushed int, and all parameters"))

(define VM_REFCOUNT_DECR_CURRENT_LOCALS
  (list
   (label VM_REFCOUNT_DECR_CURRENT_LOCALS)
          (LDA ZP_LOCALS_LB_PTR)
          (STA ZP_LOCALS_TOP_MARK) ;; restore top mark
          (LDY !$00)
          (LDA (ZP_VM_FUNC_PTR),y)
          (AND !$0f)
          (TAY) ;; y = number of locals of current tunction
          ;; loop over locals -> rt, decr refcount
          (DEY)
          (BMI DONE__BC_RET)
   (label LOOP__BC_RET)
          (LDA (ZP_LOCALS_LB_PTR),y)
          (BEQ NEXT_ITER__BC_RET)
          (STA ZP_RA)
          (AND !$03)
          (CMP !$03)
          (BEQ S0_NEXT_ITER__BC_RET) ;; definitely no pointer since lower 2 bits are set
          (LDA (ZP_LOCALS_HB_PTR),y)
          (BEQ NEXT_ITER__BC_RET)       ;; definitely no pointer, since page is 00
          (STA ZP_RA+1)
          (STY COUNTER__BC_RET)
          (JSR DEC_REFCNT_RA)
          (LDY COUNTER__BC_RET)
   (label S0_NEXT_ITER__BC_RET)
          (LDA !$00)
   (label NEXT_ITER__BC_RET)
          (STA (ZP_LOCALS_LB_PTR),y)
          (STA (ZP_LOCALS_HB_PTR),y)
          (DEY)
          (BPL LOOP__BC_RET)
   (label DONE__BC_RET)
          (RTS)

   (label COUNTER__BC_RET)
          (byte 0)
   (label ZP_RT_BACKUP)
          (word 0)))

(define RET                 #x33) ;; stack [cell paramN, ... cell param1, cell param0] -> []
(define BC_RET
  (list
   (label BC_RET)
          (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)
          (JSR VM_POP_CALL_FRAME_N)             ;; maybe move the respective code into here, (save jsr)
          (JMP VM_INTERPRETER)))

(module+ test #| bc_ret |#
  (define test-bc-ret-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_I0)
             (bc CALL) (byte 00) (byte $8f)
             (bc BRK)

             (org #x8F00)
      (label TEST_FUN)      
             (byte 0)            ;; number of locals
             (bc PUSH_I1)     ;; value to return
             (bc RET))))
  
  (check-equal? (vm-call-frame->strings test-bc-ret-state)
                   (list (format "call-frame-ptr:   $~a03, topmark: 03" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $8004"
                         "function-ptr:     $8000"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 03"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))))
  (check-equal? (vm-stack->strings test-bc-ret-state)
                   (list "stack holds 2 items"
                         "int $0001  (rt)"
                         "int $0000")
                   "previous value on the stack is there + returned value (in rt)"))

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



(define PUSH_L0_CAR #xa0)
(define PUSH_L1_CAR #xa2)
(define PUSH_L2_CAR #xa4)
(define PUSH_L3_CAR #xa6)

(define PUSH_L0_CDR #xa1)
(define PUSH_L1_CDR #xa3)
(define PUSH_L2_CDR #xa5)
(define PUSH_L3_CDR #xa7)

(define BC_PUSH_LOCAL_CXR
  (flatten
   (list
    (label BC_PUSH_LOCAL_CXR)
           (LSR)                                ;; encoding is ---- xxxp (p=1 CDR, p=0 CAR)
           (BCS CDR__BC_PUSH_LOCAL_SHORT)

    ;; CAR
           (STA ZP_RA)
           (JSR PUSH_RT_TO_EVLSTK_IF_NONEMPTY)
           (LDY ZP_RA) ;; index -> Y
           (LDA (ZP_LOCALS_LB_PTR),y)           ;; load low byte of local at index
           (STA ZP_RT)                                ;; low byte -> X
           (LDA (ZP_LOCALS_HB_PTR),y)           ;; load high byte of local at index -> A
           (STA ZP_RT+1)
           (JSR WRITE_CELLPAIR_RT_CELL0_TO_RT)
           (JSR INC_REFCNT_RT)
           (JMP VM_INTERPRETER_INC_PC)

    (label CDR__BC_PUSH_LOCAL_SHORT)
           (STA ZP_RA)
           (JSR PUSH_RT_TO_EVLSTK_IF_NONEMPTY)
           (LDY ZP_RA) ;; index -> Y
           (LDA (ZP_LOCALS_LB_PTR),y)           ;; load low byte of local at index
           (STA ZP_RT)                                ;; low byte -> X
           (LDA (ZP_LOCALS_HB_PTR),y)           ;; load high byte of local at index -> A
           (STA ZP_RT+1)
           (JSR WRITE_CELLPAIR_RT_CELL1_TO_RT)
           (JSR INC_REFCNT_RT)
           (JMP VM_INTERPRETER_INC_PC)
)))

(define PUSH_L0 #x80)
(define PUSH_L1 #x82)
(define PUSH_L2 #x84)
(define PUSH_L3 #x86)

(define WRITE_FROM_LOCAL_0 #x81)
(define WRITE_FROM_LOCAL_1 #x83)
(define WRITE_FROM_LOCAL_2 #x85)
(define WRITE_FROM_LOCAL_3 #x87)

(define BC_PUSH_LOCAL_SHORT
  (flatten
   (list
    (label BC_PUSH_LOCAL_SHORT)
           (LSR)                                ;; encoding is ---- xxxp (p=1 write local, p=0 push local)
           (BCS WRITE_FROM_LOCAL__BC_PUSH_LOCAL_SHORT)

    ;; push local           
           (PHA)
           (JSR PUSH_RT_TO_EVLSTK_IF_NONEMPTY)
           (PLA)
           (TAY) ;; index -> Y
           (LDA (ZP_LOCALS_LB_PTR),y)           ;; load low byte of local at index
           (STA ZP_RT)                                ;; low byte -> X
           (LDA (ZP_LOCALS_HB_PTR),y)           ;; load high byte of local at index -> A
           (STA ZP_RT+1)
           (JSR INC_REFCNT_RT)
           (JMP VM_INTERPRETER_INC_PC)

    (label WRITE_FROM_LOCAL__BC_PUSH_LOCAL_SHORT)
           (PHA)
           (JSR DEC_REFCNT_RT)
           (PLA)
           (TAY)                                ;; index -> Y
           (LDA (ZP_LOCALS_LB_PTR),y)           ;; load low byte of local at index
           (STA ZP_RT)                          ;; 
           (LDA (ZP_LOCALS_HB_PTR),y)           ;; load high byte of local at index 
           (STA ZP_RT+1)                        ;; 
           (JSR INC_REFCNT_RT)
           (JMP VM_INTERPRETER_INC_PC)          ;; next bc
           )))

(define BC_POP_TO_LOCAL_SHORT
  (flatten
   (list
    (label BC_POP_TO_LOCAL_SHORT)
           (LSR)                                ;; encoding is ---- xxxp (p=1 parameter, p=0 local)
           (BCS WRITE__POP_TO_LOCAL_SHORT)
       
           ;; pop to local           
           (PHA)
           (TAY)                                ;; index -> Y
           ;; decrement old local
           (LDA (ZP_LOCALS_LB_PTR),y)
           (STA ZP_RA)
           (LDA (ZP_LOCALS_HB_PTR),y)
           (STA ZP_RA+1)
           (JSR DEC_REFCNT_RA)

           (PLA)
           (TAY)                                ;; index -> Y
           (LDA ZP_RT)
           (STA (ZP_LOCALS_LB_PTR),y)           ;; store low byte of local at index                      
           (LDA ZP_RT+1)
           (STA (ZP_LOCALS_HB_PTR),y)           ;; store high byte of local at index -> A
           (JSR POP_CELL_EVLSTK_TO_RT)            ;; fill RT with next tos

           ;; no increment, since pop removes it from stack
           (JMP VM_INTERPRETER_INC_PC)          ;; next bc

    ;; write to local
   (label  WRITE__POP_TO_LOCAL_SHORT)
           (PHA)
           (TAY)                                ;; index -> Y

           ;; decrement old local
           (LDA (ZP_LOCALS_LB_PTR),y)
           (STA ZP_RA)
           (LDA (ZP_LOCALS_HB_PTR),y)
           (STA ZP_RA+1)
           (JSR DEC_REFCNT_RA)

           (PLA)
           (TAY)                                ;; index -> Y
           (LDA ZP_RT)
           (STA (ZP_LOCALS_LB_PTR),y)           ;; store low byte of local at index
           (LDA ZP_RT+1)
           (STA (ZP_LOCALS_HB_PTR),y)           ;; store high byte of local at index -> A
           ;; increment, since it is no in locals and on stack
           (JSR INC_REFCNT_RT)
           (JMP VM_INTERPRETER_INC_PC)          ;; next bc
)))

(define POP_TO_LOCAL_0 #x90)
(define POP_TO_LOCAL_1 #x92)
(define POP_TO_LOCAL_2 #x94)
(define POP_TO_LOCAL_3 #x96)

(define WRITE_TO_LOCAL_0 #x91)
(define WRITE_TO_LOCAL_1 #x93)
(define WRITE_TO_LOCAL_2 #x95)
(define WRITE_TO_LOCAL_3 #x97)

(module+ test #| BC_PUSH_LOCAL_SHORT |#
  (define test-bc-pop-to-l-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_I0)
             (bc PUSH_IM1)
             (bc CALL) (byte 00) (byte $8f)

             (org #x8F00)
      (label TEST_FUN)
             (byte 2)            ;; number of locals
             (bc PUSH_I1)     ;; value to return
             (bc POP_TO_LOCAL_0) ;;
             (bc BRK))))

  (check-equal? (vm-stack->strings test-bc-pop-to-l-state)
                (list "stack holds 2 items"
                      "int $1fff  (rt)"
                      "int $0000"))
  (check-equal? (peek test-bc-pop-to-l-state (+ PAGE_LOCALS_LB_W #x03))
                #x03)
  (check-equal? (peek test-bc-pop-to-l-state (+ PAGE_LOCALS_HB_W #x03))
                #x01)
  (check-equal? (vm-call-frame->strings test-bc-pop-to-l-state)
                   (list (format "call-frame-ptr:   $~a03, topmark: 07" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $8f03"
                         "function-ptr:     $8f00"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 05"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))
                         (format "fast-frame ($~a03..$~a06)" (format-hex-byte PAGE_CALL_FRAME)(format-hex-byte PAGE_CALL_FRAME))
                         "return-pc:           $8005"
                         "return-function-ptr: $8000"
                         (format "return-locals-ptr:   $~a03, $~a03 (lb,hb)"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))))

  (define test-bc-pop-to-p-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc PUSH_IM1)
      (bc CALL) (byte 00) (byte $8f)

      (org #x8F00)
      (label TEST_FUN)      
      (byte 2)            ;; number of locals
      (bc POP_TO_LOCAL_0)
      (bc POP_TO_LOCAL_1)
      (bc PUSH_I1)     ;; value to return
      (bc POP_TO_LOCAL_0) ;; overwrites -1
      (bc BRK))
     ))

  (check-equal? (vm-stack->strings test-bc-pop-to-p-state)
                  (list "stack is empty"))
  (check-equal? (peek test-bc-pop-to-p-state (+ PAGE_LOCALS_LB_W #x03))
                #x03)
  (check-equal? (peek test-bc-pop-to-p-state (+ PAGE_LOCALS_HB_W #x03))
                #x01
                "local0 = int 1")
  (check-equal? (peek test-bc-pop-to-p-state (+ PAGE_LOCALS_LB_W #x04))
                #x03)
  (check-equal? (peek test-bc-pop-to-p-state (+ PAGE_LOCALS_HB_W #x04))
                #x00 "local1 = int 0")
  (check-equal? (vm-call-frame->strings test-bc-pop-to-p-state)
                   (list (format "call-frame-ptr:   $~a03, topmark: 07" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $8f05"
                         "function-ptr:     $8f00"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 05"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))
                         (format "fast-frame ($~a03..$~a06)" (format-hex-byte PAGE_CALL_FRAME) (format-hex-byte PAGE_CALL_FRAME))
                         "return-pc:           $8005"
                         "return-function-ptr: $8000"
                         (format "return-locals-ptr:   $~a03, $~a03 (lb,hb)"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))))

  (define test-bc-push-l-state
    (run-bc-wrapped-in-test
     (list
      (bc CALL) (byte 00) (byte $8f)

      (org #x8F00)
      (label TEST_FUN)
      (byte 1)            ;; number of locals
      (bc PUSH_I1)     ;; value to return
      (bc POP_TO_LOCAL_0) ;;
      (bc PUSH_I0)
      (bc PUSH_L0)
      (bc BRK))))

  (check-equal? (vm-stack->strings test-bc-push-l-state)
                  (list "stack holds 2 items"
                        "int $0001  (rt)"
                        "int $0000")
                  "int 1 was pushed from local")
  (check-equal? (vm-call-frame->strings test-bc-push-l-state)
                   (list (format "call-frame-ptr:   $~a03, topmark: 07" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $8f05"
                         "function-ptr:     $8f00"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 04"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))
                         (format "fast-frame ($~a03..$~a06)" (format-hex-byte PAGE_CALL_FRAME) (format-hex-byte PAGE_CALL_FRAME))
                         "return-pc:           $8003"
                         "return-function-ptr: $8000"
                         (format "return-locals-ptr:   $~a03, $~a03 (lb,hb)"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))))

  (define test-bc-push-p-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc PUSH_IM1)
      (bc CALL) (byte 00) (byte $8f)

      (org #x8F00)
      (label TEST_FUN)      
      (byte 2)            ;; number of locals
      (bc POP_TO_LOCAL_0)
      (bc POP_TO_LOCAL_1)
      (bc PUSH_I1)   
      (bc PUSH_L0)
      (bc BRK))))

  (check-equal? (vm-stack->strings test-bc-push-p-state)
                   (list "stack holds 2 items"
                         "int $1fff  (rt)"
                         "int $0001")
                   "int -1 was pushed from local")
  (check-equal? (vm-call-frame->strings test-bc-push-p-state)
                   (list (format "call-frame-ptr:   $~a03, topmark: 07" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $8f05"
                         "function-ptr:     $8f00"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 05"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))
                         (format "fast-frame ($~a03..$~a06)" (format-hex-byte PAGE_CALL_FRAME) (format-hex-byte PAGE_CALL_FRAME))
                         "return-pc:           $8005"
                         "return-function-ptr: $8000"
                         (format "return-locals-ptr:   $~a03, $~a03 (lb,hb)"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))))

  (define test-bc-pop-push-to-p-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc PUSH_IM1)
      (bc CALL) (byte 00) (byte $8f)

      (org #x8F00)
      (label TEST_FUN)      
      (byte 2)            ;; number of locals
      (bc POP_TO_LOCAL_0)
      (bc POP_TO_LOCAL_1)
      (bc PUSH_I1)     ;; value to return
      (bc POP_TO_LOCAL_0) ;; overwrites -1
      (bc PUSH_L0)
      (bc BRK))))

  (check-equal? (vm-stack->strings test-bc-pop-push-to-p-state)
                   (list "stack holds 1 item"
                         "int $0001  (rt)"))
  (check-equal? (vm-call-frame->strings test-bc-pop-push-to-p-state)
                   (list (format "call-frame-ptr:   $~a03, topmark: 07" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $8f06"
                         "function-ptr:     $8f00"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 05"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))
                         (format "fast-frame ($~a03..$~a06)" (format-hex-byte PAGE_CALL_FRAME) (format-hex-byte PAGE_CALL_FRAME))
                         "return-pc:           $8005"
                         "return-function-ptr: $8000"
                         (format "return-locals-ptr:   $~a03, $~a03 (lb,hb)"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB)))))

(define PUSH_I  #x06) ;; op1=low byte op2=high byte, stack [] -> [cell-int]
(define PUSH_I0 #xb8)
(define PUSH_I1 #xb9)
(define PUSH_I2 #xba)
(define PUSH_IM1 #xbb)

(define BC_PUSH_CONST_NUM_SHORT
  (flatten
   (list
    (label BC_PUSH_CONST_NUM_SHORT)
           (ASL A)                                        ;; * 2 (for 2 byte index into jump_refs)!
           (TAY)                                          ;; -> Y
           (LDA VM_PUSH_CONST_NUM_SHORT__JUMP_REFS,y)     ;; get lowbyte of jumpref
           (STA VM_PUSH_CONST_NUM_SHORT__JSR_TARGET+1)    ;; store into lowbyte of jsr command
           (LDA VM_PUSH_CONST_NUM_SHORT__JUMP_REFS+1,y)   ;; load highbyte of jumpref
           (STA VM_PUSH_CONST_NUM_SHORT__JSR_TARGET+2)    ;; store into highbyte of jsr command
    (label VM_PUSH_CONST_NUM_SHORT__JSR_TARGET)
           (JSR PUSH_INT_0_TO_EVLSTK)               ;; execute (modified) jsr 
           (JMP VM_INTERPRETER_INC_PC)                    ;; interpreter loop

    (label VM_PUSH_CONST_NUM_SHORT__JUMP_REFS)
           (word-ref PUSH_INT_0_TO_EVLSTK)
           (word-ref PUSH_INT_1_TO_EVLSTK)
           (word-ref PUSH_INT_2_TO_EVLSTK)
           (word-ref PUSH_INT_m1_TO_EVLSTK)
           ;; (word-ref PUSH_B0)
           ;; (word-ref PUSH_B1)
           ;; (word-ref PUSH_B2)
           ;; (word-ref PUSH_Bm1)
           )))

(module+ test #| push const num |#
  (define use-case-push-num-s-state-after
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc PUSH_I1)
      (bc PUSH_I2)
      (bc PUSH_IM1)
      (bc BRK))))

  (check-equal? (vm-stack->strings use-case-push-num-s-state-after)
                (list "stack holds 4 items"
                      "int $1fff  (rt)"
                      "int $0002"
                      "int $0001"
                      "int $0000")))

(define BC_PUSH_I
  (list
   (label BC_PUSH_I)
          (LDY !$02)                             ;; index 1 past the byte code itself
          (LDA (ZP_VM_PC),y)                     ;; load high byte of int (not encoded)
          (TAX)                                  ;; -> X
          (DEY)                                  ;; index 2 past the byte code
          (LDA (ZP_VM_PC),y)                     ;; load low byte of int  -> A
          (JSR PUSH_INT_TO_EVLSTK)         ;; push A/X as int onto stack
          (LDA !$03)                             ;; increment program counter by 3 (bytecode + int)
          (JMP VM_INTERPRETER_INC_PC_A_TIMES)))  ;; interpreter loop

(module+ test #| VM_PUSH_CONST_INT |#
  (define use-case-push-int-state-after
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I) (byte #xf0 #x04)
      (bc BRK))))

  (check-equal? (vm-stack->strings use-case-push-int-state-after)
                (list "stack holds 1 item"
                      "int $04f0  (rt)")))

(define IADD                #x62) ;; stack [cell-int a, cell-int b] -> [sum]
(define BC_IADD
  (list
   (label BC_IADD)
          (LDY ZP_CELL_STACK_TOS)               ;; get current index to tagged byte          
          (LDA (ZP_CELL_STACK_HB_PTR),y)        ;; A = untagged lowbyte of int (stored in high byte)
          (CLC)                                 ;; for addition the carry flags needs to be clear
          (ADC ZP_RT+1)                         ;; A = A + stack value (int low byte)
          (STA ZP_RT+1)                         ;; RT untagged lowbyte = result
          
          (LDA (ZP_CELL_STACK_LB_PTR),y)       ;; A = tagged high byte of int (stored in low byte)
          (AND !$7c)                            ;; mask out lower two and highest bit
          (BCC VM_INT_PLUS__NO_INC_HIGH)        ;; if previous addition had no overflow, skip inc
          (CLC)                                 ;; clear for addition
          (ADC !$04)                            ;; increment int (adding 4 into the enoded int starting at bit 2)

    (label VM_INT_PLUS__NO_INC_HIGH)
          (ADC ZP_RT)                           ;; A = A + stack value (int high byte)
          (AND !$7f)                            ;; since ZP_RT has the lower two bits set, just mask out the highest bit
          (STA ZP_RT)                           ;; RT tagged high byte = result

          (DEC ZP_CELL_STACK_TOS)               ;; pop value from cell-stack (leave result in RT as tos)
          (JMP VM_INTERPRETER_INC_PC)))         ;; interpreter loop

(module+ test #| vm_interpreter |#
  (define (bc-int-plus-state a b)
    (define ra (if (< a 0) (+ #x2000 a) a))
    (define rb (if (< b 0) (+ #x2000 b) b))
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I) (ast-bytes-cmd '() (list (high-byte ra) (low-byte ra)))
      (bc PUSH_I) (ast-bytes-cmd '() (list (high-byte rb) (low-byte rb)))
      (bc IADD)
      (bc BRK))))

  (define (bc-int-plus-expectation state c)
    (check-equal? (vm-stack->strings state)
                  (list "stack holds 1 item"
                        (format  "int $~a  (rt)" (word->hex-string (if (< c 0) (+ #x2000 c) c))))))
 
  ;; Execute this test only, if major change to int + have been done
  ;; (define _run-bc-int-plus-tests
  ;;   (for/list ([j '(-4096 -4095 -256 -255 -10 -5 -1 0 1 5 10 255 256 4095)])
  ;;     (for/list ([i '(-4096 -4095 -256 -255 -10 -5 -1 0 1 5 10 255 256 4095)])
  ;;       (bc-int-plus-expectation (bc-int-plus-state i j) (+ i j)))))

  (define use-case-int-plus-state-after
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I1)
      (bc PUSH_I2)
      (bc BNOP)
      (bc IADD)                      ;; byte code for INT_PLUS = 3
      (bc PUSH_I) (byte #xf0 #x04) ;; push int #x4f0 (1264)
      (bc PUSH_I) (byte #x1f #x01) ;; push int #x11f (287)
      (bc IADD)                      ;; byte code for INT_PLUS (+ #x04f0 #x011f) (1551 = #x060f)
      (bc PUSH_I1)
      (bc PUSH_IM1)
      (bc IADD)                      ;; byte code for INT_PLUS = 0
      (bc BRK))))

  (inform-check-equal? (cpu-state-clock-cycles use-case-int-plus-state-after)
                       761)
  (check-equal? (vm-stack->strings use-case-int-plus-state-after)
                   (list "stack holds 3 items"
                         "int $0000  (rt)"
                         "int $060f"
                         "int $0003"
                         )))

(define ISUB                #x61) ;; stack [cell-int a, cell-int b] -> [difference]
(define BC_ISUB
  (list
   (label BC_ISUB)
          (LDY ZP_CELL_STACK_TOS)               ;; get current index to tagged byte          
          (SEC)                                 ;; for subtraction carry needs to be set
          (LDA ZP_RT+1)                         ;; A = untagged lowbyte of int (stored in high byte)
          (SBC (ZP_CELL_STACK_HB_PTR),y)      ;; A = A - stack value (int low byte)
          (STA ZP_RT+1)                         ;; RT untagged lowbyte = result

          (LDA ZP_RT)                           ;; A = tagged highbyte of int (stored in low byte)
          (BCS VM_INT_MINUS__NO_DEC_HIGH)       ;; if carry is set from subtraction of lower bits, no subtraction carry over necessary
          (SEC)                                 ;; for subtraction carry needs to be set
          (SBC !$04)                            ;; subtract 1 in the masked int highbyte (starting at bit2) => 4

   (label VM_INT_MINUS__NO_DEC_HIGH)
          (SBC (ZP_CELL_STACK_LB_PTR),y)      ;; A = A - stack value (int high byte)
          (AND !$7c)                            ;; mask out under/overflow (lower two bits and high bit)
          (ORA !$03)                            ;; set lower two bits to tag it as integer value
          (STA ZP_RT)                           ;; RT tagged high byte = result
          
   (label VM_INT_MINUS__DONE)
          (DEC ZP_CELL_STACK_TOS)               ;; pop value from cell-stack (leave result in rt untouched)
          (JMP VM_INTERPRETER_INC_PC)))         ;; interpreter loop

(module+ test #| vm_interpreter |#
  (define (bc-int-minus-state a b)
    (define ra (if (< a 0) (+ #x2000 a) a))
    (define rb (if (< b 0) (+ #x2000 b) b))
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I) (ast-bytes-cmd '() (list (high-byte ra) (low-byte ra)))
      (bc PUSH_I) (ast-bytes-cmd '() (list (high-byte rb) (low-byte rb)))
      (bc ISUB)
      (bc BRK))))

  (define (bc-int-minus-expectation state c)
    (check-equal? (vm-stack->strings state)
                    (list "stack holds 1 item"
                          (format  "int $~a  (rt)" (word->hex-string (if (< c 0) (+ #x2000 c) c))))))

  ;; Execute this test only, if major change to int - have been done
  ;; (define _run-bc-int-minus-tests
  ;;   (for/list ([j '(-4096 -4095 -256 -255 -10 -5 -1 0 1 5 10 255 256 4095)])
  ;;     (for/list ([i '(-4096 -4095 -256 -255 -10 -5 -1 0 1 5 10 255 256 4095)])
  ;;       (bc-int-minus-expectation (bc-int-minus-state i j) (- j i)))))


  (define use-case-int-minus-state-after
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I1)
      (bc PUSH_I2)
      (bc BNOP)
      (bc ISUB)                      ;; byte code for INT_MINUS = 2 - 1 = 1
      (bc PUSH_I) (byte #xf0 #x04) ;; push int #x4f0 (1264)
      (bc PUSH_I) (byte #x1f #x01) ;; push int #x11f (287)
      (bc ISUB)                      ;; byte code for INT_MINUS (287 - 1264 = -977 = #x1c2f)
      (bc PUSH_I1)
      (bc PUSH_I0)      
      (bc ISUB)                      ;; byte code for INT_MINUS => -1
      (bc BRK))))                    ;; brk


   (inform-check-equal? (cpu-state-clock-cycles use-case-int-minus-state-after)
                        761)
    (check-equal? (vm-stack->strings use-case-int-minus-state-after)
                    (list "stack holds 3 items"
                          "int $1fff  (rt)"
                          "int $1c2f"
                          "int $0001")))

(define BC_PUSH_CONST_BYTE
  (list
   (label BC_PUSH_CONST_BYTE)
          (LDY !$01)
          (LDA (ZP_VM_PC),y)
          (LDX !$ff)
          (JSR PUSH_TO_EVLSTK)
          (JMP VM_INTERPRETER_INC_PC_2_TIMES)))

(define NIL_P                #x21) ;; stack [cell-list-ptr] -> [cell-boolean]
(define BC_NIL_P
  (list
   (label BC_NIL_P)
          (JSR CP_RT_TO_RA)
          (JSR VM_NIL_P_R)                      ;; if rt is NIL replace with true (int 1) else replace with false (int 0)
          (JSR DEC_REFCNT_RA)
          (JMP VM_INTERPRETER_INC_PC)))         ;; interpreter loop

(module+ test #| bc-nil-p |#
  (define bc-nil-p-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_NIL)
      (bc NIL_P)
      (bc BRK))))

  (check-equal? (vm-stack->strings bc-nil-p-state)
                (list "stack holds 1 item"
                      "int $0001  (rt)"))

  (define bc-nil-p-2-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_NIL)
      (bc PUSH_I2)
      (bc CONS)
      (bc NIL_P)
      (bc BRK))))

  (check-equal? (vm-deref-cell-pair-w->string bc-nil-p-2-state (+ PAGE_AVAIL_0_W #x05))
                "(empty . pair-ptr NIL)")
  (check-equal? (vm-stack->strings bc-nil-p-2-state)
                (list "stack holds 1 item"
                      "int $0000  (rt)")))

(define COONS #x44)
(define BC_COONS
  (list
   (label BC_COONS)
          (JSR VM_CONS_R)
          (JSR INC_REFCNT_RT)
          (JSR VM_CONS_R)
          (JSR INC_REFCNT_RT)
          (JMP VM_INTERPRETER_INC_PC)))

(define CONS                #x42) ;; stack [cell- car, cell-list-ptr cdr] -> stack [cell-list-ptr new-list]
(define BC_CONS
  (list
   (label BC_CONS)          
          (JSR VM_CONS_R)
          (JSR INC_REFCNT_RT)
          (JMP VM_INTERPRETER_INC_PC)))

(module+ test #| bc-cons |#
   (define bc-cons-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_NIL)
      (bc PUSH_I0)
      (bc CONS)
      (bc BRK))))

   (check-equal? (vm-stack->strings bc-cons-state)
                   (list "stack holds 1 item"
                         (format "pair-ptr[1] $~a05  (rt)" (format-hex-byte PAGE_AVAIL_0))))
   (check-equal? (vm-deref-cell-pair-w->string bc-cons-state (+ PAGE_AVAIL_0_W #x05))
                    "(int $0000 . pair-ptr NIL)"))

(define CAR                 #x43) ;; stack [cell-list-ptr] -> [cell- car of list pointed at]
(define BC_CAR
  (list
   (label BC_CAR)
          (JSR CP_RT_TO_RA)
          (JSR VM_CAR_R)
          (JSR INC_REFCNT_RT)
          (JSR DEC_REFCNT_RA)
          (JMP VM_INTERPRETER_INC_PC)))

(module+ test #| bc-car |#
   (define bc-car-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_NIL)
      (bc PUSH_I2)
      (bc CONS)
      (bc CAR)
      (bc BRK))))

   (check-equal? (vm-stack->strings bc-car-state)
                 (list "stack holds 1 item"
                       "int $0002  (rt)")))

(define CDR                 #x41) ;; stack [cell-list-ptr] -> [cell-list-ptr cdr of list pointed at]
(define BC_CDR
  (list
   (label BC_CDR)
          (JSR CP_RT_TO_RA)
          (JSR VM_CDR_R)
          (JSR INC_REFCNT_RT)
          (JSR DEC_REFCNT_RA)
          (JMP VM_INTERPRETER_INC_PC)))

(module+ test #| bc-cdr |#
   (define bc-cdr-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_NIL)
      (bc PUSH_I2)
      (bc CONS)
      (bc CDR)
      (bc BRK))))

   (check-equal? (vm-stack->strings bc-cdr-state)
                 (list "stack holds 1 item"
                       "pair-ptr NIL  (rt)")))
(define SWAP #x03)
(define BC_SWAP
  (list
   (label BC_SWAP)
          (LDY ZP_CELL_STACK_TOS)
          (LDA (ZP_CELL_STACK_LB_PTR),y)
          (TAX)
          (LDA ZP_RT)
          (STA (ZP_CELL_STACK_LB_PTR),y)
          (STX ZP_RT)
          (LDA (ZP_CELL_STACK_HB_PTR),y)
          (TAX)
          (LDA ZP_RT+1)
          (STA (ZP_CELL_STACK_HB_PTR),y)
          (STX ZP_RT+1)
          (JMP VM_INTERPRETER_INC_PC)))

(module+ test #| swap |#
  (skip (check-equal? #t #f "implement")))

(define I_GT_P #x63)
(define BC_I_GT_P
  (list
   (label BC_I_GT_P)
          (LDA ZP_RT)
          (STA ZP_TEMP)
          (LDA ZP_RT+1)
          (STA ZP_TEMP2)
          (JSR POP_CELL_EVLSTK_TO_RT)
          (LDA ZP_RT)
          (CMP ZP_TEMP)
          (BMI GREATER__BC_I_GT_P)
          (BNE LESS_OR_EQUAL__BC_I_GT_P)
          (LDA ZP_RT+1)
          (CMP ZP_TEMP+1)
          (BMI GREATER__BC_I_GT_P)
   (label LESS_OR_EQUAL__BC_I_GT_P)
          (JSR WRITE_INT0_TO_RT)
          (JMP VM_INTERPRETER_INC_PC)
    (label GREATER__BC_I_GT_P)
          (JSR WRITE_INT1_TO_RT)
          (JMP VM_INTERPRETER_INC_PC)))

(module+ test #| INT GREATER? |#
  (define int-greater-0>-1-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_IM1)
      (bc PUSH_I0)
      (bc I_GT_P))
     ))

  (skip (check-equal? (vm-regt->string int-greater-0>-1-state)
                      "int $0001"
                      "comparison of negative with positive number (failing currently)"))

  (define int-greater-0>0-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc PUSH_I0)
      (bc I_GT_P))))

  (check-equal? (vm-regt->string int-greater-0>0-state)
                "int $0000")

  (define int-greater-1>1-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I1)
      (bc PUSH_I1)
      (bc I_GT_P))))

  (check-equal? (vm-regt->string int-greater-1>1-state)
                "int $0000")

  (define int-greater-2>2-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I2)
      (bc PUSH_I2)
      (bc I_GT_P))))

  (check-equal? (vm-regt->string int-greater-2>2-state)
                "int $0000")

  (define int-greater-2>1-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I1)
      (bc PUSH_I2)
      (bc I_GT_P))))

  (check-equal? (vm-regt->string int-greater-2>1-state)
                "int $0001")

  (define int-greater-1>0-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc PUSH_I1)
      (bc I_GT_P))))

  (check-equal? (vm-regt->string int-greater-1>0-state)
                "int $0001")

    (define int-greater-0>1-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I1)
      (bc PUSH_I0)
      (bc I_GT_P))))

  (check-equal? (vm-regt->string int-greater-0>1-state)
                "int $0000"))

(define INT_P #x07)
(define BC_INT_P
  (list
   (label BC_INT_P)
          (LDA ZP_RT)
          (LDX !$01)
          (AND !$83)
          (CMP !$03)
          (BEQ IS_INT__BC_INT_P)
          (JSR DEC_REFCNT_RT)
          (LDA !$03)
          (LDX !$00)
   (label IS_INT__BC_INT_P)
          (STA ZP_RT)
          (STX ZP_RT+1)
          (JMP VM_INTERPRETER_INC_PC)))

(module+ test #| int? |#
  (skip (check-equal? #t #f "implement")))

(define F_P_RET_F #x13)
(define BC_F_P_RET_F
  (list
   (label BC_F_P_RET_F)
          (LDA ZP_RT+1)
          (BNE IS_TRUE__BC_F_P_RET_F)
          ;; don't pop false value, return it!
          (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)
          (JSR VM_POP_CALL_FRAME_N)             ;; now pop the call frame
          (JMP VM_INTERPRETER)
   (label IS_TRUE__BC_F_P_RET_F)
          (JSR POP_CELL_EVLSTK_TO_RT)
          (JMP VM_INTERPRETER_INC_PC)))

(define F_P_RET #x0e)
(define BC_F_P_RET
  (list
   (label BC_F_P_RET)
          (LDA ZP_RT+1)
          (BNE IS_TRUE__BC_F_P_RET)
          (JSR POP_CELL_EVLSTK_TO_RT)
          (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)
          (JSR VM_POP_CALL_FRAME_N)             ;; now pop the call frame
          (JMP VM_INTERPRETER)
   (label IS_TRUE__BC_F_P_RET)
          (JSR POP_CELL_EVLSTK_TO_RT)
          (JMP VM_INTERPRETER_INC_PC)))

(define T_P_RET #x0b)
(define BC_T_P_RET
  (list
   (label BC_T_P_RET)
          (LDA ZP_RT+1)
          (BEQ IS_FALSE__BC_T_P_RET)
          (JSR POP_CELL_EVLSTK_TO_RT)
          (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)
          (JSR VM_POP_CALL_FRAME_N)             ;; now pop the call frame
          (JMP VM_INTERPRETER)
   (label IS_FALSE__BC_T_P_RET)
          (JSR POP_CELL_EVLSTK_TO_RT)
          (JMP VM_INTERPRETER_INC_PC)))

(define GOTO                #x32) ;; op = relative offset
(define BC_GOTO
  (list
   (label BC_GOTO)
          (CLC)
          (LDY !$01)
          (LDA (ZP_VM_PC),y)
          (BMI JUMP_BACK__BC_GOTO)

          (ADC !$02)
          (JMP VM_INTERPRETER_INC_PC_A_TIMES)

   (label JUMP_BACK__BC_GOTO)
          (ADC ZP_VM_PC)
          (STA ZP_VM_PC)
          (BCS NO_PAGE_CHANGE_ON_BACK__BC_GOTO)
          (DEC ZP_VM_PC+1)
   (label NO_PAGE_CHANGE_ON_BACK__BC_GOTO)
          (JMP VM_INTERPRETER)))

(module+ test #| goto |#
  (define goto-0-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc GOTO) (byte 2)
      (bc PUSH_IM1)
      (bc BRK)
      (bc PUSH_I1))))
  (check-equal? (vm-stack->strings goto-0-state)
                (list "stack holds 2 items"
                      "int $0001  (rt)"
                      "int $0000"))

  (define goto-1-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc GOTO) (byte $75)
      (bc BRK)
      (org-align #x78)
      (bc PUSH_I1))
     ))
  (check-equal? (vm-stack->strings goto-1-state)
                (list "stack holds 2 items"
                      "int $0001  (rt)"
                      "int $0000"))

  (define goto-2-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I0)
       (bc GOTO) (byte $7d)
       (bc BRK)
       (org-align #x80)
       (bc PUSH_I1)
       (bc GOTO) (byte $6d)
       (bc BRK)
       (org-align #xf0)
       (bc PUSH_I2)
       ;; 80f1
       (bc GOTO) (byte $0d)
       (build-list 13 (lambda (_i) (bc BRK)))
       ;; now at 8100
       (bc PUSH_IM1)))
   ))
  (check-equal? (vm-stack->strings goto-2-state)
                (list "stack holds 4 items"
                      "int $1fff  (rt)"
                      "int $0002"
                      "int $0001"
                      "int $0000"))

  (define goto-3-state
    (run-bc-wrapped-in-test
     (flatten 
      (list
       (bc PUSH_I0)
       (bc GOTO) (byte $7d)
       (bc BRK)
       (org-align #x80)
       (bc PUSH_I1)
       ;; now at 8081
       (bc GOTO) (byte $6d)
       ;; 8083
       (bc BRK)
       (org-align #xf0)
       (bc PUSH_I2)
       ;; now at 80f1
       (bc GOTO) (byte $0e)
       (build-list 14 (lambda (_i) (bc BRK)))
       ;; now at 8102
       (bc PUSH_IM1)))
   ))
  (check-equal? (vm-stack->strings goto-3-state)
                (list "stack holds 4 items"
                      "int $1fff  (rt)"
                      "int $0002"
                      "int $0001"
                      "int $0000"))

  (define goto-4-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I0)
       (bc GOTO) (byte 3)
       (bc BRK)
       (bc PUSH_I1)
       (bc BRK)
       (bc GOTO) (byte $fe)))
     ))
  (check-equal? (vm-stack->strings goto-4-state)
                (list "stack holds 2 items"
                      "int $0001  (rt)"
                      "int $0000"))

  (define goto-5-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I0)
       (bc GOTO) (byte $7d)
       (bc BRK)
       (org-align #x80)
       (bc PUSH_I1)
       ;; now at 8081
       (bc GOTO) (byte $6d)
       ;; 8083
       (bc BRK)
       (org-align #xf0)
       (bc PUSH_I2)
       ;; now at 80f1
       (bc GOTO) (byte $0e)
       (build-list 12 (lambda (_i) (bc BRK)))
       ;; 80ff
       (bc PUSH_I0)
       ;; 8100
       (bc BRK)
       ;; now at 8101
       (bc PUSH_IM1)
       (bc GOTO) (byte $fd)))
     ))
  (check-equal? (vm-stack->strings goto-5-state)
                (list "stack holds 5 items"
                      "int $0000  (rt)"
                      "int $1fff"
                      "int $0002"
                      "int $0001"
                      "int $0000")))

(define F_P_BRA #x0d)
(define BC_F_P_BRA
  (list
   (label BC_F_P_BRA)
          (CLC)
          (LDA ZP_RT+1)
          (BEQ BRANCH__BC_T_P_BRA)
          (LDA !$00)
          (BEQ POP_AND_CONTINUE__BC_T_P_BRA)))

(module+ test #| branch true |#
  (define branch-false-0-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc F_P_BRA) (byte 2)
      (bc PUSH_I0)
      (bc BRK)
      (bc PUSH_I2))))
  (check-equal? (vm-stack->strings branch-false-0-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)"))

  (define branch-false-1-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc F_P_BRA) (byte $75)
      (bc BRK)
      (org-align #x78)
      (bc PUSH_I2))))
  (check-equal? (vm-stack->strings branch-false-1-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)"))

  (define branch-false-2-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I0)
       (bc F_P_BRA) (byte $7d)
       (bc BRK)
       (org-align #x80)
       (bc PUSH_I0)
       (bc F_P_BRA) (byte $6d)
       (bc BRK)
       (org-align #xf0)
       (bc PUSH_I0)
       ;; 80f1
       (bc F_P_BRA) (byte $0d)
       (build-list 13 (lambda (_i) (bc BRK)))
       ;; now at 8100
       (bc PUSH_I2)))
   ))
  (check-equal? (vm-stack->strings branch-false-2-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)"))

  (define branch-false-3-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I0)
       (bc F_P_BRA) (byte $7d)
       (bc BRK)
       (org-align #x80)
       (bc PUSH_I0)
       ;; now at 8081
       (bc F_P_BRA) (byte $6d)
       ;; 8083
       (bc BRK)
       (org-align #xf0)
       (bc PUSH_I0)
       ;; now at 80f1
       (bc F_P_BRA) (byte $0e)
       (build-list 14 (lambda (_i) (bc BRK)))
       ;; now at 8102
       (bc PUSH_I2)))
   ))
  (check-equal? (vm-stack->strings branch-false-3-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)"))

  (define branch-false-4-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I0)
       (bc F_P_BRA) (byte 3)
       (bc BRK)
       (bc PUSH_I2)
       (bc BRK)
       (bc PUSH_I0)
       (bc F_P_BRA) (byte $fd)))
     ))
  (check-equal? (vm-stack->strings branch-false-4-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)"))

  (define branch-false-5-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I0)
       (bc F_P_BRA) (byte $7d)
       (bc BRK)
       (org-align #x80)
       (bc PUSH_I0)
       ;; now at 8081
       (bc F_P_BRA) (byte $6d)
       ;; 8083
       (bc BRK)
       (org-align #xf0)
       (bc PUSH_I0)
       ;; now at 80f1
       (bc F_P_BRA) (byte $0e)
       (build-list 12 (lambda (_i) (bc BRK)))
       ;; 80ff
       (bc PUSH_I2)
       ;; 8100
       (bc BRK)
       ;; now at 8101
       (bc PUSH_I0)
       (bc F_P_BRA) (byte $fd)))
     ))
  (check-equal? (vm-stack->strings branch-false-5-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)")))

(define T_P_BRA #x0c)
(define BC_T_P_BRA
  (list
   (label BC_T_P_BRA)
          (CLC)
          (LDA ZP_RT+1)
          (BEQ POP_AND_CONTINUE__BC_T_P_BRA) ;; when false (A = 0), just continue, no branch

   (label BRANCH__BC_T_P_BRA)
   ;; branch by adding second byte code
          (LDY !$01)
          (LDA (ZP_VM_PC),y)
          (BMI NEGATIVE_BRANCH__BC_T_P_BRA)

   (label POP_AND_CONTINUE__BC_T_P_BRA)
          (ADC !$02)
          (ADC ZP_VM_PC)
          (STA ZP_VM_PC)
          (BCC NO_PAGE_CHANGE__BC_T_P_BRA)
          (INC ZP_VM_PC+1)
   (label NO_PAGE_CHANGE__BC_T_P_BRA)
          (JSR POP_CELL_EVLSTK_TO_RT)
          (JMP VM_INTERPRETER)

   (label NEGATIVE_BRANCH__BC_T_P_BRA)
          (ADC ZP_VM_PC)
          (STA ZP_VM_PC)
          (BCS NO_PAGE_CHANGE_ON_BACK__BC_T_P_BRA)
          (DEC ZP_VM_PC+1)
   (label NO_PAGE_CHANGE_ON_BACK__BC_T_P_BRA)
          (JSR POP_CELL_EVLSTK_TO_RT)
          (JMP VM_INTERPRETER)))

(module+ test #| branch true |#
  (define branch-true-0-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I1)
      (bc T_P_BRA) (byte 2)
      (bc PUSH_I1)
      (bc BRK)
      (bc PUSH_I2))))
  (check-equal? (vm-stack->strings branch-true-0-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)"))

  (define branch-true-1-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I1)
      (bc T_P_BRA) (byte $75)
      (bc BRK)
      (org-align #x78)
      (bc PUSH_I2))))
  (check-equal? (vm-stack->strings branch-true-1-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)"))

  (define branch-true-2-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I1)
       (bc T_P_BRA) (byte $7d)
       (bc BRK)
       (org-align #x80)
       (bc PUSH_I1)
       (bc T_P_BRA) (byte $6d)
       (bc BRK)
       (org-align #xf0)
       (bc PUSH_I1)
       ;; 80f1
       (bc T_P_BRA) (byte $0d)
       (build-list 13 (lambda (_i) (bc BRK)))
       ;; now at 8100
       (bc PUSH_I2)))
   ))
  (check-equal? (vm-stack->strings branch-true-2-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)"))

  (define branch-true-3-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I1)
       (bc T_P_BRA) (byte $7d)
       (bc BRK)
       (org-align #x80)
       (bc PUSH_I1)
       ;; now at 8081
       (bc T_P_BRA) (byte $6d)
       ;; 8083
       (bc BRK)
       (org-align #xf0)
       (bc PUSH_I1)
       ;; now at 80f1
       (bc T_P_BRA) (byte $0e)
       (build-list 14 (lambda (_i) (bc BRK)))
       ;; now at 8102
       (bc PUSH_I2)))
   ))
  (check-equal? (vm-stack->strings branch-true-3-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)"))

  (define branch-true-4-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I1)
       (bc T_P_BRA) (byte 3)
       (bc BRK)
       (bc PUSH_I2)
       (bc BRK)
       (bc PUSH_I1)
       (bc T_P_BRA) (byte $fd)))
     ))
  (check-equal? (vm-stack->strings branch-true-4-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)"))

  (define branch-true-5-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I1)
       (bc T_P_BRA) (byte $7d)
       (bc BRK)
       (org-align #x80)
       (bc PUSH_I1)
       ;; now at 8081
       (bc T_P_BRA) (byte $6d)
       ;; 8083
       (bc BRK)
       (org-align #xf0)
       (bc PUSH_I1)
       ;; now at 80f1
       (bc T_P_BRA) (byte $0e)
       (build-list 12 (lambda (_i) (bc BRK)))
       ;; 80ff
       (bc PUSH_I2)
       ;; 8100
       (bc BRK)
       ;; now at 8101
       (bc PUSH_I1)
       (bc T_P_BRA) (byte $fd)))
     ))
  (check-equal? (vm-stack->strings branch-true-5-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)")))

(define CONS_PAIR_P #x0a)
(define BC_CONS_PAIR_P
  (list
   (label BC_CONS_PAIR_P)
          (LDA ZP_RT+1)
          (STA ZP_RA+1)
          (LDA ZP_RT)
          (STA ZP_RA)

          (LDX !$03) ;; low byte of int (for bool)
          (STX ZP_RT)
          (CMP !$01)
          (BEQ IS_NO_PAIR_SINCE_NIL__BC_CONS_PAIR_P)
          (AND !$03)
          (CMP !$01)
          (BEQ IS_PAIR__BC_CONS_PAIR_P)
   (label IS_NO_PAIR_SINCE_NIL__BC_CONS_PAIR_P)
          (LDA !$00)
   (label IS_PAIR__BC_CONS_PAIR_P)
          (STA ZP_RT+1)
          (JSR DEC_REFCNT_RA)
          (JMP VM_INTERPRETER_INC_PC)))

(define PUSH_NIL            #x09) ;; stack: [] -> [NIL]
(define BC_PUSH_NIL
  (list
   (label BC_PUSH_NIL)    
   (JSR PUSH_NIL_TO_EVLSTK)        ;; push NIL on the stack
   (JMP VM_INTERPRETER_INC_PC)))         ;; interpreter loop

(module+ test #| bc-push-const-nil |#
  (define bc-push-const-nil-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_NIL)
      (bc BRK))))

  (check-equal? (vm-stack->strings bc-push-const-nil-state)
                (list "stack holds 1 item"
                      "pair-ptr NIL  (rt)")))

(define INC_INT #x02) ;; extended (could be mapped to regular byte code, if needed very often!)
(define BC_INC_INT
  (list
   (label BC_INC_INT)
          (INC ZP_RT+1)
          (BNE DONE__BC_INC_INT)
          (INC ZP_RT)
          (LDA ZP_RT)
          (ORA !$03)
          (AND !$7f)
          (STA ZP_RT)
   (label DONE__BC_INC_INT)
          (JMP VM_INTERPRETER_INC_PC_2_TIMES)))

(module+ test #| inc int |#
  (define inc-int-0-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc EXT)
      (bc INC_INT))))

  (check-equal? (vm-stack->strings inc-int-0-state)
                (list "stack holds 1 item"
                      "int $0001  (rt)"))

  (define inc-int-1-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I) (byte 255) (byte 0)
      (bc EXT)
      (bc INC_INT))
     ))

  (check-equal? (vm-stack->strings inc-int-1-state)
                (list "stack holds 1 item"
                      "int $0100  (rt)"))

  (define inc-int-2-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_IM1)
      (bc EXT)
      (bc INC_INT))
     ))

  (check-equal? (vm-stack->strings inc-int-2-state)
                (list "stack holds 1 item"
                      "int $0000  (rt)"))

  (define inc-int-3-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I) (byte 255) (byte 05)
      (bc EXT)
      (bc INC_INT))
     ))

  (check-equal? (vm-stack->strings inc-int-3-state)
                (list "stack holds 1 item"
                      "int $0600  (rt)")))

(define MAX_INT #x01) ;; extended
(define BC_MAX_INT
  (list
   (label BC_MAX_INT)
          (LDY ZP_CELL_STACK_TOS)

          ;; compare high byte of int (which is lb)
          (LDA (ZP_CELL_STACK_LB_PTR),y)
          (CMP ZP_RT)
          (BNE NO_OTHER_COMPARE__BC_MAX_INT) ;; already different => no need to compare low byte

          ;; compare low byte of int (which is hb)
          (LDA (ZP_CELL_STACK_HB_PTR),y)
          (CMP ZP_RT+1)

   (label NO_OTHER_COMPARE__BC_MAX_INT)
          (BMI KEEP_RT__BC_MAX_INT)

          (JSR POP_CELL_EVLSTK_TO_RT)     ;; pop RT and move TOS into RT
          (CLC)
          (BCC AND_RETURN__BC_MAX_INT)

    (label KEEP_RT__BC_MAX_INT)
          (DEC ZP_CELL_STACK_TOS) ;; just pop but keep RT
    (label AND_RETURN__BC_MAX_INT)
          (JMP VM_INTERPRETER_INC_PC_2_TIMES)))

(define DUP #x0f)
(define BC_DUP
  (list
   (label BC_DUP)
          (JSR INC_REFCNT_RT)
          (JSR PUSH_RT_TO_EVLSTK_IF_NONEMPTY)
          (JMP VM_INTERPRETER_INC_PC)))

(define CELL_EQ #x12)
(define BC_CELL_EQ
  (list
   (label BC_CELL_EQ)
          (LDY ZP_CELL_STACK_TOS)
          (LDA (ZP_CELL_STACK_HB_PTR),y)
          (STA ZP_RA+1)
          (CMP ZP_RT+1)
          (BNE NE_LB__BC_CELL_EQ)
          (LDA (ZP_CELL_STACK_LB_PTR),y)
          (STA ZP_RA)
          (CMP ZP_RT)
          (BNE NE__BC_CELL_EQ)

          (JSR DEC_REFCNT_RT)
          (JSR DEC_REFCNT_RA)
          (DEC ZP_CELL_STACK_TOS)
          (JSR WRITE_INT1_TO_RT)
          (JMP VM_INTERPRETER_INC_PC)

   (label NE_LB__BC_CELL_EQ)
          (LDA (ZP_CELL_STACK_LB_PTR),y)
          (STA ZP_RA)
   (label NE__BC_CELL_EQ)
          (JSR DEC_REFCNT_RT)
          (JSR DEC_REFCNT_RA)
          (DEC ZP_CELL_STACK_TOS)
          (JSR WRITE_INT0_TO_RT)
          (JMP VM_INTERPRETER_INC_PC)))

(define I0_P #x22)
(define BC_I0_P
  (list
   (label BC_I0_P)
          (LDA ZP_RT+1)
          (BNE IS_NOT_ZERO__BC_I0_P)
          (LDA ZP_RT)
          (CMP !$03)
          (BEQ IS_ZERO__BC_I0_P)

   (label IS_NOT_ZERO__BC_I0_P)
          (LDA !$00)
          (STA ZP_RT+1)
          (LDA !$03)
          (STA ZP_RT)
          (JMP VM_INTERPRETER_INC_PC)

   (label IS_ZERO__BC_I0_P)
          (LDA !$01)    
          (STA ZP_RT+1)
          (JMP VM_INTERPRETER_INC_PC)))

;; garbage collect the freelist
(define GC_FL #x03)  ;; extended
(define BC_GC_FL
  (list
   (label BC_GC_FL)
          (JSR GC_ALL)
          (JMP VM_INTERPRETER_INC_PC_2_TIMES)))

(define VM_INTERPRETER_OPTABLE_EXT1_LB
  (flatten
   (list
    (label VM_INTERPRETER_OPTABLE_EXT1_LB)
           (byte-ref <VM_INTERPRETER_INC_PC)     ;; 00 - reserved (could be used for another extension command)
           (byte-ref <BC_MAX_INT)                ;; 01
           (byte-ref <BC_INC_INT)                ;; 02
           (byte-ref <BC_GC_FL)                  ;; 03
           )))

(define VM_INTERPRETER_OPTABLE_EXT1_HB
  (flatten
   (list
    (label VM_INTERPRETER_OPTABLE_EXT1_HB)
           (byte-ref >VM_INTERPRETER_INC_PC)     ;; 00 - reserved (could be used for another extension command)
           (byte-ref >BC_MAX_INT)                ;; 01
           (byte-ref >BC_INC_INT)                ;; 02
           (byte-ref >BC_GC_FL)                  ;; 03
           )))

(define EXT #x04)
(define BC_EXT1_CMD
  (list
   (label BC_EXT1_CMD)
          (INY)
          (LDA (ZP_VM_PC),y)
          (TAY)
          (LDA VM_INTERPRETER_OPTABLE_EXT1_LB,y)
          (STA CALL_COMMAND__BC_EXT1_CMD+1)
          (LDA VM_INTERPRETER_OPTABLE_EXT1_HB,y)
          (STA CALL_COMMAND__BC_EXT1_CMD+2)
   (label CALL_COMMAND__BC_EXT1_CMD)
          (JMP $cf00)))


(module+ test #| ext max-int |#
  (define max-int-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I2)
      (bc PUSH_I1)
      (bc EXT)
      (bc MAX_INT))))

  (check-equal? (vm-stack->strings max-int-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)"))

  (define max-int-2-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I1)
      (bc PUSH_I2)
      (bc EXT)
      (bc MAX_INT))))

  (check-equal? (vm-stack->strings max-int-2-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)")))

(define BNOP #x01)
(define BC_BNOP
  (list
   (label BC_BNOP)
          (JSR $0100)
          (JMP VM_INTERPRETER_INC_PC)))

(module+ test #| nop |#
  (define nop-state
    (run-bc-wrapped-in-test
     (list (bc BNOP))))

  (check-equal? (cpu-state-clock-cycles nop-state)
                31))

(define POP #x11)
(define BC_POP
  (list
   (label BC_POP)
          (JSR DEC_REFCNT_RT)
          (JSR POP_CELL_EVLSTK_TO_RT)
          (JMP VM_INTERPRETER_INC_PC)))

(module+ test #| pop |#
  (define pop-0-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc POP))))

  (check-equal? (vm-stack->strings pop-0-state)
                (list "stack is empty"))

  (define pop-1-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc PUSH_I1)
      (bc POP))))

  (check-equal? (vm-stack->strings pop-1-state)
                (list "stack holds 1 item"
                      "int $0000  (rt)"))
  (define pop-2-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc PUSH_I1)
      (bc PUSH_I2)
      (bc POP))))

  (check-equal? (vm-stack->strings pop-2-state)
                (list "stack holds 2 items"
                      "int $0001  (rt)"
                      "int $0000")))

(define CAAR #xa8)
(define CADR #xaa)
(define CDAR #xac)
(define CDDR #xae)
;; (define ? #xaf)
(define BC_CxxR
  (list
   (label BC_CxxR)
          (LDX ZP_RT)
          (STX ZP_RA)
          (LDX ZP_RT+1)
          (STX ZP_RA+1)
          (JSR VM_CxxR_R)
          (JSR INC_REFCNT_RT)
          (JSR DEC_REFCNT_RA)
          (JMP VM_INTERPRETER_INC_PC)))

(module+ test #| cxxr |#
  (define cxxr-0-state
    (run-bc-wrapped-in-test
      (list
         (bc PUSH_NIL)
         (bc PUSH_I2)
         (bc PUSH_I1)
         (bc CONS)
         (bc CONS)
         (bc CAAR))
      ))
  (check-equal? (vm-stack->strings cxxr-0-state)
                (list "stack holds 1 item"
                      "int $0001  (rt)"))

  (define cxxr-1-state
    (run-bc-wrapped-in-test
      (list
         (bc PUSH_NIL)
         (bc PUSH_I2)
         (bc PUSH_I1)
         (bc CONS)
         (bc CONS)
         (bc CDAR))
      ))
  (check-equal? (vm-stack->strings cxxr-1-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)"))

  (define cxxr-2-state
    (run-bc-wrapped-in-test
      (list
         (bc PUSH_I2)
         (bc PUSH_I1)
         (bc CONS)
         (bc PUSH_NIL)
         (bc CONS)
         (bc CADR))
      ))
  (check-equal? (vm-stack->strings cxxr-2-state)
                (list "stack holds 1 item"
                      "int $0001  (rt)"))

  (define cxxr-3-state
    (run-bc-wrapped-in-test
      (list
         (bc PUSH_I2)
         (bc PUSH_I1)
         (bc CONS)
         (bc PUSH_NIL)
         (bc CONS)
         (bc CDDR))
      ))
  (check-equal? (vm-stack->strings cxxr-3-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)")))

(define PUSH_B #x17)
(define BC_PUSH_B

  (flatten
   (list
    (label BC_PUSH_B)
           (JSR PUSH_RT_TO_EVLSTK_IF_NONEMPTY)
           (LDY !$01)
           (LDA (ZP_VM_PC),y)
           (STA ZP_RT+1)
           (LDA !TAG_BYTE_BYTE_CELL)
           (STA ZP_RT)
           (JMP VM_INTERPRETER_INC_PC_2_TIMES))))

(module+ test #| push byte |#
  (define push-byte-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_B) (byte 0)
       (bc PUSH_B) (byte 1)
       (bc PUSH_B) (byte 10)))))

  (check-equal? (vm-stack->strings push-byte-state)
                (list "stack holds 3 items"
                      "byte $0a  (rt)"
                      "byte $01"
                      "byte $00")))

;; stack: index(byte) :: cell-ptr->cell-array  :: value (cell)
;; ->     []
;;        cell-array @ index = value
(define POP_TO_AF  #x16) ;; op = array-idx, stack [cell- array-ptr-] -> []
(define BC_POP_TO_AF
  (flatten
   (list
    (label BC_POP_TO_AF)
           (LDA ZP_RT+1)                  ;; index                               (stack: index ::cell-ptr ::value )
           (PHA)
           (JSR POP_CELL_EVLSTK_TO_RA)    ;; ra = cell-ptr -> cell-array         (stack: index ::value )
           (JSR POP_CELL_EVLSTK_TO_RT)    ;; rt = value                          (stack: value)
           (PLA)                          ;; a = index
           (JSR POP_EVLSTK_TO_ARR_ATa_RA) ;; array@a <- rt (TODO: check that old value is dec-refcnt'd)
           (JSR DEC_REFCNT_RA)            ;; since array is no longer on stack dec refcnt (value moved => no change)
           (JMP VM_INTERPRETER_INC_PC))))

(module+ test #| pop to array field |#
  (define pop-to-array-field-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_B) (byte 20)
      (bc ALLOC_A)
      (bc DUP) ;; make sure to keep a reference to this array, otherwise it is freed!
      (bc PUSH_I1)
      (bc SWAP)
      (bc PUSH_B) (byte 1)
      (bc POP_TO_AF))
     ))

  (check-equal? (vm-stack->strings pop-to-array-field-state)
                (list "stack holds 1 item"
                      (format "ptr[1] $~a06  (rt)" (number->string PAGE_AVAIL_0 16))))
  (check-equal? (memory-list pop-to-array-field-state (+ PAGE_AVAIL_0_W 05) (+ PAGE_AVAIL_0_W 11))
                (list 1      ;; refcnt = 1 (one reference on the stack)
                      #x83   ;; page type = m1p3 (slot size 49, used 20*2)
                      20     ;; number of elements
                      0 0    ;; element 0
                      3 1))) ;; element 1 = int 1

;; stack: index (byte) :: cell-ptr -> cell-array
;; ->     value (cell)
(define PUSH_AF    #x15) ;; op = field-idx, stack [array-ref] -> [cell-]
(define BC_PUSH_AF
  (flatten
   (list
    (label BC_PUSH_AF)
           (JSR POP_CELL_EVLSTK_TO_RA)    ;; ra = cell-ptr -> cell-array         (stack: index)
           (LDA ZP_RT+1)                  ;; index                               (stack: index)
           (JSR WRITE_ARR_ATa_RA_TO_RT)   ;; rt <- array@a                       (stack: value)
           (JSR INC_REFCNT_RT)            ;; now on stack and in array => inc refcnt'd
           (JSR DEC_REFCNT_RA)            ;; removed from stack => dec refcnt'd
           (JMP VM_INTERPRETER_INC_PC))))

(module+ test #| push array field |#
  (define push-array-field-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_B) (byte 20)
       (bc ALLOC_A)

       (bc DUP) ;; make sure to keep a reference to this array, otherwise it is freed!
       (bc PUSH_I1)
       (bc SWAP)
       (bc PUSH_B) (byte 1)
       (bc POP_TO_AF)

       (bc DUP)
       (bc PUSH_I2)
       (bc SWAP)
       (bc PUSH_B) (byte 10)
       (bc POP_TO_AF)

       (bc DUP)
       (bc DUP)
       (bc PUSH_B) (byte 1)
       (bc PUSH_AF)

       (bc SWAP)
       (bc PUSH_B) (byte 10)
       (bc PUSH_AF)))
     ))

(check-equal? (memory-list push-array-field-state (+ PAGE_AVAIL_0_W 05) (+ PAGE_AVAIL_0_W 29))
                (list 1      ;; refcnt = 1 (one reference on the stack)
                      #x83   ;; page type = m1p3 (slot size 49, used 20*2)
                      20     ;; number of elements
                      0 0    ;; element 0
                      3 1
                      0 0
                      0 0
                      0 0
                      0 0
                      0 0
                      0 0
                      0 0
                      0 0
                      3 2   ;; element 10
                      ))

  (check-equal? (vm-stack->strings push-array-field-state)
                (list "stack holds 3 items"
                      "int $0002  (rt)"
                      "int $0001"
                      "ptr[1] $9706")))

(define GET_ARRAY_FIELD_0 #xb0)
(define GET_ARRAY_FIELD_1 #xb2)
(define GET_ARRAY_FIELD_2 #xb4)
(define GET_ARRAY_FIELD_3 #xb6)

(define SET_ARRAY_FIELD_0 #xb1)
(define SET_ARRAY_FIELD_1 #xb3)
(define SET_ARRAY_FIELD_2 #xb5)
(define SET_ARRAY_FIELD_3 #xb7)

(define BC_XET_ARRAY_FIELD
  (flatten
   (list
    (label BC_XET_ARRAY_FIELD)
           (LSR)
           (BCS BC_SET_ARRAY_FIELD)

    (label BC_GET_ARRAY_FIELD) ;; replace RT with RT.@A
           (PHA)
           (JSR CP_RT_TO_RA)
           (PLA)
           (JSR WRITE_ARR_ATa_RA_TO_RT)
           (JSR INC_REFCNT_RT)
           (JSR DEC_REFCNT_RA)
           (JMP VM_INTERPRETER_INC_PC)

    (label BC_SET_ARRAY_FIELD) ;; Write TOS-1 -> RT.@A, popping
           (PHA)
           (JSR CP_RT_TO_RA)
           (JSR POP_CELL_EVLSTK_TO_RT)
           (PLA)
           (JSR POP_EVLSTK_TO_ARR_ATa_RA)
           (JSR DEC_REFCNT_RA)
           (JMP VM_INTERPRETER_INC_PC))))

;; stack: size (byte)
;; ->      cell-ptr -> cell-array
(define ALLOC_A #x14)
(define BC_ALLOC_A
  (list
   (label BC_ALLOC_A)
          (LDA ZP_RT+1)                 ;; byte size
          (JSR ALLOC_CELLARR_TO_RA)     ;;
          (JSR INC_REFCNT_M1_SLOT_RA)   ;; only cell-array needs to be inc-refcnt'd
          (JSR CP_RA_TO_RT)             ;; overwrite byte on stack
          (JMP VM_INTERPRETER_INC_PC)))


(define NATIVE #x4a)
(define BC_NATIVE
  (list
   (label BC_NATIVE)
          ;; (INC ZP_VM_PC)
          ;; (BNE CONT__BC_NATIVE)
          ;; (INC ZP_VM_PC+1)
   (label CONT__BC_NATIVE)
          (JMP (ZP_VM_PC)))) ;; this jump actually jumps onto the current bytecode command,
                             ;; but since NATIVE is 4a (which is 6510 LSR), this can be done without the incr.

(define RETURN_TO_BC
  (list
   (label RETURN_TO_BC)
          (PLA)
          (STA ZP_VM_PC)
          (PLA)
          (STA ZP_VM_PC+1)
          (JMP VM_INTERPRETER_INC_PC)))


;; must be page aligned!
(define VM_INTERPRETER_OPTABLE
  (flatten ;; necessary because word ref creates a list of ast-byte-codes ...
   (list
    (label VM_INTERPRETER_OPTABLE)                ;;         byte code
           (word-ref BC_PUSH_LOCAL_SHORT)         ;; 00  <-  80..87 (RZ)
           (word-ref BC_BNOP)                     ;; 02  <-  01 
           (word-ref BC_BRK)                      ;; 04  <-  02 break into debugger/exit program
           (word-ref BC_SWAP)                     ;; 06  <-  03 
           (word-ref BC_EXT1_CMD)                 ;; 08  <-  04 
           (word-ref BC_PUSH_CONST_BYTE)          ;; 0a  <-  05 
           (word-ref BC_PUSH_I)           ;; 0c  <-  06
           (word-ref BC_INT_P)                    ;; 0e  <-  07 
           (word-ref VM_INTERPRETER_INC_PC)       ;; 10  <-  88..8F reserved
           (word-ref BC_PUSH_NIL)           ;; 12  <-  09 
           (word-ref BC_CONS_PAIR_P)              ;; 14  <-  0a 
           (word-ref BC_T_P_RET)               ;; 16  <-  0b 
           (word-ref BC_T_P_BRA)            ;; 18  <-  0c
           (word-ref BC_F_P_BRA)           ;; 1a  <-  0d 
           (word-ref BC_F_P_RET)              ;; 1c  <-  0e 
           (word-ref BC_DUP)                      ;; 1e  <-  0f 
           (word-ref BC_POP_TO_LOCAL_SHORT)       ;; 20  <-  90..97
           (word-ref BC_POP)                      ;; 22  <-  11
           (word-ref BC_CELL_EQ)                  ;; 24  <-  12 
           (word-ref BC_F_P_RET_F)        ;; 26  <-  13 
           (word-ref BC_ALLOC_A)              ;; 28  <-  14
           (word-ref BC_PUSH_AF)         ;; 2a  <-  15
           (word-ref BC_POP_TO_AF)       ;; 2c  <-  16
           (word-ref BC_PUSH_B)                   ;; 2e  <-  17
           (word-ref BC_NIL_P_RET_LOCAL_N_POP)    ;; 30  <-  98..9f
           (word-ref VM_INTERPRETER_INC_PC)       ;; 32  <-  19 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 34  <-  1a reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 36  <-  1b reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 38  <-  1c reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 3a  <-  1d reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 3c  <-  1e reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 3e  <-  1f reserved
           (word-ref BC_PUSH_LOCAL_CXR)           ;; 40  <-  a0..a7 
           (word-ref BC_NIL_P)                    ;; 42  <-  21 (RZ)
           (word-ref BC_I0_P)                  ;; 44  <-  22 
           (word-ref VM_INTERPRETER_INC_PC)       ;; 46  <-  23 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 48  <-  24 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 4a  <-  25 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 4c  <-  26 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 4e  <-  27 reserved
           (word-ref BC_CxxR)                     ;; 50  <-  a8..af 
           (word-ref VM_INTERPRETER_INC_PC)       ;; 52  <-  29 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 54  <-  2a reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 56  <-  2b reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 58  <-  2c reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 5a  <-  2d reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 5c  <-  2e reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 5e  <-  2f reserved
           (word-ref BC_XET_ARRAY_FIELD)          ;; 60  <-  b0..b7 
           (word-ref VM_INTERPRETER_INC_PC)       ;; 62  <-  31 reserved
           (word-ref BC_GOTO)                     ;; 64  <-  32 
           (word-ref BC_RET)                      ;; 66  <-  33 
           (word-ref BC_CALL)                     ;; 68  <-  34 
           (word-ref BC_TAIL_CALL)                ;; 6a  <-  35 
           (word-ref VM_INTERPRETER_INC_PC)       ;; 6c  <-  36 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 6e  <-  37 reserved
           (word-ref BC_PUSH_CONST_NUM_SHORT)     ;; 70  <-  b8..bf 
           (word-ref VM_INTERPRETER_INC_PC)       ;; 72  <-  39 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 74  <-  3a reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 76  <-  3b reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 78  <-  3c reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 7a  <-  3d reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 7c  <-  3e reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 7e  <-  3f reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 80  <-  c0..a7 reserved
           (word-ref BC_CDR)                      ;; 82  <-  41 
           (word-ref BC_CONS)                     ;; 84  <-  42 
           (word-ref BC_CAR)                      ;; 86  <-  43 
           (word-ref BC_COONS)                    ;; 88  <-  44
           (word-ref VM_INTERPRETER_INC_PC)       ;; 8a  <-  45 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 8c  <-  46 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 8e  <-  47 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 90  <-  c8..af reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 92  <-  49 reserved
           (word-ref BC_NATIVE)                   ;; 94  <-  4a reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 96  <-  4b reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 98  <-  4c reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 9a  <-  4d reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 9c  <-  4e reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 9e  <-  4f reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; a0  <-  d0..d7 reserved
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
           (word-ref BC_ISUB)                ;; c2  <-  61
           (word-ref BC_IADD)                 ;; c4  <-  62
           (word-ref BC_I_GT_P)            ;; c6  <-  63 
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
   (label VM_INTERPRETER_INC_PC_2_TIMES)
          (LDA !$02)
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
          (BCS SHORT_CMD__VM_INTERPRETER)       ;; bit7 was not set => normal command

          ;; normal bytecode command
   (label OPERAND__VM_INTERPRETER)
          (STA JMPOP__VM_INTERPRETER+1)         ;; lowbyte of the table
   (label JMPOP__VM_INTERPRETER)
          (JMP (VM_INTERPRETER_OPTABLE))        ;; jump by table

   (label SHORT_CMD__VM_INTERPRETER)
          ;; short command
          (AND !$F0)                            ;; only top 4 bits are used for the opcode dispatch!
          (STA SHORT_CMD_JMPOP__VM_INTERPRETER+1);; lowbyte of the table
          (LDA (ZP_VM_PC),y)                    ;; load byte code
          (AND !$07)                            ;; mask out lower 3 bits (of the fast command)
   (label SHORT_CMD_JMPOP__VM_INTERPRETER)
          (JMP (VM_INTERPRETER_OPTABLE))         ;; jump by table

))

(define just-vm-interpreter
  (append VM_INTERPRETER_VARIABLES
          VM_INTERPRETER_INIT
          BC_POP
          BC_POP_TO_LOCAL_SHORT
          BC_PUSH_LOCAL_SHORT
          BC_PUSH_LOCAL_CXR
          BC_PUSH_CONST_NUM_SHORT
          BC_PUSH_I
          BC_PUSH_CONST_BYTE
          BC_PUSH_NIL
          BC_NIL_P
          BC_I0_P
          BC_NIL_P_RET_LOCAL_N_POP
          BC_CONS
          BC_CAR
          BC_CDR
          BC_CxxR
          BC_COONS
          BC_CALL
          BC_RET
          BC_BRK
          BC_IADD
          BC_ISUB
          BC_INT_P
          BC_I_GT_P
          BC_TAIL_CALL
          BC_CELL_EQ
          BC_SWAP
          BC_DUP
          BC_T_P_RET
          BC_F_P_RET
          BC_CONS_PAIR_P
          BC_T_P_BRA
          BC_F_P_BRA
          BC_MAX_INT
          BC_GOTO
          BC_EXT1_CMD
          BC_INC_INT
          BC_BNOP
          BC_GC_FL
          BC_ALLOC_A
          BC_XET_ARRAY_FIELD
          BC_F_P_RET_F
          VM_REFCOUNT_DECR_CURRENT_LOCALS
          BC_PUSH_AF
          BC_POP_TO_AF
          BC_PUSH_B
          BC_NATIVE
          RETURN_TO_BC
          VM_INTERPRETER))

(define vm-interpreter
  (append just-vm-interpreter
          (list (label END__INTERPRETER))
          VM_INTERPRETER_OPTABLE_EXT1_HB
          VM_INTERPRETER_OPTABLE_EXT1_LB
          (list (org-align #x100)) ;; align to next page
          VM_INTERPRETER_OPTABLE
          (list (label END__INTERPRETER_DATA))
          vm-lists))

(module+ test #| vm-interpreter |#
  (inform-check-equal? (foldl + 0 (map command-len (flatten just-vm-interpreter)))
                       867))
