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
  NIL_P_RET_L0_POP_n   1  98+  n=1..4                     if tos is nil, pop n from eval-stack and return local0 as result (on tos)
  POP_TO_AF       1  16                             pop the cell at tos-2 into array (tos-1) at index (tos)
  POP_TO_Ln           1  90+  n=0..3                     pop tos into local#n
  PUSH_AF         1  15                             push field of the array (tos-1) at index (tos) onto eval-stack
  PUSH_B byte              2  17   byte=0..255 -128..+127     push a byte constant onto the eval-stack
  PUSH_I int               3  06   int=0..8191, -4096..4095   push integer constant onto eval-stack
  PUSH_Ii                  1  b8+  i=0,1,2,-1(m1)            push constant 0,1,2,-1 onto eval-stack
  PUSH_Ln                  1  80+  n=0..3                     push local#n onto eval-stack
  PUSH_NIL                 1  09                             push nil onto eval-stack
  RET                      1  33                             return from function
  TAIL_CALL                1  35                             tail call same function
  WRITE_Ln       1  81+  n=0..3                     write local#n into tos (overwriting old tos)
  WRITE_TO_Ln         1  91+  n=0..3                     write tos into local#n (without popping)


  (not implemented yet)
  PUSH_B byte           2  05   byte=0..255, -128..127      push byte constant onto eval-stack
  POP_n                    1       n=1..4                     pop top n values
  SET_CAR                  1                                 set car element to tos (of car-cdr-pair-ptr behind tos) and pop 2 values
  SET_CDR                  1                                 set cdr element to tos (of car-cdr-pair-ptr behind tos) and pop 2 values
|#

(require (only-in racket/list flatten take empty? range))

(require "../6510.rkt")
(require (only-in "../6510-utils.rkt" word->hex-string
                  high-byte
                  low-byte ))
(require (only-in "../util.rkt"
                  bytes->int
                  format-hex-byte
                  format-hex-word))
(require (only-in "../tools/6510-interpreter.rkt"
                  cpu-state-clock-cycles
                  peek-word-at-address))
(require (only-in "./vm-memory-map.rkt"
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
(require (only-in "./vm-mm-pages.rkt"
                  GLOBAL_CELLPAIR_FREE_LIST))
(require (only-in "./vm-inspector-utils.rkt"
                  vm-cell-at-nil?
                  vm-page->strings
                  vm-stack->strings
                  vm-regt->string
                  vm-cell-at->string
                  vm-cell->string
                  vm-deref-cell-pair-w->string))
(require (only-in "./vm-lists.rkt" vm-lists))
(require (only-in "./vm-call-frame.rkt"
                  vm-call-frame->strings
                  VM_POP_CALL_FRAME_N))
(require (only-in "./vm-mm-cell-stack.rkt"
                  PUSH_XA_TO_EVLSTK))
(require (only-in "../tools/6510-interpreter.rkt"
                  6510-load
                  6510-load-multiple
                  initialize-cpu
                  run-interpreter
                  run-interpreter-on
                  memory-list
                  cpu-state-accumulator
                  cpu-state-program-counter
                  peek))
(require (only-in "../ast/6510-resolver.rkt" add-label-suffix))

(module+ test
  (require "../6510-test-utils.rkt")
  (require (only-in "./vm-interpreter-test-utils.rkt" run-bc-wrapped-in-test- vm-next-instruction-bytes))
  (require (only-in "../ast/6510-relocator.rkt" command-len))
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

(provide vm-interpreter
         bc
         ALLOC_ARA
         POP_TO_RA_AF
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
         CELL_EQ_P
         CAAR
         CADR
         CDAR
         CDDR
         COONS
         DUP
         POP
         BNOP
         I_Z_P
         EXT
         IMAX
         IINC
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
         NIL_P_RET_L0_POP_1
         NIL_P_RET_L0_POP_2
         NIL_P_RET_L0_POP_3
         NIL_P_RET_L0_POP_4
         INT_P
         SWAP
         POP_TO_L0
         POP_TO_L1
         POP_TO_L2
         POP_TO_L3
         WRITE_TO_L0
         WRITE_TO_L1
         WRITE_TO_L2
         WRITE_TO_L3
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
         WRITE_L0
         WRITE_L1
         WRITE_L2
         WRITE_L3
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

;; (define ZERO?_RET_LOCAL0_POP_1 #x99)
;; (define ZERO?_RET_LOCAL0_POP_2 #x9b)
;; (define ZERO?_RET_LOCAL0_POP_3 #x9d)
;; (define ZERO?_RET_LOCAL0_POP_4 #x9f)

                                 ;; @DC-B: NIL_P_RET_L0_POP_1, group: return
(define NIL_P_RET_L0_POP_1 #x98) ;; *NIL* *P*​redicate *RET*​urn *L*​ocal *0* and *POP* *1* from evlstk
                                 ;; @DC-B: NIL_P_RET_L0_POP_2, group: return
(define NIL_P_RET_L0_POP_2 #x9a) ;; *NIL* *P*​redicate *RET*​urn *L*​ocal *0* and *POP* *2* from evlstk
                                 ;; @DC-B: NIL_P_RET_L0_POP_3, group: return
(define NIL_P_RET_L0_POP_3 #x9c) ;; *NIL* *P*​redicate *RET*​urn *L*​ocal *0* and *POP* *3* from evlstk
                                 ;; @DC-B: NIL_P_RET_L0_POP_4, group: return
(define NIL_P_RET_L0_POP_4 #x9e) ;; *NIL* *P*​redicate *RET*​urn *L*​ocal *0* and *POP* *4* from evlstk
(define BC_NIL_P_RET_L0_POP_N
  (add-label-suffix
   "__" "__BC_NIL_P_RET_L0_POP_N"
  (list
   (label BC_NIL_P_RET_L0_POP_N)
          (LDX ZP_RT)
          (CPX !<TAGGED_NIL)                 ;; lowbyte = tagged_nil lowbyte
          (BEQ RETURN__)                     ;; is nil => return param or local
          (JMP VM_INTERPRETER_INC_PC)        ;; next instruction

   (label RETURN__)
          (LSR)                              ;; lowest bit decides 0 = LOCAL_0, 1 = some other short command
          (TAX)
          (BCS SHORTCMD__)

   (label LOCAL_0_POP__)
          ;; local 0 is written into tos (which is one pop already)          
          ;; now pop the rest (0..3 times additionally)
   (label NOW_POP__)
          (TXA)
          (BEQ DONE__)
   (label LOOP_POP__)
          (DEC ZP_CELL_STACK_TOS)
          (LDY ZP_CELL_STACK_TOS)
          (LDA (ZP_CELL_STACK_LB_PTR),y)
          (STA ZP_RZ)
          (LDA (ZP_CELL_STACK_HB_PTR),y)
          (STA ZP_RZ+1)
          (TXA)
          (PHA)
          (JSR DEC_REFCNT_RZ)
          (PLA)
          (TAX)
          (LDY ZP_CELL_STACK_TOS)
          (CPY !$01)
          (BEQ STACK_DEPLETED__)
          (DEX)
          (BNE LOOP_POP__)

          (STY ZP_CELL_STACK_TOS)             ;; store new tos marker

   (label DONE__)
          (LDY !$00)
          (LDA (ZP_LOCALS_LB_PTR),y)          ;; load low byte from local
          (STA ZP_RT)                         ;; -> RT
          (LDA (ZP_LOCALS_HB_PTR),y)          ;; load high byte from local
          (STA ZP_RT+1)                       ;; -> RT

          (LDA !$00)
          (STA (ZP_LOCALS_LB_PTR),y)          ;; clear low byte from local
          (STA (ZP_LOCALS_HB_PTR),y)          ;; clear high byte from local
          (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)
          (JSR VM_POP_CALL_FRAME_N)           ;; now pop the call frame

          (JMP VM_INTERPRETER)                ;; and continue

   (label STACK_DEPLETED__)
          ;; (LDY !$01)                       ;; Y already is 01 when entering here
          (LDA (ZP_CELL_STACK_LB_PTR),y)      ;; get previous lb page
          (BEQ ERROR_EMPTY_STACK__)           ;; = 0 => stack ran empty

          (STA ZP_CELL_STACK_LB_PTR+1)        ;; store previous lb page to lb ptr
          (LDA (ZP_CELL_STACK_HB_PTR),y)      ;; get previous hb page
          (STA ZP_CELL_STACK_HB_PTR+1)        ;; store previous hb page into hb ptr
          (LDY !$ff)                          ;; assume $ff as new cell_stack_tos
          (BNE LOOP_POP__)                    ;; always jump


   (label SHORTCMD__)
          ;; open for other shortcut command
   (label ERROR_EMPTY_STACK__)
          (BRK))))

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
             (bc POP_TO_L0)          ;; pop tos into local 0 (now int 1)
             (bc NIL_P_RET_L0_POP_1)  ;; return local 0  if tos = nil (which it is)
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
             (bc POP_TO_L1)
             (bc POP_TO_L0)
             (bc PUSH_L1)
             (bc NIL_P_RET_L0_POP_1)     ;; return local 0 (int 1) if nil
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

;; @DC-B: TAIL_CALL, group: call
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
             (bc POP_TO_L0)
             (bc PUSH_L0)
             (bc NIL_P_RET_L0_POP_1)    ;; return param0 if nil
             (bc POP_TO_L0)
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
             (bc POP_TO_L0)        ;; b-list (#refs stay)
             (bc WRITE_TO_L1)      ;; a-list (#refs increase)
             (bc NIL_P_RET_L0_POP_1);; return b-list if a-list is nil (if popping, #refs decrease)
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
                4737)
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

;; @DC-B: CALL, group: call
(define CALL                #x34) ;; stack [int-cell: function index, cell paramN, ... cell param1, cell param0] -> [cell paramN, ... cell param1, cell param0]
(define BC_CALL
  (add-label-suffix
   "__" "__CALL"
  (list
   (label BC_CALL)
          ;; load the two bytes following into ZP_RP (ptr to function descriptor)
          (LDY !$01)
          (LDA (ZP_VM_PC),y)                    ;; load lowbyte of call target, right behind byte-code
          (STA ZP_RP)                           ;; -> RA
          (INY)
          (LDA (ZP_VM_PC),y)                    ;; load highbyte of call target, behind lowbyte
          (STA ZP_RP+1)                         ;; -> RA
          ;; RA now holds the call target function address

          ;; put return to adress into zp_vm_pc (for save)
          (LDA !$03)                            ;; call is 3 bytes long (bc + address)
          (CLC)
          (ADC ZP_VM_PC)
          (STA ZP_VM_PC)                        ;; write into program counter
          (BCC DONE_INC_PC__)
          (INC ZP_VM_PC+1)                      ;; inc page of program counter
          ;; zp_vm_pc holds follow bc after this call
   (label DONE_INC_PC__)

   (label VM_CALL_NO_PUSH_FUN_IN_RA)
          ;; ZP_RP holds pointer to function descriptor
          (LDY !$00)                            ;; index to number of locals (0)
          (LDA (ZP_RP),y)                       ;; A = #locals
          (TAX)
          (JSR VM_PUSH_CALL_FRAME_N)
          (LDY !$00)                            ;; index to number of locals (0)
          (LDA (ZP_RP),y)                       ;; A = #locals
          (AND !$0f)                            ;; mask out the number of locals
          (JSR VM_ALLOC_LOCALS)                 ;; even if A=0 will set the top_mark and the locals appropriately

          ;; load zp_vm_pc with address of function bytecode
          (LDA ZP_RP)
          (STA ZP_VM_PC)
          (STA ZP_VM_FUNC_PTR)
          (LDA ZP_RP+1)
          (STA ZP_VM_PC+1)
          (STA ZP_VM_FUNC_PTR+1)

          (JMP VM_INTERPRETER_INC_PC)))) ;; function starts at function descriptor + 1

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
  (add-label-suffix
   "__" "__VM_REFCOUNT_DECR_CURRENT_LOCALS"
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
          (BMI DONE__)
   (label LOOP__)
          (LDA (ZP_LOCALS_LB_PTR),y)
          (BEQ NEXT_ITER__)
          (STA ZP_RZ)
          (AND !$03)
          (CMP !$03)
          (BEQ S0_NEXT_ITER__) ;; definitely no pointer since lower 2 bits are set
          (LDA (ZP_LOCALS_HB_PTR),y)
          (BEQ NEXT_ITER__)       ;; definitely no pointer, since page is 00
          (STA ZP_RZ+1)
          (STY COUNTER__)
          (JSR DEC_REFCNT_RZ)
          (LDY COUNTER__)
   (label S0_NEXT_ITER__)
          (LDA !$00)
   (label NEXT_ITER__)
          (STA (ZP_LOCALS_LB_PTR),y)
          (STA (ZP_LOCALS_HB_PTR),y)
          (DEY)
          (BPL LOOP__)
   (label DONE__)
          (RTS)

   (label COUNTER__)
          (byte 0))))

;; @DC-B: RET, group: return
(define RET #x33) ;; stack [cell paramN, ... cell param1, cell param0] -> []
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


;;                           @DC-B: PUSH_L0_CAR, group: stack
(define PUSH_L0_CAR #xa0) ;; *PUSH* *L*​ocal *0* and *CAR*
;;                           @DC-B: PUSH_L1_CAR, group: stack
(define PUSH_L1_CAR #xa2) ;; *PUSH* *L*​ocal *1* and *CAR*
;;                           @DC-B: PUSH_L2_CAR, group: stack
(define PUSH_L2_CAR #xa4) ;; *PUSH* *L*​ocal *2* and *CAR*
;;                           @DC-B: PUSH_L3_CAR, group: stack
(define PUSH_L3_CAR #xa6) ;; *PUSH* *L*​ocal *3* and *CAR*

;;                           @DC-B: PUSH_L0_CDR, group: stack
(define PUSH_L0_CDR #xa1) ;; *PUSH* *L*​ocal *0* and *CDR*
;;                           @DC-B: PUSH_L1_CDR, group: stack
(define PUSH_L1_CDR #xa3) ;; *PUSH* *L*​ocal *1* and *CDR*
;;                           @DC-B: PUSH_L2_CDR, group: stack
(define PUSH_L2_CDR #xa5) ;; *PUSH* *L*​ocal *2* and *CDR*
;;                           @DC-B: PUSH_L3_CDR, group: stack
(define PUSH_L3_CDR #xa7) ;; *PUSH* *L*​ocal *3* and *CDR*

(define BC_PUSH_LOCAL_CXR
  (add-label-suffix
   "__" "__BC_PUSH_LOCAL_CXR"
  (flatten
   (list
    (label BC_PUSH_LOCAL_CXR)
           (LSR)                                ;; encoding is ---- xxxp (p=1 CDR, p=0 CAR)
           (BCS CDR__)

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

    (label CDR__)
           (STA ZP_RA)
           (JSR PUSH_RT_TO_EVLSTK_IF_NONEMPTY)
           (LDY ZP_RA) ;; index -> Y
           (LDA (ZP_LOCALS_LB_PTR),y)           ;; load low byte of local at index
           (STA ZP_RT)                                ;; low byte -> X
           (LDA (ZP_LOCALS_HB_PTR),y)           ;; load high byte of local at index -> A
           (STA ZP_RT+1)
           (JSR WRITE_CELLPAIR_RT_CELL1_TO_RT)
           (JSR INC_REFCNT_RT)
           (JMP VM_INTERPRETER_INC_PC)))))


;;                       @DC-B: PUSH_L0, group: stack
(define PUSH_L0 #x80) ;; *PUSH* *L*​ocal *0* on evlstk
;;                       @DC-B: PUSH_L1, group: stack
(define PUSH_L1 #x82) ;; *PUSH* *L*​ocal *1* on evlstk
;;                       @DC-B: PUSH_L2, group: stack
(define PUSH_L2 #x84) ;; *PUSH* *L*​ocal *2* on evlstk
;;                       @DC-B: PUSH_L3, group: stack
(define PUSH_L3 #x86) ;; *PUSH* *L*​ocal *3* on evlstk

;;                        @DC-B: WRITE_L0, group: stack
(define WRITE_L0 #x81) ;; *WRITE* *L*​ocal *0* into rt
;;                        @DC-B: WRITE_L1, group: stack
(define WRITE_L1 #x83) ;; *WRITE* *L*​ocal *1* into rt
;;                        @DC-B: WRITE_L2, group: stack
(define WRITE_L2 #x85) ;; *WRITE* *L*​ocal *2* into rt
;;                        @DC-B: WRITE_L3, group: stack
(define WRITE_L3 #x87) ;; *WRITE* *L*​ocal *3* into rt

(define BC_PUSH_LOCAL_SHORT
  (add-label-suffix
   "__" "__BC_PUSH_LOCAL_SHORT"
  (flatten
   (list
    (label BC_PUSH_LOCAL_SHORT)
           (LSR)                                ;; encoding is ---- xxxp (p=1 write local, p=0 push local)
           (BCS WRITE_FROM_LOCAL__)

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

    (label WRITE_FROM_LOCAL__)
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
           ))))


;;                         @DC-B: POP_TO_L0, group: stack
(define POP_TO_L0 #x90) ;; *POP* *TO* *L*​ocal *0* from evlstk
;;                         @DC-B: POP_TO_L1, group: stack
(define POP_TO_L1 #x92) ;; *POP* *TO* *L*​ocal *1* from evlstk
;;                         @DC-B: POP_TO_L2, group: stack
(define POP_TO_L2 #x94) ;; *POP* *TO* *L*​ocal *2* from evlstk
;;                         @DC-B: POP_TO_L3, group: stack
(define POP_TO_L3 #x96) ;; *POP* *TO* *L*​ocal *3* from evlstk

;;                         @DC-B: WRITE_TO_L0, group: stack
(define WRITE_TO_L0 #x91) ;; *WRITE* *TO* *L*​ocal *0* from evlstk
;;                         @DC-B: WRITE_TO_L1, group: stack
(define WRITE_TO_L1 #x93) ;; *WRITE* *TO* *L*​ocal *1* from evlstk
;;                         @DC-B: WRITE_TO_L2, group: stack
(define WRITE_TO_L2 #x95) ;; *WRITE* *TO* *L*​ocal *2* from evlstk
;;                         @DC-B: WRITE_TO_L3, group: stack
(define WRITE_TO_L3 #x97) ;; *WRITE* *TO* *L*​ocal *3* from evlstk

(define BC_POP_TO_LOCAL_SHORT
  (add-label-suffix
   "__" "__BC_POP_TO_LOCAL_SHORT"
  (flatten
   (list
    (label BC_POP_TO_LOCAL_SHORT)
           (LSR)                                ;; encoding is ---- xxxp (p=1 parameter, p=0 local)
           (BCS WRITE__)
       
    ;; pop to local
           (PHA)
           (TAY)                                ;; index -> Y
           ;; decrement old local
           (LDA (ZP_LOCALS_LB_PTR),y)
           (BEQ POP_NO_GC__)
           (STA ZP_RZ)
           (LDA (ZP_LOCALS_HB_PTR),y)
           (STA ZP_RZ+1)
           (JSR DEC_REFCNT_RZ)
    (label POP_NO_GC__)
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
   (label  WRITE__)
           (PHA)
           (TAY)                                ;; index -> Y

           ;; decrement old local
           (LDA (ZP_LOCALS_LB_PTR),y)
           (BEQ WRITE_NO_GC__)
           (STA ZP_RZ)
           (LDA (ZP_LOCALS_HB_PTR),y)
           (STA ZP_RZ+1)
           (JSR DEC_REFCNT_RZ)
    (label WRITE_NO_GC__)
           (PLA)
           (TAY)                                ;; index -> Y
           (LDA ZP_RT)
           (STA (ZP_LOCALS_LB_PTR),y)           ;; store low byte of local at index
           (LDA ZP_RT+1)
           (STA (ZP_LOCALS_HB_PTR),y)           ;; store high byte of local at index -> A
           ;; increment, since it is now in locals and on stack
           (JSR INC_REFCNT_RT)
           (JMP VM_INTERPRETER_INC_PC)          ;; next bc
))))

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
             (bc POP_TO_L0) ;;
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
      (bc POP_TO_L0)
      (bc POP_TO_L1)
      (bc PUSH_I1)     ;; value to return
      (bc POP_TO_L0) ;; overwrites -1
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
      (bc POP_TO_L0) ;;
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
      (bc POP_TO_L0)
      (bc POP_TO_L1)
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
      (bc POP_TO_L0)
      (bc POP_TO_L1)
      (bc PUSH_I1)     ;; value to return
      (bc POP_TO_L0) ;; overwrites -1
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


;;                        @DC-B: PUSH_I0, group: stack
(define PUSH_I0 #xb8)  ;; *PUSH* *I*​nt *0* onto evlstk
;;                        @DC-B: PUSH_I1, group: stack
(define PUSH_I1 #xb9)  ;; *PUSH* *I*​nt *1* onto evlstk
;;                        @DC-B: PUSH_I2, group: stack
(define PUSH_I2 #xba)  ;; *PUSH* *I*​nt *2* onto evlstk
;;                        @DC-B: PUSH_IM1, group: stack
(define PUSH_IM1 #xbb) ;; *PUSH* *I*​nt *-1* onto evlstk

(define BC_PUSH_CONST_NUM_SHORT
  (add-label-suffix
   "__" "__BC_PUSH_CONST_NUM_SHORT"
  (flatten
   (list
    (label BC_PUSH_CONST_NUM_SHORT)
           (ASL A)                     ;; * 2 (for 2 byte index into jump_refs)!
           (TAY)                       ;; -> Y
           (LDA REFS__,y)              ;; get lowbyte of jumpref
           (STA JSR_TARGET__+1)        ;; store into lowbyte of jsr command
           (LDA REFS__+1,y)            ;; load highbyte of jumpref
           (STA JSR_TARGET__+2)        ;; store into highbyte of jsr command
    (label JSR_TARGET__)
           (JSR PUSH_INT_0_TO_EVLSTK)  ;; execute (modified) jsr
           (JMP VM_INTERPRETER_INC_PC) ;; interpreter loop

    (label REFS__)
           (word-ref PUSH_INT_0_TO_EVLSTK)
           (word-ref PUSH_INT_1_TO_EVLSTK)
           (word-ref PUSH_INT_2_TO_EVLSTK)
           (word-ref PUSH_INT_m1_TO_EVLSTK)
           ;; (word-ref PUSH_B0)
           ;; (word-ref PUSH_B1)
           ;; (word-ref PUSH_B2)
           ;; (word-ref PUSH_Bm1)
           ))))

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

;; @DC-B: PUSH_I, group: stack
(define PUSH_I  #x06) ;; *PUSH* *I*​nt onto evlstk, op1=low byte op2=high byte, stack [] -> [cell-int]
;; len: 3
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

;; @DC-B: IADD, group: int
;; len: 1
;; stack [cell-int a, cell-int b] -> [sum]
(define IADD #x62)
(define BC_IADD
  (add-label-suffix
   "__" "__IADD"
  (list
   (label BC_IADD)
          (LDY ZP_CELL_STACK_TOS)               ;; get current index to tagged byte          
          (LDA (ZP_CELL_STACK_HB_PTR),y)        ;; A = untagged lowbyte of int (stored in high byte)
          (CLC)                                 ;; for addition the carry flags needs to be clear
          (ADC ZP_RT+1)                         ;; A = A + stack value (int low byte)
          (STA ZP_RT+1)                         ;; RT untagged lowbyte = result
          
          (LDA (ZP_CELL_STACK_LB_PTR),y)       ;; A = tagged high byte of int (stored in low byte)
          (AND !$7c)                            ;; mask out lower two and highest bit
          (BCC NO_INC_HIGH__)        ;; if previous addition had no overflow, skip inc
          (CLC)                                 ;; clear for addition
          (ADC !$04)                            ;; increment int (adding 4 into the enoded int starting at bit 2)

    (label NO_INC_HIGH__)
          (ADC ZP_RT)                           ;; A = A + stack value (int high byte)
          (AND !$7f)                            ;; since ZP_RT has the lower two bits set, just mask out the highest bit
          (STA ZP_RT)                           ;; RT tagged high byte = result

          (DEC ZP_CELL_STACK_TOS)               ;; pop value from cell-stack (leave result in RT as tos)
          (JMP VM_INTERPRETER_INC_PC))))         ;; interpreter loop

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

;; @DC-B: ISUB, group: int
(define ISUB #x61) ;; stack [cell-int a, cell-int b] -> [difference]
(define BC_ISUB
  (add-label-suffix
   "__" "__ISUB"
  (list
   (label BC_ISUB)
          (LDY ZP_CELL_STACK_TOS)               ;; get current index to tagged byte          
          (SEC)                                 ;; for subtraction carry needs to be set
          (LDA ZP_RT+1)                         ;; A = untagged lowbyte of int (stored in high byte)
          (SBC (ZP_CELL_STACK_HB_PTR),y)      ;; A = A - stack value (int low byte)
          (STA ZP_RT+1)                         ;; RT untagged lowbyte = result

          (LDA ZP_RT)                           ;; A = tagged highbyte of int (stored in low byte)
          (BCS NO_DEC_HIGH__)       ;; if carry is set from subtraction of lower bits, no subtraction carry over necessary
          (SEC)                                 ;; for subtraction carry needs to be set
          (SBC !$04)                            ;; subtract 1 in the masked int highbyte (starting at bit2) => 4

   (label NO_DEC_HIGH__)
          (SBC (ZP_CELL_STACK_LB_PTR),y)      ;; A = A - stack value (int high byte)
          (AND !$7c)                            ;; mask out under/overflow (lower two bits and high bit)
          (ORA !$03)                            ;; set lower two bits to tag it as integer value
          (STA ZP_RT)                           ;; RT tagged high byte = result
          
   (label DONE__)
          (DEC ZP_CELL_STACK_TOS)               ;; pop value from cell-stack (leave result in rt untouched)
          (JMP VM_INTERPRETER_INC_PC))))         ;; interpreter loop

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

;; @DC-B: NIL_P, group: predicates
(define NIL_P #x21) ;; stack [cell-list-ptr] -> [cell-boolean]
(define BC_NIL_P
  (list
   (label BC_NIL_P)
          (JSR CP_RT_TO_RZ)             ;; keep for dec-refcnt
          (JSR VM_NIL_P)                      ;; if rt is NIL replace with true (int 1) else replace with false (int 0)
          (JSR DEC_REFCNT_RZ)
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

;; @DC-B: COONS, group: cell_pair
(define COONS #x44)
(define BC_COONS
  (list
   (label BC_COONS)
          (JSR VM_CONS__REFCNTD)
          (JSR VM_CONS__REFCNTD)
          (JMP VM_INTERPRETER_INC_PC)))

;; @DC-B: CONS, group: cell_pair
(define CONS #x42) ;; stack [cell- car, cell-list-ptr cdr] -> stack [cell-list-ptr new-list]
(define BC_CONS
  (list
   (label BC_CONS)          
          (JSR VM_CONS__REFCNTD)
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

;; @DC-B: CAR, group: cell_pair
(define CAR #x43) ;; stack [cell-list-ptr] -> [cell- car of list pointed at]
(define BC_CAR
  (list
   (label BC_CAR)
          (JSR CP_RT_TO_RZ)
          (JSR VM_CAR)
          (JSR INC_REFCNT_RT)
          (JSR DEC_REFCNT_RZ)
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

;; @DC-B: CDR, group: cell_pair
(define CDR #x41) ;; stack [cell-list-ptr] -> [cell-list-ptr cdr of list pointed at]
(define BC_CDR
  (list
   (label BC_CDR)
          (JSR CP_RT_TO_RZ)
          (JSR VM_CDR)
          (JSR INC_REFCNT_RT)
          (JSR DEC_REFCNT_RZ)
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

;; @DC-B: SWAP, group: stack
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

;; @DC-B: I_GT_P, group: predicates
(define I_GT_P #x63) ;; *I*​nt *G*​reater *T*​han *P*​redicates
(define BC_I_GT_P
  (add-label-suffix
   "__" "__I_GT_P"
  (list
   (label BC_I_GT_P)
          (LDA ZP_RT)
          (STA ZP_RP)
          (LDA ZP_RT+1)
          (STA ZP_RP+1)
          (JSR POP_CELL_EVLSTK_TO_RT)
          (LDA ZP_RT)
          (CMP ZP_RP)
          (BMI GREATER__)
          (BNE LESS_OR_EQUAL__)
          (LDA ZP_RT+1)
          (CMP ZP_RP+1)
          (BMI GREATER__)
   (label LESS_OR_EQUAL__)
          (JSR WRITE_INT0_TO_RT)
          (JMP VM_INTERPRETER_INC_PC)
    (label GREATER__)
          (JSR WRITE_INT1_TO_RT)
          (JMP VM_INTERPRETER_INC_PC))))

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
  (add-label-suffix
   "__" "__INT_P"
  (list
   (label BC_INT_P)
          (LDA ZP_RT)
          (LDX !$01)
          (AND !$83)
          (CMP !$03)
          (BEQ IS_INT__)
          (JSR DEC_REFCNT_RT)
          (LDA !$03)
          (LDX !$00)
   (label IS_INT__)
          (STA ZP_RT)
          (STX ZP_RT+1)
          (JMP VM_INTERPRETER_INC_PC))))

(module+ test #| int? |#
  (skip (check-equal? #t #f "implement")))

;; @DC-B: F_P_RET_F, group: return
(define F_P_RET_F #x13) ;; *F*​alse *P*​redicate *RET*​urn *F*​alse
(define BC_F_P_RET_F
  (add-label-suffix
   "__" "__F_P_RET_F"
  (list
   (label BC_F_P_RET_F)
          (LDA ZP_RT+1)
          (BNE IS_TRUE__)
          ;; don't pop false value, return it!
          (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)
          (JSR VM_POP_CALL_FRAME_N)             ;; now pop the call frame
          (JMP VM_INTERPRETER)
   (label IS_TRUE__)
          (JSR POP_CELL_EVLSTK_TO_RT)
          (JMP VM_INTERPRETER_INC_PC))))

;; @DC-B: F_P_RET, group: return
(define F_P_RET #x0e) ;; *F*​alse *P*​redicate *RET*​urn
(define BC_F_P_RET
  (add-label-suffix
   "__" "__F_P_RET"
  (list
   (label BC_F_P_RET)
          (LDA ZP_RT+1)
          (BNE IS_TRUE__)
          (JSR POP_CELL_EVLSTK_TO_RT)
          (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)
          (JSR VM_POP_CALL_FRAME_N)             ;; now pop the call frame
          (JMP VM_INTERPRETER)
   (label IS_TRUE__)
          (JSR POP_CELL_EVLSTK_TO_RT)
          (JMP VM_INTERPRETER_INC_PC))))

;; @DC-B: T_P_RET, group: return
;; *T*​rue *P*​redicate *RET*​urn
;; len: 1
(define T_P_RET #x0b)
(define BC_T_P_RET
  (add-label-suffix
   "__" "__BC_T_P_RET"
  (list
   (label BC_T_P_RET)
          (LDA ZP_RT+1)
          (BEQ IS_FALSE__)
          (JSR POP_CELL_EVLSTK_TO_RT)
          (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)
          (JSR VM_POP_CALL_FRAME_N)             ;; now pop the call frame
          (JMP VM_INTERPRETER)
   (label IS_FALSE__)
          (JSR POP_CELL_EVLSTK_TO_RT)
          (JMP VM_INTERPRETER_INC_PC))))

;; @DC-B: GOTO, group: flow
;; goto relative by byte following in code
;; len: 2
(define GOTO #x32) ;; op = relative offset
(define BC_GOTO
  (add-label-suffix
   "__" "__GOTO"
  (list
   (label BC_GOTO)
          (CLC)
          (LDY !$01)
          (LDA (ZP_VM_PC),y)
          (BMI JUMP_BACK__)

          (ADC !$02)
          (JMP VM_INTERPRETER_INC_PC_A_TIMES)

   (label JUMP_BACK__)
          (ADC ZP_VM_PC)
          (STA ZP_VM_PC)
          (BCS NO_PAGE_CHANGE_ON_BACK__)
          (DEC ZP_VM_PC+1)
   (label NO_PAGE_CHANGE_ON_BACK__)
          (JMP VM_INTERPRETER))))

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

;; @DC-B: F_P_BRA, group: flow
;; *F*​alse *P*​redicate *BRA*​nch
;; len: 1
(define F_P_BRA #x0d)
(define BC_F_P_BRA
  (list
   (label BC_F_P_BRA)
          (CLC)
          (LDA ZP_RT+1)
          (BEQ BRANCH_BY_NEXT_BYTE)
          (LDA !$00)
          (BEQ POP_AND_CONTINUE_AFTER_BRA)))

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

;; @DC-B: T_P_BRA, group: flow
;; *T*​rue *P*​redicate *BRA*​nch
;; len: 2
(define T_P_BRA #x0c)
(define BC_T_P_BRA
  (add-label-suffix
   "__" "__T_P_BRA"
  (list
   (label BC_T_P_BRA)
          (CLC)
          (LDA ZP_RT+1)
          (BEQ POP_AND_CONTINUE_AFTER_BRA) ;; when false (A = 0), just continue, no branch

   (label BRANCH_BY_NEXT_BYTE)
   ;; branch by adding second byte code
          (LDY !$01)
          (LDA (ZP_VM_PC),y)
          (BMI NEGATIVE_BRANCH__)

   (label POP_AND_CONTINUE_AFTER_BRA)
          (ADC !$02)
          (ADC ZP_VM_PC)
          (STA ZP_VM_PC)
          (BCC NO_PAGE_CHANGE__)
          (INC ZP_VM_PC+1)
   (label NO_PAGE_CHANGE__)
          (JSR POP_CELL_EVLSTK_TO_RT)
          (JMP VM_INTERPRETER)

   (label NEGATIVE_BRANCH__)
          (ADC ZP_VM_PC)
          (STA ZP_VM_PC)
          (BCS NO_PAGE_CHANGE_ON_BACK__)
          (DEC ZP_VM_PC+1)
   (label NO_PAGE_CHANGE_ON_BACK__)
          (JSR POP_CELL_EVLSTK_TO_RT)
          (JMP VM_INTERPRETER))))

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

;; @DC-B: CONS_PAIR_P, group: predicates
;; *CONS* *PAIR* *P*​redicate
;; len: 1
(define CONS_PAIR_P #x0a)
(define BC_CONS_PAIR_P
  (list
   (label BC_CONS_PAIR_P)
          (JSR CP_RT_TO_RZ)

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
          (JSR DEC_REFCNT_RZ)
          (JMP VM_INTERPRETER_INC_PC)))

;; @DC-B: PUSH_NIL, group: stack
;; *PUSH* *NIL* to evlstk
;; len: 1
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

;; @DC-B: IINC, group: int
;; *I*​nt *INC*​rement
;; len: 2 (extended)
(define IINC #x02) ;; extended (could be mapped to regular byte code, if needed very often!)
(define BC_IINC
  (add-label-suffix
   "__" "__IINC"
  (list
   (label BC_IINC)
          (INC ZP_RT+1)
          (BNE DONE__)
          (INC ZP_RT)
          (LDA ZP_RT)
          (ORA !$03)
          (AND !$7f)
          (STA ZP_RT)
   (label DONE__)
          (JMP VM_INTERPRETER_INC_PC_2_TIMES))))

(module+ test #| inc int |#
  (define inc-int-0-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc EXT)
      (bc IINC))))

  (check-equal? (vm-stack->strings inc-int-0-state)
                (list "stack holds 1 item"
                      "int $0001  (rt)"))

  (define inc-int-1-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I) (byte 255) (byte 0)
      (bc EXT)
      (bc IINC))
     ))

  (check-equal? (vm-stack->strings inc-int-1-state)
                (list "stack holds 1 item"
                      "int $0100  (rt)"))

  (define inc-int-2-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_IM1)
      (bc EXT)
      (bc IINC))
     ))

  (check-equal? (vm-stack->strings inc-int-2-state)
                (list "stack holds 1 item"
                      "int $0000  (rt)"))

  (define inc-int-3-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I) (byte 255) (byte 05)
      (bc EXT)
      (bc IINC))
     ))

  (check-equal? (vm-stack->strings inc-int-3-state)
                (list "stack holds 1 item"
                      "int $0600  (rt)")))

;; @DC-B: IMAX, group: int
;; *I*​nt *MAX*​imum, return the maximum of two ints
;; len: 2 (extended)
(define IMAX #x01) ;; extended
(define BC_IMAX
  (add-label-suffix
   "__" "__IMAX"
  (list
   (label BC_IMAX)
          (LDY ZP_CELL_STACK_TOS)

          ;; compare high byte of int (which is lb)
          (LDA (ZP_CELL_STACK_LB_PTR),y)
          (CMP ZP_RT)
          (BNE NO_OTHER_COMPARE__) ;; already different => no need to compare low byte

          ;; compare low byte of int (which is hb)
          (LDA (ZP_CELL_STACK_HB_PTR),y)
          (CMP ZP_RT+1)

   (label NO_OTHER_COMPARE__)
          (BMI KEEP_RT__)

          (JSR POP_CELL_EVLSTK_TO_RT)     ;; pop RT and move TOS into RT
          (JMP VM_INTERPRETER_INC_PC_2_TIMES)

    (label KEEP_RT__)
          (DEC ZP_CELL_STACK_TOS) ;; just pop but keep RT, since INT no GC necessary
          (JMP VM_INTERPRETER_INC_PC_2_TIMES))))

;; @DC-B: DUP, group: stack
;; *DUP*​licate top of stack
;; len: 1
(define DUP #x0f)
(define BC_DUP
  (list
   (label BC_DUP)
          (JSR INC_REFCNT_RT)
          (JSR PUSH_RT_TO_EVLSTK_IF_NONEMPTY)
          (JMP VM_INTERPRETER_INC_PC)))

;; @DC-B: CELL_EQ_P, group: stack
;; *CELL* *EQ*​ual *P*​redicate
;; len: 1
(define CELL_EQ_P #x12)
(define BC_CELL_EQ_P
  (add-label-suffix
   "__" "__CELL_EQ_P"
  (list
   (label BC_CELL_EQ_P)
          (LDY ZP_CELL_STACK_TOS)
          (LDA (ZP_CELL_STACK_HB_PTR),y)
          (STA ZP_RZ+1)
          (CMP ZP_RT+1)
          (BNE NE_LB__)
          (LDA (ZP_CELL_STACK_LB_PTR),y)
          (STA ZP_RZ)
          (CMP ZP_RT)
          (BNE NE__)

          (JSR DEC_REFCNT_RT)
          (JSR DEC_REFCNT_RZ)
          (DEC ZP_CELL_STACK_TOS)
          (JSR WRITE_INT1_TO_RT)
          (JMP VM_INTERPRETER_INC_PC)

   (label NE_LB__)
          (LDA (ZP_CELL_STACK_LB_PTR),y)
          (STA ZP_RZ)
   (label NE__)
          (JSR DEC_REFCNT_RT)
          (JSR DEC_REFCNT_RZ)
          (DEC ZP_CELL_STACK_TOS)
          (JSR WRITE_INT0_TO_RT)
          (JMP VM_INTERPRETER_INC_PC))))

;; @DC-B: I_Z_P, group: predicates
;; *I*​nt *Z*​ero *P*​redicate
;; len: 1
(define I_Z_P #x22)
(define BC_I_Z_P
  (add-label-suffix
   "__" "__I_Z_P"
  (list
   (label BC_I_Z_P)
          (LDA ZP_RT+1)
          (BNE IS_NOT_ZERO__)
          (LDA ZP_RT)
          (CMP !$03)
          (BEQ IS_ZERO__)

   (label IS_NOT_ZERO__)
          (LDA !$00)
          (STA ZP_RT+1)
          (LDA !$03)
          (STA ZP_RT)
          (JMP VM_INTERPRETER_INC_PC)

   (label IS_ZERO__)
          (LDA !$01)    
          (STA ZP_RT+1)
          (JMP VM_INTERPRETER_INC_PC))))

;; @DC-B: GC_FL, group: gc
;; garbage collect the freelist
;; len: 2 (extended)
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
           (byte-ref <BC_IMAX)                ;; 01
           (byte-ref <BC_IINC)                ;; 02
           (byte-ref <BC_GC_FL)                  ;; 03
           )))

(define VM_INTERPRETER_OPTABLE_EXT1_HB
  (flatten
   (list
    (label VM_INTERPRETER_OPTABLE_EXT1_HB)
           (byte-ref >VM_INTERPRETER_INC_PC)     ;; 00 - reserved (could be used for another extension command)
           (byte-ref >BC_IMAX)                ;; 01
           (byte-ref >BC_IINC)                ;; 02
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
      (bc IMAX))))

  (check-equal? (vm-stack->strings max-int-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)"))

  (define max-int-2-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I1)
      (bc PUSH_I2)
      (bc EXT)
      (bc IMAX))))

  (check-equal? (vm-stack->strings max-int-2-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)")))

;; @DC-B: BNOP, misc
;; *N*​o *OP*​eration
;; len: 1
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


;; @DC-B: POP_TO_RA, group: stack
;; *POP* top of evlstk *TO* *RA*, setting RAI=0
;; len: 1
(define POP_TO_RA #x4b)
;; @DC-B: POP, group: stack
;; len: 1
(define POP #x11)
(define BC_POP
  (list
   (label BC_POP_TO_RA)
          (LDA !$00)
          (STA ZP_RAI)          ;; initialize index to 0
          (JSR CP_RT_TO_RA)     ;; copy tos to ra

   (label BC_POP) ;;--------------------------------------------------------------------------------
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

;;                    @DC-B: CAAR, group: cell-pair
(define CAAR #xa8) ;; len: 1
;;                    @DC-B: CADR, group: cell-pair
(define CADR #xaa) ;; len: 1
;;                    @DC-B: CDAR, group: cell-pair
(define CDAR #xac) ;; len: 1
;;                    @DC-B: CDDR, group: cell-pair
(define CDDR #xae) ;; len: 1
(define BC_CxxR
  (list
   (label BC_CxxR)
          (LDX ZP_RT)
          (STX ZP_RZ)
          (LDX ZP_RT+1)
          (STX ZP_RZ+1)
          (JSR VM_CxxR)
          (JSR INC_REFCNT_RT)
          (JSR DEC_REFCNT_RZ)
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

;; @DC-B: PUSH_B
;; *PUSH* *B*​yte, following the instruction
;; len: 2
(define PUSH_B #x17)
(define BC_PUSH_B
  (list
   (label BC_PUSH_B)
          (LDY !$01)
          (LDA (ZP_VM_PC),y)
          (LDX !$ff)
          (JSR PUSH_XA_TO_EVLSTK)
          (JMP VM_INTERPRETER_INC_PC_2_TIMES)))

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

;; @DC-B: POP_TO_RA_AF, group: cell-array
;; *POP* top of evlstk *TO* *RA* *A*​rray *F*​ield
;; len: 1
(define POP_TO_RA_AF #x4e)
(define BC_POP_TO_RA_AF
  (list
   (label BC_POP_TO_RA_AF)
          (LDA ZP_RAI)
          (JSR POP_EVLSTK_TO_ARR_ATa_RA) ;; array@a <- rt (TODO: check that old value is dec-refcnt'd)
          (INC ZP_RAI)
          (JMP VM_INTERPRETER_INC_PC)))

;; @DC-B: POP_TO_AF, group: cell-array
;; *POP* *TO* *A*​rray *F*​ield using the stack
;; len: 1
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

;; alternative coding
;; [x] POP_TO_RA, writes tos into RA and initializes RAi to 0

;; maybe extended commands
;; POP_TO_RB
;; POP_TO_RC

;; [x] ALLOC_ARA, allocate array of size (tos) into RA, (?and push it onto the stack?), and initilaize rai to 0

;; [x] PUSH_RA_AF         ;; push cell from array RA @ RAI onto stack

;; [x] POP_TO_RA_AF       ;; pop tos cell into array RA @ RAI
;; WRITE_L0_TO_RA_AF  ;; no stack change
;; WRITE_RA_AF_TO_L0

;; BINC_RAI           ;; byte increment RAi(ndex)
;; BDEC_RAI
;; BADD_RAI
;; BSUB_RAI
;; CP_RAI_TO_RBI

;; WRITE_RA_TO_L0
;; WRITE_RAI_TO_L0
;; WRITE_RA_TO_L1
;; WRITE_RA_TO_L2
;; WRITE_RA_TO_L3

;; WRITE_L0_TO_RA
;; WRITE_L1_TO_RA
;; WRITE_L2_TO_RA
;; WRITE_L3_TO_RA


;; maybe extended commands
;; CP_RA_TO_RB copy array pointer and index
;; CP_RB_TO_RA
;; CP_RA_TO_RC
;; CP_RC_TO_RA
;; CP_RB_TO_RC
;; CP_RC_TO_RA

(define PUSH_RA_AF #x4d)
(define BC_PUSH_RA_AF
  (list
   (label BC_PUSH_RA_AF)
          (JSR PUSH_RT_TO_EVLSTK_IF_NONEMPTY)
          (LDA ZP_RAI)
          (JSR WRITE_ARR_ATa_RA_TO_RT)
          (JSR INC_REFCNT_RT)
          (JMP VM_INTERPRETER_INC_PC)))


(define GET_ARRAY_FIELD_0 #xb0) ;; stack: [array-ptr] -> [cell@0 of array]
(define GET_ARRAY_FIELD_1 #xb2) ;; stack: [array-ptr] -> [cell@1 of array]
(define GET_ARRAY_FIELD_2 #xb4) ;; stack: [array-ptr] -> [cell@2 of array]
(define GET_ARRAY_FIELD_3 #xb6) ;; stack: [array-ptr] -> [cell@3 of array]

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

(define ALLOC_ARA #x4c)
(define BC_ALLOC_ARA
  (list
   (label BC_ALLOC_ARA)
          (LDA ZP_RT+1)                 ;; byte size
          (JSR ALLOC_CELLARR_TO_RA)     ;;
          (JSR INC_REFCNT_M1_SLOT_RA)   ;; only cell-array needs to be inc-refcnt'd
          (LDA !$00)
          (STA ZP_RAI)
          (JSR CP_RA_TO_RT)
          (JMP VM_INTERPRETER_INC_PC)))

(define BINC_RAI #x49)
(define BC_BINC_RAI
  (list
   (label BC_BINC_RAI)
          (INC ZP_RAI)
          (JMP VM_INTERPRETER_INC_PC)))

(define POP_TO_RAI #x4f)
(define BC_POP_TO_RAI
  (list
   (label BC_POP_TO_RAI)
          (LDA ZP_RT+1)
          (STA ZP_RAI)
          (JMP VM_INTERPRETER_INC_PC)))

;; @DC-B: ALLOC_A, group: cell_array
;; allocate a cell-array of A size into RA
;; len: 1
;; stack: size (byte)
;; ->     cell-ptr -> cell-array
(define ALLOC_A #x14)
(define BC_ALLOC_A
  (list
   (label BC_ALLOC_A)
          (LDA ZP_RT+1)                 ;; byte size
          (JSR ALLOC_CELLARR_TO_RA)     ;;
          (JSR INC_REFCNT_M1_SLOT_RA)   ;; only cell-array needs to be inc-refcnt'd
          (JSR CP_RA_TO_RT)             ;; overwrite byte on stack
          (JMP VM_INTERPRETER_INC_PC)))


;; @DC-B: NATIVE, group: misc
;; following bytes are native 6510 commands, JSR RETURN_TO_BC ends this sequence
;; len: 1
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

(module+ test #| bdec |#
  (define native-return-test
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_B) (byte #x14)
       (bc NATIVE)
       (INC ZP_RT+1)
       (JSR RETURN_TO_BC)
       (bc PUSH_B) (byte #x16)))))

  (check-equal? (vm-stack->strings native-return-test)
                (list "stack holds 2 items"
                      "byte $16  (rt)"
                      "byte $15")))

;; @DC-B: BDEC, group: byte
;; *B*​yte *DEC*​rement, increment byte RT (no checks)
;; len: 1
(define BDEC #x1a)
(define BC_BDEC
  (list
   (label BC_BDEC)
   (DEC ZP_RT+1)
   (JMP VM_INTERPRETER_INC_PC)))

(module+ test #| bdec |#
  (define bdec-20
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_B) (byte #x14)
       (bc BDEC)))))

  (check-equal? (vm-stack->strings bdec-20)
                (list "stack holds 1 item"
                      "byte $13  (rt)")))

;; @DC-B: BINC, group: byte
;; *B*​yte *INC*​rement, increment byte RT (no checks)
;; len: 1
(define BINC #x1c)
(define BC_BINC
  (list
   (label BC_BINC)
   (INC ZP_RT+1)
   (JMP VM_INTERPRETER_INC_PC)))

(module+ test #| bdec |#
  (define binc-20
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_B) (byte #x14)
       (bc BINC)))))

  (check-equal? (vm-stack->strings binc-20)
                (list "stack holds 1 item"
                      "byte $15  (rt)")))

;; @DC-B: Z_P_RET_POP_0, group: return
(define Z_P_RET_POP_0 #xc1) ;; *Z*​ero *P*​redicate then *RET*​urn and *POP*, if rt holds byte = 0 or int = 0 return without popping anything
                            ;; len: 1
                            ;; @DC-B: Z_P_RET_POP_1, group: return
(define Z_P_RET_POP_1 #xc3) ;; *Z*​ero *P*​redicate then *RET*​urn and *POP*, if rt holds byte = 0 or int = 0 return, popping 1 value from evlstk
                            ;; len: 1
                            ;; @DC-B: Z_P_RET_POP_2, group: return
(define Z_P_RET_POP_2 #xc5) ;; *Z*​ero *P*​redicate then *RET*​urn and *POP*, if rt holds byte = 0 or int = 0 return, popping 2 values from evlstk
                            ;; len: 1
                            ;; @DC-B: Z_P_RET_POP_3, group: return
(define Z_P_RET_POP_3 #xc7) ;; *Z*​ero *P*​redicate then *RET*​urn and *POP*, if rt holds byte = 0 or int = 0 return, popping 3 values from evlstk
                            ;; len: 1
                            ;; @DC-B: NZ_P_RET_POP_0, group: return
(define NZ_P_RET_POP_0 #xc0) ;; *N*​ot *Z*​ero *P*​redicate then *RET*​urn and *POP*, if rt does hold byte != 0 or int != 0 return without popping anything
                             ;; len: 1
                             ;; @DC-B: NZ_P_RET_POP_1, group: return
(define NZ_P_RET_POP_1 #xc2) ;; *N*​ot *Z*​ero *P*​redicate then *RET*​urn and *POP*, if rt does hold byte != 0 or int != 0 return, popping 1 value from evlstk
                             ;; len: 1
                             ;; @DC-B: NZ_P_RET_POP_2, group: return
(define NZ_P_RET_POP_2 #xc4) ;; *N*​ot *Z*​ero *P*​redicate then *RET*​urn and *POP*, if rt does hold byte != 0 or int != 0 return, popping 2 values from evlstk
                             ;; len: 1
                             ;; @DC-B: NZ_P_RET_POP_3, group: return
(define NZ_P_RET_POP_3 #xc6) ;; *N*​ot *Z*​ero *P*​redicate then *RET*​urn and *POP*, if rt does hold byte != 0 or int != 0 return, popping 3 values from evlstk
                             ;; len: 1
(define BC_Z_P_RET_POP_N
  (add-label-suffix
   "__" "BC_Z_P_RET_POP_N"
   (list
    (label BC_Z_P_RET_POP_N)
           (LSR)                        ;; number to pop
           (BCC NOT_ZERO__)
           (LDX ZP_RT+1)
           (BNE DONE__)                 ;; tos != byte 0 => do nothing
           (LDX ZP_RT)
           (CPX !>TAGGED_INT_0)
           (BEQ POP_N_RET__)            ;; tos = int 0 => pop and return
           (CPX !TAG_BYTE_BYTE_CELL)
           (BNE DONE__)                 ;; tos is neither int nor byte => do nothing

    (label POP_N_RET__)
           (CMP !$00)                   ;;
           (BEQ RET__)                  ;; nothing to pop -> just return
           (STA COUNT__)                ;; keep count to pop
    (label POP_LOOP__)
           (JSR DEC_REFCNT_RT)
           (JSR POP_CELL_EVLSTK_TO_RT)
           (DEC COUNT__)
           (BNE POP_LOOP__)
    (label RET__)
           (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)
           (JSR VM_POP_CALL_FRAME_N)                     ;; now pop the call frame

    (label DONE__)
           (JMP VM_INTERPRETER_INC_PC)

    (label NOT_ZERO__)
           (LDX ZP_RT+1)
           (BEQ DONE__)                 ;; tos (hb) = 0 => do nothing, cannot be byte 0 nor int 0
           (LDX ZP_RT)
           (CPX !TAG_BYTE_BYTE_CELL)
           (BEQ POP_N_RET__)            ;; is a byte cell (and high byte !=0) -> do pop and return
           (CPX !>TAGGED_INT_0)
           (BNE POP_N_RET__)            ;; tos = int != 0 => pop and return
           (JMP VM_INTERPRETER_INC_PC)  ;; tos is int 0 => do nothing

    ;; type specialized method (byte)
    (label BC_BZ_P_RET_POP_N)
           (LSR)                        ;; number to pop
           (BCC BYTE_NOT_ZERO__)
           (LDX ZP_RT+1)
           (BEQ POP_N_RET__)
           (JMP VM_INTERPRETER_INC_PC)

    (label BYTE_NOT_ZERO__)
           (LDX ZP_RT+1)
           (BNE POP_N_RET__)
           (JMP VM_INTERPRETER_INC_PC)

    ;; type specialized method (int)
    (label BC_IZ_P_RET_POP_N)
           (LSR)                        ;; number to pop
           (BCC INT_NOT_ZERO__)

           (LDX ZP_RT+1)
           (BNE DONE__)
           (LDX ZP_RT)
           (CMP !>TAGGED_INT_0)
           (BEQ POP_N_RET__)
           (JMP VM_INTERPRETER_INC_PC)

    (label INT_NOT_ZERO__)
           (LDX ZP_RT+1)
           (BNE POP_N_RET__)
           (LDX ZP_RT)
           (CPX !>TAGGED_INT_0)
           (BNE POP_N_RET__)
           (JMP VM_INTERPRETER_INC_PC)

    (label COUNT__)
           (byte 0))))

(define Z_P_BRA #x1b)
(define BC_Z_P_BRA
  (flatten
   (list
    (label BC_Z_P_BRA)
           (LDX ZP_RT+1)
           (BNE NO_BRA__BC_Z_P_BRA)
           (LDX ZP_RT)
           (CPX !$03)
           (BEQ BRA__BC_Z_P_BRA)
           (CPX !TAG_BYTE_BYTE_CELL)
           (BNE NO_BRA__BC_Z_P_BRA)
    (label BRA__BC_Z_P_BRA)
           (JSR POP_CELL_EVLSTK_TO_RT)
           (JMP BRANCH_BY_NEXT_BYTE)
    (label NO_BRA__BC_Z_P_BRA)
           (JMP VM_INTERPRETER_INC_PC_2_TIMES))))

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
           (word-ref VM_INTERPRETER_INC_PC)       ;; 0a  <-  05 reserved
           (word-ref BC_PUSH_I)                   ;; 0c  <-  06
           (word-ref BC_INT_P)                    ;; 0e  <-  07 
           (word-ref VM_INTERPRETER_INC_PC)       ;; 10  <-  88..8F reserved
           (word-ref BC_PUSH_NIL)                 ;; 12  <-  09
           (word-ref BC_CONS_PAIR_P)              ;; 14  <-  0a 
           (word-ref BC_T_P_RET)                  ;; 16  <-  0b
           (word-ref BC_T_P_BRA)                  ;; 18  <-  0c
           (word-ref BC_F_P_BRA)                  ;; 1a  <-  0d
           (word-ref BC_F_P_RET)                  ;; 1c  <-  0e
           (word-ref BC_DUP)                      ;; 1e  <-  0f 
           (word-ref BC_POP_TO_LOCAL_SHORT)       ;; 20  <-  90..97
           (word-ref BC_POP)                      ;; 22  <-  11
           (word-ref BC_CELL_EQ_P)                  ;; 24  <-  12 
           (word-ref BC_F_P_RET_F)                ;; 26  <-  13
           (word-ref BC_ALLOC_A)                  ;; 28  <-  14
           (word-ref BC_PUSH_AF)                  ;; 2a  <-  15
           (word-ref BC_POP_TO_AF)                ;; 2c  <-  16
           (word-ref BC_PUSH_B)                   ;; 2e  <-  17
           (word-ref BC_NIL_P_RET_L0_POP_N)       ;; 30  <-  98..9f
           (word-ref VM_INTERPRETER_INC_PC)       ;; 32  <-  19 reserved
           (word-ref BC_BDEC)                     ;; 34  <-  1a
           (word-ref BC_Z_P_BRA)                  ;; 36  <-  1b
           (word-ref BC_BINC)                     ;; 38  <-  1c
           (word-ref VM_INTERPRETER_INC_PC)       ;; 3a  <-  1d reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 3c  <-  1e reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 3e  <-  1f reserved
           (word-ref BC_PUSH_LOCAL_CXR)           ;; 40  <-  a0..a7 
           (word-ref BC_NIL_P)                    ;; 42  <-  21 (RZ)
           (word-ref BC_I_Z_P)                     ;; 44  <-  22
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
           (word-ref BC_Z_P_RET_POP_N)            ;; 80  <-  c0..c7
           (word-ref BC_CDR)                      ;; 82  <-  41
           (word-ref BC_CONS)                     ;; 84  <-  42 
           (word-ref BC_CAR)                      ;; 86  <-  43 
           (word-ref BC_COONS)                    ;; 88  <-  44
           (word-ref VM_INTERPRETER_INC_PC)       ;; 8a  <-  45 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 8c  <-  46 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 8e  <-  47 reserved
           (word-ref VM_INTERPRETER_INC_PC)       ;; 90  <-  c8..cf reserved
           (word-ref BC_BINC_RAI)                 ;; 92  <-  49 reserved
           (word-ref BC_NATIVE)                   ;; 94  <-  4a reserved
           (word-ref BC_POP_TO_RA)                ;; 96  <-  4b reserved
           (word-ref BC_ALLOC_ARA)                ;; 98  <-  4c reserved
           (word-ref BC_PUSH_RA_AF)               ;; 9a  <-  4d reserved
           (word-ref BC_POP_TO_RA_AF)             ;; 9c  <-  4e reserved
           (word-ref BC_POP_TO_RAI)               ;; 9e  <-  4f reserved
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
           (word-ref BC_ISUB)                     ;; c2  <-  61
           (word-ref BC_IADD)                     ;; c4  <-  62
           (word-ref BC_I_GT_P)                   ;; c6  <-  63
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
          BC_PUSH_NIL
          BC_NIL_P
          BC_I_Z_P
          BC_NIL_P_RET_L0_POP_N
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
          BC_CELL_EQ_P
          BC_SWAP
          BC_DUP
          BC_T_P_RET
          BC_F_P_RET
          BC_CONS_PAIR_P
          BC_T_P_BRA
          BC_F_P_BRA
          BC_IMAX
          BC_GOTO
          BC_EXT1_CMD
          BC_IINC
          BC_BNOP
          BC_GC_FL
          BC_ALLOC_A
          BC_ALLOC_ARA
          BC_XET_ARRAY_FIELD
          BC_F_P_RET_F
          VM_REFCOUNT_DECR_CURRENT_LOCALS
          BC_PUSH_AF
          BC_POP_TO_AF
          BC_PUSH_B
          BC_NATIVE
          RETURN_TO_BC
          BC_BINC_RAI
          BC_PUSH_RA_AF
          BC_POP_TO_RA_AF
          BC_POP_TO_RAI
          BC_BDEC
          BC_BINC
          BC_Z_P_BRA
          BC_Z_P_RET_POP_N
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
                       1001))
