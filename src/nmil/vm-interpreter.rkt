#lang racket/base

#|

implementation of a byte code interpreter completely in 6510 assembler
this is a proof of concept and used to identify problems in the architecture of the overall implementation.
if something cannot be elegantly implemented using 6510 assembler, some redesign has to take place.

|#


;; TODO: implement ~/repo/+1/6510/mil.readlist.org::*what part of the 6510 vm design should be implement w/ racket to validate design?
;; TODO: implement constant pool
;; TODO: implement structure creation
;; TODO: implement strings
;; TODO: implement string-operations and output

;; PLANNED: harmonize virtual byte code machine with this implementation?

;; IDEA: implement exact numbers (as list of bcd digits e.g. 3 bcds in 16 bit?)

(require (only-in racket/list flatten take empty? range drop))

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
                  ZP_RA
                  ZP_VM_PC
                  ZP_LOCALS_LB_PTR
                  ZP_LOCALS_HB_PTR
                  ZP_VM_FUNC_PTR
                  ZP_CALL_FRAME
                  ZP_CELL_STACK_TOS
                  ZP_CELL_STACK_LB_PTR
                  ZP_CELL_STACK_HB_PTR))
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
                  VM_POP_CALL_FRAME_N
                  VM_REFCOUNT_DECR_CURRENT_LOCALS))
(require (only-in "./vm-mm-cell-stack.rkt"
                  PUSH_XA_TO_EVLSTK
                  POP_CELL_EVLSTK_TO_RT))
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
(require (only-in "./vm-interpreter-bc.rkt"
                  BC_PUSH_LOCAL_SHORT
                  BC_EXT1_CMD
                  PUSH_RT_WRITE_LOCAL_bc_enc))
(require (only-in "./vm-bc-opcode-definitions.rkt"
                  full-extended-optable-lb
                  full-extended-optable-hb
                  full-interpreter-opcode-table))
(require (only-in "./vm-interpreter-loop.rkt"
                  VM_INTERPRETER
                  VM_INTERPRETER_INIT))
(require (only-in "./vm-interpreter-bc.arrays.rkt"
                  BC_DEC_RBI_NZ_P_BRA
                  BC_DEC_RAI
                  BC_WRITE_TO_RBI
                  BC_WRITE_TO_RAI
                  BC_POP_TO_RAI
                  BC_BINC_RAI
                  BC_ALLOC_ARA
                  BC_XET_RA_ARRAY_FIELD
                  BC_GET_RA_ARRAY_FIELD
                  BC_SET_RA_ARRAY_FIELD
                  BC_GET_ARRAY_FIELD
                  BC_SET_ARRAY_FIELD
                  BC_XET_ARRAY_FIELD
                  BC_WRITE_RA                    ;; write cell-array register RA into tos
                  BC_PUSH_RA                     ;; push cell-array register RA itself onto eval stack
                  BC_PUSH_RA_AF                  ;; push cell-array RA field A onto the eval stack (inc ref count)
                  BC_POP_TO_RA_AF
                  BC_PUSH_AF
                  BC_POP_TO_AF))
(require (only-in "./vm-interpreter-bc.atom-num.rkt"
                  BC_BINC
                  BC_BDEC
                  BC_BADD))
(require (only-in "./vm-interpreter-bc.branch.rkt"
                  BC_Z_P_BRA
                  BC_NZ_P_BRA
                  BC_T_P_BRA
                  BC_F_P_BRA))
(require (only-in "./vm-interpreter-bc.call_ret.rkt"
                  BC_CALL
                  BC_Z_P_RET_POP_N))
(require (only-in "./vm-interpreter-bc.cell-pair.rkt"
                  BC_PUSH_NIL
                  BC_CxxR
                  BC_CONS
                  BC_COONS
                  BC_NIL_P
                  BC_CAR
                  BC_CDR))
(require (only-in "./vm-interpreter-bc.compare.rkt"
                  BC_B_GT_P
                  BC_B_LT_P
                  BC_B_GE_P
                  BC_I_GT_P))
(require (only-in "./vm-interpreter-bc.native.rkt" BC_POKE_B BC_NATIVE RETURN_TO_BC))
(require (only-in "./vm-interpreter-bc.push_const.rkt" BC_PUSH_CONST_NUM_SHORT))
(require (only-in "./vm-interpreter-bc.push_n_pop.rkt"
                  BC_PUSH_B
                  BC_DUP
                  BC_SWAP))
(require (only-in "./vm-interpreter-bc.pop_local.rkt" BC_POP_TO_LOCAL_SHORT))

(module+ test
  (require (only-in "./vm-bc-opcode-definitions.rkt" bc))
  (require "../6510-test-utils.rkt")
  (require (only-in "./vm-interpreter-test-utils.rkt"
                    run-bc-wrapped-in-test-
                    vm-next-instruction-bytes))
  (require (only-in "../ast/6510-relocator.rkt" command-len))

  (define (wrap-bytecode-for-test bc-to-wrap)
    (append (list (org #x7000)
                  (JSR VM_INITIALIZE_MEMORY_MANAGER)
                  (JSR VM_INITIALIZE_CALL_FRAME)
                  (JSR VM_INTERPRETER_INIT)
                  (JMP VM_INTERPRETER))
            (list (org #x8000))
            bc-to-wrap
            (list (bc BREAK))
            (list (org #xa000))
            vm-interpreter))


  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (run-bc-wrapped-in-test- bc wrapped-code debug)))

(module+ test #| after mem init |#
  (define PAGE_CALL_FRAME #x8d)
  (define PAGE_LOCALS_LB #x8b)
  (define PAGE_LOCALS_LB_W #x8b00)
  (define PAGE_LOCALS_HB #x8c)
  (define PAGE_LOCALS_HB_W #x8c00)
  (define PAGE_AVAIL_0 #x8a)
  (define PAGE_AVAIL_0_W #x8a00)
  (define PAGE_AVAIL_1 #x89)
  (define PAGE_AVAIL_1_W #x8900))

(provide vm-interpreter
         full-interpreter-opcode-table
         full-extended-optable-hb
         full-extended-optable-lb
         just-vm-interpreter
         vm-interpreter-wo-jt)

(define VM_INTERPRETER_VARIABLES
  (list
   ;; avail:
   ;; $0b..0e
   ;; $14..15
   ;; $0f..11
   ;; $18..25   
   ))
                                 ;; @DC-B: NIL_P_RET_L0_POP_1, group: return
(define NIL_P_RET_L0_POP_1 #xb0) ;; *NIL* *P*​redicate *RET*​urn *L*​ocal *0* and *POP* *1* from evlstk
                                 ;; @DC-B: NIL_P_RET_L0_POP_2, group: return
(define NIL_P_RET_L0_POP_2 #xb2) ;; *NIL* *P*​redicate *RET*​urn *L*​ocal *0* and *POP* *2* from evlstk
                                 ;; @DC-B: NIL_P_RET_L0_POP_3, group: return
(define NIL_P_RET_L0_POP_3 #xb4) ;; *NIL* *P*​redicate *RET*​urn *L*​ocal *0* and *POP* *3* from evlstk
                                 ;; @DC-B: NIL_P_RET_L0_POP_4, group: return
(define NIL_P_RET_L0_POP_4 #xb6) ;; *NIL* *P*​redicate *RET*​urn *L*​ocal *0* and *POP* *4* from evlstk
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
          (LSR)                              ;;
          (AND !$03)
          (BEQ DONE__)
          (TAX)

   (label LOOP_POP__)
          (DEC ZP_CELL_STACK_TOS)
          (LDY ZP_CELL_STACK_TOS)
          (LDA (ZP_CELL_STACK_LB_PTR),y)
          (STA ZP_RZ)
          (LDA (ZP_CELL_STACK_HB_PTR),y)
          (STA ZP_RZ+1)
          (STX ZP_RP)
          (JSR DEC_REFCNT_RZ)
          (LDX ZP_RP)
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
             (bc CALL) (byte 00) (byte $87)
             (bc BREAK)

             (org #x8700)
      (label TEST_FUN)
             (byte 1)                     ;; number of locals
             (bc POP_TO_L0)          ;; pop tos into local 0 (now int 1)
             (bc NIL_P_RET_L0_POP_1)  ;; return local 0  if tos = nil (which it is)
             (bc BREAK))
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
             (bc CALL) (byte 00) (byte $87)
             (bc BREAK)

             (org #x8700)
      (label TEST_FUN)    
             (byte 2)            ;; number of locals
             (bc POP_TO_L1)
             (bc POP_TO_L0)
             (bc PUSH_L1)
             (bc NIL_P_RET_L0_POP_1)     ;; return local 0 (int 1) if nil
             (bc BREAK))
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

;; @DC-B: TAIL_CALL, group: flow
(define TAIL_CALL #x6a) ;; stack [new-paramN .. new-param0, ..., original-paramN ... original-param0] -> [new-paramN .. new-param0]
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
             (bc CALL) (byte 00) (byte $87)
             (bc BREAK)

             (org #x8700)
      (label TEST_FUN)
             (byte 1)            ;; number of locals
             (bc POP_TO_L0)
             (bc PUSH_L0)
             (bc NIL_P_RET_L0_POP_1)    ;; return param0 if nil
             (bc POP_TO_L0)
             (bc PUSH_NIL)       ;; value to use with tail call
             (bc TAIL_CALL)
             (bc BREAK))))

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
             (bc CALL) (byte 00) (byte $87)
             (bc BREAK)                   ;; << to make debugger stop/exit

             (org #x8700)
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
             (bc BREAK))                  ;; just in case to make debugger stop/exit
     ))

  (check-equal? (memory-list bc-tail-call-reverse-state #xcec5 (add1 #xcec5))
                   (list #x05 PAGE_AVAIL_0))
  (check-equal? (vm-page->strings bc-tail-call-reverse-state PAGE_AVAIL_0)
                   (list "page-type:      cell-pair page"
                         "previous page:  $00"
                         "slots used:     4"
                         "next free slot: $49"))
  (inform-check-equal? (cpu-state-clock-cycles bc-tail-call-reverse-state)
                4415)
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

;; @DC-FUN: VM_REFCOUNT_DECR_ARRAY_REGS, group: gc
;; decrement refcount to all array register (ra, rb, rc)
;; rb is only checked, if ra != 0,
;; rc is only checked, if rb != 0,
(define VM_REFCOUNT_DECR_ARRAY_REGS
  (add-label-suffix
   "__" "__VM_REFCOUNT_DECR_ARRAY_REGS"
  (list
   (label VM_REFCOUNT_DECR_ARRAY_REGS)
          (LDA ZP_RA)
          (BEQ DONE__)
          (JSR DEC_REFCNT_RA)
   (label TRY_RB__)
          (LDA ZP_RB)
          (BEQ CLEAR_RA__)
          (JSR DEC_REFCNT_RB)
   (label TRY_RC__)
          (LDA ZP_RC)
          (BEQ CLEAR_RAB__)
          (JSR DEC_REFCNT_RC)
          (LDA !$00)
          (STA ZP_RC)
          (STA ZP_RC+1) ;; can most probably be optimized away (if dec refcnt checks 0 in low byte)
   (label CLEAR_RAB__)
          (STA ZP_RB)
          (STA ZP_RB+1) ;; can most probably be optimized away (if dec refcnt checks 0 in low byte)
   (label CLEAR_RA__)
          (STA ZP_RA)
          (STA ZP_RA+1) ;; can most probably be optimized away (if dec refcnt checks 0 in low byte)
   (label DONE__)
          (RTS))))

;; @DC-B: RET, group: return
(define RET #x7a) ;; stack [cell paramN, ... cell param1, cell param0] -> []
(define BC_RET
  (add-label-suffix
   "__" "__BC_RET"
  (list
   (label BC_RET)
          ;; load # locals = 0 skip this step
          (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)
          (LDA ZP_RA)
          (BEQ NO_RA__)
          (JSR VM_REFCOUNT_DECR_ARRAY_REGS)
   (label NO_RA__)
          (JSR VM_POP_CALL_FRAME_N)             ;; maybe move the respective code into here, (save jsr)
          (JMP VM_INTERPRETER))))

(module+ test #| bc_ret |#
  (define test-bc-ret-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_I0)
             (bc CALL) (byte 00) (byte $87)
             (bc BREAK)

             (org #x8700)
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

;; @DC-B: BREAK, group: misc
(define BREAK #x54) ;; collision with 6510 BRK code
(define BC_BREAK
  (list
   (label BC_BREAK)
          (BRK)))

(module+ test #| bc_brk |#
  (define use-case-brk-state-after
    (run-bc-wrapped-in-test
     (list
      (bc BREAK))))

  (check-equal? (vm-next-instruction-bytes use-case-brk-state-after)
                (list BREAK)
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
(define PUSH_L0_CDR #xd0) ;; *PUSH* *L*​ocal *0* and *CDR*
;;                           @DC-B: PUSH_L1_CDR, group: stack
(define PUSH_L1_CDR #xd2) ;; *PUSH* *L*​ocal *1* and *CDR*
;;                           @DC-B: PUSH_L2_CDR, group: stack
(define PUSH_L2_CDR #xd4) ;; *PUSH* *L*​ocal *2* and *CDR*
;;                           @DC-B: PUSH_L3_CDR, group: stack
(define PUSH_L3_CDR #xd6) ;; *PUSH* *L*​ocal *3* and *CDR*
(define BC_PUSH_LOCAL_CXR
  (add-label-suffix
   "__" "__BC_PUSH_LOCAL_CXR"
  (flatten
   (list
    (label BC_PUSH_LX_CAR)
           (JSR PUSH_RT_WRITE_LOCAL_bc_enc)
           (JSR WRITE_CELLPAIR_RT_CELL0_TO_RT)
           (JSR INC_REFCNT_RT)
           (JMP VM_INTERPRETER_INC_PC)

    (label BC_PUSH_LX_CDR)
           (JSR PUSH_RT_WRITE_LOCAL_bc_enc)
           (JSR WRITE_CELLPAIR_RT_CELL1_TO_RT)
           (JSR INC_REFCNT_RT)
           (JMP VM_INTERPRETER_INC_PC)))))

;; @DC-B: PUSH_I, group: stack
(define PUSH_I  #x0c) ;; *PUSH* *I*​nt onto evlstk, op1=low byte op2=high byte, stack [] -> [cell-int]
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
      (bc BREAK))))

  (check-equal? (vm-stack->strings use-case-push-int-state-after)
                (list "stack holds 1 item"
                      "int $04f0  (rt)")))

;; @DC-B: IADD, group: int
;; len: 1
;; stack [cell-int a, cell-int b] -> [sum]
(define IADD #xbe)
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

(module+ test #| IADD |#
  (define (bc-int-plus-state a b)
    (define ra (if (< a 0) (+ #x2000 a) a))
    (define rb (if (< b 0) (+ #x2000 b) b))
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I) (ast-bytes-cmd '() (list (high-byte ra) (low-byte ra)))
      (bc PUSH_I) (ast-bytes-cmd '() (list (high-byte rb) (low-byte rb)))
      (bc IADD)
      (bc BREAK))))

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
      (bc BREAK))))

  (inform-check-equal? (cpu-state-clock-cycles use-case-int-plus-state-after)
                       668)
  (check-equal? (vm-stack->strings use-case-int-plus-state-after)
                   (list "stack holds 3 items"
                         "int $0000  (rt)"
                         "int $060f"
                         "int $0003"
                         )))

;; @DC-B: ISUB, group: int
(define ISUB #xbc) ;; stack [cell-int a, cell-int b] -> [difference]
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

(module+ test #| ISUB |#
  (define (bc-int-minus-state a b)
    (define ra (if (< a 0) (+ #x2000 a) a))
    (define rb (if (< b 0) (+ #x2000 b) b))
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I) (ast-bytes-cmd '() (list (high-byte ra) (low-byte ra)))
      (bc PUSH_I) (ast-bytes-cmd '() (list (high-byte rb) (low-byte rb)))
      (bc ISUB)
      (bc BREAK))))

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
      (bc BREAK))))                    ;; brk


   (inform-check-equal? (cpu-state-clock-cycles use-case-int-minus-state-after)
                        667)
    (check-equal? (vm-stack->strings use-case-int-minus-state-after)
                    (list "stack holds 3 items"
                          "int $1fff  (rt)"
                          "int $1c2f"
                          "int $0001")))

;; @DC-B: BSHR, group: byte
(define BSHR #x4e)
(define BC_BSHR
  (add-label-suffix
   "__" "__BC_BSHR"
   (list
    (label BC_BSHR)
           (LDA ZP_RT+1)
           (LSR)
           (STA ZP_RT+1)
           (JMP VM_INTERPRETER_INC_PC))))

;; @DC-B: INT_P, group: predicates
;; is top of evlstk an *INT*​eger (*P*​redicate)?
(define INT_P #x0e)
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

(module+ test #| int_p |#
  (skip (check-equal? #t #f "implement")))

;; @DC-B: F_P_RET_F, group: return
(define F_P_RET_F #x3e) ;; *F*​alse *P*​redicate *RET*​urn *F*​alse
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
          (JMP VM_POP_EVLSTK_AND_INC_PC))))

;; @DC-B: F_P_RET, group: return
(define F_P_RET #x1c) ;; *F*​alse *P*​redicate *RET*​urn
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
          (JMP VM_POP_EVLSTK_AND_INC_PC))))

;; @DC-B: T_P_RET, group: return
;; *T*​rue *P*​redicate *RET*​urn
;; len: 1
(define T_P_RET #x5c)
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
          (JMP VM_POP_EVLSTK_AND_INC_PC))))

;; @DC-B: GOTO, group: flow
;; goto relative by byte following in code
;; len: 2
(define GOTO #x78) ;; op = relative offset
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
      (bc BREAK)
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
      (bc BREAK)
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
       (bc BREAK)
       (org-align #x80)
       (bc PUSH_I1)
       (bc GOTO) (byte $6d)
       (bc BREAK)
       (org-align #xf0)
       (bc PUSH_I2)
       ;; 80f1
       (bc GOTO) (byte $0d)
       (build-list 13 (lambda (_i) (bc BREAK)))
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
       (bc BREAK)
       (org-align #x80)
       (bc PUSH_I1)
       ;; now at 8081
       (bc GOTO) (byte $6d)
       ;; 8083
       (bc BREAK)
       (org-align #xf0)
       (bc PUSH_I2)
       ;; now at 80f1
       (bc GOTO) (byte $0e)
       (build-list 14 (lambda (_i) (bc BREAK)))
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
       (bc BREAK)
       (bc PUSH_I1)
       (bc BREAK)
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
       (bc BREAK)
       (org-align #x80)
       (bc PUSH_I1)
       ;; now at 8081
       (bc GOTO) (byte $6d)
       ;; 8083
       (bc BREAK)
       (org-align #xf0)
       (bc PUSH_I2)
       ;; now at 80f1
       (bc GOTO) (byte $0e)
       (build-list 12 (lambda (_i) (bc BREAK)))
       ;; 80ff
       (bc PUSH_I0)
       ;; 8100
       (bc BREAK)
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

;; @DC-B: CONS_PAIR_P, group: predicates
;; *CONS* *PAIR* *P*​redicate
;; len: 1
(define CONS_PAIR_P #x5a)
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
     (flatten
      (list
       (bc PUSH_I0)
       (bc IINC)))))

  (check-equal? (vm-stack->strings inc-int-0-state)
                (list "stack holds 1 item"
                      "int $0001  (rt)"))

  (define inc-int-1-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I) (byte 255) (byte 0)
       (bc IINC)))
     ))

  (check-equal? (vm-stack->strings inc-int-1-state)
                (list "stack holds 1 item"
                      "int $0100  (rt)"))

  (define inc-int-2-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_IM1)
       (bc IINC)))
     ))

  (check-equal? (vm-stack->strings inc-int-2-state)
                (list "stack holds 1 item"
                      "int $0000  (rt)"))

  (define inc-int-3-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I) (byte 255) (byte 05)
       (bc IINC)))
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



;; @DC-B: CELL_EQ_P, group: predicates
;; *CELL* *EQ*​ual *P*​redicate
;; len: 1
(define CELL_EQ_P #x3c)
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
(define I_Z_P #x44)
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

(module+ test #| ext max-int |#
  (define max-int-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I2)
       (bc PUSH_I1)
       (bc IMAX)))))

  (check-equal? (vm-stack->strings max-int-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)"))

  (define max-int-2-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I1)
       (bc PUSH_I2)
       (bc IMAX)))))

  (check-equal? (vm-stack->strings max-int-2-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)")))

;; @DC-B: BNOP, misc
;; *N*​o *OP*​eration
;; len: 1
(define BNOP #x7c)
(define BC_BNOP
  (list
   (label BC_BNOP)
          (JSR $0100)
          (JMP VM_INTERPRETER_INC_PC)))

(module+ test #| nop |#
  (define nop-state
    (run-bc-wrapped-in-test
     (list (bc BNOP))))

  (inform-check-equal? (cpu-state-clock-cycles nop-state)
                27))

;; @DC-B: SWAP_RA_RB, group: array
;; swap array register RA with RB
(define SWAP_RA_RB #x8a)
(define BC_SWAP_RA_RB
  (list
   (label BC_SWAP_RA_RB)
          (JSR SWAP_RA_RB)
          (JMP VM_INTERPRETER_INC_PC)))


;; @DC-B: POP_TO_RA, group: cell_array
;; *POP* top of evlstk *TO* *RA*, setting RAI=0
;; len: 1
(define POP_TO_RA #xce)
;; @DC-B: POP_TO_RB, group: cell_array
;; *POP* top of evlstk *TO* *RB*, setting RAI=0
;; len: 1
(define POP_TO_RB #x8c)
;; @DC-B: POP, group: stack
;; len: 1
(define POP #x58)
(define BC_POP
  (list
   (label BC_POP_TO_RB)
          (LDA !$00)
          (STA ZP_RBI)          ;; initialize index to 0
          (JSR CP_RT_TO_RB)     ;; copy tos to rb
          (JMP VM_POP_EVLSTK_AND_INC_PC)

   (label BC_POP_TO_RA)
          (LDA !$00)
          (STA ZP_RAI)          ;; initialize index to 0
          (JSR CP_RT_TO_RA)     ;; copy tos to ra
          (JMP VM_POP_EVLSTK_AND_INC_PC)

   (label BC_POP) ;;--------------------------------------------------------------------------------
          (JSR DEC_REFCNT_RT) ;; no dec, since ra is refcounted too
          (JMP VM_POP_EVLSTK_AND_INC_PC)))

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

(define just-vm-interpreter
  (append VM_INTERPRETER_VARIABLES
          VM_INTERPRETER_INIT
          BC_POP
          BC_PUSH_LOCAL_SHORT
          BC_POP_TO_LOCAL_SHORT
          BC_PUSH_LOCAL_CXR
          BC_PUSH_B
          BC_CALL
          BC_PUSH_CONST_NUM_SHORT
          PUSH_RT_WRITE_LOCAL_bc_enc
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
          BC_RET
          BC_BREAK
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
          BC_ALLOC_ARA
          BC_XET_ARRAY_FIELD
          BC_XET_RA_ARRAY_FIELD
          BC_F_P_RET_F
          VM_REFCOUNT_DECR_ARRAY_REGS
          BC_PUSH_AF
          BC_POP_TO_AF
          BC_NATIVE
          RETURN_TO_BC
          BC_BINC_RAI
          BC_PUSH_RA_AF
          BC_POP_TO_RA_AF
          BC_POP_TO_RAI
          BC_BDEC
          BC_BINC
          BC_Z_P_BRA
          BC_NZ_P_BRA
          BC_Z_P_RET_POP_N
          BC_PUSH_RA
          BC_SWAP_RA_RB
          BC_BADD
          BC_B_GT_P
          BC_B_LT_P
          BC_B_GE_P
          BC_BSHR
          BC_WRITE_TO_RAI
          BC_DEC_RAI
          BC_WRITE_TO_RBI
          BC_DEC_RBI_NZ_P_BRA
          BC_POKE_B
          VM_INTERPRETER))

(define vm-interpreter-wo-jt
  (append just-vm-interpreter
          (list (label END__INTERPRETER))
          full-extended-optable-hb
          full-extended-optable-lb
          vm-lists))

(define vm-interpreter
  (append vm-interpreter-wo-jt
          (list (org-align #x100)) ;; align to next page
          full-interpreter-opcode-table
          (list (label END__INTERPRETER_DATA))))

(module+ test #| vm-interpreter |#
  (inform-check-equal? (foldl + 0 (map command-len (flatten just-vm-interpreter)))
                       1637
                       "estimated len of (just) the interpreter"))

(module+ test #| vm-interpreter total len |#
  (define interpreter-len (foldl + 0 (map command-len (flatten vm-interpreter))))
  (inform-check-equal?
   (< interpreter-len (- 4096 256)) ;; 4 k (c000-cfff) minus one page
   #t
   (format "total memory usage of the interpreter (now ~a) should stay within c000..ceff" interpreter-len)))
