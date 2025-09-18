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
                  VM_REFCOUNT_DECR_ARRAY_REGS
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
                  BC_POP_TO_AF
                  BC_SWAP_RA_RB))
(require (only-in "./vm-interpreter-bc.atom-num.rkt"
                  BC_BINC
                  BC_BDEC
                  BC_BADD
                  BC_IMAX
                  BC_IINC
                  BC_IADD
                  BC_BSHR
                  BC_ISUB))
(require (only-in "./vm-interpreter-bc.branch.rkt"
                  BC_Z_P_BRA
                  BC_NZ_P_BRA
                  BC_T_P_BRA
                  BC_F_P_BRA))
(require (only-in "./vm-interpreter-bc.call_ret.rkt"
                  BC_CALL
                  BC_Z_P_RET_POP_N
                  BC_NIL_P_RET_L0_POP_N
                  BC_RET
                  BC_TAIL_CALL))
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
(require (only-in "./vm-interpreter-bc.misc.rkt" BC_BNOP))
(require (only-in "./vm-interpreter-bc.native.rkt" BC_POKE_B BC_NATIVE RETURN_TO_BC))
(require (only-in "./vm-interpreter-bc.push_const.rkt" BC_PUSH_CONST_NUM_SHORT))
(require (only-in "./vm-interpreter-bc.push_n_pop.rkt"
                  BC_PUSH_B
                  BC_DUP
                  BC_SWAP
                  BC_POP
                  BC_PUSH_I))
(require (only-in "./vm-interpreter-bc.push_local.rkt" BC_PUSH_LOCAL_CXR))
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
