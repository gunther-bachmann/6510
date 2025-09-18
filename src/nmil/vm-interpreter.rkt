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
(require (only-in "./vm-lists.rkt" vm-lists))
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
(require [only-in "./vm-interpreter-bc.branch.rkt"
                  BC_Z_P_BRA
                  BC_NZ_P_BRA
                  BC_T_P_BRA
                  BC_F_P_BRA
                  BC_GOTO])
(require (only-in "./vm-interpreter-bc.call_ret.rkt"
                  BC_CALL
                  BC_Z_P_RET_POP_N
                  BC_NIL_P_RET_L0_POP_N
                  BC_RET
                  BC_TAIL_CALL
                  BC_T_P_RET
                  BC_F_P_RET
                  BC_F_P_RET_F))
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
(require (only-in "./vm-interpreter-bc.misc.rkt"
                  BC_BNOP
                  BC_BREAK
                  BC_GC_FL))
(require (only-in "./vm-interpreter-bc.native.rkt"
                  BC_POKE_B
                  BC_NATIVE
                  RETURN_TO_BC))
(require (only-in "./vm-interpreter-bc.push_const.rkt"
                  BC_PUSH_CONST_NUM_SHORT))
(require (only-in "./vm-interpreter-bc.push_n_pop.rkt"
                  BC_PUSH_B
                  BC_DUP
                  BC_SWAP
                  BC_POP
                  BC_PUSH_I))
(require (only-in "./vm-interpreter-bc.push_local.rkt"
                  BC_PUSH_LOCAL_CXR))
(require (only-in "./vm-interpreter-bc.pop_local.rkt"
                  BC_POP_TO_LOCAL_SHORT))
(require (only-in "./vm-interpreter-bc.predicates.rkt"
                  BC_I_Z_P
                  BC_INT_P
                  BC_CONS_PAIR_P
                  BC_CELL_EQ_P))

(module+ test
  (require "../6510-test-utils.rkt")
  (require (only-in "../ast/6510-relocator.rkt" command-len)))

(provide vm-interpreter
         ;; full-interpreter-opcode-table
         ;; full-extended-optable-hb
         ;; full-extended-optable-lb
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
