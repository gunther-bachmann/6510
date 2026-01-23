#lang racket/base

(provide bc-opcode-definitions
         (struct-out od-simple-bc)
         (struct-out od-extended-bc)
         find-dyn-opcode-def
         get-dyn-opcode-simple-def
         disassemble-od-simple-bc
         byte-count-od-simple-bc
         fetch-opcode-list
         get-single-opcode
         bc
         full-extended-optable-lb
         full-extended-optable-hb
         full-interpreter-opcode-table
         build-extended-optable-hb
         build-extended-optable-lb
         build-interpreter-optable
         filtered-opcode-definitions)
#|

  Byte Code Opcodes are completely defined here to be able to quickly switch between
  - param encoded byte code
  - single byte code
  - multiple byte code (extended byte code)
  depending on number of usage to make it as compact as possible!

|#

(require (for-syntax racket/base)
         (for-syntax (only-in "../scheme-asm/6510-addressing-utils.rkt"
                             retrieve-meta-info-from))
         (only-in racket/contract
                  define/contract
                  struct-guard/c
                  or/c
                  ->
                  listof
                  ->*)
         (only-in racket/list
                  take
                  flatten
                  empty?
                  drop)
         (only-in "../6510-utils.rkt"
                  word->hex-string
                  byte->hex-string)
         (only-in "../ast/6510-command.rkt"
                  ast-command?
                  ast-bytes-cmd
                  ast-bytes-cmd-bytes
                  ast-unresolved-bytes-cmd
                  ast-resolve-byte-scmd
                  ast-resolve-word-scmd)
         (only-in "../tools/6510-disassembler.rkt"
                  info-for-label)
         (only-in "../tools/data-tools.rkt"
                  bytes->int)
         (only-in "./vm-interpreter-bc/arrays.rkt"
                  BC_DEC_RBI_NZ_P_BRA            ;; decrement cell array index register RBI and branch if NOT Zero
                  BC_DEC_RAI                     ;; decrement cell array index register RAI
                  BC_WRITE_TO_RBI                ;; write tos byte cell array index register RBI
                  BC_WRITE_TO_RAI                ;; write tos byte into cell array index register RAI
                  BC_POP_TO_RAI                  ;; pop tos byte into cell array index register RAI
                  BC_BINC_RAI                    ;; increment cell array index register RAI
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
                  BC_POP_TO_RA_AF                ;; pop tos into cell-array RA field A onto the eval stack (TODO: ref count old content!)
                  BC_PUSH_AF                     ;; push array field (stack: index :: cell-array-ptr)
                  BC_POP_TO_AF                   ;; pop tos to array field (stack: index :: cell-ptr->cell-array  :: value )
                  BC_SWAP_RA_RB)
         (only-in "./vm-interpreter-bc/atom-num.rkt"
                  BC_BINC                        ;; increment byte (tos)
                  BC_BDEC                        ;; decrement
                  BC_BADD                        ;; add two topmost bytes
                  BC_IMAX                        ;; get max of two topmost integers
                  BC_IINC                        ;; increment integer (tos)
                  BC_IDEC                        ;; decrement integer (tos)
                  BC_IADD                        ;; add two topmost integer
                  BC_BSHR                        ;; shift tos byte one bit to the right
                  BC_ISUB)
         (only-in "./vm-interpreter-bc/branch.rkt"
                  BC_Z_P_BRA                     ;; branch by next byte if tos is zero (byte or int), pop if branching
                  BC_NZ_P_BRA                    ;; branch by next byte if tos is NOT zero (byte or int), pop if not branching
                  BC_T_P_BRA                     ;; branch by next byte if tos is true (actually anything != 0), always pop
                  BC_F_P_BRA                     ;; branch by next byte if tos is false (actually = 0), always pop
                  BC_GOTO)
         (only-in "./vm-interpreter-bc/call_ret.rkt"
                  BC_CALL
                  BC_Z_P_RET_POP_N
                  BC_NZ_P_RET_POP_N
                  BC_NIL_P_RET_L0_POP_N
                  BC_TAIL_CALL
                  BC_F_P_RET_F
                  BC_F_P_RET
                  BC_T_P_RET
                  BC_RET)
         (only-in "./vm-interpreter-bc/cell-pair.rkt"
                  BC_CxxR
                  BC_PUSH_NIL
                  BC_CONS
                  BC_COONS
                  BC_NIL_P
                  BC_CDR
                  BC_CAR)
         (only-in "./vm-interpreter-bc/compare.rkt"
                  BC_B_GT_P
                  BC_B_LT_P
                  BC_B_GE_P
                  BC_I_GT_P)
         (only-in "./vm-interpreter-bc/ext.rkt"
                  BC_EXT1_CMD)
         (only-in "./vm-interpreter-bc/misc.rkt"
                  BC_BNOP
                  BC_BREAK
                  BC_GC_FL)
         (only-in "./vm-interpreter-bc/native.rkt"
                  BC_POKE_B
                  BC_NATIVE)
         (only-in "./vm-interpreter-bc/pop_local.rkt"
                  BC_POP_TO_LOCAL_SHORT
                  BC_WRITE_TO_LOCAL_SHORT)
         (only-in "./vm-interpreter-bc/predicates.rkt"
                  BC_I_Z_P
                  BC_INT_P
                  BC_CELL_EQ_P
                  BC_CONS_PAIR_P)
         (only-in "./vm-interpreter-bc/push_const.rkt"
                  BC_PUSH_CONST_NUM_SHORT
                  BC_PUSH_INT0
                  BC_PUSH_INT1
                  BC_PUSH_INT2
                  BC_PUSH_INTm1)
         (only-in "./vm-interpreter-bc/push_local.rkt"
                  BC_WRITE_LOCAL_SHORT           ;; write a local into the tos (rt)
                  BC_PUSH_LOCAL_SHORT            ;; push the local onto the eval stack
                  BC_PUSH_LX_CDR
                  BC_PUSH_LX_CAR)                ;; push local 0-3 and then car

         (only-in "./vm-interpreter-bc/push_n_pop.rkt"
                  BC_PUSH_B
                  BC_DUP
                  BC_SWAP
                  BC_POP
                  BC_POP_TO_RA
                  BC_POP_TO_RB
                  BC_PUSH_I)
         (only-in "./vm-interpreter-bc/vm-interpreter-bc.rkt"
                  BC_PUSH_LOCAL_SHORT
                  BC_EXT1_CMD
                  VM_INTERPRETER_OPTABLE_EXT1_LB
                  VM_INTERPRETER_OPTABLE_EXT1_HB)
         (only-in "./vm-interpreter-loop.rkt"
                  VM_INTERPRETER_OPTABLE))

(module+ test
  (require "../6510-test-utils.rkt"))

;; simple dynamic opcode definition
(define-struct od-simple-bc
  (-label               ;; ast label into the implementation of this bc within the interpreter
   -bc-opcode           ;; opcode to be used in programs
   -byte-code           ;; the actual byte code (must be even!)
   -byte-count          ;; function returning the number of bytes used by this byte code and its operands
   -disassembler)       ;; function to disassemble this byte code (and its operands)
  #:transparent
  #:guard (struct-guard/c
           string?
           string?
           byte?
           (or/c byte? (-> byte? byte? byte?))
           (or/c string? (-> hash? byte? byte? byte? string?))))

;; extended opcode definition
(define-struct od-extended-bc
  (-label               ;; ast label into the implementation of this bc (loading code from the extended table)
   -byte-code           ;; the actual byte code (can be any byte, no need to be even)
   -sub-commands)       ;; subcommands (all od-simple-bc definitions) jumped at by the extended command
  #:transparent
  #:guard (struct-guard/c
           string?
           byte?
           (listof od-simple-bc?)))

;; syntactic sugar to not use strings but symbols
(define-syntax-rule (define-bc label cmd byte-code dis len)
  (od-simple-bc (symbol->string 'label) (symbol->string 'cmd) byte-code dis len))

;; syntactic sugar to not use strings but symbols
(define-syntax-rule (def-xt-bc label byte-code lst)
  (od-extended-bc (symbol->string 'label) byte-code lst))

;; operator description:
;; - implicit:
;;   - pc += len
;; - explicit:
;;   - stack change
;;     notation: a : b : c ::  (stack with n>=3 elements, tos = a, 2os = b, 3os = c, other irrelevant and unchanged)
;;               ::            (stack with n>=0 elements, all irrelevant and unchanged)
;;               a :: -> ::     (stack pops a)
;;   - variable change (e.g. Ln for local #n, RAI)
;;     notation: l0 <- ..      (.. is moved into l0)
;;   - gc operations/checks done
;;     gc[a]
;;   - array change (structure change etc.)
;;     notation: [.. a_i ..]        value of ra at index rai
;;   - list change
;;     notation: (a ..) -> (..)  remove a from head of list
;;               l0 <- a        l0 is assigned head of list just removed
;; stack operation:
;; e.g.: IADD: a:b:: -> a+b:: (before a is tos, b is 2os, after a+b is tos, a and b were popped
;; m[pc] = byte @ current pc
;; <<8 *256 or shift 8 times left
;;   copy template
;;   (define-bc VM_INTERPRETER_INC_PC    _                   #x00 1 "reserved")           ;; reserved
;;
(define bc-opcode-definitions
  (list ;; opcode must start at 0 at increment by 2!
   ;;         code to include          bc command          byte len disassembled to
   (define-bc BC_PUSH_LOCAL_SHORT      PUSH_L0              #x00 1 "push l0")           ;; stack: :: -> l0::, ensure opcode & #x06 = 0
   (define-bc BC_PUSH_LOCAL_SHORT      PUSH_L1              #x02 1 "push l1")           ;; stack: :: -> l1::, ensure opcode & #x06 = 2
   (define-bc BC_PUSH_LOCAL_SHORT      PUSH_L2              #x04 1 "push l2")           ;; stack: :: -> l2::, ensure opcode & #x06 = 4
   (define-bc BC_PUSH_LOCAL_SHORT      PUSH_L3              #x06 1 "push l3")           ;; stack: :: -> l3::, ensure opcode & #x06 = 6
   (def-xt-bc BC_EXT1_CMD                                   #x08
     (list (define-bc VM_INTERPRETER_INC_PC_2_TIMES _             #x00 1 "reserved")    ;; stack: -, opcode must start at 0 and increment by 1
           (define-bc BC_IMAX           IMAX                      #x01 1 "int max")     ;; stack: a:b:: -> max(a,b)::
           (define-bc BC_IINC           IINC                      #x02 1 "int inc")     ;; stack: a:: -> a+1::
           (define-bc BC_GC_FL          GC                        #x03 1 "gc")          ;; stack: -
           (define-bc BC_IDEC           IDEC                      #x04 1 "int dec")     ;; stack: a:: -> a-1::
           ))

   (define-bc VM_INTERPRETER_INC_PC    _                    #x0a 1 "reserved")          ;; reserved
   (define-bc BC_PUSH_I                PUSH_I               #x0c 3                      ;; stack: :: -> m[pc+1]<<8+m[pc+2]::
     (lambda (_l _bc bc-p1 bc-p2) (format "push int $~a" (word->hex-string (bytes->int bc-p1 bc-p2)))))
   (define-bc BC_INT_P                 INT_P                #x0e 1 "int?")              ;; stack: a:: -> bool::
   (define-bc BC_WRITE_LOCAL_SHORT     WRITE_L0             #x10 1 "write l0")          ;; stack: a:: -> l0::, ensure opcode & #x06 = 0
   (define-bc BC_WRITE_LOCAL_SHORT     WRITE_L1             #x12 1 "write l1")          ;; stack: a:: -> l1::, ensure opcode & #x06 = 2
   (define-bc BC_WRITE_LOCAL_SHORT     WRITE_L2             #x14 1 "write l2")          ;; stack: a:: -> l2::, ensure opcode & #x06 = 4
   (define-bc BC_WRITE_LOCAL_SHORT     WRITE_L3             #x16 1 "write l3")          ;; stack: a:: -> l3::, ensure opcode & #x06 = 6
   (define-bc BC_T_P_BRA               T_P_BRA              #x18 2                      ;; stack: b:: -> ::, pc+=signed m[pc+1]
     (lambda (_l _bc bc-p1 _bc-p2) (format "branch on true? by $~a" (byte->hex-string bc-p1))))
   (define-bc BC_F_P_BRA               F_P_BRA              #x1a 2                      ;; stack: b:: -> ::, pc+=signed m[pc+1]
     (lambda (_l _bc bc-p1 _bc-p2) (format "branch on false? by $~a" (byte->hex-string bc-p1))))
   (define-bc BC_F_P_RET               F_P_RET              #x1c 1 "ret on false?")     ;; stack: b:: -> ::, pc:=previous call frame
   (define-bc BC_DUP                   DUP                  #x1e 1 "dup")               ;; stack: a:: -> a:a::
   (define-bc BC_POP_TO_LOCAL_SHORT    POP_TO_L0            #x20 1 "pop to l0")         ;; l0 := a, stack: a:: -> ::, ensure opcode & #x06 = 0
   (define-bc BC_POP_TO_LOCAL_SHORT    POP_TO_L1            #x22 1 "pop to l1")         ;; l1 := a, stack: a:: -> ::, ensure opcode & #x06 = 2
   (define-bc BC_POP_TO_LOCAL_SHORT    POP_TO_L2            #x24 1 "pop to l2")         ;; l2 := a, stack: a:: -> ::, ensure opcode & #x06 = 4
   (define-bc BC_POP_TO_LOCAL_SHORT    POP_TO_L3            #x26 1 "pop to l3")         ;; l3 := a, stack: a:: -> ::, ensure opcode & #x06 = 6
   (define-bc BC_PUSH_NIL              PUSH_NIL             #x28 1 "push nil")          ;; stack: :: -> nil::
   (define-bc BC_PUSH_AF               PUSH_AF              #x2a 1 "push array field")  ;; stack: y:ca:: -> (ca),y::
   (define-bc BC_POP_TO_AF             POP_TO_AF            #x2c 1 "pop to array field");; (ca),y := a, stack: y:ca:a -> ::
   (define-bc BC_PUSH_B                PUSH_B               #x2e 2                      ;; stack: :: -> m[pc+1]::
     (lambda (_l _bc bc-p1 _bc_p2) (format "push byte $~a" (byte->hex-string bc-p1))))
   (define-bc BC_WRITE_TO_LOCAL_SHORT  WRITE_TO_L0          #x30 1 "write to l0")       ;; l0:=tos, stack: a:: -> a::, ensure opcode & #x06 = 0
   (define-bc BC_WRITE_TO_LOCAL_SHORT  WRITE_TO_L1          #x32 1 "write to l1")       ;; l1:=tos, stack: a:: -> a::, ensure opcode & #x06 = 2
   (define-bc BC_WRITE_TO_LOCAL_SHORT  WRITE_TO_L2          #x34 1 "write to l2")       ;; l2:=tos, stack: a:: -> a::, ensure opcode & #x06 = 4
   (define-bc BC_WRITE_TO_LOCAL_SHORT  WRITE_TO_L3          #x36 1 "write to l3")       ;; l3:=tos, stack: a:: -> a::, ensure opcode & #x06 = 6
   (define-bc BC_BINC                  BINC                 #x38 1 "byte inc")          ;; stack: a:: -> a+1::
   (define-bc BC_NZ_P_BRA              NZ_P_BRA             #x3a 2                      ;; on branch, stack: a:: -> a::, on no branch, stack: a:: -> ::, pc+=signed m[pc+1]
     (lambda (_l _bc bc-p1 _bc-p2) (format "branch on not zero? by $~a" (byte->hex-string bc-p1))))
   (define-bc BC_CELL_EQ_P             CELL_EQ_P            #x3c 1 "cell eq?")          ;; stack: a:b:: -> a==b::
   (define-bc BC_F_P_RET_F             F_P_RET_F            #x3e 1 "ret false on false?");; stack: b:: -> ::
   (define-bc VM_INTERPRETER_INC_PC    _                    #x40 1 "reserved")          ;; reserved
   (define-bc BC_NIL_P                 NIL_P                #x42 1 "nil?")              ;; stack: a:: -> NIL(A)?::
   (define-bc BC_I_Z_P                 I_Z_P                #x44 1 "int 0?")            ;; stack: a:: -> a==0?::
   (define-bc BC_BADD                  BADD                 #x46 1 "byte +")            ;; stack: a:b:: -> a+b::
   (define-bc BC_B_GT_P                B_GT_P               #x48 1 "byte >?")           ;; stack: a:b:: -> a>b::
   (define-bc BC_NATIVE                NATIVE               #x4a 1 "native")            ;; need to be fixed on 4a, implementation dependes on it
   (define-bc BC_B_GE_P                B_GE_P               #x4c 1 "byte >=?")          ;; stack: a:b:: -> a>=b::
   (define-bc BC_BSHR                  BSHR                 #x4e 1 "byte shift right")  ;; stack: a:: -> a>>1::
   (define-bc VM_INTERPRETER_INC_PC    _                    #x50 1 "reserved")          ;; reserved
   (define-bc BC_CxxR                  CDDR                 #x52 1 "cddr")              ;; stack: l:: -> cddr(l)::, (bitwise-and #x1f x) must be #x12 (18)!
   (define-bc BC_BREAK                 BREAK                #x54 1 "break")             ;;
   (define-bc BC_SWAP                  SWAP                 #x56 1 "swap")              ;; stack: a:b:: -> b:a::
   (define-bc BC_POP                   POP                  #x58 1 "pop")               ;; stack: a:: -> ::
   (define-bc BC_CONS_PAIR_P           CONS_PAIR_P          #x5a 1 "pair?")             ;; stack: l:: -> b::
   (define-bc BC_T_P_RET               T_P_RET              #x5c 1 "ret on true?")      ;;
   (define-bc BC_Z_P_BRA               Z_P_BRA              #x5e 2                      ;; stack: a:: -> ::, pc+=m[pc+1]
     (lambda (_l _bc bc-p1 _bc-p2) (format "branch on zero? by $~a" (byte->hex-string bc-p1))))
   (define-bc BC_SET_ARRAY_FIELD       SET_AF0              #x60 1 "set array field 0") ;; ensure opcode & #x06 = 0
   (define-bc BC_SET_ARRAY_FIELD       SET_AF1              #x62 1 "set array field 1") ;; ensure opcode & #x06 = 2
   (define-bc BC_SET_ARRAY_FIELD       SET_AF2              #x64 1 "set array field 2") ;; ensure opcode & #x06 = 4
   (define-bc BC_SET_ARRAY_FIELD       SET_AF3              #x66 1 "set array field 3") ;; ensure opcode & #x06 = 6
   (define-bc BC_CALL                  CALL                 #x68 3                      ;;
     (lambda (labels _bc bc-p1 bc-p2)
       (format "call $~a ~a" (word->hex-string (add1 (bytes->int bc-p1 bc-p2))) ;; add 1 because byte code starts there (after #locals)
               (info-for-label (number->string (bytes->int bc-p1 bc-p2) 16) labels))))
   (define-bc BC_TAIL_CALL             TAIL_CALL           #x6a 1 "tail call")          ;;
   (define-bc BC_BDEC                  BDEC                #x6c 1 "byte dec")           ;; stack: a:: -> a-1::
   (define-bc BC_CONS                  CONS                #x6e 1 "cons")               ;; stack: a:l:: -> a++l::
   (define-bc BC_PUSH_INT0             PUSH_I0             #x70 1 "push int 0")         ;; stack: :: -> 0::, ensure opcode & #x06 = 0
   (define-bc BC_PUSH_INT1             PUSH_I1             #x72 1 "push int 1")         ;; stack: :: -> 1::, ensure opcode & #x06 = 2
   (define-bc BC_PUSH_INT2             PUSH_I2             #x74 1 "push int 2")         ;; stack: :: -> 2::, ensure opcode & #x06 = 4
   (define-bc BC_PUSH_INTm1            PUSH_IM1            #x76 1 "push int -1")        ;; stack: :: -> -1::, ensure opcode & #x06 = 6
   (define-bc BC_GOTO                  GOTO                #x78 2                       ;; pc+=m[pc+1]
     (lambda (_l _bc bc-p1 _bc-p2) (format "goto relative by $~a" (byte->hex-string bc-p1))))
   (define-bc BC_RET                   RET                 #x7a 1 "ret")                ;;
   (define-bc BC_BNOP                  BNOP                #x7c 1 "nop")                ;;
   (define-bc BC_CDR                   CDR                 #x7e 1 "cdr")                ;; stack: a++l:: -> l::
   (define-bc BC_Z_P_RET_POP_N         Z_P_RET_POP_1       #x80 1 "ret on zero? and pop 1") ;; stack: a:: -> ::, ensure opcode & #x06 = 0
   (define-bc BC_Z_P_RET_POP_N         Z_P_RET_POP_2       #x82 1 "ret on zero? and pop 2") ;; stack: a:b:: -> ::, ensure opcode & #x06 = 2
   (define-bc BC_Z_P_RET_POP_N         Z_P_RET_POP_3       #x84 1 "ret on zero? and pop 3") ;; stack: a:b:c:: -> ::, ensure opcode & #x06 = 4
   (define-bc BC_Z_P_RET_POP_N         Z_P_RET_POP_4       #x86 1 "ret on zero? and pop 4") ;; stack: a:b:c:d:: -> ::, ensure opcode & #x06 = 6
   (define-bc BC_COONS                 COONS               #x88 1 "coons")              ;; stack: a:b:l:: -> a++b++l::
   (define-bc BC_SWAP_RA_RB            SWAP_RA_RB          #x8a 1 "swap RA<->RB")       ;;
   (define-bc BC_POP_TO_RB             POP_TO_RB           #x8c 1 "pop to rb")          ;;
   (define-bc BC_PUSH_RA               PUSH_RA             #x8e 1 "push ra")            ;; stack: :: -> ra::
   (define-bc BC_SET_RA_ARRAY_FIELD    SET_RA_AF_0         #x90 1 "set (ra),0")         ;; ensure opcode & #x06 = 0
   (define-bc BC_SET_RA_ARRAY_FIELD    SET_RA_AF_1         #x92 1 "set (ra),1")         ;; ensure opcode & #x06 = 2
   (define-bc BC_SET_RA_ARRAY_FIELD    SET_RA_AF_2         #x94 1 "set (ra),2")         ;; ensure opcode & #x06 = 4
   (define-bc BC_SET_RA_ARRAY_FIELD    SET_RA_AF_3         #x96 1 "set (ra),3")         ;; ensure opcode & #x06 = 6
   (define-bc BC_ALLOC_ARA             ALLOC_ARA           #x98 1 "alloc array to ra")  ;; ra := allocated array
   (define-bc BC_PUSH_RA_AF            PUSH_RA_AF          #x9a 1 "push (ra),rai")      ;; stack: :: -> (ra),rai::
   (define-bc BC_POP_TO_RA_AF          POP_TO_RA_AF        #x9c 1 "pop to (ra),rai")    ;; (ra),rai:=tos, stack: a:: -> ::
   (define-bc BC_POP_TO_RAI            POP_TO_RAI          #x9e 1 "pop byte to rai")    ;; rai:=tos, stack: a:: -> ::
   (define-bc BC_PUSH_LX_CAR           PUSH_L0_CAR         #xa0 1 "push (car l0)")      ;; stack: :: -> car(l0)::, ensure opcode & #x06 = 0
   (define-bc BC_PUSH_LX_CAR           PUSH_L1_CAR         #xa2 1 "push (car l1)")      ;; stack: :: -> car(l1)::, ensure opcode & #x06 = 2
   (define-bc BC_PUSH_LX_CAR           PUSH_L2_CAR         #xa4 1 "push (car l2)")      ;; stack: :: -> car(l2)::, ensure opcode & #x06 = 4
   (define-bc BC_PUSH_LX_CAR           PUSH_L3_CAR         #xa6 1 "push (car l3)")      ;; stack: :: -> car(l3)::, ensure opcode & #x06 = 6
   (define-bc BC_DEC_RBI_NZ_P_BRA      DEC_RBI_NZ_P_BRA    #xa8 2                       ;;
     (lambda (_l _bc bc-p1 _bc-p2) (format "dec rbi, not zero? -> branch by  $~a" (byte->hex-string bc-p1))))
   (define-bc BC_WRITE_RA              WRITE_RA            #xaa 1 "write ra")           ;; stack: a:: -> ra::
   (define-bc BC_WRITE_TO_RAI          WRITE_TO_RAI        #xac 1 "write byte to rai")  ;; rai:=tos, stack: i:: -> ::
   (define-bc BC_DEC_RAI               DEC_RAI             #xae 1 "dec rai")            ;;
   (define-bc BC_NIL_P_RET_L0_POP_N    NIL_P_RET_L0_POP_1  #xb0 1 "ret l0 on zero? and pop 1") ;; ensure opcode & #x06 = 0
   (define-bc BC_NIL_P_RET_L0_POP_N    NIL_P_RET_L0_POP_2  #xb2 1 "ret l0 on zero? and pop 2") ;; ensure opcode & #x06 = 2
   (define-bc BC_NIL_P_RET_L0_POP_N    NIL_P_RET_L0_POP_3  #xb4 1 "ret l0 on zero? and pop 3") ;; ensure opcode & #x06 = 4
   (define-bc BC_NIL_P_RET_L0_POP_N    NIL_P_RET_L0_POP_4  #xb6 1 "ret l0 on zero? and pop 4") ;; ensure opcode & #x06 = 6
   (define-bc BC_WRITE_TO_RBI          WRITE_TO_RBI        #xb8 1 "write to rbi")       ;;
   (define-bc BC_CAR                   CAR                 #xba 1 "car")                ;; stack: a++l:: -> a::
   (define-bc BC_ISUB                  ISUB                #xbc 1 "int -")              ;; stack: a:b:: -> a-b::
   (define-bc BC_IADD                  IADD                #xbe 1 "int +")              ;; stack: a:b:: -> a+b::
   (define-bc BC_NZ_P_RET_POP_N        NZ_P_RET_POP_1      #xc0 1 "ret on not zero? and pop 1") ;; ensure opcode & #x06 = 0
   (define-bc BC_NZ_P_RET_POP_N        NZ_P_RET_POP_2      #xc2 1 "ret on not zero? and pop 2") ;; ensure opcode & #x06 = 2
   (define-bc BC_NZ_P_RET_POP_N        NZ_P_RET_POP_3      #xc4 1 "ret on not zero? and pop 3") ;; ensure opcode & #x06 = 4
   (define-bc BC_NZ_P_RET_POP_N        NZ_P_RET_POP_4      #xc6 1 "ret on not zero? and pop 4") ;; ensure opcode & #x06 = 6
   (define-bc BC_I_GT_P                I_GT_P              #xc8 1 "int >?")
   (define-bc BC_BINC_RAI              BINC_RAI            #xca 1 "inc rai")
   (define-bc BC_B_LT_P                B_LT_P              #xcc 1 "byte <?")
   (define-bc BC_POP_TO_RA             POP_TO_RA           #xce 1 "pop to ra")
   (define-bc BC_PUSH_LX_CDR           PUSH_L0_CDR         #xd0 1 "push (cdr l0)")      ;; ensure opcode & #x06 = 0
   (define-bc BC_PUSH_LX_CDR           PUSH_L1_CDR         #xd2 1 "push (cdr l1)")      ;; ensure opcode & #x06 = 2
   (define-bc BC_PUSH_LX_CDR           PUSH_L2_CDR         #xd4 1 "push (cdr l2)")      ;; ensure opcode & #x06 = 4
   (define-bc BC_PUSH_LX_CDR           PUSH_L3_CDR         #xd6 1 "push (cdr l3)")      ;; ensure opcode & #x06 = 6
   (define-bc BC_POKE_B                POKE_B              #xd8 1 "poke byte")
   (define-bc VM_INTERPRETER_INC_PC    _                   #xda 1 "reserved")           ;; reserved
   (define-bc VM_INTERPRETER_INC_PC    _                   #xdc 1 "reserved")           ;; reserved
   (define-bc VM_INTERPRETER_INC_PC    _                   #xde 1 "reserved")           ;; reserved
   (define-bc BC_CxxR                  CAAR                #xe0 1 "caar")               ;; and #x1f needs to produce 0!
   (define-bc VM_INTERPRETER_INC_PC    _                   #xe2 1 "reserved")           ;; reserved
   (define-bc VM_INTERPRETER_INC_PC    _                   #xe4 1 "reserved")           ;; reserved
   (define-bc BC_CxxR                  CADR                #xe6 1 "cadr")               ;; and #x1f needs to produce #x06 (6)!
   (define-bc VM_INTERPRETER_INC_PC    _                   #xe8 1 "reserved")           ;; reserved
   (define-bc VM_INTERPRETER_INC_PC    _                   #xea 1 "reserved")           ;; reserved
   (define-bc BC_CxxR                  CDAR                #xec 1 "cdar")               ;; and #x1f needs to produce #x0c (12)!
   (define-bc VM_INTERPRETER_INC_PC    _                   #xee 1 "reserved")           ;; reserved
   (define-bc BC_GET_ARRAY_FIELD       GET_AF_0            #xf0 1 "get array field 0")  ;; ensure opcode & #x06 = 0
   (define-bc BC_GET_ARRAY_FIELD       GET_AF_1            #xf2 1 "get array field 1")  ;; ensure opcode & #x06 = 2
   (define-bc BC_GET_ARRAY_FIELD       GET_AF_2            #xf4 1 "get array field 2")  ;; ensure opcode & #x06 = 4
   (define-bc BC_GET_ARRAY_FIELD       GET_AF_3            #xf6 1 "get array field 3")  ;; ensure opcode & #x06 = 6
   (define-bc BC_GET_RA_ARRAY_FIELD    GET_RA_AF_0         #xf8 1 "get (ra),0")         ;; ensure opcode & #x06 = 0
   (define-bc BC_GET_RA_ARRAY_FIELD    GET_RA_AF_1         #xfa 1 "get (ra),1")         ;; ensure opcode & #x06 = 2
   (define-bc BC_GET_RA_ARRAY_FIELD    GET_RA_AF_2         #xfc 1 "get (ra),2")         ;; ensure opcode & #x06 = 4
   (define-bc BC_GET_RA_ARRAY_FIELD    GET_RA_AF_3         #xfe 1 "get (ra),3")         ;; ensure opcode & #x06 = 6
   ))

(module+ test #| bc-opcode-definitions |#
  (check-equal?
   (map (lambda (od)
          (cond
            [(od-simple-bc? od) (od-simple-bc--byte-code od)]
            [(od-extended-bc? od) (od-extended-bc--byte-code od)]
            [else (raise-user-error "unknown opcode definition")]))
        bc-opcode-definitions )
   (build-list 128 (lambda (idx) (* 2 idx)))
   "the opcodes must have even numbered byte codes, starting at 0")

  (define extended-opcode (findf (lambda (od) (od-extended-bc? od)) bc-opcode-definitions))

  (check-equal?
   (map (lambda (od)
          (cond
            [(od-simple-bc? od) (od-simple-bc--byte-code od)]
            [else (raise-user-error "no nesting of extended commands allowed (yet)")]))
        (od-extended-bc--sub-commands extended-opcode))
   (build-list (length (od-extended-bc--sub-commands extended-opcode)) (lambda (i) i))
   "the extended opcodes must be consequatively numbered, starting at 0"))

;; disassemble the given opcode using the actual 3 bytes bc, bc_p1, bc_p2
(define/contract (disassemble-od-simple-bc dyn-opcode-def labels bc bc_p1 bc_p2)
  (-> od-simple-bc? hash? byte? byte? byte? string? )
  (define df (od-simple-bc--disassembler dyn-opcode-def))
  (format "~a\t(~a)"
          (od-simple-bc--bc-opcode dyn-opcode-def)
          (if (string? df)
              df
              (apply df (list labels bc bc_p1 bc_p2)))))

(module+ test
  (check-equal? (disassemble-od-simple-bc (find-dyn-opcode-def (get-single-opcode "CALL"))
                                          (hash)
                                          (get-single-opcode "CALL")
                                          #x00 #x20)
                "CALL\t(call $2001 )")
  (check-equal? (disassemble-od-simple-bc (find-dyn-opcode-def (get-single-opcode "PUSH_I"))
                                          (hash)
                                          (get-single-opcode "PUSH_I")
                                          #x38 #x17)
                "PUSH_I\t(push int $1738)")
    (check-equal? (disassemble-od-simple-bc (find-dyn-opcode-def (get-single-opcode "IADD"))
                                          (hash)
                                          (get-single-opcode "IADD")
                                          0 0)
                "IADD\t(int +)"))

;; get the number of bytes used by this simple command
(define/contract (byte-count-od-simple-bc dyn-opcode-def bc bc_p1)
  (-> od-simple-bc? byte? byte? byte?)
  (define df (od-simple-bc--byte-count dyn-opcode-def))
  (if (byte? df)
      df
      (apply df (list bc bc_p1))))

;; find the opcode definition of the given bc (using a list of definitions)
(define/contract (find-dyn-opcode-def bc (opcode-defs bc-opcode-definitions))
  (->* [byte?] [(listof (or/c od-simple-bc? od-extended-bc?))] (or/c od-simple-bc? od-extended-bc? #f))
  (findf (lambda (od)
           (or (and (od-simple-bc? od)
                 (eq? (od-simple-bc--byte-code od) bc))
              (and (od-extended-bc? od)
                 (eq? (od-extended-bc--byte-code od) bc))))
         opcode-defs))

;; get the simple defintition of the given byte codes, searching extended definitions if need be
(define/contract (get-dyn-opcode-simple-def bc bc-p1)
  (-> byte? byte? (or/c #f od-simple-bc?))
  (define opcode (find-dyn-opcode-def bc))
  (cond
    [(od-simple-bc? opcode)
     opcode]
    [(od-extended-bc? opcode)
     (find-dyn-opcode-def bc-p1 (od-extended-bc--sub-commands opcode))]
    [else #f]))

;; write the given jump target label for the byte-code into the optable
;; optable:   jump table for all byte code commands
;; byte-code: opcode to write into the jump table
;; label:     label of the assembler routine implementing the code for byte-code
(define/contract (write-opcode-into-optable optable byte-code label)
  (-> (listof ast-command?) byte? string? (listof ast-command?))
  (define idx (add1 (arithmetic-shift byte-code -1)))
  (append
   (take optable idx)
   (list (ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd label)))
   (drop optable (add1 idx))))

;; write the high byte for the extended jump table
;; x-optable: extended jump table for extended byte code commands
;; label:     label of the assembler routine implementing the code for byte-code
;; byte-code: opcode to write into the jump table
(define (write-hb-opcode-into-x-optable x-optable label byte-code)
  (write-byte-opcode-into-x-optable x-optable label byte-code 'high-byte))

;; write the low byte for the extended jump table
;; x-optable: extended jump table for extended byte code commands
;; label:     label of the assembler routine implementing the code for byte-code
;; byte-code: opcode to write into the jump table
(define (write-lb-opcode-into-x-optable x-optable label byte-code)
  (write-byte-opcode-into-x-optable x-optable label byte-code 'low-byte))

;; generic write the low or high byte for the extended jump table
(define (write-byte-opcode-into-x-optable x-optable label byte-code lb-indicator)
  (define idx (add1 byte-code))
  (define table-len (length x-optable))
  (define x-table (if (> table-len idx)
                      x-optable
                      (append x-optable
                              (build-list (- idx table-len)
                                          (lambda (_i) (ast-unresolved-bytes-cmd '() '() (ast-resolve-byte-scmd "VM_INTERPRETER_INC_PC" lb-indicator)))))))
  (append
   (take x-table idx)
   (list (ast-unresolved-bytes-cmd '() '() (ast-resolve-byte-scmd label lb-indicator)))
   (if (> table-len idx)
       (drop x-table (add1 idx))
       (list))))

;; get the list of byte codes for the given opcode-label (searching the opcode definitions)
(define (fetch-opcode-list opcode-label (opdefs bc-opcode-definitions))
  (define fod (findf (lambda (od)
                      (cond
                        [(and (od-simple-bc? od)
                            (string=? (od-simple-bc--bc-opcode od) opcode-label))
                         od]
                        [(and (od-extended-bc? od)
                            (findf (lambda (ods)
                                     (cond
                                       [(and (od-simple-bc? ods)
                                           (string=? (od-simple-bc--bc-opcode ods) opcode-label))
                                        ods]
                                       [else #f]))
                                   (od-extended-bc--sub-commands od)))
                         od]
                        [else #f]))
                    opdefs))
  (cond
    [(od-simple-bc? fod)
     (list (od-simple-bc--byte-code fod))]
    [(od-extended-bc? fod)
     (flatten (append
               (list (od-extended-bc--byte-code fod))
               (fetch-opcode-list opcode-label (od-extended-bc--sub-commands fod))))]
    [else (raise-user-error (format "opcode not found ~a" opcode-label))]))

;; get a single byte code for the given byte code command
(define (get-single-opcode label)
  (define opcodes (fetch-opcode-list label))
  (when (> (length opcodes) 1)
    (raise-user-error (format "opcode ~a has more than one byte code (rather a list)" opcodes)))
  (car opcodes))

;; syntactic sugar to get the assembler command for the given byte code
(define-syntax (bc stx)
  (syntax-case stx ()
    ([_ label]
     #`(ast-bytes-cmd #,(retrieve-meta-info-from stx);; (list '#:filename #,(syntax-source #'stx) '#:line #,(syntax-line #'stx))
                      (fetch-opcode-list (symbol->string 'label))))))

(module+ test #| bc |#
  (check-match (bc CALL)
               (ast-bytes-cmd _ '(#x68)))

  ;; extended command consisting of two byte codes
  (check-match (bc IMAX)
               (ast-bytes-cmd _ '(#x08 #x01)))

  (check-match (bc IADD)
               (ast-bytes-cmd _ '(#xbe))))


;; build table of lowbyte jump table for extended opcodes
(define (build-extended-optable-lb bc-defs (lb-table VM_INTERPRETER_OPTABLE_EXT1_LB))
  (let ([ext-cmd (findf (lambda (od) (od-extended-bc? od)) bc-defs)])
   (foldl (lambda (od-simple acc)
            (cond
              [(od-simple-bc? od-simple)
               (write-lb-opcode-into-x-optable
                acc
                (string-append (od-simple-bc--label od-simple) "-1") ;; add -1 to label to allow for RTS jumps (see src/nmil/vm-interpreter-bc/misc.rkt)
                (od-simple-bc--byte-code od-simple))]
              [else (raise-user-error "unknown opcode definition")]))
          lb-table
          (if ext-cmd
              (od-extended-bc--sub-commands ext-cmd)
              (list)))))

(define full-extended-optable-lb
  (build-extended-optable-lb bc-opcode-definitions))

;; build table of high byte jump table for extended opcodes
(define (build-extended-optable-hb bc-defs (hb-table VM_INTERPRETER_OPTABLE_EXT1_HB))
  (let ([ext-cmd (findf (lambda (od) (od-extended-bc? od)) bc-defs)])
   (foldl (lambda (od-simple acc)
            (cond
              [(od-simple-bc? od-simple)
               (write-hb-opcode-into-x-optable
                acc
                (string-append (od-simple-bc--label od-simple) "-1") ;; add -1 to label to allow for RTS jumps (see src/nmil/vm-interpreter-bc/misc.rkt)
                (od-simple-bc--byte-code od-simple))]
              [else (raise-user-error "unknown opcode definition")]))
          hb-table
          (if ext-cmd
              (od-extended-bc--sub-commands ext-cmd)
              (list)))))

(define full-extended-optable-hb
  (build-extended-optable-hb bc-opcode-definitions))

;; build jumptable for non extended (one byte) bcs (lb,hb combinations)
(define (build-interpreter-optable bc-defs (table VM_INTERPRETER_OPTABLE))
  (foldl (lambda (od acc)
           (cond
             [(od-simple-bc? od)
              (write-opcode-into-optable acc (od-simple-bc--byte-code od) (od-simple-bc--label od) )]
             [(od-extended-bc? od)
              (write-opcode-into-optable acc (od-extended-bc--byte-code od) (od-extended-bc--label od))]
             [else (raise-user-error "unknown opcode definition")]))
         table
         bc-defs))

(define full-interpreter-opcode-table
  (build-interpreter-optable bc-opcode-definitions))

;; get a filtered list of opcodes (useful for testing, filtering out irrelevant codes)
(define (filtered-opcode-definitions wanted-bc-list (bc-defs bc-opcode-definitions))
  (map (lambda (od)
         (cond
           [(od-simple-bc? od) od]
           [(od-extended-bc? od)
            (define new-sub-commands (filtered-opcode-definitions wanted-bc-list (od-extended-bc--sub-commands od)))
            (struct-copy od-extended-bc od [-sub-commands new-sub-commands])]
           [else (raise-user-error (format "unknown definition type ~a" od))]))
       (filter (lambda (od)
                 (cond
                   [(od-simple-bc? od)
                    (memf (lambda (s) (string=? s (od-simple-bc--label od))) wanted-bc-list)]
                   [(od-extended-bc? od)
                    (not (empty? (filtered-opcode-definitions wanted-bc-list (od-extended-bc--sub-commands od))))]
                   [else (raise-user-error (format "unknown definition type ~a" od))]))
               bc-defs)))

(module+ test #| filtered-opcode-definitions |#
  (check-equal? (filtered-opcode-definitions (list "BC_B_GT_P"))
                (list (od-simple-bc "BC_B_GT_P" "B_GT_P" 72 1 "byte >?")))

  (check-equal? (filtered-opcode-definitions (list "BC_IMAX" "BC_B_GT_P"))
                (list (od-extended-bc "BC_EXT1_CMD" #x08 (list (od-simple-bc "BC_IMAX" "IMAX" 1 1 "int max")))
                      (od-simple-bc "BC_B_GT_P" "B_GT_P" 72 1 "byte >?")))

  (check-equal? (length (filtered-opcode-definitions (list "BC_PUSH_B" "BC_B_GT_P")))
                2))
