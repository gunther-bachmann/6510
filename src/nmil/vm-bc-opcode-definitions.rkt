#lang racket/base

#|

Byte Code Opcodes are completely defined here to be able to quickly switch between
- param encoded byte code
- single byte code
- multiple byte code (extended byte code)
depending on number of usage to make it as compact as possible!

|#

(require (only-in racket/contract define/contract struct-guard/c or/c -> listof ->*))
(require (only-in "../util.rkt"
                  bytes->int
                  format-hex-byte
                  format-hex-word))
(require (only-in racket/list
                  take
                  flatten
                  drop))
(require (only-in "../ast/6510-command.rkt"
                  ast-command?
                  ast-bytes-cmd
                  ast-bytes-cmd-bytes
                  ast-unresolved-bytes-cmd
                  ast-resolve-byte-scmd
                  ast-resolve-word-scmd))
(require (only-in "../tools/6510-disassembler.rkt" info-for-label))

(provide bc-opcode-definitions
         (struct-out od-simple-bc)
         (struct-out od-extended-bc)
         get-dyn-opcode-simple-def
         find-dyn-opcode-def
         write-opcode-into-optable
         write-hb-opcode-into-x-optable
         write-lb-opcode-into-x-optable
         disassemble-od-simple-bc
         byte-count-od-simple-bc
         fetch-opcode-list
         get-single-opcode
         bc)

(define-struct od-simple-bc
  (-label               ;; ast label into the implementation of this bc within the interpreter
   -bc-opcode           ;; opcode to be used in programs
   -byte-code           ;; the actual byte code (must be even!)
   -disassembler        ;; function to disassemble this byte code (and its operands)
   -byte-count)         ;; function returning the number of bytes used by this byte code and its operands
  #:transparent
  #:guard (struct-guard/c
           string?
           string?
           byte?
           (or/c string? (-> hash? byte? byte? byte? string?))
           (or/c byte? (-> byte? byte? byte?))))

(define-struct od-extended-bc
  (-label               ;; ast label into the implementation of this bc (loading code from the extended table)
   -byte-code           ;; the actual byte code (can be any byte, no need to be even)
   -sub-commands)       ;; subcommands (all od-simple-bc definitions) jumped at by the extended command
  #:transparent
  #:guard (struct-guard/c
           string?
           byte?
           (listof od-simple-bc?)))



(define bc-opcode-definitions
  (list
   (od-simple-bc "BC_PUSH_LOCAL_SHORT" "PUSH_L0"       #x00 "push l0" 1)
   (od-simple-bc "BC_PUSH_LOCAL_SHORT" "PUSH_L1"       #x02 "push l1" 1)
   (od-simple-bc "BC_PUSH_LOCAL_SHORT" "PUSH_L2"       #x04 "push l2" 1)
   (od-simple-bc "BC_PUSH_LOCAL_SHORT" "PUSH_L3"       #x06 "push l3" 1)

   (od-extended-bc "BC_EXT1_CMD"             #x08
    (list (od-simple-bc "VM_INTERPRETER_INC_PC_2_TIMES" "" #x00 "reserved" 1)
          (od-simple-bc "BC_IMAX"      "IMAX"                    #x01 "int max" 1)
          (od-simple-bc "BC_IINC"      "IINC"                    #x02 "int inc" 1)
          (od-simple-bc "BC_GC_FL"     "GC"                      #x03 "gc" 1)))

   (od-simple-bc "VM_INTERPRETER_INC_PC" ""                #x0a "reserved" 1) ;; reserved
   (od-simple-bc "BC_PUSH_I"             "PUSH_I"           #x0c
                 (lambda (_l _bc bc-p1 bc-p2) (format "push int $~a" (format-hex-word (bytes->int bc-p1 bc-p2))))
                 3)
   (od-simple-bc "BC_INT_P"             "INT_P"             #x0e "int?" 1)
   (od-simple-bc "BC_WRITE_LOCAL_SHORT" "WRITE_L0"     #x10 "write l0" 1)
   (od-simple-bc "BC_WRITE_LOCAL_SHORT" "WRITE_L1"     #x12 "write l1" 1)
   (od-simple-bc "BC_WRITE_LOCAL_SHORT" "WRITE_L2"     #x14 "write l2" 1)
   (od-simple-bc "BC_WRITE_LOCAL_SHORT" "WRITE_L3"     #x16 "write l3" 1)
   (od-simple-bc "BC_T_P_BRA"           "T_P_BRA"          #x18
                 (lambda (_l _bc bc-p1 _bc-p2) (format "branch on true? by $~a" (format-hex-byte bc-p1)))
                 2)
   (od-simple-bc "BC_F_P_BRA"           "F_P_BRA"          #x1a
                 (lambda (_l _bc bc-p1 _bc-p2) (format "branch on false? by $~a" (format-hex-byte bc-p1)))
                 2)
   (od-simple-bc "BC_F_P_RET"            "F_P_RET"         #x1c "ret on false?" 1)
   (od-simple-bc "BC_DUP"                "DUP"             #x1e "int?" 1)
   (od-simple-bc "BC_POP_TO_LOCAL_SHORT" "POP_TO_L0"   #x20 "pop to l0" 1)
   (od-simple-bc "BC_POP_TO_LOCAL_SHORT" "POP_TO_L1"   #x22 "pop to l1" 1)
   (od-simple-bc "BC_POP_TO_LOCAL_SHORT" "POP_TO_L2"   #x24 "pop to l2" 1)
   (od-simple-bc "BC_POP_TO_LOCAL_SHORT" "POP_TO_L3"   #x26 "pop to l3" 1)
   (od-simple-bc "BC_PUSH_NIL"           "PUSH_NIL"        #x28 "push nil" 1)
   (od-simple-bc "BC_PUSH_AF"            "PUSH_AF"         #x2a "push array field" 1)
   (od-simple-bc "BC_POP_TO_AF"          "POP_TO_AF"       #x2c "pop to array field" 1)
   (od-simple-bc "BC_PUSH_B"             "PUSH_B"           #x2e
                 (lambda (_l _bc bc-p1 _bc_p2) (format "push byte $~a" (format-hex-byte bc-p1)))
                 2)
   (od-simple-bc "BC_WRITE_TO_LOCAL_SHORT" "WRITE_TO_L0"  #x30 "write to l0" 1)
   (od-simple-bc "BC_WRITE_TO_LOCAL_SHORT" "WRITE_TO_L1"  #x32 "write to l1" 1)
   (od-simple-bc "BC_WRITE_TO_LOCAL_SHORT" "WRITE_TO_L2"  #x34 "write to l2" 1)
   (od-simple-bc "BC_WRITE_TO_LOCAL_SHORT" "WRITE_TO_L3"  #x36 "write to l3" 1)
   (od-simple-bc "BC_BINC"                 "BINC"  #x38 "byte inc" 1)
   (od-simple-bc "BC_NZ_P_BRA"             "NZ_P_BRA"  #x3a
                 (lambda (_l _bc bc-p1 _bc-p2) (format "branch on not zero? by $~a" (format-hex-byte bc-p1)))
                 2)
   (od-simple-bc "BC_CELL_EQ_P"            "CELL_EQ_P"  #x3c "cell eq?" 1)
   (od-simple-bc "BC_F_P_RET_F"            "F_P_RET_F"  #x3e "ret false on false?" 1)
   (od-simple-bc "VM_INTERPRETER_INC_PC"   ""  #x40 "reserved" 1) ;; reserved
   (od-simple-bc "BC_NIL_P"                "NIL_P"  #x42 "nil?" 1)
   (od-simple-bc "BC_I_Z_P"                "I_Z_P"  #x44 "int 0?" 1)
   (od-simple-bc "BC_BADD"                 "BADD"  #x46 "byte +" 1)
   (od-simple-bc "BC_B_GT_P"               "B_GT_P"  #x48 "byte >?" 1)
   (od-simple-bc "BC_NATIVE"               "NATIVE"  #x4a "native" 1) ;; need to be fixed on 4a, implementation dependes on it
   (od-simple-bc "BC_B_GE_P"               "B_GE_P"  #x4c "byte >=?" 1)
   (od-simple-bc "BC_BSHR"                 "BSHR"  #x4e "byte shift right" 1)
   (od-simple-bc "VM_INTERPRETER_INC_PC"   ""  #x50 "reserved" 1) ;; reserved
   (od-simple-bc "BC_CxxR"                 "CDDR"  #x52 "cddr" 1) ;; CDDR (bitwise-and #x1f x) must be #x12 (18)!
   (od-simple-bc "BC_BREAK"                "BREAK"  #x54 "break" 1)
   (od-simple-bc "BC_SWAP"                 "SWAP"  #x56 "swap" 1)
   (od-simple-bc "BC_POP"                  "POP"  #x58 "pop" 1)
   (od-simple-bc "BC_CONS_PAIR_P"          "CONS_PAIR_P"  #x5a "pair?" 1)
   (od-simple-bc "BC_T_P_RET"              "T_P_RET"  #x5c "ret on true?" 1)
   (od-simple-bc "BC_Z_P_BRA"              "Z_P_BRA"  #x5e
                 (lambda (_l _bc bc-p1 _bc-p2) (format "branch on zero? by $~a" (format-hex-byte bc-p1)))
                 2)
   (od-simple-bc "BC_SET_ARRAY_FIELD"      "SET_AF0"  #x60 "set array field 0" 1)
   (od-simple-bc "BC_SET_ARRAY_FIELD"      "SET_AF1"  #x62 "set array field 1" 1)
   (od-simple-bc "BC_SET_ARRAY_FIELD"      "SET_AF2"  #x64 "set array field 2" 1)
   (od-simple-bc "BC_SET_ARRAY_FIELD"      "SET_AF3"  #x66 "set array field 3" 1)
   (od-simple-bc "BC_CALL"                 "CALL"  #x68
                 (lambda (labels _bc bc-p1 bc-p2)
                   (format "call ~a ~a" (format-hex-word (add1 (bytes->int bc-p1 bc-p2))) ;; add 1 because byte code starts there (after #locals)
                           (info-for-label (number->string (bytes->int bc-p1 bc-p2) 16) labels)))
                 3)
   (od-simple-bc "BC_TAIL_CALL"             "TAIL_CALL"  #x6a "tail call" 1)
   (od-simple-bc "BC_BDEC"                  "BDEC"  #x6c "byte dec" 1)
   (od-simple-bc "BC_CONS"                  "CONS"  #x6e "cons" 1)
   (od-simple-bc "BC_PUSH_INT0"             "PUSH_I0"  #x70 "push int 0" 1)
   (od-simple-bc "BC_PUSH_INT1"             "PUSH_I1"  #x72 "push int 1" 1)
   (od-simple-bc "BC_PUSH_INT2"             "PUSH_I2"  #x74 "push int 2" 1)
   (od-simple-bc "BC_PUSH_INTm1"            "PUSH_IM1"  #x76 "push int -1" 1)
   (od-simple-bc "BC_GOTO"                  "GOTO"  #x78
                 (lambda (_l _bc bc-p1 _bc-p2) (format "goto relative by $~a" (format-hex-byte bc-p1)))
                 2)
   (od-simple-bc "BC_RET"                    "RET"  #x7a "ret" 1)
   (od-simple-bc "BC_BNOP"                   "BNOP"  #x7c "nop" 1)
   (od-simple-bc "BC_CDR"                    "CDR"  #x7e "cdr" 1)
   (od-simple-bc "BC_Z_P_RET_POP_N"          "Z_P_RET_POP_1"  #x80 "ret on zero? and pop 1" 1)
   (od-simple-bc "BC_Z_P_RET_POP_N"          "Z_P_RET_POP_2"  #x82 "ret on zero? and pop 2" 1)
   (od-simple-bc "BC_Z_P_RET_POP_N"          "Z_P_RET_POP_3"  #x84 "ret on zero? and pop 3" 1)
   (od-simple-bc "BC_Z_P_RET_POP_N"          "Z_P_RET_POP_4"  #x86 "ret on zero? and pop 4" 1)
   (od-simple-bc "BC_COONS"                  "COONS"  #x88 "coons" 1)
   (od-simple-bc "BC_SWAP_RA_RB"             "SWAP_RA_RB"  #x8a "swap RA<->RB" 1)
   (od-simple-bc "BC_POP_TO_RB"              "POP_TO_RB"  #x8c "pop to rb" 1)
   (od-simple-bc "BC_PUSH_RA"                "PUSH_RA"  #x8e "push ra" 1)
   (od-simple-bc "BC_SET_RA_ARRAY_FIELD"     "SET_RA_AF_0"  #x90 "set (ra),0" 1)
   (od-simple-bc "BC_SET_RA_ARRAY_FIELD"     "SET_RA_AF_1"  #x92 "set (ra),1" 1)
   (od-simple-bc "BC_SET_RA_ARRAY_FIELD"     "SET_RA_AF_2"  #x94 "set (ra),2" 1)
   (od-simple-bc "BC_SET_RA_ARRAY_FIELD"     "SET_RA_AF_3"  #x96 "set (ra),3" 1)
   (od-simple-bc "BC_ALLOC_ARA"              "ALLOC_ARA"  #x98 "alloc array to ra" 1)
   (od-simple-bc "BC_PUSH_RA_AF"             "PUSH_RA_AF"  #x9a "push (ra),rai" 1)
   (od-simple-bc "BC_POP_TO_RA_AF"           "POP_TO_RA_AF"  #x9c "pop to (ra),rai" 1)
   (od-simple-bc "BC_POP_TO_RAI"             "POP_TO_RAI"  #x9e "pop byte to rai" 1)
   (od-simple-bc "BC_PUSH_LX_CAR"            "PUSH_L0_CAR"  #xa0 "push (car l0)" 1)
   (od-simple-bc "BC_PUSH_LX_CAR"            "PUSH_L1_CAR"  #xa2 "push (car l1)" 1)
   (od-simple-bc "BC_PUSH_LX_CAR"            "PUSH_L2_CAR"  #xa4 "push (car l2)" 1)
   (od-simple-bc "BC_PUSH_LX_CAR"            "PUSH_L3_CAR"  #xa6 "push (car l3)" 1)
   (od-simple-bc "BC_DEC_RBI_NZ_P_BRA"       "DEC_RBI_NZ_P_BRA"  #xa8
                 (lambda (_l _bc bc-p1 _bc-p2) (format "dec rbi, not zero? -> branch by  $~a" (format-hex-byte bc-p1)))
                 2)
   (od-simple-bc "BC_WRITE_RA"               "WRITE_RA"  #xaa "write ra" 1)
   (od-simple-bc "BC_WRITE_TO_RAI"           "WRITE_TO_RAI"  #xac "write byte to rai" 1)
   (od-simple-bc "BC_DEC_RAI"                "DEC_RAI"  #xae "dec rai" 1)
   (od-simple-bc "BC_NIL_P_RET_L0_POP_N"     "NIL_P_RET_L0_POP_1"  #xb0 "ret l0 on zero? and pop 1" 1)
   (od-simple-bc "BC_NIL_P_RET_L0_POP_N"     "NIL_P_RET_L0_POP_2"  #xb2 "ret l0 on zero? and pop 2" 1)
   (od-simple-bc "BC_NIL_P_RET_L0_POP_N"     "NIL_P_RET_L0_POP_3"  #xb4 "ret l0 on zero? and pop 3" 1)
   (od-simple-bc "BC_NIL_P_RET_L0_POP_N"     "NIL_P_RET_L0_POP_4"  #xb6 "ret l0 on zero? and pop 4" 1)
   (od-simple-bc "BC_WRITE_TO_RBI"           "WRITE_TO_RBI"  #xb8 "write to rbi" 1)
   (od-simple-bc "BC_CAR"                    "CAR"  #xba "car" 1)
   (od-simple-bc "BC_ISUB"                   "ISUB"  #xbc "int -" 1)
   (od-simple-bc "BC_IADD"                   "IADD"  #xbe "int +" 1)
   (od-simple-bc "BC_NZ_P_RET_POP_N"         "NZ_P_RET_POP_1"  #xc0 "ret on not zero? and pop 1" 1)
   (od-simple-bc "BC_NZ_P_RET_POP_N"         "NZ_P_RET_POP_2"  #xc2 "ret on not zero? and pop 2" 1)
   (od-simple-bc "BC_NZ_P_RET_POP_N"         "NZ_P_RET_POP_3"  #xc4 "ret on not zero? and pop 3" 1)
   (od-simple-bc "BC_NZ_P_RET_POP_N"         "NZ_P_RET_POP_4"  #xc6 "ret on not zero? and pop 4" 1)
   (od-simple-bc "BC_I_GT_P"                 "I_GT_P"  #xc8 "int >?" 1)
   (od-simple-bc "BC_BINC_RAI"               "BINC_RAI"  #xca "inc rai" 1)
   (od-simple-bc "BC_B_LT_P"                 "B_LT_P"  #xcc "byte <?" 1)
   (od-simple-bc "BC_POP_TO_RA"              "POP_TO_RA"  #xce "pop to ra" 1)
   (od-simple-bc "BC_PUSH_LX_CDR"            "PUSH_L0_CDR"  #xd0 "push (cdr l0)" 1)
   (od-simple-bc "BC_PUSH_LX_CDR"            "PUSH_L1_CDR"  #xd2 "push (cdr l1)" 1)
   (od-simple-bc "BC_PUSH_LX_CDR"            "PUSH_L2_CDR"  #xd4 "push (cdr l2)" 1)
   (od-simple-bc "BC_PUSH_LX_CDR"            "PUSH_L3_CDR"  #xd6 "push (cdr l3)" 1)
   (od-simple-bc "BC_POKE_B"                 "POKE_B"  #xd8 "poke byte" 1)
   (od-simple-bc "VM_INTERPRETER_INC_PC"     ""  #xda "reserved" 1) ;; reserved
   (od-simple-bc "VM_INTERPRETER_INC_PC"     ""  #xdc "reserved" 1) ;; reserved
   (od-simple-bc "VM_INTERPRETER_INC_PC"     ""  #xde "reserved" 1) ;; reserved
   (od-simple-bc "BC_CxxR"                   "CAAR"  #xe0 "caar" 1) ;; and #x1f needs to produce 0!
   (od-simple-bc "VM_INTERPRETER_INC_PC"     ""  #xe2 "reserved" 1) ;; reserved
   (od-simple-bc "VM_INTERPRETER_INC_PC"     ""  #xe4 "reserved" 1) ;; reserved
   (od-simple-bc "BC_CxxR"                   "CADR"  #xe6 "cadr" 1) ;; and #x1f needs to produce #x06 (6)!
   (od-simple-bc "VM_INTERPRETER_INC_PC"     ""  #xe8 "reserved" 1) ;; reserved
   (od-simple-bc "VM_INTERPRETER_INC_PC"     ""  #xea "reserved" 1) ;; reserved
   (od-simple-bc "BC_CxxR"                   "CDAR"  #xec "cdar" 1) ;; and #x1f needs to produce #x0c (12)!
   (od-simple-bc "VM_INTERPRETER_INC_PC"     ""  #xee "reserved" 1) ;; reserved
   (od-simple-bc "BC_GET_ARRAY_FIELD"        "GET_AF_0"  #xf0 "get array field 0" 1)
   (od-simple-bc "BC_GET_ARRAY_FIELD"        "GET_AF_1"  #xf2 "get array field 1" 1)
   (od-simple-bc "BC_GET_ARRAY_FIELD"        "GET_AF_2"  #xf4 "get array field 2" 1)
   (od-simple-bc "BC_GET_ARRAY_FIELD"        "GET_AF_3"  #xf6 "get array field 3" 1)
   (od-simple-bc "BC_GET_RA_ARRAY_FIELD"     "GET_RA_AF_0"  #xf8 "get (ra),0" 1)
   (od-simple-bc "BC_GET_RA_ARRAY_FIELD"     "GET_RA_AF_1"  #xfa "get (ra),1" 1)
   (od-simple-bc "BC_GET_RA_ARRAY_FIELD"     "GET_RA_AF_2"  #xfc "get (ra),2" 1)
   (od-simple-bc "BC_GET_RA_ARRAY_FIELD"     "GET_RA_AF_3"  #xfe "get (ra),3" 1)
   ))

(define/contract (disassemble-od-simple-bc dyn-opcode-def labels bc bc_p1 bc_p2)
  (-> od-simple-bc? hash? byte? byte? byte? string? )
  (define df (od-simple-bc--disassembler dyn-opcode-def))
  (if (string? df)
      df
      (apply df (list labels bc bc_p1 bc_p2))))

(define/contract (byte-count-od-simple-bc dyn-opcode-def bc bc_p1)
  (-> od-simple-bc? byte? byte? byte?)
  (define df (od-simple-bc--byte-count dyn-opcode-def))
  (if (byte? df)
      df
      (apply df (list bc bc_p1))))

(define/contract (find-dyn-opcode-def bc (opcode-defs bc-opcode-definitions))
  (->* [byte?] [(listof (or/c od-simple-bc? od-extended-bc?))] (or/c od-simple-bc? od-extended-bc?))
  (findf (lambda (od)
           (or (and (od-simple-bc? od)
                 (eq? (od-simple-bc--byte-code od) bc))
              (and (od-extended-bc? od)
                 (eq? (od-extended-bc--byte-code od) bc))))
         opcode-defs))

(define/contract (get-dyn-opcode-simple-def bc bc-p1)
  (-> byte? byte? (or/c #f od-simple-bc?))
  (define opcode (find-dyn-opcode-def bc))
  (cond
    [(od-simple-bc? opcode)
     opcode]
    [(od-extended-bc? opcode)
     (find-dyn-opcode-def bc-p1 (od-extended-bc--sub-commands opcode))]
    [else #f]))

(define/contract (write-opcode-into-optable optable byte-code label)
  (-> (listof ast-command?) byte? string? (listof ast-command?))
  (define idx (add1 (arithmetic-shift byte-code -1)))
  (append
   (take optable idx)
   (list (ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd label)))
   (drop optable (add1 idx))))

(define (write-hb-opcode-into-x-optable x-optable label byte-code)
  (write-byte-opcode-into-x-optable x-optable label byte-code 'high-byte))

(define (write-lb-opcode-into-x-optable x-optable label byte-code)
  (write-byte-opcode-into-x-optable x-optable label byte-code 'low-byte))

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

(define (fetch-opcode-list opcode-label (opdefs bc-opcode-definitions))
  (define od (findf (lambda (od)
                      (cond
                        [(and (od-simple-bc? od)
                            (string=? (od-simple-bc--bc-opcode od) opcode-label))
                         od]
                        [(and (od-extended-bc? od)
                            (findf (lambda (ods)
                                     (cond
                                       [(and (od-simple-bc? ods)
                                           (string=? (od-simple-bc--bc-opcode ods) opcode-label))]
                                       [else #f]))
                                   (od-extended-bc--sub-commands od)))
                         od]
                        [else #f]))
                    opdefs))
  (cond
    [(od-simple-bc? od)
     (list (od-simple-bc--byte-code od))]
    [(od-extended-bc? od)
     (flatten (append
               (list (od-extended-bc--byte-code od))
               (fetch-opcode-list opcode-label (od-extended-bc--sub-commands od))))]
    [else (raise-user-error (format "opcode not found ~a" opcode-label))]))

(define (get-single-opcode label)
  (define opcodes (fetch-opcode-list label))
  (when (> (length opcodes) 1)
    (raise-user-error (format "opcode ~a has more than one byte code (rather a list)" opcodes)))
  (car opcodes))

(define-syntax-rule  (bc label)
  (ast-bytes-cmd '() (fetch-opcode-list (symbol->string 'label))))

(module+ test #| bc |#
  (require "../6510-test-utils.rkt")

  (check-equal? (bc IMAX)
                (ast-bytes-cmd '() '(8 1)))

  (check-equal? (bc IADD)
                (ast-bytes-cmd '() '(190))))
