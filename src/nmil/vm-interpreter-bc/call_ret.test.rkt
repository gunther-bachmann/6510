#lang racket/base

#|

  implement bc call/return commands

|#

(module+ test
  (require (only-in "./arrays.rkt"
                    VM_REFCOUNT_DECR_ARRAY_REGS)
           (only-in "./call_ret.rkt"
                    BC_CALL
                    BC_NIL_P_RET_L0_POP_N
                    BC_TAIL_CALL
                    BC_RET)
           (only-in "./cell-pair.rkt"
                    BC_PUSH_NIL)
           (only-in "./cell-pair.rkt"
                    BC_CONS
                    BC_CAR
                    BC_CDR
                    bc-cell-pair-code)
           (only-in "./misc.rkt"
                    BC_BNOP)
           (only-in "./pop_local.rkt"
                    BC_POP_TO_LOCAL_SHORT
                    BC_WRITE_TO_LOCAL_SHORT)
           (only-in "./push_const.rkt"
                    BC_PUSH_CONST_NUM_SHORT)
           (only-in "./push_local.rkt"
                    BC_PUSH_LOCAL_SHORT
                    PUSH_RT_WRITE_LOCAL_bc_enc
                    BC_PUSH_LOCAL_CXR)
           "./test-utils.rkt")

  (define relevant-opcode-definitions (filtered-opcode-definitions
                                       (list "BC_PUSH_LOCAL_SHORT"        ;; PUSH_L0-3
                                             "BC_PUSH_LX_CAR"             ;; PUSH L0-3 CAR
                                             "BC_PUSH_CONST_NUM_SHORT"    ;;
                                             "BC_PUSH_INT0"               ;; PUSH_I0
                                             "BC_PUSH_INT1"               ;; ..
                                             "BC_PUSH_INT2"               ;; ..
                                             "BC_PUSH_INTm1"              ;; PUSH_IM1
                                             "BC_CALL"                    ;; CALL
                                             "BC_TAIL_CALL"
                                             "BC_RET"
                                             "BC_BREAK"
                                             "BC_POP_TO_LOCAL_SHORT"      ;; POP_TO_L0-3
                                             "BC_WRITE_TO_LOCAL_SHORT"    ;; WRITE_TO_L0-3
                                             "BC_PUSH_NIL"
                                             "BC_CONS"
                                             "BC_CDR"
                                             "BC_CAR"
                                             "BC_NIL_P_RET_L0_POP_N"
                                             "BC_BNOP")))   ;;

  (define (wrap-bytecode-for-test bc-to-wrap)
    (wrap-bytecode-for-bc-test
     bc-to-wrap
     relevant-opcode-definitions
     (list BC_CALL
           BC_NIL_P_RET_L0_POP_N
           BC_TAIL_CALL
           BC_RET
           ;; ---
           BC_PUSH_LOCAL_SHORT
           BC_PUSH_LOCAL_CXR
           PUSH_RT_WRITE_LOCAL_bc_enc
           BC_PUSH_CONST_NUM_SHORT
           BC_POP_TO_LOCAL_SHORT
           BC_CONS
           BC_CAR
           BC_CDR
           BC_PUSH_NIL
           BC_BNOP
           VM_REFCOUNT_DECR_ARRAY_REGS)))

  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (run-bc-wrapped-in-test- bc wrapped-code debug)))

(module+ test #| bc_call |#
  (define test-bc-before-call-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_I0)
             (bc BREAK))
     ))

  (check-equal? (vm-call-frame->strings test-bc-before-call-state)
                (list (format "call-frame-ptr:   $~a03, topmark: 03" (format-hex-byte PAGE_CALL_FRAME))
                      "program-counter:  $0801"
                      "function-ptr:     $0800"
                      (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 03"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))))
   (check-equal? (vm-stack->strings test-bc-before-call-state)
                 (list "stack holds 2 items"
                       "int $0000  (rt)"
                       "ptr NIL")
                 "stack holds just the pushed int")

  (define test-bc-call-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_I0)
             (bc CALL) (byte 00) (byte $87)
             (bc BREAK)

             (org #x8700)
      (label TEST_FUN)
             (byte 0)            ;; number of locals
             (bc PUSH_I1)     ;; value to return
             (bc BREAK))
     ))

   (check-equal? (vm-call-frame->strings test-bc-call-state)
                   (list (format "call-frame-ptr:   $~a03, topmark: 07" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $8702"
                          "function-ptr:     $8700"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 03"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))
                         (format "slim-frame ($~a03..$~a06)" (format-hex-byte PAGE_CALL_FRAME) (format-hex-byte PAGE_CALL_FRAME))
                         "return-pc:           $0804"
                         "return-function-ptr: $0800"
                         (format "return-locals-ptr:   $~a03, $~a03 (lb,hb)"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))))
   (check-equal? (vm-stack->strings test-bc-call-state)
                    (list "stack holds 3 items"
                          "int $0001  (rt)"
                          "int $0000"
                          "ptr NIL")
                    "stack holds the pushed int and the parameter passed")

  (define test-bc-call-wp-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_I0)
             (bc PUSH_IM1)
             (bc CALL) (byte 00) (byte $17)
             (bc BREAK)

             (org #x1700)
      (label TEST_FUN)
             (byte 0)            ;; number of locals
             (bc PUSH_I1)     ;; value to return
             (bc BREAK))
     ))

  (check-equal? (vm-call-frame->strings test-bc-call-wp-state)
                   (list (format "call-frame-ptr:   $~a03, topmark: 07" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $1702"
                         "function-ptr:     $1700"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 03"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))
                         (format "slim-frame ($~a03..$~a06)" (format-hex-byte PAGE_CALL_FRAME) (format-hex-byte PAGE_CALL_FRAME))
                         "return-pc:           $0805"
                         "return-function-ptr: $0800"
                         (format "return-locals-ptr:   $~a03, $~a03 (lb,hb)"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))))
  (check-equal? (vm-stack->strings test-bc-call-wp-state)
                   (list "stack holds 4 items"
                         "int $0001  (rt)"
                         "int $3fff"
                         "int $0000"
                         "ptr NIL")
                   "stack holds the pushed int, and all parameters")

  (define test-bc-call-wl-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_I0)
             (bc PUSH_IM1)
             (bc CALL) (byte 00) (byte $17)
             (bc BREAK)

             (org #x1700)
      (label TEST_FUN)
             (byte 2)            ;; number of locals
             (bc PUSH_I1)     ;; value to return
             (bc BREAK))))

  (check-equal? (vm-call-frame->strings test-bc-call-wl-state)
                   (list (format "call-frame-ptr:   $~a03, topmark: 07" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $1702"
                         "function-ptr:     $1700"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 05"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))
                         (format "slim-frame ($~a03..$~a06)" (format-hex-byte PAGE_CALL_FRAME) (format-hex-byte PAGE_CALL_FRAME))
                         "return-pc:           $0805"
                         "return-function-ptr: $0800"
                         (format "return-locals-ptr:   $~a03, $~a03 (lb,hb)"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))))
  (check-equal? (vm-stack->strings test-bc-call-wl-state)
                   (list "stack holds 4 items"
                         "int $0001  (rt)"
                         "int $3fff"
                         "int $0000"
                         "ptr NIL")
                   "stack holds the pushed int, and all parameters"))


(module+ test #| bc-nil-ret |#
  (define bc-nil-ret-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_NIL)
             (bc PUSH_I1)
             (bc CALL) (byte 00) (byte $17)
             (bc BREAK)

             (org #x1700)
      (label TEST_FUN)
             (byte 1)                     ;; number of locals
             (bc POP_TO_L0)          ;; pop tos into local 0 (now int 1)
             (bc NIL_P_RET_L0_POP_1)  ;; return local 0  if tos = nil (which it is)
             (bc BREAK))
     ))

 (check-equal? (vm-stack->strings bc-nil-ret-state)
                  (list "stack holds 2 items"
                        "int $0001  (rt)"
                        "ptr NIL"))
 (check-equal? (vm-call-frame->strings bc-nil-ret-state)
               (list (format "call-frame-ptr:   $~a03, topmark: 03" (format-hex-byte PAGE_CALL_FRAME))
                     "program-counter:  $0805"
                     "function-ptr:     $0800"
                     (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 03"
                             (format-hex-byte PAGE_LOCALS_LB)
                             (format-hex-byte PAGE_LOCALS_HB))))

  (define bc-nil-ret-local-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_I1)
             (bc PUSH_NIL)
             (bc CALL) (byte 00) (byte $17)
             (bc BREAK)

             (org #x1700)
      (label TEST_FUN)
             (byte 2)            ;; number of locals
             (bc POP_TO_L1)
             (bc POP_TO_L0)
             (bc PUSH_L1)
             (bc NIL_P_RET_L0_POP_1)     ;; return local 0 (int 1) if nil
             (bc BREAK))
     ))

  (check-equal? (vm-stack->strings bc-nil-ret-local-state)
                   (list "stack holds 2 items"
                         "int $0001  (rt)"
                         "ptr NIL"))
  (check-equal? (vm-call-frame->strings bc-nil-ret-local-state)
                (list (format "call-frame-ptr:   $~a03, topmark: 03" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $0805"
                         "function-ptr:     $0800"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 03"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB)))))

(module+ test #| bc-tail-call |#
  (define bc-tail-call-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_NIL)
             (bc PUSH_I0)
             (bc CONS)
             (bc CALL) (byte 00) (byte $17)
             (bc BREAK)

             (org #x1700)
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
                   (list "stack holds 2 items"
                         "ptr NIL  (rt)"
                         "ptr NIL"))
   (check-equal? (vm-call-frame->strings bc-tail-call-state)
                 (list (format "call-frame-ptr:   $~a03, topmark: 03" (format-hex-byte PAGE_CALL_FRAME))
                          "program-counter:  $0806"
                          "function-ptr:     $0800"
                          (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 03"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))))

  ;; convert the list given by cell-pair-ptr (addresss) as a list of strings
  (define (vm-list->strings state address (string-list '()))
    (cond [(= address #x0000) ;; this is the nil ptr
           (reverse string-list)]
          [else
           (unless (= (bitwise-and #x01 address) #x00)
             (raise-user-error (format "address is not a cell-array ~a" (format-hex-word address))))
           (unless (= (peek state (bitwise-and #xff00 address )) #x20)
             (raise-user-error (format "m1 page referenced is not a profile 0 cell-arrray page ~a"
                                       (format-hex-word address))))
           (define cell-cdr (peek-word-at-address state (+ address 4)))
           (unless (= (bitwise-and #x01 cell-cdr) #x00)
             (raise-user-error (format "cdr cell is not a ptr => this is no list ~a" (format-hex-word cell-cdr)) ))
           (if (vm-cell-at-nil? state address)
               (reverse string-list)
               (vm-list->strings state
                                cell-cdr
                                (cons (vm-cell-at->string state (+ 2 address))
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
             (bc CALL) (byte 00) (byte $17)
             (bc BREAK)                   ;; << to make debugger stop/exit

             (org #x1700)
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

  (check-equal? (memory-list bc-tail-call-reverse-state ZP_PAGE_FREE_SLOTS_LIST)
                   (list PAGE_AVAIL_0)
                   "the page with free slots for profile 0")
  (check-equal? (vm-page->strings bc-tail-call-reverse-state PAGE_AVAIL_0)
                   (list "page-type:      m1 page p0"
                         "previous page:  $00"
                         "slots used:     3"
                         "next free slot: $02"))
  (inform-check-equal? (cpu-state-clock-cycles bc-tail-call-reverse-state)
                4226)
  (check-equal? (vm-list->strings bc-tail-call-reverse-state (peek-word-at-address bc-tail-call-reverse-state ZP_RT))
                   (list "int $0000"
                         "int $0001"
                         "int $0002")
                   "list got reversed")
  (check-equal? (vm-stack->strings bc-tail-call-reverse-state)
                   (list "stack holds 2 items"
                         (format "ptr[1] $~a08  (rt)" (format-hex-byte PAGE_AVAIL_0))
                         "ptr NIL"))
  (check-equal? (vm-call-frame->strings bc-tail-call-reverse-state)
                   (list (format "call-frame-ptr:   $~a03, topmark: 03" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $080c"
                         "function-ptr:     $0800"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 03"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB)))))

(module+ test #| bc_ret |#
  (define test-bc-ret-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_I0)
             (bc CALL) (byte 00) (byte $17)
             (bc BREAK)

             (org #x1700)
      (label TEST_FUN)
             (byte 0)            ;; number of locals
             (bc PUSH_I1)     ;; value to return
             (bc RET))))

  (check-equal? (vm-call-frame->strings test-bc-ret-state)
                   (list (format "call-frame-ptr:   $~a03, topmark: 03" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $0804"
                         "function-ptr:     $0800"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 03"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))))
  (check-equal? (vm-stack->strings test-bc-ret-state)
                   (list "stack holds 3 items"
                         "int $0001  (rt)"
                         "int $0000"
                         "ptr NIL")
                   "previous value on the stack is there + returned value (in rt)"))


(module+ test #| code len |#
  (inform-check-equal? (code-len bc-cell-pair-code)
                       87
                       "code len"))
