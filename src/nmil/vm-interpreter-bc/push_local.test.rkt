#lang racket/base

#|

implement bc push/write local commands

|#

(module+ test
  (require (only-in "./call_ret.rkt"
                    BC_CALL)
           (only-in "./pop_local.rkt"
                    BC_POP_TO_LOCAL_SHORT)
           (only-in "./push_const.rkt"
                    BC_PUSH_CONST_NUM_SHORT)
           (only-in "./push_local.rkt"
                    BC_PUSH_LOCAL_SHORT
                    PUSH_RT_WRITE_LOCAL_bc_enc)
           "./test-utils.rkt")

  (define relevant-opcode-definitions (filtered-opcode-definitions
                                       (list "BC_PUSH_LOCAL_SHORT"        ;; POP_TO_L0-3
                                             "BC_PUSH_INT0"               ;; PUSH_I0
                                             "BC_PUSH_INT1"               ;; ..
                                             "BC_PUSH_INT2"               ;; ..
                                             "BC_PUSH_INTm1"              ;; PUSH_IM1
                                             "BC_CALL"                    ;; CALL
                                             "BC_BREAK"
                                             "BC_POP_TO_LOCAL_SHORT")))   ;;

  (define (wrap-bytecode-for-test bc-to-wrap)
    (wrap-bytecode-for-bc-test
     bc-to-wrap
     relevant-opcode-definitions
     (list  BC_PUSH_LOCAL_SHORT
            ;; ---
            PUSH_RT_WRITE_LOCAL_bc_enc
            BC_PUSH_CONST_NUM_SHORT
            BC_CALL
            BC_POP_TO_LOCAL_SHORT)))

  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (run-bc-wrapped-in-test- bc wrapped-code debug)))


(module+ test #| BC_PUSH_LOCAL_SHORT |#
  (define test-bc-pop-to-l-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_I0)
             (bc PUSH_IM1)
             (bc CALL) (byte 00) (byte $17)

             (org #x1700)
      (label TEST_FUN)
             (byte 2)            ;; number of locals
             (bc PUSH_I1)     ;; value to return
             (bc POP_TO_L0) ;;
             )))

  (check-equal? (vm-stack->strings test-bc-pop-to-l-state)
                (list "stack holds 3 items"
                      "int $3fff  (rt)"
                      "int $0000"
                      "ptr NIL"))
  (check-equal? (peek test-bc-pop-to-l-state (+ PAGE_LOCALS_LB_W #x03))
                #x03)
  (check-equal? (peek test-bc-pop-to-l-state (+ PAGE_LOCALS_HB_W #x03))
                #x01)
  (check-equal? (vm-call-frame->strings test-bc-pop-to-l-state)
                   (list (format "call-frame-ptr:   $~a03, topmark: 07" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $1703"
                         "function-ptr:     $1700"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 05"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))
                         (format "slim-frame ($~a03..$~a06)" (format-hex-byte PAGE_CALL_FRAME)(format-hex-byte PAGE_CALL_FRAME))
                         "return-pc:           $0805"
                         "return-function-ptr: $0800"
                         (format "return-locals-ptr:   $~a03, $~a03 (lb,hb)"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))))

  (define test-bc-pop-to-p-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_I0)
             (bc PUSH_IM1)
             (bc CALL) (byte 00) (byte $17)

      (org #x1700)
      (label TEST_FUN)
             (byte 2)            ;; number of locals
             (bc POP_TO_L0)
             (bc POP_TO_L1)
             (bc PUSH_I1)     ;; value to return
             (bc POP_TO_L0) ;; overwrites -1
             )))

  (check-equal? (vm-stack->strings test-bc-pop-to-p-state)
                  (list "stack is empty or tos=nil"))
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
                         "program-counter:  $1705"
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

  (define test-bc-push-l-state
    (run-bc-wrapped-in-test
     (list
             (bc CALL) (byte 00) (byte $17)

      (org #x1700)
      (label TEST_FUN)
             (byte 1)            ;; number of locals
             (bc PUSH_I1)     ;; value to return
             (bc POP_TO_L0) ;;
             (bc PUSH_I0)
             (bc PUSH_L0))))

  (check-equal? (vm-stack->strings test-bc-push-l-state)
                  (list "stack holds 3 items"
                        "int $0001  (rt)"
                        "int $0000"
                        "ptr NIL")
                  "int 1 was pushed from local")
  (check-equal? (vm-call-frame->strings test-bc-push-l-state)
                   (list (format "call-frame-ptr:   $~a03, topmark: 07" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $1705"
                         "function-ptr:     $1700"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 04"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))
                         (format "slim-frame ($~a03..$~a06)" (format-hex-byte PAGE_CALL_FRAME) (format-hex-byte PAGE_CALL_FRAME))
                         "return-pc:           $0803"
                         "return-function-ptr: $0800"
                         (format "return-locals-ptr:   $~a03, $~a03 (lb,hb)"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))))

  (define test-bc-push-p-state
    (run-bc-wrapped-in-test
     (list
             (bc PUSH_I0)
             (bc PUSH_IM1)
             (bc CALL) (byte 00) (byte $17)

      (org #x1700)
      (label TEST_FUN)
             (byte 2)            ;; number of locals
             (bc POP_TO_L0)
             (bc POP_TO_L1)
             (bc PUSH_I1)
             (bc PUSH_L0))))

  (check-equal? (vm-stack->strings test-bc-push-p-state)
                   (list "stack holds 3 items"
                         "int $3fff  (rt)"
                         "int $0001"
                         "ptr NIL")
                   "int -1 was pushed from local")
  (check-equal? (vm-call-frame->strings test-bc-push-p-state)
                   (list (format "call-frame-ptr:   $~a03, topmark: 07" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $1705"
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

  (define test-bc-pop-push-to-p-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc PUSH_IM1)
      (bc CALL) (byte 00) (byte $17)

      (org #x1700)
      (label TEST_FUN)
      (byte 2)            ;; number of locals
      (bc POP_TO_L0)
      (bc POP_TO_L1)
      (bc PUSH_I1)     ;; value to return
      (bc POP_TO_L0) ;; overwrites -1
      (bc PUSH_L0)
      (bc BREAK))))

  (check-equal? (vm-stack->strings test-bc-pop-push-to-p-state)
                   (list "stack holds 2 items"
                         "int $0001  (rt)"
                         "ptr NIL"))
  (check-equal? (vm-call-frame->strings test-bc-pop-push-to-p-state)
                   (list (format "call-frame-ptr:   $~a03, topmark: 07" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $1706"
                         "function-ptr:     $1700"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 05"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))
                         (format "slim-frame ($~a03..$~a06)" (format-hex-byte PAGE_CALL_FRAME) (format-hex-byte PAGE_CALL_FRAME))
                         "return-pc:           $0805"
                         "return-function-ptr: $0800"
                         (format "return-locals-ptr:   $~a03, $~a03 (lb,hb)"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB)))))
