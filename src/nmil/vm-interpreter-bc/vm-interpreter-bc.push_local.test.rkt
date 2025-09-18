#lang racket/base

#|

implement bc push/write local commands

|#

(module+ test
  (require "./vm-interpreter-bc.test-utils.rkt")
  (require (only-in "./vm-interpreter-bc.push_local.rkt" BC_PUSH_LOCAL_SHORT PUSH_RT_WRITE_LOCAL_bc_enc))
  (require (only-in "./vm-interpreter-bc.push_const.rkt" BC_PUSH_CONST_NUM_SHORT))
  (require (only-in "./vm-interpreter-bc.call_ret.rkt" BC_CALL))
  (require (only-in "./vm-interpreter-bc.pop_local.rkt" BC_POP_TO_LOCAL_SHORT))

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
             (bc CALL) (byte 00) (byte $87)

             (org #x8700)
      (label TEST_FUN)
             (byte 2)            ;; number of locals
             (bc PUSH_I1)     ;; value to return
             (bc POP_TO_L0) ;;
             (bc BREAK))
     ))

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
                         "program-counter:  $8703"
                         "function-ptr:     $8700"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 05"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))
                         (format "slim-frame ($~a03..$~a06)" (format-hex-byte PAGE_CALL_FRAME)(format-hex-byte PAGE_CALL_FRAME))
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
      (bc CALL) (byte 00) (byte $87)

      (org #x8700)
      (label TEST_FUN)
      (byte 2)            ;; number of locals
      (bc POP_TO_L0)
      (bc POP_TO_L1)
      (bc PUSH_I1)     ;; value to return
      (bc POP_TO_L0) ;; overwrites -1
      (bc BREAK))
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
                         "program-counter:  $8705"
                         "function-ptr:     $8700"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 05"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))
                         (format "slim-frame ($~a03..$~a06)" (format-hex-byte PAGE_CALL_FRAME) (format-hex-byte PAGE_CALL_FRAME))
                         "return-pc:           $8005"
                         "return-function-ptr: $8000"
                         (format "return-locals-ptr:   $~a03, $~a03 (lb,hb)"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))))

  (define test-bc-push-l-state
    (run-bc-wrapped-in-test
     (list
      (bc CALL) (byte 00) (byte $87)

      (org #x8700)
      (label TEST_FUN)
      (byte 1)            ;; number of locals
      (bc PUSH_I1)     ;; value to return
      (bc POP_TO_L0) ;;
      (bc PUSH_I0)
      (bc PUSH_L0)
      (bc BREAK))))

  (check-equal? (vm-stack->strings test-bc-push-l-state)
                  (list "stack holds 2 items"
                        "int $0001  (rt)"
                        "int $0000")
                  "int 1 was pushed from local")
  (check-equal? (vm-call-frame->strings test-bc-push-l-state)
                   (list (format "call-frame-ptr:   $~a03, topmark: 07" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $8705"
                         "function-ptr:     $8700"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 04"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))
                         (format "slim-frame ($~a03..$~a06)" (format-hex-byte PAGE_CALL_FRAME) (format-hex-byte PAGE_CALL_FRAME))
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
      (bc CALL) (byte 00) (byte $87)

      (org #x8700)
      (label TEST_FUN)
      (byte 2)            ;; number of locals
      (bc POP_TO_L0)
      (bc POP_TO_L1)
      (bc PUSH_I1)
      (bc PUSH_L0)
      (bc BREAK))))

  (check-equal? (vm-stack->strings test-bc-push-p-state)
                   (list "stack holds 2 items"
                         "int $1fff  (rt)"
                         "int $0001")
                   "int -1 was pushed from local")
  (check-equal? (vm-call-frame->strings test-bc-push-p-state)
                   (list (format "call-frame-ptr:   $~a03, topmark: 07" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $8705"
                         "function-ptr:     $8700"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 05"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))
                         (format "slim-frame ($~a03..$~a06)" (format-hex-byte PAGE_CALL_FRAME) (format-hex-byte PAGE_CALL_FRAME))
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
      (bc CALL) (byte 00) (byte $87)

      (org #x8700)
      (label TEST_FUN)
      (byte 2)            ;; number of locals
      (bc POP_TO_L0)
      (bc POP_TO_L1)
      (bc PUSH_I1)     ;; value to return
      (bc POP_TO_L0) ;; overwrites -1
      (bc PUSH_L0)
      (bc BREAK))))

  (check-equal? (vm-stack->strings test-bc-pop-push-to-p-state)
                   (list "stack holds 1 item"
                         "int $0001  (rt)"))
  (check-equal? (vm-call-frame->strings test-bc-pop-push-to-p-state)
                   (list (format "call-frame-ptr:   $~a03, topmark: 07" (format-hex-byte PAGE_CALL_FRAME))
                         "program-counter:  $8706"
                         "function-ptr:     $8700"
                         (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 05"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB))
                         (format "slim-frame ($~a03..$~a06)" (format-hex-byte PAGE_CALL_FRAME) (format-hex-byte PAGE_CALL_FRAME))
                         "return-pc:           $8005"
                         "return-function-ptr: $8000"
                         (format "return-locals-ptr:   $~a03, $~a03 (lb,hb)"
                                 (format-hex-byte PAGE_LOCALS_LB)
                                 (format-hex-byte PAGE_LOCALS_HB)))))
