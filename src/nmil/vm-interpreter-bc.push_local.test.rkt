#lang racket/base

#|

implement bc push/write local commands

|#

(require "../6510.rkt")
(require (only-in "../tools/6510-interpreter.rkt" peek))
(require (only-in "../ast/6510-resolver.rkt" add-label-suffix))
(require (only-in racket/list flatten))

;; (require (only-in "./vm-memory-map.rkt"
;;                   ZP_LOCALS_HB_PTR
;;                   ZP_LOCALS_LB_PTR
;;                   ZP_RT))
;; (require (only-in "./vm-interpreter-loop.rkt"
;;                   VM_INTERPRETER_INC_PC))
;; (require (only-in "./vm-memory-manager.rkt"
;;                   INC_REFCNT_RT
;;                   DEC_REFCNT_RT))
;; (require (only-in "./vm-mm-cell-stack.rkt"
;;                   PUSH_RT_TO_EVLSTK_IF_NONEMPTY))


(module+ test
  (require "../6510.rkt")
  (require "../6510-test-utils.rkt")
  (require (only-in "./vm-interpreter-bc.push_local.rkt" BC_PUSH_LOCAL_SHORT PUSH_RT_WRITE_LOCAL_bc_enc))
  (require (only-in "./vm-interpreter-bc.push_const.rkt" BC_PUSH_CONST_NUM_SHORT))
  (require (only-in "./vm-interpreter-bc.call_ret.rkt" BC_CALL))
  (require (only-in "./vm-interpreter-bc.pop_local.rkt" BC_POP_TO_LOCAL_SHORT))
  (require (only-in "./vm-call-frame.rkt" vm-call-frame->strings))
  (require (only-in "../util.rkt" format-hex-byte))
  (require (only-in "./vm-interpreter-loop.rkt"
                    VM_INTERPRETER
                    VM_INTERPRETER_INIT))
  (require (only-in "./vm-memory-manager.rkt"
                    VM_INITIALIZE_MEMORY_MANAGER
                    vm-memory-manager))
  (require (only-in "./vm-interpreter-test-utils.rkt"
                    run-bc-wrapped-in-test-
                    vm-next-instruction-bytes))
  (require (only-in "../ast/6510-relocator.rkt" command-len))
  (require (only-in "./vm-bc-opcode-definitions.rkt"
                    bc
                    bc-opcode-definitions
                    build-extended-optable-hb
                    build-extended-optable-lb
                    build-interpreter-optable
                    filtered-opcode-definitions))
  (require (only-in "vm-lists.rkt" vm-lists))
  (require (only-in "./vm-inspector-utils.rkt"
                    vm-cell-at-nil?
                    vm-page->strings
                    vm-stack->strings
                    vm-regt->string
                    vm-cell-at->string
                    vm-cell->string
                    vm-deref-cell-pair-w->string))

  (define relevant-opcode-definitions (filtered-opcode-definitions
                                       (list "BC_PUSH_LOCAL_SHORT"        ;; POP_TO_L0-3
                                             "BC_PUSH_CONST_NUM_SHORT"    ;;
                                             "BC_PUSH_INT0"               ;; PUSH_I0
                                             "BC_PUSH_INT1"               ;; ..
                                             "BC_PUSH_INT2"               ;; ..
                                             "BC_PUSH_INTm1"              ;; PUSH_IM1
                                             "BC_CALL"                    ;; CALL
                                             "BC_BREAK"
                                             "BC_POP_TO_LOCAL_SHORT")))   ;;

  (define (wrap-bytecode-for-test bc-to-wrap)
    [append (list (org #x7000)
                  (JSR VM_INITIALIZE_MEMORY_MANAGER)
                  (JSR VM_INITIALIZE_CALL_FRAME)
                  (JSR VM_INTERPRETER_INIT)
                  (JMP VM_INTERPRETER))
            (list (org #x8000))
            bc-to-wrap
            (list (bc BREAK))
            ;; ---
            BC_PUSH_LOCAL_SHORT
            ;; ---
            PUSH_RT_WRITE_LOCAL_bc_enc
            BC_PUSH_CONST_NUM_SHORT
            BC_CALL
            BC_POP_TO_LOCAL_SHORT
            (list  (label BC_BREAK) (BRK))
            (list (org #xa000))
            VM_INTERPRETER_INIT
            VM_INTERPRETER
            (build-extended-optable-hb relevant-opcode-definitions)
            (build-extended-optable-lb relevant-opcode-definitions)
            vm-lists ;; includes vm-memory-manager
            (list (org-align #x100)) ;; align to next page
            (build-interpreter-optable relevant-opcode-definitions)]) ;; TODO create opcode table w/ wanted / knonwn opcodes only?


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
