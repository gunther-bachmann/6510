#lang racket/base

(module+ test
  (require "../6510.rkt")
  (require "../6510-test-utils.rkt")
  (require (only-in racket/list flatten))

  (require (only-in "./vm-interpreter-bc.branch.rkt" BC_T_P_BRA BC_F_P_BRA))
  (require (only-in "./vm-interpreter-bc.push_const.rkt" BC_PUSH_CONST_NUM_SHORT))

  (require (only-in "./vm-call-frame.rkt" vm-call-frame->strings))
  (require (only-in "../util.rkt" format-hex-byte))
  (require (only-in "./vm-interpreter-loop.rkt"
                    VM_INTERPRETER
                    VM_INTERPRETER_INIT))
  (require (only-in "./vm-memory-manager.rkt" VM_INITIALIZE_MEMORY_MANAGER))
  (require (only-in "./vm-interpreter-test-utils.rkt"
                    run-bc-wrapped-in-test-
                    vm-next-instruction-bytes))
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
                                       (list "BC_PUSH_INT0"               ;; PUSH_I0
                                             "BC_PUSH_INT1"               ;; ..
                                             "BC_PUSH_INT2"               ;; ..
                                             "BC_PUSH_INTm1"              ;; PUSH_IM1
                                             "BC_BREAK"
                                             "BC_T_P_BRA"
                                             "BC_F_P_BRA")))

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
            BC_T_P_BRA
            BC_F_P_BRA
            ;; ---
            BC_PUSH_CONST_NUM_SHORT
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

(module+ test #| branch true |#
  (define branch-true-0-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I1)
      (bc T_P_BRA) (byte 2)
      (bc PUSH_I1)
      (bc BREAK)
      (bc PUSH_I2))))
  (check-equal? (vm-stack->strings branch-true-0-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)"))

  (define branch-true-1-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I1)
      (bc T_P_BRA) (byte $75)
      (bc BREAK)
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
       (bc BREAK)
       (org-align #x80)
       (bc PUSH_I1)
       (bc T_P_BRA) (byte $6d)
       (bc BREAK)
       (org-align #xf0)
       (bc PUSH_I1)
       ;; 80f1
       (bc T_P_BRA) (byte $0d)
       (build-list 13 (lambda (_i) (bc BREAK)))
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
       (bc BREAK)
       (org-align #x80)
       (bc PUSH_I1)
       ;; now at 8081
       (bc T_P_BRA) (byte $6d)
       ;; 8083
       (bc BREAK)
       (org-align #xf0)
       (bc PUSH_I1)
       ;; now at 80f1
       (bc T_P_BRA) (byte $0e)
       (build-list 14 (lambda (_i) (bc BREAK)))
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
       (bc BREAK)
       (bc PUSH_I2)
       (bc BREAK)
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
       (bc BREAK)
       (org-align #x80)
       (bc PUSH_I1)
       ;; now at 8081
       (bc T_P_BRA) (byte $6d)
       ;; 8083
       (bc BREAK)
       (org-align #xf0)
       (bc PUSH_I1)
       ;; now at 80f1
       (bc T_P_BRA) (byte $0e)
       (build-list 12 (lambda (_i) (bc BREAK)))
       ;; 80ff
       (bc PUSH_I2)
       ;; 8100
       (bc BREAK)
       ;; now at 8101
       (bc PUSH_I1)
       (bc T_P_BRA) (byte $fd)))
     ))
  (check-equal? (vm-stack->strings branch-true-5-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)")))

(module+ test #| branch true |#
  (define branch-false-0-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc F_P_BRA) (byte 2)
      (bc PUSH_I0)
      (bc BREAK)
      (bc PUSH_I2))
     ))
  (check-equal? (vm-stack->strings branch-false-0-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)"))

  (define branch-false-1-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc F_P_BRA) (byte $75)
      (bc BREAK)
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
       (bc BREAK)
       (org-align #x80)
       (bc PUSH_I0)
       (bc F_P_BRA) (byte $6d)
       (bc BREAK)
       (org-align #xf0)
       (bc PUSH_I0)
       ;; 80f1
       (bc F_P_BRA) (byte $0d)
       (build-list 13 (lambda (_i) (bc BREAK)))
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
       (bc BREAK)
       (org-align #x80)
       (bc PUSH_I0)
       ;; now at 8081
       (bc F_P_BRA) (byte $6d)
       ;; 8083
       (bc BREAK)
       (org-align #xf0)
       (bc PUSH_I0)
       ;; now at 80f1
       (bc F_P_BRA) (byte $0e)
       (build-list 14 (lambda (_i) (bc BREAK)))
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
       (bc BREAK)
       (bc PUSH_I2)
       (bc BREAK)
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
       (bc BREAK)
       (org-align #x80)
       (bc PUSH_I0)
       ;; now at 8081
       (bc F_P_BRA) (byte $6d)
       ;; 8083
       (bc BREAK)
       (org-align #xf0)
       (bc PUSH_I0)
       ;; now at 80f1
       (bc F_P_BRA) (byte $0e)
       (build-list 12 (lambda (_i) (bc BREAK)))
       ;; 80ff
       (bc PUSH_I2)
       ;; 8100
       (bc BREAK)
       ;; now at 8101
       (bc PUSH_I0)
       (bc F_P_BRA) (byte $fd)))
     ))
  (check-equal? (vm-stack->strings branch-false-5-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)")))
