#lang racket/base

#|

test of bytecode implementation of comparison commands

|#

(module+ test
  (require "../6510.rkt")
  (require "../6510-test-utils.rkt")
  (require (only-in "./vm-interpreter-bc.push_n_pop.rkt" BC_PUSH_B))
  (require (only-in "./vm-interpreter-bc.compare.rkt"
                    BC_B_GT_P
                    BC_B_LT_P
                    BC_B_GE_P
                    BC_I_GT_P))
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
                                       (list "BC_PUSH_B"
                                             "BC_I_GT_P"
                                             "BC_B_GE_P"
                                             "BC_B_LT_P"
                                             "BC_B_GT_P")))
  (define (wrap-bytecode-for-test bc-to-wrap)
    [append (list (org #x7000)
                  (JSR VM_INITIALIZE_MEMORY_MANAGER)
                  (JSR VM_INTERPRETER_INIT)
                  (JMP VM_INTERPRETER))
            (list (org #x8000))
            bc-to-wrap
            (list (bc BREAK))
            ;; ---
            BC_I_GT_P
            BC_B_GE_P
            BC_B_LT_P
            BC_B_GT_P
            ;; ---
            BC_PUSH_B
            (list (org #xc000))
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

(module+ test #| BC_B_GT_P |#
  (define gt-01-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_B) (byte 10)
      (bc PUSH_B) (byte 20)
      (bc B_GT_P))))

  (check-equal? (vm-regt->string gt-01-state)
                "int $0001")

  (define gt-02-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_B) (byte 20)
      (bc PUSH_B) (byte 20)
      (bc B_GT_P))))

  (check-equal? (vm-regt->string gt-02-state)
                "int $0000")

  (define gt-03-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_B) (byte 20)
      (bc PUSH_B) (byte 21)
      (bc B_GT_P))))

  (check-equal? (vm-regt->string gt-03-state)
                "int $0001")

  (define gt-04-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_B) (byte 20)
      (bc PUSH_B) (byte 19)
      (bc B_GT_P))))

  (check-equal? (vm-regt->string gt-04-state)
                "int $0000"))
