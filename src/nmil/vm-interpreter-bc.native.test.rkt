#lang racket/base

#|

test of bytecode implementation of native commands

|#

(module+ test
  (require "../6510.rkt")
  (require "../6510-test-utils.rkt")
  (require (only-in racket/list flatten))
  (require (only-in "./vm-interpreter-bc.native.rkt"
                    BC_POKE_B
                    BC_NATIVE
                    RETURN_TO_BC))
  (require (only-in "./vm-interpreter-bc.compare.rkt"
                    BC_B_GT_P
                    BC_B_LT_P
                    BC_B_GE_P
                    BC_I_GT_P))
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
  (require (only-in "./vm-interpreter-bc.push_n_pop.rkt" BC_PUSH_B))

  (define relevant-opcode-definitions (filtered-opcode-definitions
                                       (list "BC_POKE_B"
                                             "BC_PUSH_B"
                                             "BC_NATIVE")))

  (define (wrap-bytecode-for-test bc-to-wrap)
    [append (list (org #x7000)
                  (JSR VM_INITIALIZE_MEMORY_MANAGER)
                  (JSR VM_INTERPRETER_INIT)
                  (JMP VM_INTERPRETER))
            (list (org #x8000))
            bc-to-wrap
            (list (bc BREAK))
            ;; ---
            BC_NATIVE
            BC_POKE_B
            RETURN_TO_BC
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


(module+ test #| bdec |#
  (define native-return-test
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_B) (byte #x14)
       (bc NATIVE)
       (INC ZP_RT+1)
       (JSR RETURN_TO_BC)
       (bc PUSH_B) (byte #x16)))
     ))

  (check-equal? (vm-stack->strings native-return-test)
                (list "stack holds 2 items"
                      "byte $16  (rt)"
                      "byte $15")
                "last element was increment from $14->$15 before another push was issue"))
