#lang racket/base


(provide (all-from-out "../vm-inspector-utils.rkt")
         (all-from-out "../vm-interpreter-test-utils.rkt")
         (all-from-out "../vm-runtime/vm-call-frame.rkt")
         (all-from-out "../vm-bc-opcode-definitions.rkt")
         (all-from-out "../../6510.rkt")
         (all-from-out "../../6510-test-utils.rkt")
         (all-from-out "../../tools/6510-interpreter.rkt")
         (all-from-out "../vm-runtime/vm-memory-map.rkt")
         (all-from-out racket/list)
         wrap-bytecode-for-full-bc-test)

(require (only-in racket/list
                  flatten
                  make-list)
         "../../6510-test-utils.rkt"
         "../../6510.rkt"
         (only-in "../../tools/6510-interpreter.rkt"
                  peek
                  peek-word-at-address
                  memory-list
                  cpu-state-clock-cycles)
         (only-in "../vm-runtime/vm-call-frame.rkt"
                  vm-call-frame->strings)
         (only-in "../vm-inspector-utils.rkt"
                  shorten-cell-strings
                  shorten-cell-string
                  vm-cell-at-nil-n?
                  vm-page-n->strings
                  vm-stack-n->strings
                  vm-regt-n->string
                  vm-cell-at-n->string
                  vm-cell-n->string
                  vm-deref-cell-pair-w-n->string)
         (only-in "../vm-interpreter.rkt"
                  just-vm-interpreter)
         (only-in "../vm-bc-opcode-definitions.rkt"
                  bc
                  bc-opcode-definitions
                  full-extended-optable-lb
                  full-extended-optable-hb
                  full-interpreter-opcode-table)
         (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER
                  VM_INTERPRETER_ZP
                  VM_INTERPRETER_INIT)
         (only-in "../vm-interpreter-test-utils.rkt"
                  run-bc-wrapped-in-test-
                  vm-num-slots-used-in-page
                  vm-list->strings
                  vm-slots-used-in-page
                  vm-slots-free-in-page
                  vm-next-instruction-bytes)
         (only-in "../vm-runtime/vm-memory-manager-n.rkt"
                  VM_INITIALIZE_MEMORY_MANAGER
                  vm-memory-manager-code)
         "../vm-runtime/vm-memory-map.rkt")

(define (wrap-bytecode-for-full-bc-test bc-to-wrap)
  (flatten
   (append
    (list (org #x07f0)
          (JSR VM_INITIALIZE_MEMORY_MANAGER)
          (JSR VM_INTERPRETER_INIT)
          (JMP VM_INTERPRETER))
    (list (org #x0800))
    bc-to-wrap
    (list (bc BREAK))
    ;; ---
    ;; ---
    (list (org #xa000))
    just-vm-interpreter
    full-extended-optable-hb
    full-extended-optable-lb
    vm-memory-manager-code
    (list (org-align #x100)) ;; align to next page
    full-interpreter-opcode-table
    VM_INTERPRETER_ZP)))
