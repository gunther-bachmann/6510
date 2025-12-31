#lang racket/base

#|

  implement some useful test utils for testing bcs in a somewhat isolated environment

|#

(require (only-in racket/list
                  flatten
                  make-list)
         rackunit
         "../../6510-test-utils.rkt"
         "../../6510.rkt"
         (only-in "../../ast/6510-relocator.rkt"
                  code-len)
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
                  vm-cell-at-nil?
                  vm-page->strings
                  vm-stack->strings
                  vm-regt->string
                  vm-cell-at->string
                  vm-cell->string
                  vm-deref-cell-pair-w->string)
         (only-in "../vm-interpreter.rkt"
                  just-vm-interpreter)
         (only-in "../vm-bc-opcode-definitions.rkt"
                  bc
                  full-extended-optable-lb
                  full-extended-optable-hb
                  full-interpreter-opcode-table
                  filtered-opcode-definitions
                  build-extended-optable-hb
                  build-extended-optable-lb
                  build-interpreter-optable)
         (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER
                  VM_INTERPRETER_ZP
                  VM_INTERPRETER_INIT)
         (only-in "../vm-interpreter-test-utils.rkt"
                  run-bc-wrapped-in-test-
                  vm-list->strings
                  vm-next-instruction-bytes)
         (only-in "../vm-runtime/vm-memory-manager-n.rkt"
                  VM_INITIALIZE_MEMORY_MANAGER
                  vm-memory-manager-code)
         "../vm-runtime/vm-memory-map.rkt")

(provide (all-from-out "../vm-inspector-utils.rkt")
         (all-from-out "../vm-interpreter-test-utils.rkt")
         (all-from-out "../vm-runtime/vm-call-frame.rkt")
         (all-from-out "../vm-bc-opcode-definitions.rkt")
         (all-from-out "../../6510.rkt")
         (all-from-out "../../ast/6510-relocator.rkt")
         (all-from-out "../../6510-test-utils.rkt")
         (all-from-out "../../tools/6510-interpreter.rkt")
         (all-from-out "../vm-runtime/vm-memory-map.rkt")
         (all-from-out racket/list)
         (all-from-out rackunit)
         wrap-bytecode-for-bc-test
         PAGE_CALL_FRAME
         PAGE_LOCALS_HB
         PAGE_LOCALS_LB
         PAGE_LOCALS_HB_W
         PAGE_LOCALS_LB_W
         PAGE_AVAIL_0
         PAGE_AVAIL_1
         PAGE_AVAIL_0_W
         PAGE_AVAIL_1_W)

(define (wrap-bytecode-for-bc-test bc-to-wrap relevant-opcode-definitions bc-implementations)
  (flatten
   (append (list (org #x07f0)
                 (JSR VM_INITIALIZE_MEMORY_MANAGER)
                 (JSR VM_INTERPRETER_INIT)
                 (JMP VM_INTERPRETER))
           (list (org #x0800))
           bc-to-wrap
           (list (bc BREAK))
           ;; ---
           bc-implementations
           ;; ---
           (list  (label BC_BREAK) (BRK))
           (list (org #xa000))
           VM_INTERPRETER_INIT
           VM_INTERPRETER
           (build-extended-optable-hb relevant-opcode-definitions)
           (build-extended-optable-lb relevant-opcode-definitions)
           vm-memory-manager-code
           (list (org-align #x100)) ;; align to next page
           (build-interpreter-optable relevant-opcode-definitions)
           VM_INTERPRETER_ZP))) ;; TODO create opcode table w/ wanted / knonwn opcodes only?

;; cell_stack_lb #xcf
;; cell_stack_hb #xce
(define PAGE_CALL_FRAME #xcd)
(define PAGE_LOCALS_LB #xcb)
(define PAGE_LOCALS_LB_W #xcb00)
(define PAGE_LOCALS_HB #xcc)
(define PAGE_LOCALS_HB_W #xcc00)
(define PAGE_AVAIL_0 #xca)
(define PAGE_AVAIL_0_W #xca00)
(define PAGE_AVAIL_1 #xc9)
(define PAGE_AVAIL_1_W #xc900)
