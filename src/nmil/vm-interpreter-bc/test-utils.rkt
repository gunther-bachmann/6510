#lang racket/base

#|

implement some useful test utils for testing bcs in a somewhat isolated environment

|#


(require (only-in racket/list flatten))
(require "../../6510.rkt")
(require "../../6510-test-utils.rkt")
(require (only-in "../vm-call-frame.rkt" vm-call-frame->strings))
(require (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER
                  VM_INTERPRETER_INIT))
(require (only-in "../vm-memory-manager.rkt" VM_INITIALIZE_MEMORY_MANAGER))
(require (only-in "../vm-lists.rkt" vm-lists))

(require (only-in "../../tools/6510-interpreter.rkt"
                  peek
                  peek-word-at-address
                  memory-list
                  cpu-state-clock-cycles))
(require (only-in "../vm-interpreter-test-utils.rkt"
                  run-bc-wrapped-in-test-
                  vm-next-instruction-bytes))
(require (only-in "../vm-bc-opcode-definitions.rkt"
                  bc
                  bc-opcode-definitions
                  build-extended-optable-hb
                  build-extended-optable-lb
                  build-interpreter-optable
                  filtered-opcode-definitions))
(require (only-in "../vm-inspector-utils.rkt"
                  vm-cell-at-nil?
                  vm-page->strings
                  vm-stack->strings
                  vm-regt->string
                  vm-cell-at->string
                  vm-cell->string
                  vm-deref-cell-pair-w->string))
(require "../vm-memory-map.rkt")

(provide (all-from-out "../vm-inspector-utils.rkt")
         (all-from-out "../vm-interpreter-test-utils.rkt")
         (all-from-out "../vm-call-frame.rkt")
         (all-from-out "../vm-bc-opcode-definitions.rkt")
         (all-from-out "../../6510.rkt")
         (all-from-out "../../6510-test-utils.rkt")
         (all-from-out "../../tools/6510-interpreter.rkt")
         (all-from-out "../vm-memory-map.rkt")
         (all-from-out racket/list)
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
   (append (list (org #x7000)
                 (JSR VM_INITIALIZE_MEMORY_MANAGER)
                 (JSR VM_INITIALIZE_CALL_FRAME)
                 (JSR VM_INTERPRETER_INIT)
                 (JMP VM_INTERPRETER))
           (list (org #x8000))
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
           vm-lists ;; includes vm-memory-manager
           (list (org-align #x100)) ;; align to next page
           (build-interpreter-optable relevant-opcode-definitions)))) ;; TODO create opcode table w/ wanted / knonwn opcodes only?

(define PAGE_CALL_FRAME #x8d)
(define PAGE_LOCALS_LB #x8b)
(define PAGE_LOCALS_LB_W #x8b00)
(define PAGE_LOCALS_HB #x8c)
(define PAGE_LOCALS_HB_W #x8c00)
(define PAGE_AVAIL_0 #x8a)
(define PAGE_AVAIL_0_W #x8a00)
(define PAGE_AVAIL_1 #x89)
(define PAGE_AVAIL_1_W #x8900)
