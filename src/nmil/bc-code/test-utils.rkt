#lang racket/base


(provide (all-from-out "../vm-inspector-utils.rkt")
         (all-from-out "../vm-interpreter-test-utils.rkt")
         (all-from-out "../vm-runtime/vm-call-frame.rkt")
         (all-from-out "../vm-bc-opcode-definitions.rkt")
         (all-from-out "../../6510.rkt")
         (all-from-out "../../6510-test-utils.rkt")
         (all-from-out "../../tools/6510-interpreter.rkt")
         (all-from-out "../vm-runtime/vm-memory-map.rkt")
         (all-from-out "../../util.rkt")
         (all-from-out racket/list)
         run-bc-wrapped-in-test
         PAGE_CALL_FRAME
         PAGE_LOCALS_LB
         PAGE_LOCALS_LB_W
         PAGE_LOCALS_HB
         PAGE_LOCALS_HB_W
         PAGE_AVAIL_0
         PAGE_AVAIL_0_W
         PAGE_AVAIL_1
         PAGE_AVAIL_1_W)

#|

 utility functions to do byte code tests

 |#

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
         (only-in "../../util.rkt"
                  bytes->int
                  format-hex-byte
                  format-hex-word)
         (only-in "../vm-bc-opcode-definitions.rkt"
                  bc
                  bc-opcode-definitions
                  fetch-opcode-list
                  full-extended-optable-lb
                  full-extended-optable-hb
                  full-interpreter-opcode-table)
         (only-in "../vm-inspector-utils.rkt"
                  shorten-cell-strings
                  shorten-cell-string
                  vm-cell-at-nil?
                  vm-slot->string
                  vm-page->strings
                  vm-stack->strings
                  vm-regt->string
                  vm-cell-at->string
                  vm-cell->string
                  vm-deref-cell-pair-w->string)
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
         (only-in "../vm-interpreter.rkt"
                  just-vm-interpreter)
         (only-in "../vm-runtime/vm-call-frame.rkt"
                  vm-call-frame->strings)
         (only-in "../vm-runtime/vm-memory-manager.rkt"
                  VM_INIT_MEMORY_MANAGER
                  vm-memory-manager-code)
         "../vm-runtime/vm-memory-map.rkt")

(define (wrap-bytecode-for-full-bc-test bc-to-wrap)
  (flatten
   (append
    (list (org #x07f0)
          (JSR VM_INIT_MEMORY_MANAGER)
          (JSR VM_INTERPRETER_INIT)
          (JMP VM_INTERPRETER))
    (list (org #x0800))
    bc-to-wrap
    (list (ast-bytes-cmd '() (fetch-opcode-list "BREAK")))
    ;;(bc BREAK)
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

;; cell_stack_lb #xcf
;; cell_stack_hb #xce
(define PAGE_CALL_FRAME #xcd)
(define PAGE_CALL_FRAME_HB #xcc)
(define PAGE_LOCALS_LB #xcd)
(define PAGE_LOCALS_LB_W #xcd00)
(define PAGE_LOCALS_HB #xcc)
(define PAGE_LOCALS_HB_W #xcc00)
(define PAGE_AVAIL_0 #xcb)
(define PAGE_AVAIL_0_W #xcb00)
(define PAGE_AVAIL_1 #xca)
(define PAGE_AVAIL_1_W #xca00)

(define (run-bc-wrapped-in-test bc (debug #f))
  (define wrapped-code (wrap-bytecode-for-full-bc-test (flatten bc)))
  (run-bc-wrapped-in-test- (flatten bc) wrapped-code debug))
