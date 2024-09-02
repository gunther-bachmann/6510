#lang racket/base

(require "../6510.rkt")
(require "../ast/6510-calc-opcode-facades.rkt")

(module+ test
  (require "../6510-test-utils.rkt")
  (require (only-in "../tools/6510-interpreter.rkt" memory-list run-interpreter cpu-state-accumulator)))

(define VM_ALLOC_PAGE_C
  (list
   ;; page type list-pair-cells
   (byte-const PT_LIST_PAIR_CELLS #x00)))


(define VM_ALLOC_PAGE_JUMP_TABLE
  (list
   (label VM_ALLOC_PAGE__LIST_PAIR_CELLS_JT)
   (word $0000)
   (label VM_ALLOC_PAGE__CALL_STACK_JT)
   (word $0000)))

;; parameter:
;;   y = page-type to allocate (00 = fixed slot size list-pair-cells,
;;                              01 = variable slot size native arrays for call-stack)
;;
(define VM_ALLOC_PAGE
  (list
   (label VM_ALLOC_PAGE)
   (LDA VM_ALLOC_PAGE__LIST_PAIR_CELLS_JT+1,y)
   (STA VM_ALLOC_PAGE__JT+2)
   (LDA VM_ALLOC_PAGE__LIST_PAIR_CELLS_JT,y)
   (STA VM_ALLOC_PAGE__JT+1)
   (label VM_ALLOC_PAGE__JT)
   (JMP $0000)
   ))

(module+ test #| VM_ALLOC_PAGE |#


  )
