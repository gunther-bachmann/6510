#lang racket/base

(require "../6510.rkt")
(require "../ast/6510-calc-opcode-facades.rkt")
(require (only-in "../ast/6510-resolver.rkt" commands->bytes))

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
  (define org 2064)
  (define program (append (list)
                          VM_ALLOC_PAGE_JUMP_TABLE
                          VM_ALLOC_PAGE))
  (check-equal? (commands->bytes #x2068 program)
                (list 0 0 0 0
                      185 105 32
                      141 122 32
                      185 104 32
                      141 121 32
                      76 0 0)))
