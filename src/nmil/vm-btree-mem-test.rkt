#lang racket/base

(require "./vm-btree.rkt")

(require "../6510.rkt")

(module+ test
  (require rackunit)
  (require (only-in racket/list flatten))
  (require (only-in "./vm-interpreter-test-utils.rkt"
                    run-bc-wrapped-in-test-
                    vm-list->strings
                    vm-cell-pair-pages
                    vm-cell-pairs-free-in-page
                    vm-cell-pairs-used-num-in-page))
  (require (only-in "../cisc-vm/stack-virtual-machine.rkt" BRK))
  (require (only-in "./vm-memory-manager.rkt"
                    ZP_VM_PC
                    cleanup-strings
                    cleanup-string
                    vm-stack->strings))
  (require (only-in "../cisc-vm/stack-virtual-machine.rkt"
                    CONS
                    CAR
                    CDR
                    GOTO
                    RET
                    BYTE+
                    INT+
                    INT-
                    BRA
                    CALL
                    NIL?
                    TAIL_CALL

                    PUSH_INT
                    PUSH_BYTE
                    PUSH_NIL
                    PUSH_LOCAL
                    PUSH_GLOBAL
                    PUSH_STRUCT_FIELD

                    POP_TO_LOCAL
                    POP_TO_GLOBAL))
  (require [only-in "./vm-interpreter.rkt"
                    vm-interpreter
                    bc
                    CELL_EQ
                    EXT
                    CAAR
                    CADR
                    CDAR
                    CDDR
                    COONS
                    POP
                    DUP
                    BNOP
                    INT_0_P
                    INC_INT
                    MAX_INT
                    FALSE_P_BRANCH
                    TRUE_P_BRANCH
                    INT_GREATER_P
                    CONS_PAIR_P
                    TRUE_P_RET
                    FALSE_P_RET
                    NIL?_RET_LOCAL_0_POP_1
                    INT_P
                    SWAP
                    POP_TO_LOCAL_0
                    POP_TO_LOCAL_1
                    POP_TO_LOCAL_2
                    POP_TO_LOCAL_3
                    WRITE_TO_LOCAL_0
                    WRITE_TO_LOCAL_1
                    WRITE_TO_LOCAL_2
                    WRITE_TO_LOCAL_3
                    PUSH_LOCAL_0
                    PUSH_LOCAL_1
                    PUSH_LOCAL_2
                    PUSH_LOCAL_3
                    PUSH_LOCAL_0_CAR
                    PUSH_LOCAL_1_CAR
                    PUSH_LOCAL_2_CAR
                    PUSH_LOCAL_3_CAR
                    PUSH_LOCAL_0_CDR
                    PUSH_LOCAL_1_CDR
                    PUSH_LOCAL_2_CDR
                    PUSH_LOCAL_3_CDR
                    PUSH_INT_0
                    PUSH_INT_1
                    PUSH_INT_2
                    PUSH_INT_m1
                    WRITE_FROM_LOCAL_0
                    WRITE_FROM_LOCAL_1
                    WRITE_FROM_LOCAL_2
                    WRITE_FROM_LOCAL_3])

  (define (wrap-bytecode-for-test bc)
    (append (list (org #x7000)
                  (JSR VM_INITIALIZE_MEMORY_MANAGER)
                  (JSR VM_INITIALIZE_CALL_FRAME)
                  (JSR VM_INTERPRETER_INIT)
                  (JMP VM_INTERPRETER))
            (list (org #x8000))
            (flatten bc)
            (list (org #xc000))
            vm-interpreter))

  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (run-bc-wrapped-in-test- bc wrapped-code debug)))

(module+ test #| memory pages |#

  (define create-tree-0-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_INT) (word $0004)
       (bc CONS)
       (bc PUSH_INT) (word $0003)
       (bc CONS)
       (bc PUSH_INT) (word $0002)
       (bc CONS)
       (bc PUSH_INT) (word $0001)
       (bc CONS)
       (bc PUSH_NIL)
       (bc SWAP)
       (bc CALL) (word-ref BTREE_FROM_LIST)
       (bc BRK))
      vm-btree)
     ))

  (check-equal? (cleanup-strings (vm-stack->strings create-tree-0-state 10 #t))
                (list "stack holds 1 item"
                      "((1 . 2) . (3 . 4))  (rt)"))

  (check-equal? (vm-cell-pair-pages create-tree-0-state)
                (list #x97)) ;; corresponds to (define PAGE_AVAIL_0 #x97) in vm-interpreter

  (check-equal? (length (vm-cell-pairs-free-in-page create-tree-0-state #x97))
                37) ;; after garbage collection, this should rise to (- 49 3)

  (check-equal? (vm-cell-pairs-used-num-in-page create-tree-0-state #x97)
                (- 49 37))) ;; should be 3 (since the resulting tree only needs 3 cells


