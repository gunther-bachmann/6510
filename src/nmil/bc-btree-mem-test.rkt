#lang racket/base

(require "./vm-btree.rkt")

(require "../6510.rkt")
(require "../6510-test-utils.rkt")

(module+ test
  (require rackunit)
  (require (only-in racket/list flatten make-list))
  (require (only-in "./vm-btree.rkt" BTREE_PATH_TO_FIRST))
  (require (only-in "./vm-interpreter-test-utils.rkt"
                    run-bc-wrapped-in-test-
                    vm-list->strings
                    vm-cell-pair-pages
                    vm-cell-pairs-free-in-page
                    vm-cell-pairs-used-info
                    vm-cell-pairs-used-num-in-page))
  (require (only-in "../cisc-vm/stack-virtual-machine.rkt" BRK))
  (require (only-in "./vm-memory-manager.rkt"
                    ZP_VM_PC
                    vm-page->strings
                    shorten-cell-strings
                    shorten-cell-string
                    vm-stack->strings))
  (require (only-in "../tools/6510-interpreter.rkt" initialize-cpu cpu-state-clock-cycles))
  (require [only-in "./vm-interpreter.rkt"
                    vm-interpreter
                    bc
                    CONS
                    CALL
                    PUSH_NIL
                    PUSH_I
                    PUSH_B
                    GC_FL
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
                    I0_P
                    INC_INT
                    MAX_INT
                    F_P_BRA
                    T_P_BRA
                    I_GT_P
                    CONS_PAIR_P
                    T_P_RET
                    F_P_RET
                    NIL_P_RET_L0_POP_1
                    INT_P
                    SWAP
                    POP_TO_L0
                    POP_TO_L1
                    POP_TO_L2
                    POP_TO_L3
                    WRITE_TO_L0
                    WRITE_TO_L1
                    WRITE_TO_L2
                    WRITE_TO_L3
                    PUSH_L0
                    PUSH_L1
                    PUSH_L2
                    PUSH_L3
                    PUSH_L0_CAR
                    PUSH_L1_CAR
                    PUSH_L2_CAR
                    PUSH_L3_CAR
                    PUSH_L0_CDR
                    PUSH_L1_CDR
                    PUSH_L2_CDR
                    PUSH_L3_CDR
                    PUSH_I0
                    PUSH_I1
                    PUSH_I2
                    PUSH_IM1
                    WRITE_L0
                    WRITE_L1
                    WRITE_L2
                    WRITE_L3])

  (define (wrap-bytecode-for-test bc)
    (append (list (org #x7000)
                  (JSR VM_INITIALIZE_MEMORY_MANAGER)
                  (JSR VM_INITIALIZE_CALL_FRAME)
                  (JSR VM_INTERPRETER_INIT)
                  (JMP VM_INTERPRETER))
            (list (org #x8000))
            (flatten bc)
            (list (org #xa000))
            vm-interpreter))

  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (run-bc-wrapped-in-test- bc wrapped-code debug)))

(module+ test #| btree from list |#
  (define b-tree-0-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_I) (word $0004)
       (bc CONS)
       (bc PUSH_I) (word $0003)
       (bc CONS)
       (bc PUSH_I) (word $0002)
       (bc CONS)
       (bc PUSH_I) (word $0001)
       (bc CONS)
       (bc PUSH_NIL)
       (bc SWAP)
       (bc CALL) (word-ref BTREE_FROM_LIST)
       (bc EXT) (bc GC_FL)
       (bc BRK))
      vm-btree)
     ))

  (check-equal? (shorten-cell-strings (vm-stack->strings b-tree-0-state 10 #t))
                (list "stack holds 1 item"
                      "((1 . 2) . (3 . 4))  (rt)"))
  (check-equal? (vm-cell-pair-pages b-tree-0-state)
                (list #x97)) ;; corresponds to (define PAGE_AVAIL_0 #x97) in vm-interpreter
  (check-equal? (length (vm-cell-pairs-free-in-page b-tree-0-state #x97))
                46) ;; after garbage collection, this should rise to (- 49 3)
  (check-equal? (vm-cell-pairs-used-num-in-page b-tree-0-state #x97)
                3)) ;; should be 3 (since the resulting tree only needs 3 cells)

(module+ test #| btree from-list, to-list |#
  (define b-tree-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_I) (word $0004)
       (bc CONS)
       (bc PUSH_I) (word $0003)
       (bc CONS)
       (bc PUSH_I) (word $0002)
       (bc CONS)
       (bc PUSH_I) (word $0001)
       (bc CONS)
       (bc PUSH_NIL)
       (bc SWAP)
       (bc CALL) (word-ref BTREE_FROM_LIST)
       ;; make sure to have the two defaul parameter filled with nil on the stack before the function
       (bc PUSH_NIL)
       (bc SWAP)
       (bc PUSH_NIL)
       (bc SWAP)
       (bc CALL) (word-ref BTREE_TO_LIST)
       (bc EXT) (bc GC_FL)
       (bc BRK))
      vm-btree)
     ))

  (cond [(void? b-tree-1-state)
         (skip (check-equal? #t #f "left debug session"))]
        [else
         (check-equal? (shorten-cell-strings (vm-stack->strings b-tree-1-state 10 #t))
                       (list "stack holds 1 item"
                             "(1 . (2 . (3 . (4 . NIL))))  (rt)"))
         (check-equal? (vm-cell-pair-pages b-tree-1-state)
                       (list #x97)) ;; corresponds to (define PAGE_AVAIL_0 #x97) in vm-interpreter
         (check-equal? (length (vm-cell-pairs-free-in-page b-tree-1-state #x97))
                       45) ;; after garbage collection, this should rise to (- 49 3)
         (check-equal? (vm-cell-pairs-used-num-in-page b-tree-1-state #x97)
                       4)]))

(module+ test #| btree from-list, path-to-first, add-value-after, to-list |#
  (define b-tree-2-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_I) (word $0060)
       (bc CONS)
       (bc PUSH_I) (word $0050)
       (bc CONS)
       (bc PUSH_I) (word $0040)
       (bc CONS)
       (bc PUSH_I) (word $0030)
       (bc CONS)
       (bc PUSH_I) (word $0020)
       (bc CONS)
       (bc PUSH_I) (word $0010)
       (bc CONS)
       (bc PUSH_NIL)
       (bc SWAP)
       (bc CALL) (word-ref BTREE_FROM_LIST)

       (bc PUSH_NIL)
       (bc SWAP)
       (bc CALL) (word-ref BTREE_PATH_TO_FIRST)

       (bc PUSH_I) (word $0015)
       (bc CALL) (word-ref BTREE_ADD_VALUE_AFTER)

       (bc CALL) (word-ref BTREE_ROOT_FOR_PATH)

       ;; ;; make sure to have the two defaul parameter filled with nil on the stack before the function
       (bc PUSH_NIL)
       (bc SWAP)
       (bc PUSH_NIL)
       (bc SWAP)
       (bc CALL) (word-ref BTREE_TO_LIST)
       (bc EXT) (bc GC_FL)
       (bc BRK))
      vm-btree)
     ))

  (cond [(void? b-tree-2-state)
         (skip (check-equal? #t #f "left debug session"))]
        [else
         (check-equal? (shorten-cell-strings (vm-stack->strings b-tree-2-state 10 #t))
                       (list "stack holds 1 item"
                             "(10 . (15 . (20 . (30 . (40 . (50 . (60 . NIL)))))))  (rt)"
                             ))
         (check-equal? (vm-cell-pair-pages b-tree-2-state)
                       (list #x97)) ;; corresponds to (define PAGE_AVAIL_0 #x97) in vm-interpreter
         (check-equal? (length (vm-cell-pairs-free-in-page b-tree-2-state #x97))
                       42) 
         (check-equal? (vm-cell-pairs-used-num-in-page b-tree-2-state #x97)
                       7) ;; is actually the number of cons cells (which is the number of dots in the list above)
         (check-equal? (map (lambda (str) (regexp-replace #rx"^pair-ptr\\[1\\].*" str ""))
                            (vm-cell-pairs-used-info  b-tree-2-state #x97))
                       (make-list 7 "")
                       "all pair ptrs in use are referenced only once!")]))


(module+ test #| btree from-list, path-to-first, add-value-after, to-list |#
  (define b-tree-3-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_I) (word $0060)
       (bc CONS)
       (bc PUSH_I) (word $0050)
       (bc CONS)
       (bc PUSH_I) (word $0040)
       (bc CONS)
       (bc PUSH_I) (word $0030)
       (bc CONS)
       (bc PUSH_I) (word $0020)
       (bc CONS)
       (bc PUSH_I) (word $0010)
       (bc CONS)
       (bc PUSH_NIL)
       (bc SWAP)
       (bc CALL) (word-ref BTREE_FROM_LIST)

       (bc PUSH_NIL)
       (bc SWAP)
       (bc CALL) (word-ref BTREE_PATH_TO_FIRST)

       (bc PUSH_I) (word $0015)
       (bc CALL) (word-ref BTREE_ADD_VALUE_AFTER)

       (bc CALL) (word-ref BTREE_NEXT)

       (bc PUSH_I) (word $0025)
       (bc CALL) (word-ref BTREE_ADD_VALUE_AFTER)

       (bc CALL) (word-ref BTREE_NEXT)
       (bc CALL) (word-ref BTREE_NEXT)

       ;; make sure to have the two defaul parameter filled with nil on the stack before the function
       (bc PUSH_NIL)
       (bc SWAP)
       (bc PUSH_NIL)
       (bc SWAP)
       (bc CALL) (word-ref BTREE_REMOVE_VALUE_AT)

       (bc CALL) (word-ref BTREE_ROOT_FOR_PATH)

       ;; ;; make sure to have the two defaul parameter filled with nil on the stack before the function
       (bc PUSH_NIL)
       (bc SWAP)
       (bc PUSH_NIL)
       (bc SWAP)
       (bc CALL) (word-ref BTREE_TO_LIST)
       (bc EXT) (bc GC_FL)
       (bc BRK))
      vm-btree)
     ))

  (cond [(void? b-tree-3-state)
         (skip (check-equal? #t #f "left debug session"))]
        [else
         (check-equal? (shorten-cell-strings (vm-stack->strings b-tree-3-state 10 #t))
                       (list "stack holds 1 item"
                             "(10 . (15 . (20 . (25 . (30 . (50 . (60 . NIL)))))))  (rt)"
                             ))
         (check-equal? (vm-cell-pair-pages b-tree-3-state)
                       (list #x97)) ;; corresponds to (define PAGE_AVAIL_0 #x97) in vm-interpreter
         (check-equal? (length (vm-cell-pairs-free-in-page b-tree-3-state #x97))
                       42)
         (check-equal? (vm-cell-pairs-used-num-in-page b-tree-3-state #x97)
                       7) ;; is actually the number of cons cells (which is the number of dots in the list above)
         (check-equal? (map (lambda (str) (regexp-replace #rx"^pair-ptr\\[1\\].*" str "ok"))
                            (vm-cell-pairs-used-info  b-tree-3-state #x97))
                       (make-list 7 "ok")
                       "all pair ptrs in use are referenced only once!")]))

(module+ test #| btree reverse |#
  (define btree-reverse-0-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I0)
       (bc PUSH_I1)
       (bc CONS)
       (bc BNOP)
       (bc CALL) (word-ref BTREE_REVERSE)
       (bc EXT) (bc GC_FL)
       (bc BRK))
      vm-btree)
     ))

  (inform-check-equal? (cpu-state-clock-cycles btree-reverse-0-state)
                       17321)
  (check-equal? (shorten-cell-strings (vm-stack->strings btree-reverse-0-state 10 #t))
                (list "stack holds 1 item"
                      "(0 . 1)  (rt)"))
  (check-equal? (vm-cell-pairs-used-num-in-page btree-reverse-0-state #x97)
                1) ;; is actually the number of cons cells (which is the number of dots in the list above)
  (check-equal? (map (lambda (str) (regexp-replace #rx"^pair-ptr\\[1\\].*" str "ok"))
                     (vm-cell-pairs-used-info  btree-reverse-0-state #x97))
                (make-list 1 "ok")
                "all pair ptrs in use are referenced only once!"))
