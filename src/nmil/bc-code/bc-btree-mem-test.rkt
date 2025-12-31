#lang racket/base

(require "../../6510-test-utils.rkt"
         "../../6510.rkt"
         "./bc-btree.rkt")

(module+ test
  (require
   (only-in "./bc-btree.rkt" BTREE_PATH_TO_FIRST)
   "./test-utils.rkt"))

(module+ test #| btree from list |#
  (define b-tree-0-state
    (run-bc-wrapped-in-test
     (append
      (flatten
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
        (bc GC)))
      vm-btree)
    ))

  (pcheck-equal? (shorten-cell-strings (vm-stack->strings b-tree-0-state 10 #t))
                (list "stack holds 2 items"
                      "((1 . 2) . (3 . 4))  (rt)"
                      "NIL"))
  (pcheck-equal? (length (vm-slots-free-in-page b-tree-0-state PAGE_AVAIL_0))
                39
                "after garbage collection, this should rise to (- 42 3)")
  (pcheck-equal? (vm-num-slots-used-in-page b-tree-0-state PAGE_AVAIL_0)
                3
                "should be 3 (since the resulting tree only needs 3 cells)"))

(module+ test #| btree from-list, to-list |#
  (define b-tree-1-state
    (run-bc-wrapped-in-test
     (append
      (flatten
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
        (bc GC)))
      vm-btree)
     ))

  (cond [(void? b-tree-1-state)
         (skip (pcheck-equal? #t #f "left debug session"))]
        [else
         (pcheck-equal? (shorten-cell-strings (vm-stack->strings b-tree-1-state 10 #t))
                       (list "stack holds 2 items"
                             "(1 . (2 . (3 . (4 . NIL))))  (rt)"
                             "NIL"))
         (pcheck-equal? (length (vm-slots-free-in-page b-tree-1-state PAGE_AVAIL_0))
                       38
                       "after garbage collection, this should rise to (- 42 4)")
         (pcheck-equal? (vm-num-slots-used-in-page b-tree-1-state PAGE_AVAIL_0)
                       4
                       "only four elements of the list remain")]))

(module+ test #| btree from-list, path-to-first, add-value-after, to-list |#
  (define b-tree-2-state
    (run-bc-wrapped-in-test
     (append
      (flatten
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
        (bc GC)))
      vm-btree)
     ))

  (cond [(void? b-tree-2-state)
         (skip (pcheck-equal? #t #f "left debug session"))]
        [else
         (pcheck-equal? (shorten-cell-strings (vm-stack->strings b-tree-2-state 10 #t))
                       (list "stack holds 2 items"
                             "(10 . (15 . (20 . (30 . (40 . (50 . (60 . NIL)))))))  (rt)"
                             "NIL"
                             ))
         (pcheck-equal? (length (vm-slots-free-in-page b-tree-2-state PAGE_AVAIL_0))
                       38
                       "38 slots left on page 0") ;; 42  - 4
         (pcheck-equal? (length (vm-slots-free-in-page b-tree-2-state PAGE_AVAIL_1))
                       39
                       "39 slots left on page 1") ;; 42  - 3
         (pcheck-equal? (+ (vm-num-slots-used-in-page b-tree-2-state PAGE_AVAIL_0)
                          (vm-num-slots-used-in-page b-tree-2-state PAGE_AVAIL_1))
                       7
                       "is actually the number of cons cells (which is the number of dots in the list above)")
         (pcheck-equal? (map (lambda (slot-offset)
                              (peek b-tree-2-state (bytes->int slot-offset PAGE_AVAIL_0)))
                            (vm-slots-used-in-page  b-tree-2-state PAGE_AVAIL_0))
                       (make-list 4 1)
                       "all pair ptrs in use are referenced only once!")
         (pcheck-equal? (map (lambda (slot-offset)
                              (peek b-tree-2-state (bytes->int slot-offset PAGE_AVAIL_1)))
                            (vm-slots-used-in-page  b-tree-2-state PAGE_AVAIL_1))
                       (make-list 3 1)
                       "all pair ptrs in use are referenced only once!")]))

(module+ test #| btree from-list, path-to-first, add-value-after, to-list |#
  (define b-tree-3-state
    (run-bc-wrapped-in-test
     (append
      (flatten
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
        (bc GC)))
      vm-btree)
     ))

  (cond [(void? b-tree-3-state)
         (skip (pcheck-equal? #t #f "left debug session"))]
        [else
         (pcheck-equal? (shorten-cell-strings (vm-stack->strings b-tree-3-state 10 #t))
                       (list "stack holds 2 items"
                             "(10 . (15 . (20 . (25 . (30 . (50 . (60 . NIL)))))))  (rt)"
                             "NIL"
                             ))
         (pcheck-equal? (vm-num-slots-used-in-page b-tree-3-state PAGE_AVAIL_0)
                       0
                       "page 0 is empty, no slots used anymore")
         (pcheck-equal? (length (vm-slots-free-in-page b-tree-3-state PAGE_AVAIL_1))
                       35
                       "page 1 uses 7 slots and has 35 left")
         (pcheck-equal? (vm-num-slots-used-in-page b-tree-3-state PAGE_AVAIL_1)
                       7
                       "is actually the number of cons cells (which is the number of dots in the list above)")
         (pcheck-equal? (map (lambda (slot-offset)
                              (peek b-tree-3-state (bytes->int slot-offset PAGE_AVAIL_1)))
                            (vm-slots-used-in-page  b-tree-3-state PAGE_AVAIL_1))
                       (make-list 7 1)
                       "all reference counts of all used slots is 1")]))

(module+ test #| btree reverse |#
  (define btree-reverse-0-state
    (run-bc-wrapped-in-test
     (append
      (flatten
       (list
        (bc PUSH_I0)
        (bc PUSH_I1)
        (bc CONS)
        (bc BNOP)
        (bc CALL) (word-ref BTREE_REVERSE)
        (bc GC)))
      vm-btree)
     ))

  (inform-check-equal? (cpu-state-clock-cycles btree-reverse-0-state)
                       14933)
  (pcheck-equal? (shorten-cell-strings (vm-stack->strings btree-reverse-0-state 10 #t))
                (list "stack holds 2 items"
                      "(0 . 1)  (rt)"
                      "NIL"))
  (pcheck-equal? (vm-num-slots-used-in-page btree-reverse-0-state PAGE_AVAIL_0)
                1
                "is actually the number of cons cells (which is the number of dots in the list above)")
  (pcheck-equal? (map (lambda (slot-offset)
                              (peek btree-reverse-0-state (bytes->int slot-offset PAGE_AVAIL_0)))
                            (vm-slots-used-in-page btree-reverse-0-state PAGE_AVAIL_0))
                       '(1)
                       "all ptrs in use are referenced only once!"))
