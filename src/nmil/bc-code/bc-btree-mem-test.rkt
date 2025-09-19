#lang racket/base

(require "../../6510-test-utils.rkt"
         "../../6510.rkt"
         "./bc-btree.rkt")

(module+ test
  (require (only-in racket/list flatten make-list)
           rackunit
           (only-in "../../cisc-vm/stack-virtual-machine.rkt" BRK)
           (only-in "../../tools/6510-interpreter.rkt" initialize-cpu cpu-state-clock-cycles)
           (only-in "./bc-btree.rkt" BTREE_PATH_TO_FIRST)
           (only-in "../vm-bc-opcode-definitions.rkt" bc)
           (only-in "../vm-inspector-utils.rkt"
                    vm-page->strings
                    shorten-cell-strings
                    shorten-cell-string
                    vm-stack->strings)
           (only-in "../vm-interpreter-test-utils.rkt"
                    run-bc-wrapped-in-test-
                    vm-list->strings
                    vm-cell-pair-pages
                    vm-cell-pairs-free-in-page
                    vm-cell-pairs-used-info
                    vm-cell-pairs-used-num-in-page)
           [only-in "../vm-interpreter.rkt" vm-interpreter])

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
        (bc GC)
        (bc BREAK)))
      vm-btree)
     ))

  (check-equal? (shorten-cell-strings (vm-stack->strings b-tree-0-state 10 #t))
                (list "stack holds 1 item"
                      "((1 . 2) . (3 . 4))  (rt)"))
  (check-equal? (vm-cell-pair-pages b-tree-0-state)
                (list #x8a)) ;; corresponds to (define PAGE_AVAIL_0 #x8b) in vm-interpreter
  (check-equal? (length (vm-cell-pairs-free-in-page b-tree-0-state #x8a))
                46) ;; after garbage collection, this should rise to (- 49 3)
  (check-equal? (vm-cell-pairs-used-num-in-page b-tree-0-state #x8a)
                3)) ;; should be 3 (since the resulting tree only needs 3 cells)

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
        (bc GC)
        (bc BREAK)))
      vm-btree)
     ))

  (cond [(void? b-tree-1-state)
         (skip (check-equal? #t #f "left debug session"))]
        [else
         (check-equal? (shorten-cell-strings (vm-stack->strings b-tree-1-state 10 #t))
                       (list "stack holds 1 item"
                             "(1 . (2 . (3 . (4 . NIL))))  (rt)"))
         (check-equal? (vm-cell-pair-pages b-tree-1-state)
                       (list #x8a)) ;; corresponds to (define PAGE_AVAIL_0 #x8b) in vm-interpreter
         (check-equal? (length (vm-cell-pairs-free-in-page b-tree-1-state #x8a))
                       45) ;; after garbage collection, this should rise to (- 49 3)
         (check-equal? (vm-cell-pairs-used-num-in-page b-tree-1-state #x8a)
                       4)]))

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
        (bc GC)
        (bc BREAK)))
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
                       (list #x8a)) ;; corresponds to (define PAGE_AVAIL_0 #x8a) in vm-interpreter
         (check-equal? (length (vm-cell-pairs-free-in-page b-tree-2-state #x8a))
                       42) 
         (check-equal? (vm-cell-pairs-used-num-in-page b-tree-2-state #x8a)
                       7) ;; is actually the number of cons cells (which is the number of dots in the list above)
         (check-equal? (map (lambda (str) (regexp-replace #rx"^pair-ptr\\[1\\].*" str ""))
                            (vm-cell-pairs-used-info  b-tree-2-state #x8a))
                       (make-list 7 "")
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
        (bc GC)
        (bc BREAK)))
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
                       (list #x8a)) ;; corresponds to (define PAGE_AVAIL_0 #x8a) in vm-interpreter
         (check-equal? (length (vm-cell-pairs-free-in-page b-tree-3-state #x8a))
                       42)
         (check-equal? (vm-cell-pairs-used-num-in-page b-tree-3-state #x8a)
                       7) ;; is actually the number of cons cells (which is the number of dots in the list above)
         (check-equal? (map (lambda (str) (regexp-replace #rx"^pair-ptr\\[1\\].*" str "ok"))
                            (vm-cell-pairs-used-info  b-tree-3-state #x8a))
                       (make-list 7 "ok")
                       "all pair ptrs in use are referenced only once!")]))

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
        (bc GC)
        (bc BREAK)))
      vm-btree)
     ))

  (inform-check-equal? (cpu-state-clock-cycles btree-reverse-0-state)
                       16306)
  (check-equal? (shorten-cell-strings (vm-stack->strings btree-reverse-0-state 10 #t))
                (list "stack holds 1 item"
                      "(0 . 1)  (rt)"))
  (check-equal? (vm-cell-pairs-used-num-in-page btree-reverse-0-state #x8a)
                1) ;; is actually the number of cons cells (which is the number of dots in the list above)
  (check-equal? (map (lambda (str) (regexp-replace #rx"^pair-ptr\\[1\\].*" str "ok"))
                     (vm-cell-pairs-used-info  btree-reverse-0-state #x8a))
                (make-list 1 "ok")
                "all pair ptrs in use are referenced only once!"))
