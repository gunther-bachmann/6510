#lang racket/base

(require "../../6510-test-utils.rkt"
         "../../6510.rkt"
         "./bc-btree.rkt")

(module+ test
  (require (only-in racket/list flatten make-list)
           rackunit
           (only-in "../../cisc-vm/stack-virtual-machine.rkt" BRK)
           (only-in "../../tools/6510-interpreter.rkt"
                    initialize-cpu
                    cpu-state-clock-cycles
                    peek)
           (only-in "./bc-btree.rkt" BTREE_PATH_TO_FIRST)
           (only-in "../vm-bc-opcode-definitions.rkt" bc)
           (only-in "../vm-inspector-utils.rkt"
                    shorten-cell-strings
                    shorten-cell-string
                    vm-cell-n->string
                    vm-stack-n->strings)
           (only-in "../vm-interpreter-test-utils.rkt"
                    run-bc-wrapped-in-test-
                    vm-list->strings
                    ;; vm-cell-pair-pages
                    ;; vm-cell-pairs-free-in-page
                    ;; vm-cell-pairs-used-info
                    ;; vm-cell-pairs-used-num-in-page
                    vm-num-slots-used-in-page
                    vm-slots-used-in-page
                    vm-slots-free-in-page)
           (only-in "../vm-interpreter-loop.rkt" VM_INTERPRETER_ZP)
           [only-in "../vm-interpreter.rkt" vm-interpreter]
           (only-in "../vm-interpreter-bc/test-utils.rkt"
                    wrap-bytecode-for-full-bc-test))

  ;; (define (wrap-bytecode-for-test bc)
  ;;   (append (list (org #x7000)
  ;;                 (JSR VM_INITIALIZE_MEMORY_MANAGER)
  ;;                 (JSR VM_INITIALIZE_CALL_FRAME)
  ;;                 (JSR VM_INTERPRETER_INIT)
  ;;                 (JMP VM_INTERPRETER))
  ;;           (list (org #x8000))
  ;;           (flatten bc)
  ;;           (list (org #xa000))
  ;;           vm-interpreter
  ;;           VM_INTERPRETER_ZP))

  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-full-bc-test bc))
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

  (check-equal? (shorten-cell-strings (vm-stack-n->strings b-tree-0-state 10 #t))
                (list "stack holds 2 items"
                      "((1 . 2) . (3 . 4))  (rt)"
                      "NIL"))
  (check-equal? (length (vm-slots-free-in-page b-tree-0-state #xca))
                39) ;; after garbage collection, this should rise to (- 42 3)
  (check-equal? (vm-num-slots-used-in-page b-tree-0-state #xca)
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
         (check-equal? (shorten-cell-strings (vm-stack-n->strings b-tree-1-state 10 #t))
                       (list "stack holds 2 items"
                             "(1 . (2 . (3 . (4 . NIL))))  (rt)"
                             "NIL"))
         (check-equal? (length (vm-slots-free-in-page b-tree-1-state #xca))
                       38) ;; after garbage collection, this should rise to (- 42 4)
         (check-equal? (vm-num-slots-used-in-page b-tree-1-state #xca)
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
         (check-equal? (shorten-cell-strings (vm-stack-n->strings b-tree-2-state 10 #t))
                       (list "stack holds 2 items"
                             "(10 . (15 . (20 . (30 . (40 . (50 . (60 . NIL)))))))  (rt)"
                             "NIL"
                             ))
         (check-equal? (length (vm-slots-free-in-page b-tree-2-state #xca))
                       38) ;; 42  - 4
         (check-equal? (length (vm-slots-free-in-page b-tree-2-state #xc9))
                       39) ;; 42  - 3
         (check-equal? (+ (vm-num-slots-used-in-page b-tree-2-state #xca)
                          (vm-num-slots-used-in-page b-tree-2-state #xc9))
                       7) ;; is actually the number of cons cells (which is the number of dots in the list above)
         (check-equal? (map (lambda (slot-offset)
                              (peek b-tree-2-state (bytes->int slot-offset #xca)))
                            (vm-slots-used-in-page  b-tree-2-state #xca))
                       (make-list 4 1)
                       "all pair ptrs in use are referenced only once!")
         (check-equal? (map (lambda (slot-offset)
                              (peek b-tree-2-state (bytes->int slot-offset #xc9)))
                            (vm-slots-used-in-page  b-tree-2-state #xc9))
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
        (bc GC)
        (bc BREAK)))
      vm-btree)
     ))

  (cond [(void? b-tree-3-state)
         (skip (check-equal? #t #f "left debug session"))]
        [else
         (check-equal? (shorten-cell-strings (vm-stack-n->strings b-tree-3-state 10 #t))
                       (list "stack holds 2 items"
                             "(10 . (15 . (20 . (25 . (30 . (50 . (60 . NIL)))))))  (rt)"
                             "NIL"
                             ))
         (check-equal? (vm-num-slots-used-in-page b-tree-3-state #xca)
                       0)
         (check-equal? (length (vm-slots-free-in-page b-tree-3-state #xc9))
                       35)
         (check-equal? (vm-num-slots-used-in-page b-tree-3-state #xc9)
                       7) ;; is actually the number of cons cells (which is the number of dots in the list above)
         (check-equal? (map (lambda (slot-offset)
                              (peek b-tree-3-state (bytes->int slot-offset #xc9)))
                            (vm-slots-used-in-page  b-tree-3-state #xc9))
                       (make-list 7 1))]))

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
                       15336)
  (check-equal? (shorten-cell-strings (vm-stack-n->strings btree-reverse-0-state 10 #t))
                (list "stack holds 2 items"
                      "(0 . 1)  (rt)"
                      "NIL"))
  (check-equal? (vm-num-slots-used-in-page btree-reverse-0-state #xca)
                1) ;; is actually the number of cons cells (which is the number of dots in the list above)
  (check-equal? (map (lambda (slot-offset)
                              (peek btree-reverse-0-state (bytes->int slot-offset #xca)))
                            (vm-slots-used-in-page btree-reverse-0-state #xca))
                       '(1)
                       "all pair ptrs in use are referenced only once!"))
