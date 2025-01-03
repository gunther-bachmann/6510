#lang racket/base

#|

implementation of a b-tree with values at leafs from pure bytecode

this implementation will be the testbed for all refcounting gc testing


TODOS:
  implement:
    DONE btree-make-root
    btree-node?
    btree-value?
    btree-validate
    btree-depth
    btree-path-to-first
    btree-path-to-list
    btree-node-for-path
    btree-prev
    btree-next
    recursive-rebuild-path-at-with
    btree-add-value-after
    btree-add-value-before
    btree->list
    btree<-list
    btree-remove-value-at
    btree-root-of-path
|#

(require (only-in racket/list flatten))

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
                  CONS_PAIR_P
                  TRUE_P_RET
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
                  PUSH_INT_0
                  PUSH_INT_1
                  PUSH_INT_2
                  PUSH_INT_m1
                  WRITE_FROM_LOCAL_0
                  WRITE_FROM_LOCAL_1
                  WRITE_FROM_LOCAL_2
                  WRITE_FROM_LOCAL_3])


  (require "../6510.rkt")



(module+ test #|  |#
  (require "../6510-test-utils.rkt")

  (require (only-in "./vm-interpreter-test-utils.rkt" run-bc-wrapped-in-test-))
  (require (only-in "../cisc-vm/stack-virtual-machine.rkt" BRK))

  (require (only-in "./vm-memory-manager.rkt"
                  vm-cell-at-nil?
                  vm-page->strings
                  vm-stack->strings
                  vm-regt->string
                  vm-cell-at->string
                  vm-cell->string
                  vm-deref-cell-pair-w->string))
  (require (only-in "../util.rkt" bytes->int format-hex-byte format-hex-word))


  (define PAGE_AVAIL_0 #x97)
  (define PAGE_AVAIL_0_W #x9700)
  (define PAGE_AVAIL_1 #x96)
  (define PAGE_AVAIL_1_W #x9600)

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


;; (define (btree-make-root value)
;;   (cons value null))
(define BTREE_MAKE_ROOT
  (list 
   (label BTREE_MAKE_ROOT)
          (byte 0)    ;; locals
          (bc PUSH_NIL)
          (bc SWAP)
          (bc CONS)
          (bc RET)))

(module+ test #| make root |#
  (define btree-make-root-state
    (run-bc-wrapped-in-test
     (append      
      (list
       (bc PUSH_INT_1)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc BRK))

      (list (org #x8F00))
      BTREE_MAKE_ROOT)
     ))

  (check-equal? (vm-stack->strings btree-make-root-state)
                  (list "stack holds 1 item"
                        (format "cell-pair-ptr $~a05  (rt)" (format-hex-byte PAGE_AVAIL_0))))

  (define btree-make-root-2-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_INT_1)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc CAR)
       (bc BRK))

      (list (org #x8F00))
      BTREE_MAKE_ROOT)))

  (check-equal? (vm-stack->strings btree-make-root-2-state)
                  (list "stack holds 1 item"
                        "cell-int $0001  (rt)")))

;; (define (btree-value? node)
;;   (or (string? node) (integer? node)))
(define BTREE_VALUE_P
  (list (byte 1)   ;; local
        (bc WRITE_TO_LOCAL_0)
        (bc INT_P)
        (bc TRUE_P_RET) ;; or (bc TRUE_P_BRANCH +3) ;; <- to RET
        ;; (bc PUSH_LOCAL_0) ;; currently only int (later maybe strings)
        ;; (bc STRING_P)
        (bc RET)))

(module+ test #|  |#)

;; (define (btree-node? node)
;;   (pair? node))
(define BTREE_NODE_P
  (list (byte 0) ;; locals
        (bc CONS_PAIR_P)
        (bc RET)))

(module+ test #|  |#)

