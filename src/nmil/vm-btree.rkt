#lang racket/base

#|

implementation of a b-tree with values at leafs from pure bytecode

this implementation will be the testbed for all refcounting gc testing


TODOS:
    DONE btree-make-root
    DONE btree-node?
    DONE btree-value?
    IMPLEMENT btree-validate
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
                  FALSE_P_BRANCH
                  TRUE_P_BRANCH
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
(require (only-in "./vm-memory-manager.rkt" ZP_VM_PC))


(require "../6510.rkt")
(require (only-in "../tools/6510-interpreter.rkt" memory-list))



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
  (list
   (label BTREE_VALUE_P)
          (byte 1)   ;; local
          (bc WRITE_TO_LOCAL_0)
          (bc INT_P)
          (bc TRUE_P_RET) ;; or (bc TRUE_P_BRANCH +3) ;; <- to RET
          ;; (bc PUSH_LOCAL_0) ;; currently only int (later maybe strings)
          ;; (bc STRING_P)
          (bc RET)))

(module+ test #| value? |#
  (define btree-value-p-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_INT_2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc CAR)
       (bc CALL) (word-ref BTREE_VALUE_P)
       (bc BRK))

      (list (org #x8F00))
      BTREE_MAKE_ROOT
      BTREE_VALUE_P)
     ))

  (check-equal? (vm-stack->strings btree-value-p-state)
                (list "stack holds 1 item"
                      "cell-int $0001  (rt)")
                "car of the btree root is the value 2 => result is true (which is int 1)")

  (define btree-value-p2-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_INT_2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc CDR)
       (bc CALL) (word-ref BTREE_VALUE_P)
       (bc BRK))

      (list (org #x8F00))
      BTREE_MAKE_ROOT
      BTREE_VALUE_P)
     ))

  (check-equal? (vm-stack->strings btree-value-p2-state)
                (list "stack holds 1 item"
                      "cell-int $0000  (rt)")
                "cdr of the btree root is NIL => result is false (which is int 0)"))

;; (define (btree-node? node)
;;   (pair? node))
(define BTREE_NODE_P
  (list
   (label BTREE_NODE_P)
          (byte 0) ;; locals
          (bc CONS_PAIR_P)
          (bc RET)))

(module+ test #| node? |#
  (define btree-node-p-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_INT_2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc CAR)
       (bc CALL) (word-ref BTREE_NODE_P)
       (bc BRK))

      (list (org #x8F00))
      BTREE_MAKE_ROOT
      BTREE_NODE_P)
     ))

  (check-equal? (vm-stack->strings btree-node-p-state)
                (list "stack holds 1 item"
                      "cell-int $0000  (rt)")
                "car of the btree root is the value 2 => result is false (which is int 0)")

  (define btree-node-p2-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_INT_2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc CDR)
       (bc CALL) (word-ref BTREE_NODE_P)
       (bc BRK))

      (list (org #x8F00))
      BTREE_MAKE_ROOT
      BTREE_NODE_P)
     ))

  (check-equal? (vm-stack->strings btree-node-p2-state)
                (list "stack holds 1 item"
                      "cell-int $0001  (rt)")
                "cdr of the btree root is NIL => result is true (which is int 1)"))

;; (define (btree-validate node (print-error #f))
;;   (define is-pair-or-value (or (btree-node? node) (btree-value? node)))

;;   (when (and print-error (not is-pair-or-value))
;;     (displayln (format "validation failed: is pair or value: ~a" node)))

;;   (cond [(pair? node)
;;          (define car-is-not-nil (not (empty? (car node))))

;;          (when (and print-error (not car-is-not-nil))
;;            (displayln (format "validation failed: car is not nil: ~a" node)))

;;          (define left-is-valid
;;            (btree-validate (car node) print-error))

;;          (define right-is-valid
;;            (if (empty? (cdr node))
;;                #t
;;                (btree-validate (cdr node) print-error)))

;;          (and is-pair-or-value
;;             car-is-not-nil
;;             left-is-valid
;;             right-is-valid)]
;;         [else
;;          (and is-pair-or-value)]))
(define BTREE_VALIDATE
  (list
   (label BTREE_VALIDATE)
          (byte 2) ;; locals (0 = node, 1 = car/cdr
          (bc WRITE_TO_LOCAL_0)
          (bc CALL) (word-ref BTREE_NODE_P)
          (bc TRUE_P_BRANCH) (byte 7) ;; jump to is-pair
          (bc PUSH_LOCAL_0)
          (bc CALL) (word-ref BTREE_VALUE_P)
          (bc TRUE_P_BRANCH) (byte 22) ;; jump to is-value
          (byte 2)               ;; BRK error, passed parameter is neither value nor node!

   (label IS_PAIR__BTREE_VALIDATE)
          (bc PUSH_LOCAL_0)
          (bc CAR)
          (bc WRITE_TO_LOCAL_1) ;; local 1 now car of node
          (bc NIL?)
          (bc FALSE_P_BRANCH) (byte 1)
          (byte 2)               ;; BRK error, car of pair must not be nil!

          (bc PUSH_LOCAL_1) ;; car of node
          (bc CALL) (word-ref BTREE_VALIDATE) ;; recursive call (not tail recursive)

          (bc PUSH_LOCAL_0)
          (bc CDR)
          (bc WRITE_TO_LOCAL_1) ;; local 1 now cdr of node
          (bc NIL?)
          (bc TRUE_P_BRANCH) (byte 4)

          (bc PUSH_LOCAL_1) ;; cdr of node
          (bc CALL) (word-ref BTREE_VALIDATE) ;; recursive call (not tail recursive)

   (label IS_VALUE__BTREE_VALIDATE)

          (bc RET)))

(module+ test #| validate |#
  (define btree-validate-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_INT_2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc CALL) (word-ref BTREE_VALIDATE)
       (bc BRK))

      (list (org #x8F00))
      BTREE_MAKE_ROOT
      BTREE_NODE_P
      BTREE_VALUE_P
      BTREE_VALIDATE)
    ))

  (check-equal? (vm-stack->strings btree-validate-state)
                (list "stack is empty")
                "validation leaves no value on the stack")
  (check-equal? (memory-list btree-validate-state ZP_VM_PC (add1 ZP_VM_PC))
                (list #x07 #x80)
                "program counter points to expected break"))
