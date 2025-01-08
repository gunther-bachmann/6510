#lang racket/base

#|

implementation of a b-tree with values at leafs from pure bytecode

this implementation will be the testbed for all refcounting gc testing


TODOS:
    DONE btree-make-root
    DONE btree-node?
    DONE btree-value?
    DONE btree-validate
    DONE btree-depth
    DONE btree-path-to-first
    DONE btree-path-to-list
    DONE btree-node-for-path
    IMPLEMENT btree-prev
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
                  EXT
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
(require (only-in "./vm-memory-manager.rkt" ZP_VM_PC))


(require "../6510.rkt")
(require (only-in "../tools/6510-interpreter.rkt" memory-list))



(module+ test #|  |#
  (require "../6510-test-utils.rkt")

  (require (only-in "./vm-interpreter-test-utils.rkt" run-bc-wrapped-in-test- vm-list->strings))
  (require (only-in "../cisc-vm/stack-virtual-machine.rkt" BRK))
  (require (only-in "../tools/6510-interpreter.rkt" cpu-state-clock-cycles))

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
                        (format "pair-ptr $~a05  (rt)" (format-hex-byte PAGE_AVAIL_0))))

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
                        "int $0001  (rt)")))

;; (define (btree-value? node)
;;   (or (string? node) (integer? node)))
(define BTREE_VALUE_P
  (list
   (label BTREE_VALUE_P)
          (byte 0)   ;; local
          (bc INT_P)
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
                      "int $0001  (rt)")
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
                      "int $0000  (rt)")
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
                      "int $0000  (rt)")
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
                      "int $0000  (rt)")
                "cdr of the btree root is NIL => result is false (which is int 0)"))

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
          (bc TRUE_P_BRANCH) (byte 20) ;; jump to is-value
          (byte 2)               ;; BRK error, passed parameter is neither value nor node!

   (label IS_PAIR__BTREE_VALIDATE)
          (bc PUSH_LOCAL_0_CAR)
          (bc WRITE_TO_LOCAL_1) ;; local 1 now car of node
          (bc NIL?)
          (bc FALSE_P_BRANCH) (byte 1)
          (byte 2)               ;; BRK error, car of pair must not be nil!

          (bc PUSH_LOCAL_1) ;; car of node
          (bc CALL) (word-ref BTREE_VALIDATE) ;; recursive call (not tail recursive)

          (bc PUSH_LOCAL_0_CDR)
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
                "program counter points to expected break")

  (define btree-validate2-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_INT_2)
       (bc PUSH_NIL)
       (bc CONS)
       (bc CALL) (word-ref BTREE_VALIDATE)
       (bc BRK))

      (list (org #x8F00))
      BTREE_NODE_P
      BTREE_VALUE_P
      BTREE_VALIDATE)
    ))

  (check-equal? (memory-list btree-validate2-state (add1 ZP_VM_PC) (add1 ZP_VM_PC))
                (list #x8f)
                "program counter on other page => validation failed ")

  (define btree-validate3-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_BYTE) (byte 15)
       (bc CONS)
       (bc CALL) (word-ref BTREE_VALIDATE)
       (bc BRK))

      (list (org #x8F00))
      BTREE_NODE_P
      BTREE_VALUE_P
      BTREE_VALIDATE)
    ))

  (check-equal? (memory-list btree-validate3-state (add1 ZP_VM_PC) (add1 ZP_VM_PC))
                (list #x8f)
                "program counter on other page => validation failed (bytes are not allowed, yet)"))

;; (define (btree-depth node (right-list (list)) (depth 0) (max-depth 0))
;;   (cond [(and (not (pair? node))
;;             (empty? right-list))
;;          (max depth max-depth)]
;;         [(not (pair? node))
;;          (btree-depth (caar right-list) (cdr right-list) (cdar right-list) (max depth max-depth))]
;;         [else
;;          (define l (car node))
;;          (define r (cdr node))
;;          (btree-depth l (cons (cons r (add1 depth)) right-list) (add1 depth) max-depth)]))
(define BTREE_DEPTH
  (list
   (label BTREE_DEPTH)
          (byte 3) ;;# of locals
          (bc WRITE_TO_LOCAL_0) ;; local0 <- node
          (bc CONS_PAIR_P)
          (bc TRUE_P_BRANCH) (byte 15) ;; jump to else
          (bc WRITE_TO_LOCAL_1)        ;; local1 <- right list
          (bc NIL?)
          (bc FALSE_P_BRANCH) (byte 3);; jump to (not (pair? node)) case

    ;;   [(and (not (pair? node))
    ;;             (empty? right-list))
    ;;          (max depth max-depth)]
          (bc EXT)
          (bc MAX_INT)
          (bc RET)

   ;;     [(not (pair? node))  
   ;;      (btree-depth (caar right-list) (cdr right-list) (cdar right-list) (max depth max-depth))]
          (bc EXT)
          (bc MAX_INT)

          (bc PUSH_LOCAL_1_CAR)        ;; car right-list
          (bc WRITE_TO_LOCAL_2)        ;; remember car of right-list for later
          (bc CDR)
          
          (bc PUSH_LOCAL_1_CDR)

          (bc PUSH_LOCAL_2_CAR)        ;; push car of right-list
          (bc TAIL_CALL)

   ;;     [else
   ;;          (define l (car node))
   ;;          (define r (cdr node))
   ;;          (btree-depth l (cons (cons r (add1 depth)) right-list) (add1 depth) max-depth)]))
   ;;                                 ;; stack currently: [right-list :: depth :: max-depth]
          (bc POP_TO_LOCAL_1)         ;; local1 = right-list
          (bc EXT)
          (bc INC_INT)
          (bc WRITE_TO_LOCAL_2)       ;; local2 = depth +1
          (bc PUSH_LOCAL_1)           ;; [right-list :: depth+1 :: max-depth]
          (bc PUSH_LOCAL_2)           ;; [depth+1 :: right-list :: depth+1 :: max-depth]
          (bc PUSH_LOCAL_0_CDR)       ;; [right :: depth+1 :: right-list :: depth+1 :: max-depth]
          (bc CONS)                   ;; [(right . depth+1) :: right-list :: depth+1 :: max-depth]
          (bc CONS)                   ;; [((right . depth+1) . right-list) :: depth+1 :: max-depth]
          (bc PUSH_LOCAL_0_CAR)       ;; [left :: ((right . depth+1) . right-list) :: depth+1 :: max-depth]
          (bc TAIL_CALL)))


(module+ test #| btree depth |#
  (define btree-depth-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_INT_0)
       (bc PUSH_INT_0)
       (bc PUSH_NIL)
       (bc PUSH_INT_2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc CALL) (word-ref BTREE_DEPTH)
       (bc BRK))
      BTREE_MAKE_ROOT
      BTREE_DEPTH)
     ))

  (check-equal? (vm-regt->string btree-depth-1-state)
                "int $0001")

  (define btree-depth-2-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_INT_0)
       (bc PUSH_INT_0)
       (bc PUSH_NIL)
       (bc PUSH_INT_2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc PUSH_INT_1)
       (bc CONS)
       (bc CALL) (word-ref BTREE_DEPTH)
       (bc BRK))
      BTREE_MAKE_ROOT
      BTREE_DEPTH)
    ))

  (check-equal? (vm-regt->string btree-depth-2-state)
                "int $0002")

  (define btree-depth-3-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_INT_0)
       (bc PUSH_INT_0)
       (bc PUSH_NIL)
       (bc PUSH_INT_2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc PUSH_INT_1)
       (bc CONS)
       (bc PUSH_INT_0)
       (bc CONS)
       (bc CALL) (word-ref BTREE_DEPTH)
       (bc BRK))
      BTREE_MAKE_ROOT
      BTREE_DEPTH)
    ))

  (check-equal? (vm-regt->string btree-depth-3-state)
                "int $0003")

  (define btree-depth-5-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_INT_0)
       (bc PUSH_INT_0)
       (bc PUSH_NIL)
       (bc PUSH_INT_2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)     ;;-> o
       (bc PUSH_INT_1)                          ;;  / \
       (bc SWAP)                                ;; 2  nil
       (bc CONS)                                ;;            ->o
       (bc CALL) (word-ref BTREE_DEPTH)         ;;            /   \    
       (bc BRK))                                ;;           o     1                 
      BTREE_MAKE_ROOT                           ;;          / \                      
      BTREE_DEPTH)                              ;;          2  nil                   
    ))                                          ;;                    
                                                ;;                    
  (check-equal? (vm-regt->string btree-depth-5-state)
                "int $0002")

  (define btree-depth-4-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_INT_0)
       (bc PUSH_INT_0)
       (bc PUSH_NIL)
       (bc PUSH_INT_2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)     ;;-> o
       (bc PUSH_INT_1)                          ;;  / \
       (bc SWAP)                                ;; 2  nil
       (bc CONS)                                ;;             ->o
       (bc PUSH_INT_0)                          ;;             /   \
       (bc SWAP)                                ;;            o     1
       (bc CONS)                                ;;           / \                 o
       (bc CALL) (word-ref BTREE_DEPTH)         ;;          2  nil             /   \
       (bc BRK))                                ;;                            o     0 
      BTREE_MAKE_ROOT                           ;;                          /   \
      BTREE_DEPTH)                              ;;                         o     1
    ))                                          ;;                        / \
                                                ;;                       2  nil

  (check-equal? (vm-regt->string btree-depth-4-state)
                "int $0003")


  (define btree-depth-6-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_INT_0)
       (bc PUSH_INT_0)
       (bc PUSH_NIL)
       (bc PUSH_INT_2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)     ;;-> o
       (bc PUSH_INT_1)                          ;;  / \
       (bc SWAP)                                ;; 2  nil
       (bc CONS)                                ;;             ->o
       (bc PUSH_INT_0)                          ;;             /   \
       ;; (bc SWAP)                             ;;            o     1
       (bc CONS)                                ;;           / \        -> o
       (bc BNOP)                                ;;          2  nil       /   \
       (bc CALL) (word-ref BTREE_DEPTH)         ;;                      0     o    
       (bc BRK))                                ;;                          /   \   
      BTREE_MAKE_ROOT                           ;;                         o     1 
      BTREE_DEPTH)                              ;;                        / \      
    ))                                          ;;                       2  nil    

   (check-equal? (vm-regt->string btree-depth-6-state)
                   "int $0003")
   (check-equal? (cpu-state-clock-cycles btree-depth-6-state)
                 8430))

;; (define (btree-path-to-first node (path (list)))
;;   (cond [(btree-value? node) path]
;;         [else (btree-path-to-first (car node) (cons (cons -1 node) path))]))
(define BTREE_PATH_TO_FIRST
  (list
   (label BTREE_PATH_TO_FIRST)
          (byte 1)
          (bc WRITE_TO_LOCAL_0)                 ;; local0 = node
          (bc CALL) (word-ref BTREE_VALUE_P)
          (bc TRUE_P_RET)  ;; [(btree-value? node) path]

   ;; [else (btree-path-to-first (car node) (cons (cons -1 node) path))]))
          (bc PUSH_LOCAL_0)                     ;; node :: path
          (bc PUSH_INT_0)                       ;; 0 :: node :: path
          (bc CONS)                             ;; (0 . node) :: path
          (bc CONS)                             ;; ((0 . node) . path)
          (bc PUSH_LOCAL_0_CAR)                 ;; (car node) :: ((0 . node) . path)
          (bc TAIL_CALL)))

(module+ test #| path to first |#
  (define path-to-first-0-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_INT_2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc CALL) (word-ref BTREE_PATH_TO_FIRST)
       (bc BRK))
      BTREE_MAKE_ROOT
      BTREE_VALUE_P
      BTREE_PATH_TO_FIRST)
     ))

  (check-equal? (regexp-replace*
                 "pair-ptr (\\$[0-9A-Fa-f]*)?"
                 (vm-regt->string path-to-first-0-state #t) "")
                "((int $0000 . (int $0002 . NIL)) . NIL)"
                "result is a path to the node with value 2: ((0 . (2 . NIL)))")

  (define path-to-first-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_INT_2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)     ;;
       (bc PUSH_INT_1)                          ;;         o        
       (bc SWAP)                                ;;       /   \      
       (bc CONS)                                ;;    ->0     o      
       (bc PUSH_INT_0)                          ;;          /   \   
       (bc CONS)                                ;;         o     1  
       (bc CALL) (word-ref BTREE_PATH_TO_FIRST) ;;        / \       
       (bc BRK))                                ;;       2  nil     
      BTREE_MAKE_ROOT                           ;;
      BTREE_VALUE_P                             ;;
      BTREE_PATH_TO_FIRST)                      ;;
     ))                                         ;;

  (check-equal? (regexp-replace*
                 "pair-ptr (\\$[0-9A-Fa-f]*)?"
                 (vm-regt->string path-to-first-1-state #t) "")
                "((int $0000 . (int $0000 . ((int $0002 . NIL) . int $0001))) . NIL)"
                "result is a path to the node with value 1: ((0 . (1 . ((2 . nil) . 1))"))


;; (define (btree-path-to-last  node (path (list)))
;;   (cond [(btree-value? node) path]
;;         [(empty? (cdr node)) (btree-path-to-last (car node) (cons (cons -1 node) path))]
;;         [else (btree-path-to-last (cdr node) (cons (cons 1 node) path))]))
(define BTREE_PATH_TO_LAST
  (list
   (label BTREE_PATH_TO_LAST)
          (byte 1)
          (bc WRITE_TO_LOCAL_0)             ;; local0 = node
          (bc CALL) (word-ref BTREE_VALUE_P) ;; 
          (bc TRUE_P_RET)                   ;;     [(btree-value? node) path]

    ;;     [(empty? (cdr node)) (btree-path-to-last (car node) (cons (cons -1 node) path))]
          (bc PUSH_LOCAL_0_CDR)
          (bc NIL?)
          (bc FALSE_P_BRANCH) (byte 6)

          (bc PUSH_LOCAL_0)
          (bc PUSH_INT_0)
          (bc CONS)
          (bc CONS)
          (bc PUSH_LOCAL_0_CAR)
          (bc TAIL_CALL)

    ;;    [else (btree-path-to-last (cdr node) (cons (cons 1 node) path))]))
          (bc PUSH_LOCAL_0)
          (bc PUSH_INT_1)
          (bc CONS)
          (bc CONS)
          (bc PUSH_LOCAL_0_CDR)
          (bc TAIL_CALL)))

(module+ test #| path to last |#
  (define path-to-last-0-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)

       (bc PUSH_INT_2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)

       (bc CALL) (word-ref BTREE_PATH_TO_LAST)
       (bc BRK))
      BTREE_PATH_TO_LAST
      BTREE_VALUE_P
      BTREE_MAKE_ROOT)))

  (check-equal? (regexp-replace*
                 "pair-ptr (\\$[0-9A-Fa-f]*)?"
                 (vm-regt->string path-to-last-0-state #t)
                 "")
                "((int $0000 . (int $0002 . NIL)) . NIL)")

  (define path-to-last-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_INT_2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)     ;;
       (bc PUSH_NIL)                            ;;         o
       (bc SWAP)                                ;;       /   \
       (bc CONS)                                ;;    ->0     o
       (bc PUSH_INT_0)                          ;;          /   \
       (bc CONS)                                ;;         o     nil
       (bc CALL) (word-ref BTREE_PATH_TO_LAST)  ;;        / \       
       (bc BRK))                                ;;    -> 2  nil
      BTREE_MAKE_ROOT                           ;;
      BTREE_VALUE_P                             ;;
      BTREE_PATH_TO_LAST)                       ;;
     ))

  (check-equal? (regexp-replace*
                   "pair-ptr (\\$[0-9A-Fa-f]*)?"
                   (vm-regt->string path-to-last-1-state #t)
                   "")
                (string-append
                 "((int $0000 . (int $0002 . NIL))"
                 " . ((int $0000 . ((int $0002 . NIL) . NIL))"
                 " . ((int $0001 . (int $0000 . ((int $0002 . NIL) . NIL)))"
                 " . NIL)))"))

  (define path-to-last-2-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_INT_2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)     ;;
       (bc PUSH_INT_1)                          ;;         o
       (bc SWAP)                                ;;       /   \
       (bc CONS)                                ;;      0     o
       (bc PUSH_INT_0)                          ;;          /   \
       (bc CONS)                                ;;         o  -> 1
       (bc CALL) (word-ref BTREE_PATH_TO_LAST)  ;;        / \       
       (bc BRK))                                ;;       2  nil
      BTREE_MAKE_ROOT                           ;;
      BTREE_VALUE_P                             ;;
      BTREE_PATH_TO_LAST)                       ;;
     ))

  (check-equal? (regexp-replace*
                   "pair-ptr (\\$[0-9A-Fa-f]*)?"
                   (vm-regt->string path-to-last-2-state #t)
                   "")
                (string-append
                 "((int $0001 . ((int $0002 . NIL) . int $0001))"
                 " . ((int $0001 . (int $0000 . ((int $0002 . NIL) . int $0001)))"
                 " . NIL))"))
  )

;; (define (btree-node-for-path path)
;;   (cond [(empty? path) '()]
;;         [(= -1 (caar path)) (car (cdar path))]
;;         [(= 1 (caar path)) (cdr (cdar path))]
;;         [else (raise-user-error (format "btree path may only contain 1 | -1:" path))]))
(define BTREE_NODE_FOR_PATH
  (list
   (label BTREE_NODE_FOR_PATH)
          (byte 1) ;; locals
          (bc WRITE_TO_LOCAL_0)
          (bc NIL?)
          (bc FALSE_P_BRANCH) (byte 2)

    ;; [(empty? path) '()]
          (bc PUSH_NIL)
          (bc RET)

    ;; [(= 0 (caar path)) (car (cdar path))]
          (bc PUSH_LOCAL_0_CAR)
          (bc WRITE_TO_LOCAL_0)

          (bc CAR)
          (bc INT_0_P)
          (bc FALSE_P_BRANCH) (byte 3) 

          (bc PUSH_LOCAL_0_CDR)
          (bc CAR)
          (bc RET)

    ;; [else (cdr (cdar path))]  ;; no error handling
          (bc PUSH_LOCAL_0_CDR)
          (bc CDR)
          (bc RET)))

(module+ test #| node for path |#
  (define node-for-path-0-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_INT_2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc PUSH_INT_0)
       (bc CONS)
       (bc CONS)
       (bc CALL) (word-ref BTREE_NODE_FOR_PATH)
       (bc BRK))
      BTREE_NODE_FOR_PATH
      BTREE_MAKE_ROOT
      BTREE_VALUE_P)))

  (check-equal? (vm-regt->string node-for-path-0-state)
                "int $0002")

  (define node-for-path-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_INT_0)  ;; right
       (bc PUSH_INT_2)  ;; left
       (bc CONS)        ;; node
       (bc PUSH_INT_1)  ;; path selector
       (bc CONS)
       (bc CONS)
       (bc CALL) (word-ref BTREE_NODE_FOR_PATH)
       (bc BRK))
      BTREE_NODE_FOR_PATH
      BTREE_MAKE_ROOT
      BTREE_VALUE_P)))

  (check-equal? (vm-regt->string node-for-path-1-state)
                "int $0000"))

;; (define (btree-prev path)
;;   (cond [(empty? path)
;;          '()]

;;         [(= -1 (caar path))
;;          (define top-most-relevant (dropf (cdr path)
;;                                           (lambda (pe) (= -1 (car pe))
;;                                                   ;; left must not be empyt => no further check
;;                                             )))
;;          (if (empty? top-most-relevant)
;;              '()
;;              (append (btree-path-to-last (cadar top-most-relevant))
;;                      (cons (cons -1 (cdar top-most-relevant))
;;                            (cdr top-most-relevant))))]

;;         [(= 1 (caar path))
;;          ;; left must not be empyt => no additional check
;;          (define top-most-relevant  path)
;;          (append (btree-path-to-last (cadar top-most-relevant))
;;                  (cons (cons -1 (cdar top-most-relevant))
;;                        (cdr top-most-relevant)))]

;;         [else (raise-user-error "unknown case")]))
(define BTREE_PREV
  (list
   (label BTREE_PREV)
          (byte 3)

          (bc WRITE_TO_LOCAL_0)
          (bc NIL?)
          (bc FALSE_P_BRANCH) (byte 2)

    ;; [(empty? path) '()]
          (bc PUSH_NIL)
          (bc RET)


    ;; [(= 0 (caar path))
          (bc PUSH_LOCAL_0_CAR)
          (bc CAR)
          (bc INT_0_P)
          (bc FALSE_P_BRANCH) (byte 34) 

          (bc PUSH_LOCAL_0_CDR)
          (bc WRITE_TO_LOCAL_2)         ;; top-most-relevant = local2 = (cdr path) <- looping cdr
          (bc NIL?)                     ;; 
          (bc TRUE_P_BRANCH) (byte $f6) ;; -10 return nil
          (bc PUSH_LOCAL_2_CAR)
          (bc CAR)
          (bc INT_0_P)
          (bc FALSE_P_BRANCH) (byte 3) 
          (bc PUSH_LOCAL_2_CDR)
          (bc GOTO) (byte $f6)          ;; cdr and loop -->

          (bc PUSH_LOCAL_2)             ;; top-most-relevant
          (bc NIL?)
          (bc TRUE_P_BRANCH) (byte $ea) ;; -22 return nil

          (bc PUSH_LOCAL_2)             ;; top-most-relevant

          (bc CDR)                      ;; entry for construct path <-- 
          (bc PUSH_LOCAL_2_CAR)         ;; top-most-relevant
          (bc CDR)
          (bc WRITE_TO_LOCAL_1)         ;; local1 = (cdar top-most-relevant)
          (bc PUSH_INT_0)
          (bc CONS)
          (bc CONS)

          (bc PUSH_NIL)
          (bc PUSH_LOCAL_1_CAR)         ;; (cdar top-most-relevant)
          (bc CALL) (word-ref BTREE_PATH_TO_LAST)

          (bc CALL) (word-ref APPEND)
          (bc RET)

    ;; [else
          (bc PUSH_LOCAL_0)
          (bc WRITE_TO_LOCAL_2)         ;; write other top-most-relevant and jump to first CDR
          (bc GOTO) (byte $ee)          ;; (-18) construct path -->
          ))

(module+ test #| prev |#
  (define prev-0-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)                            ;; for call to path_to_first
       (bc PUSH_INT_2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)     ;; got the tree, now construct a path
       (bc CALL) (word-ref BTREE_PATH_TO_FIRST)
       (bc BNOP)
       (bc CALL) (word-ref BTREE_PREV)
       (bc BRK))
      BTREE_PATH_TO_FIRST
      BTREE_MAKE_ROOT
      BTREE_PREV
      BTREE_PATH_TO_LAST
      BTREE_VALUE_P
      BTREE_NODE_P
      APPEND
      REVERSE)
     ))

  (check-equal? (vm-regt->string prev-0-state #t)
                "pair-ptr NIL")

  (define prev-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)                            ;; for call to path_to_first
       (bc PUSH_INT_2)                          ;;    o
       (bc PUSH_INT) (word $0003)               ;;   / \
       (bc CONS)                                ;;  2   3
       (bc CALL) (word-ref BTREE_PATH_TO_FIRST) ;; ((0 . (2 . 3)) . NIL)
       (bc BNOP)
       (bc CALL) (word-ref BTREE_PREV)
       (bc BRK))
      BTREE_PATH_TO_FIRST
      BTREE_MAKE_ROOT
      BTREE_PREV
      BTREE_PATH_TO_LAST
      BTREE_VALUE_P
      BTREE_NODE_P
      APPEND
      REVERSE)
     ))

  (check-equal? (vm-regt->string prev-1-state #t)
                "pair-ptr NIL")

  (define prev-2-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)                            ;; for call to path_to_first
       (bc PUSH_INT)  (word $0003)              ;;    o
       (bc PUSH_INT_2)                          ;;   / \
       (bc CONS)                                ;;  2   3
       (bc CALL) (word-ref BTREE_PATH_TO_LAST)  ;; ((1 . (2 . 3)) . NIL)
       (bc BNOP)
       (bc CALL) (word-ref BTREE_PREV)
       (bc BRK))
      BTREE_PATH_TO_FIRST
      BTREE_MAKE_ROOT
      BTREE_PREV
      BTREE_PATH_TO_LAST
      BTREE_VALUE_P
      BTREE_NODE_P
      APPEND
      REVERSE)
     ))

  (check-equal? (regexp-replace*
                 "(pair-ptr (\\$[0-9A-Fa-f]*)?|int \\$000)"
                 (vm-regt->string prev-2-state #t)
                 "")
                "((0 . (2 . 3)) . NIL)")

  (define prev-3-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)                            ;; for call to path_to_first
       (bc PUSH_INT)  (word $0003)              ;;    o
       (bc PUSH_INT_2)                          ;;   / \
       (bc CONS)                                ;;  1   o  
       (bc PUSH_INT_1)                          ;;     / \ 
       (bc CONS)                                ;;    2   3
       (bc CALL) (word-ref BTREE_PATH_TO_LAST)  ;; ((1 . (2 . 3)) . ((1 . (1 . (2 . 3))) . NIL))
       (bc BNOP)
       (bc CALL) (word-ref BTREE_PREV)
       (bc BRK))
      BTREE_PATH_TO_FIRST
      BTREE_MAKE_ROOT
      BTREE_PREV
      BTREE_PATH_TO_LAST
      BTREE_VALUE_P
      BTREE_NODE_P
      APPEND
      REVERSE)
     ))

  (check-equal? (regexp-replace*
                    "(pair-ptr (\\$[0-9A-Fa-f]*)?|int \\$000)"
                    (vm-regt->string prev-3-state #t)
                    "")
                   "((0 . (2 . 3)) . ((1 . (1 . (2 . 3))) . NIL))")

  (define prev-4-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)                            ;; for call to path_to_first
       (bc PUSH_INT)  (word $0003)              ;;    o
       (bc PUSH_INT_2)                          ;;   / \
       (bc CONS)                                ;;  1   o
       (bc PUSH_INT_1)                          ;;     / \
       (bc CONS)                                ;;    2   3
       (bc CALL) (word-ref BTREE_PATH_TO_LAST)  ;; ((1 . (2 . 3)) . ((1 . (1 . (2 . 3))) . NIL))
       (bc CALL) (word-ref BTREE_PREV)          ;; ((0 . (2 . 3)) . ((1 . (1 . (2 . 3))) . NIL))
       (bc BNOP)
       (bc CALL) (word-ref BTREE_PREV)          ;; ((0 . (1 . (2 . 3))) . NIL)
       (bc BRK))
      BTREE_PATH_TO_FIRST
      BTREE_MAKE_ROOT
      BTREE_PREV
      BTREE_PATH_TO_LAST
      BTREE_VALUE_P
      BTREE_NODE_P
      APPEND
      REVERSE)
     ))

  (check-equal? (regexp-replace*
                    "(pair-ptr (\\$[0-9A-Fa-f]*)?|int \\$000)"
                    (vm-regt->string prev-4-state #t)
                    "")
                   "((0 . (1 . (2 . 3))) . NIL)")

  (define prev-5-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)                            ;; for call to path_to_first
       (bc PUSH_INT) (word $0004)               ;;     o
       (bc PUSH_INT) (word $0003)               ;;    / \
       (bc PUSH_INT_2)                          ;;   1   o
       (bc CONS)                                ;;      / \
       (bc CONS)                                ;;     o   4
       (bc PUSH_INT_1)                          ;;    / \
       (bc CONS)                                ;;   2   3 
       (bc CALL) (word-ref BTREE_PATH_TO_LAST)  ;; ((1 . (2 . 3)) . ((1 . (1 . ((2 . 3) . 4))) . NIL))
       (bc BNOP)
       (bc CALL) (word-ref BTREE_PREV)          ;; ((1 . (2 . 3)) . ((0 . ((2 . 3) . 4)) . ((1 . (1 . ((2 . 3) . 4))) . NIL)))      
       (bc BRK))
      BTREE_PATH_TO_FIRST
      BTREE_MAKE_ROOT
      BTREE_PREV
      BTREE_PATH_TO_LAST
      BTREE_VALUE_P
      BTREE_NODE_P
      APPEND
      REVERSE)
     ))

  (check-equal? (regexp-replace*
                    "(pair-ptr (\\$[0-9A-Fa-f]*)?|int \\$000)"
                    (vm-regt->string prev-5-state #t)
                    "")
                "((1 . (2 . 3)) . ((0 . ((2 . 3) . 4)) . ((1 . (1 . ((2 . 3) . 4))) . NIL)))")

  (define prev-6-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)                            ;; for call to path_to_first
       (bc PUSH_INT) (word $0004)               ;;     o
       (bc PUSH_INT) (word $0003)               ;;    / \
       (bc PUSH_INT_2)                          ;;   1   o
       (bc CONS)                                ;;      / \
       (bc CONS)                                ;;     o   4
       (bc PUSH_INT_1)                          ;;    / \
       (bc CONS)                                ;;   2   3
       (bc CALL) (word-ref BTREE_PATH_TO_LAST)  ;; ((1 . (2 . 3)) . ((1 . (1 . ((2 . 3) . 4))) . NIL))
       (bc CALL) (word-ref BTREE_PREV)          ;; ((1 . (2 . 3)) . ((0 . ((2 . 3) . 4)) . ((1 . (1 . ((2 . 3) . 4))) . NIL)))
       (bc BNOP)
       (bc CALL) (word-ref BTREE_PREV)          ;; ((0 . (2 . 3)) . ((0 . ((2 . 3) . 4)) . ((1 . (1 . ((2 . 3) . 4))) . NIL)))
       (bc BRK))
      BTREE_PATH_TO_FIRST
      BTREE_MAKE_ROOT
      BTREE_PREV
      BTREE_PATH_TO_LAST
      BTREE_VALUE_P
      BTREE_NODE_P
      APPEND
      REVERSE)
     ))

  (check-equal? (regexp-replace*
                    "(pair-ptr (\\$[0-9A-Fa-f]*)?|int \\$000)"
                    (vm-regt->string prev-6-state #t)
                    "")
                "((0 . (2 . 3)) . ((0 . ((2 . 3) . 4)) . ((1 . (1 . ((2 . 3) . 4))) . NIL)))")

  (define prev-7-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)                            ;; for call to path_to_first
       (bc PUSH_INT) (word $0004)               ;;     o
       (bc PUSH_INT) (word $0003)               ;;    / \
       (bc PUSH_INT_2)                          ;;   1   o
       (bc CONS)                                ;;      / \
       (bc CONS)                                ;;     o   4
       (bc PUSH_INT_1)                          ;;    / \
       (bc CONS)                                ;;   2   3
       (bc CALL) (word-ref BTREE_PATH_TO_LAST)  ;; ((1 . ((2 . 3) . 4)) . ((1 . (1 . ((2 . 3) . 4))) . NIL))
       (bc CALL) (word-ref BTREE_PREV)          ;; ((1 . (2 . 3)) . ((0 . ((2 . 3) . 4)) . ((1 . (1 . ((2 . 3) . 4))) . NIL)))
       (bc CALL) (word-ref BTREE_PREV)          ;; ((0 . (2 . 3)) . ((0 . ((2 . 3) . 4)) . ((1 . (1 . ((2 . 3) . 4))) . NIL)))
       (bc BNOP)
       (bc CALL) (word-ref BTREE_PREV)          ;; ((0 . (1 . ((2 . 3) . 4))) . NIL)))
       (bc BRK))
      BTREE_PATH_TO_FIRST
      BTREE_MAKE_ROOT
      BTREE_PREV
      BTREE_PATH_TO_LAST
      BTREE_VALUE_P
      BTREE_NODE_P
      APPEND
      REVERSE)
     ))

  (check-equal? (regexp-replace*
                    "(pair-ptr (\\$[0-9A-Fa-f]*)?|int \\$000)"
                    (vm-regt->string prev-7-state #t)
                    "")
                "((0 . (1 . ((2 . 3) . 4))) . NIL)")
  )

;; optimization idea: NIL?_RET instead of NIL?, TRUE_P_RET
(define REVERSE
  (list
   (label REVERSE)
   (byte 1)
   (bc WRITE_TO_LOCAL_0)     ;; local0 = list
   (bc NIL?)
   (bc TRUE_P_RET)          ;; return second parameter
   (bc PUSH_LOCAL_0_CAR)
   (bc CONS)
   (bc PUSH_LOCAL_0_CDR)
   ;; (bc GOTO) (byte $fa)
   (bc TAIL_CALL)))

(module+ test #| reverse |#
  (define reverse-0-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_INT_0)
       (bc CONS)
       (bc PUSH_INT_1)
       (bc CONS)
       (bc PUSH_INT_2)
       (bc CONS)
       (bc PUSH_INT_m1)
       (bc CONS)
       (bc PUSH_NIL)
       (bc SWAP)
       (bc BNOP)
       (bc CALL) (word-ref REVERSE)
       (bc BRK))
      REVERSE)
     ))

  (check-equal? (regexp-replace*
                 "pair-ptr (\\$[0-9A-Fa-f]*)?"
                 (vm-regt->string reverse-0-state #t)
                 "")
                "(int $0000 . (int $0001 . (int $0002 . (int $1fff . NIL))))")
  (check-equal? (cpu-state-clock-cycles reverse-0-state)
                3423))

(define APPEND
  (list
   (label APPEND)
          (byte 0)
          (bc PUSH_NIL)
          (bc SWAP)
          (bc CALL) (word-ref REVERSE)
          
          (bc WRITE_TO_LOCAL_0) ;; local0 = reversed list     <- loop
          (bc NIL?)
          (bc TRUE_P_RET)      ;; return second
          (bc PUSH_LOCAL_0_CAR)
          (bc CONS)
          (bc PUSH_LOCAL_0_CDR)
          (bc GOTO) (byte $fa) ;; (-6) loop ->
   ))

(module+ test #| append |#
  (define append-0-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_INT_0)
       (bc CONS)
       (bc PUSH_INT_1)
       (bc CONS)
       (bc PUSH_INT_2)
       (bc CONS)

       (bc PUSH_NIL)
       (bc PUSH_INT) (word $0003)
       (bc CONS)
       (bc PUSH_INT) (word $0004)
       (bc CONS)
       (bc PUSH_INT) (word $0005)
       (bc CONS)

       (bc BNOP)
       (bc CALL) (word-ref APPEND)
       (bc BRK))
      APPEND
      REVERSE)))

  (check-equal? (regexp-replace*
                 "pair-ptr (\\$[0-9A-Fa-f]*)?"
                 (vm-regt->string append-0-state #t)
                 "")
                "(int $0005 . (int $0004 . (int $0003 . (int $0002 . (int $0001 . (int $0000 . NIL))))))")
  (check-equal? (cpu-state-clock-cycles append-0-state)
                5548))
