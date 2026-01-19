#lang racket/base

(provide vm-btree
    REVERSE                       ;; reverse a list:  list :: result=nil -> list
    APPEND                        ;; append to lists:  head-list :: tail-list -> list

    BTREE_MAKE_ROOT               ;; create a root for a btree: value -> node

    BTREE_VALUE_P                 ;; is this node a value:  node -> bool
    BTREE_NODE_P                  ;; is this node a node w/ children:  node -> bool
    BTREE_DEPTH                   ;; get the max depth of this tree:  node :: right-list=nil :: depth=0 :: max-depth=0 -> int

    BTREE_PATH_TO_LAST            ;; generate path to the last element in the tree:  node :: result-path=nil -> path
    BTREE_PATH_TO_FIRST           ;; generate path to the first element in the tree:  node :: result-path=nil -> path
    BTREE_NEXT                    ;; find the next node following the given path:  path -> path
    BTREE_PREV                    ;; find the previous node before the given path:  path -> path

    BTREE_NODE_FOR_PATH           ;; extract the node for the given path:  path -> node

    BTREE_REC_REBUILD_PATH_WITH   ;; recursively construct a path:  (list path) :: repl-node :: result=nil -> (list path)
    ;; BTREE_VALIDATE

    BTREE_ADD_VALUE_BEFORE        ;; add a value to the tree before the given:  value :: path -> path
    BTREE_ADD_VALUE_AFTER         ;; add a value to the tree after the given:  value :: path -> path

    BTREE_TO_LIST                 ;; translate tree into an ordered list of values:  node :: btree-prefix=nil :: result=nil -> (list node)
    BTREE_FROM_LIST               ;; construct a (balanced) tree from a list of values:  (list node) :: result=nil -> node

    BTREE_REMOVE_VALUE_AT         ;; remove the given value from the tree:  path :: result=nil :: old-prev=nil

    BTREE_ROOT_FOR_PATH           ;; get the root of this path in the btree:  path -> node

    BTREE_REVERSE)                ;; reverse the given btree (left/right)

#|

  implementation of a persistent b-tree with values at leafs in pure bytecode

  this implementation will be the testbed for all refcounting gc testing
  exported scheme list: vm-btree <- contains the complete bytecode implementation

  data
  ----
    node = val|node . val|node|NIL    [car of node is never nil]
    path = (list pa-element)          [list of path elements, first list element points to leaf, ... last to root]
    pa-element = 0|1 . node           [path points to.. 0 = car element of node, 1 = cdr element of node]

  public methods
  --------------
    REVERSE
      list :: result=nil -> list
      [return the given list reversed]
    APPEND
      head-list :: tail-list -> list
      [return the concat of the two lists]

    BTREE_MAKE_ROOT 
      value -> node
      [create a tree node (root) form the given value]

    BTREE_VALUE_P 
      node -> bool
      [is this node actually a value?]
    BTREE_NODE_P 
      node -> bool
      [is this node a node with children]
    BTREE_DEPTH
      node :: right-list=nil :: depth=0 :: max-depth=0 -> int
      [give the max depth of this tree]

    BTREE_PATH_TO_LAST
      node :: result-path=nil -> path
      [return the path to the last node of the given tree]
    BTREE_PATH_TO_FIRST
      node :: result-path=nil -> path
      [return the path to the first node of the given tree]
    BTREE_NEXT
      path -> path
      [give the path to the next element (or nil if it was last)]
    BTREE_PREV
      path -> path
      [give the path to the prev element (or nil if it was first)]

    BTREE_NODE_FOR_PATH
      path -> node
      [give the node referenced by the given path]

    BTREE_VALIDATE
      node -> void  :: run into break if node cannot be
      validated

    BTREE_ADD_VALUE_BEFORE
      value :: path -> path
      [add a value into the tree before the given path, returning the path to the inserted item]
    BTREE_ADD_VALUE_AFTER
      value :: path -> path
      [add a value into the tree after the given path, returning the path to the inserted item]

    BTREE_TO_LIST
      node :: btree-prefix=nil :: result=nil -> (list node)
      [create a list of values from the given tree]
    BTREE_FROM_LIST
      (list node) :: result=nil -> node
      [create a balanced tree from the list of values/nodes]

    BTREE_REMOVE_VALUE_AT
      path :: result=nil :: old-prev=nil -> path
      [remove the node referenced by path from the tree, returning the path to the previous value (if present else the next)]

    BTREE_ROOT_FOR_PATH
      path -> node
      [return the root node of the given path]


  private methods
  ---------------
    BTREE_REC_REBUILD_PATH_WITH
      (list path) :: repl-node :: result=nil -> (list path)
      [method to replace the given node up the tree in the path]

 |#

(require "../../6510.rkt"
         "../vm-bc-ast.rkt"
         (only-in "../vm-bc-opcode-definitions.rkt"
                  bc)
         (only-in "../vm-bc-resolver.rkt"
                  bc-resolve
                  bc-bytes)
         (only-in "../vm-interpreter-loop.rkt" ZP_VM_PC))

(module+ test #|  |#
  (require (only-in "../test-utils.rkt"
                    regression-test)
           "./test-utils.rkt"))


;; (define (btree-make-root value)
;;   (cons value null))
(define BTREE_MAKE_ROOT ;; value -> node
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
       (bc PUSH_I1)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc BREAK))
      BTREE_MAKE_ROOT)
     ))

  (regression-test
   btree-make-root-state
   "make btree root"
   (check-equal? (vm-stack->strings btree-make-root-state)
                 (list "stack holds 2 items"
                       (format "ptr[1] $~a02  (rt)" (format-hex-byte PAGE_AVAIL_0))
                       "ptr NIL")))

  (define btree-make-root-2-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I1)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc CAR)
       (bc BREAK))
      BTREE_MAKE_ROOT)))

  (regression-test
   btree-make-root-2-state
   "make btree root -> car"
   (check-equal? (vm-stack->strings btree-make-root-2-state)
                 (list "stack holds 2 items"
                       "int $0001  (rt)"
                       "ptr NIL"))))

;; (define (btree-value? node)
;;   (or (string? node) (integer? node)))
(define BTREE_VALUE_P ;; node -> bool
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
       (bc PUSH_I2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc CAR)
       (bc CALL) (word-ref BTREE_VALUE_P)
       (bc BREAK))

      (list (org #x1800))
      BTREE_MAKE_ROOT
      BTREE_VALUE_P)
     ))

  (regression-test
   btree-value-p-state
   "make root -> car -> btree value?"
   (check-equal? (vm-stack->strings btree-value-p-state)
                 (list "stack holds 2 items"
                       "int $0001  (rt)"
                       "ptr NIL")
                 "car of the btree root is the value 2 => result is true (which is int 1)"))

  (define btree-value-p2-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc CDR)
       (bc CALL) (word-ref BTREE_VALUE_P)
       (bc BREAK))

      (list (org #x1800))
      BTREE_MAKE_ROOT
      BTREE_VALUE_P)
     ))

  (regression-test
   btree-value-p2-state
   "make root -> cdr -> btree value?"
   (check-equal? (vm-stack->strings btree-value-p2-state)
                 (list "stack holds 2 items"
                       "int $0000  (rt)"
                       "ptr NIL")
                 "cdr of the btree root is NIL => result is false (which is int 0)")))

;; (define (btree-node? node)
;;   (pair? node))
(define BTREE_NODE_P ;; node -> bool
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
       (bc PUSH_I2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc CAR)
       (bc CALL) (word-ref BTREE_NODE_P)
       (bc BREAK))

      (list (org #x1800))
      BTREE_MAKE_ROOT
      BTREE_NODE_P)
     ))

  (regression-test
   btree-node-p-state
   "make root -> car -> btree node?"
   (check-equal? (vm-stack->strings btree-node-p-state)
                 (list "stack holds 2 items"
                       "int $0000  (rt)"
                       "ptr NIL")
                 "car of the btree root is the value 2 => result is false (which is int 0)"))

  (define btree-node-p2-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc CDR)
       (bc CALL) (word-ref BTREE_NODE_P)
       (bc BREAK))

      (list (org #x1800))
      BTREE_MAKE_ROOT
      BTREE_NODE_P)
     ))

  (regression-test
   btree-node-p2-state
   "make root -> cdr -> btree node?"
   (check-equal? (vm-stack->strings btree-node-p2-state)
                 (list "stack holds 2 items"
                       "int $0000  (rt)"
                       "ptr NIL")
                 "cdr of the btree root is NIL => result is false (which is int 0)")))

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
(define BTREE_VALIDATE ;; node -> void
  (bc-resolve
   (flatten
     (list
      (label BTREE_VALIDATE)
             (byte 2) ;; locals (0 = node, 1 = car/cdr)
             (bc WRITE_TO_L0)
             (bc CALL) (word-ref BTREE_NODE_P)
             (bc T_P_BRA) (bc-rel-ref IS_PAIR__BTREE_VALIDATE);; (byte 7) ;; jump to is-pair
             (bc PUSH_L0)
             (bc CALL) (word-ref BTREE_VALUE_P)
             (bc T_P_BRA) (bc-rel-ref DONE__BTREE_VALIDATE) ;; done since is-value
             (bc BREAK)               ;; BRK error, passed parameter is neither value nor node!
   
      (label IS_PAIR__BTREE_VALIDATE)
             (bc PUSH_L0_CAR)
             (bc WRITE_TO_L1) ;; local 1 now car of node
             (bc NIL_P)
             (bc F_P_BRA) (bc-rel-ref IS_NOT_NIL__BTREE_VALIDATE)
             (bc BREAK)               ;; BRK error, car of pair must not be nil!
   
      (label IS_NOT_NIL__BTREE_VALIDATE)
             (bc PUSH_L1) ;; car of node
             (bc CALL) (word-ref BTREE_VALIDATE) ;; recursive call (not tail recursive)
   
             (bc PUSH_L0_CDR)
             (bc WRITE_TO_L1) ;; local 1 now cdr of node
             (bc NIL_P)
             (bc T_P_BRA) (bc-rel-ref DONE__BTREE_VALIDATE)
   
             (bc PUSH_L1) ;; cdr of node
             (bc CALL) (word-ref BTREE_VALIDATE) ;; recursive call (not tail recursive)
   
      (label DONE__BTREE_VALIDATE)
   
             (bc RET)))))

(module+ test #| validate |#
  (define btree-validate-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc CALL) (word-ref BTREE_VALIDATE)
       (bc BREAK))

      (list (org #x1800))
      BTREE_MAKE_ROOT
      BTREE_NODE_P
      BTREE_VALUE_P
      BTREE_VALIDATE)
    ))

  (regression-test
   btree-validate-state
   "make root -> validate"
   (check-equal? (vm-stack->strings btree-validate-state)
                 (list "stack is empty or tos=nil")
                 "validation leaves no value on the stack")
   (check-equal? (memory-list btree-validate-state ZP_VM_PC (add1 ZP_VM_PC))
                 (list #x07 #x08)
                 "program counter points to expected break"))

  (define btree-validate2-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I2)
       (bc PUSH_NIL)
       (bc CONS)
       (bc CALL) (word-ref BTREE_VALIDATE)
       (bc BREAK))

      (list (org #x1800))
      BTREE_NODE_P
      BTREE_VALUE_P
      BTREE_VALIDATE)
    ))

  (regression-test
   btree-validate2-state
   "btree validate 2"
   (check-equal? (memory-list btree-validate2-state (add1 ZP_VM_PC))
                 (list #x18)
                 "program counter on other page => validation failed "))

  (define btree-validate3-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_B) (byte 15)
       (bc CONS)
       (bc CALL) (word-ref BTREE_VALIDATE)
       (bc BREAK))

      (list (org #x1800))
      BTREE_NODE_P
      BTREE_VALUE_P
      BTREE_VALIDATE)
    ))

  (regression-test
   btree-validate3-state
   "btree validate 3"
   (check-equal? (memory-list btree-validate3-state (add1 ZP_VM_PC) (add1 ZP_VM_PC))
                 (list #x18)
                 "program counter on other page => validation failed (bytes are not allowed, yet)")))

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
(define BTREE_DEPTH ;; node :: right-list=nil :: depth=0 :: max-depth=0 -> int
  (bc-resolve
   (flatten
    (list
     (label BTREE_DEPTH)
            (byte 3) ;;# of locals: local0 = node, local1 = right-list, local2 = temp(car right-list|depth+1)
            (bc WRITE_TO_L0) ;; local0 <- node
            (bc CONS_PAIR_P)
            (bc T_P_BRA) (bc-rel-ref ELSE_COND__BTREE_DEPTH) ;; jump to else
            (bc WRITE_TO_L1)        ;; local1 <- right list
            (bc NIL_P)
            (bc F_P_BRA) (bc-rel-ref NOT_PAIR_COND__BTREE_DEPTH);; jump to (not (pair? node)) case

      ;;   [(and (not (pair? node))
      ;;             (empty? right-list))
      ;;          (max depth max-depth)]
            (bc IMAX)
            (bc RET)
  
     (label NOT_PAIR_COND__BTREE_DEPTH)
     ;;     [(not (pair? node))  
     ;;      (btree-depth (caar right-list) (cdr right-list) (cdar right-list) (max depth max-depth))]
            (bc IMAX)
  
            (bc PUSH_L1_CAR)        ;; car right-list
            (bc WRITE_TO_L2)        ;; remember car of right-list for later
            (bc CDR)                ;; cdar right-list
            
            (bc PUSH_L1_CDR)
  
            (bc PUSH_L2_CAR)        ;; push caar of right-list
            (bc TAIL_CALL)
  
     (label ELSE_COND__BTREE_DEPTH)
     ;;     [else
     ;;          (define l (car node))
     ;;          (define r (cdr node))
     ;;          (btree-depth l (cons (cons r (add1 depth)) right-list) (add1 depth) max-depth)]))
     ;;                                 ;; stack currently: [right-list :: depth :: max-depth]
            (bc POP_TO_L1)         ;; local1 = right-list
            (bc IINC)
            (bc WRITE_TO_L2)       ;; local2 = depth +1
            (bc PUSH_L1)           ;; [right-list :: depth+1 :: max-depth]
            (bc PUSH_L2)           ;; [depth+1 :: right-list :: depth+1 :: max-depth]
            (bc PUSH_L0_CDR)       ;; [right :: depth+1 :: right-list :: depth+1 :: max-depth]
                                        ;; [(right . depth+1) :: right-list :: depth+1 :: max-depth]
            (bc COONS)                  ;; [((right . depth+1) . right-list) :: depth+1 :: max-depth]
            (bc PUSH_L0_CAR)       ;; [left :: ((right . depth+1) . right-list) :: depth+1 :: max-depth]
            (bc TAIL_CALL)))))


(module+ test #| btree depth |#
  (define btree-depth-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I0)
       (bc PUSH_I0)
       (bc PUSH_NIL)
       (bc PUSH_I2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc CALL) (word-ref BTREE_DEPTH)
       (bc BREAK))
      BTREE_MAKE_ROOT
      BTREE_DEPTH)
     ))

  (regression-test
   btree-depth-1-state
   "make root -> depth"
   (check-equal? (shorten-cell-strings (vm-stack->strings btree-depth-1-state 10 #t))
                 (list "stack holds 2 items"
                       "1  (rt)"
                       "NIL")))

  (define btree-depth-2-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I0)
       (bc PUSH_I0)
       (bc PUSH_NIL)
       (bc PUSH_I2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc PUSH_I1)
       (bc CONS)
       (bc CALL) (word-ref BTREE_DEPTH)
       (bc BREAK))
      BTREE_MAKE_ROOT
      BTREE_DEPTH)
    ))

  (regression-test
   btree-depth-2-state
   "make root -> depth 2"
   (check-equal? (shorten-cell-strings (vm-stack->strings btree-depth-2-state 10 #t))
                 (list "stack holds 2 items"
                       "2  (rt)"
                       "NIL")))

  (define btree-depth-3-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I0)
       (bc PUSH_I0)
       (bc PUSH_NIL)
       (bc PUSH_I2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc PUSH_I1)
       (bc CONS)
       (bc PUSH_I0)
       (bc CONS)
       (bc CALL) (word-ref BTREE_DEPTH)
       (bc BREAK))
      BTREE_MAKE_ROOT
      BTREE_DEPTH)
    ))

  (regression-test
   btree-depth-3-state
   "make tree -> depth 3"
   (check-equal? (shorten-cell-strings (vm-stack->strings btree-depth-3-state 10 #t))
                 (list "stack holds 2 items"
                       "3  (rt)"
                       "NIL")))

  (define btree-depth-5-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I0)
       (bc PUSH_I0)
       (bc PUSH_NIL)
       (bc PUSH_I2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)     ;;-> o
       (bc PUSH_I1)                             ;;  / \
       (bc SWAP)                                ;; 2  nil
       (bc CONS)                                ;;            ->o
       (bc CALL) (word-ref BTREE_DEPTH)         ;;            /   \    
       (bc BREAK))                              ;;           o     1
      BTREE_MAKE_ROOT                           ;;          / \                      
      BTREE_DEPTH)                              ;;          2  nil                   
    ))                                          ;;                    
                                                ;;
  (regression-test
   btree-depth-5-state
   "make tree -> depth 5"
   (check-equal? (shorten-cell-strings (vm-stack->strings btree-depth-5-state 10 #t))
                 (list "stack holds 2 items"
                       "2  (rt)"
                       "NIL")))

  (define btree-depth-4-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I0)
       (bc PUSH_I0)
       (bc PUSH_NIL)
       (bc PUSH_I2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)     ;;-> o
       (bc PUSH_I1)                             ;;  / \
       (bc SWAP)                                ;; 2  nil
       (bc CONS)                                ;;             ->o
       (bc PUSH_I0)                             ;;             /   \
       (bc SWAP)                                ;;            o     1
       (bc CONS)                                ;;           / \                 o
       (bc CALL) (word-ref BTREE_DEPTH)         ;;          2  nil             /   \
       (bc BREAK))                              ;;                            o     0
      BTREE_MAKE_ROOT                           ;;                          /   \
      BTREE_DEPTH)                              ;;                         o     1
    ))                                          ;;                        / \
                                                ;;                       2  nil
  (regression-test
   btree-depth-4-state
   "make tree -> depth 4"
   (check-equal? (shorten-cell-strings (vm-stack->strings btree-depth-4-state 10 #t))
                 (list "stack holds 2 items"
                       "3  (rt)"
                       "NIL")))

  (define btree-depth-6-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I0)
       (bc PUSH_I0)
       (bc PUSH_NIL)
       (bc PUSH_I2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)     ;;-> o
       (bc PUSH_I1)                             ;;  / \
       (bc SWAP)                                ;; 2  nil
       (bc CONS)                                ;;             ->o
       (bc PUSH_I0)                             ;;             /   \
       ;; (bc SWAP)                             ;;            o     1
       (bc CONS)                                ;;           / \        -> o
       (bc BNOP)                                ;;          2  nil       /   \
       (bc CALL) (word-ref BTREE_DEPTH)         ;;                      0     o    
       (bc BREAK))                              ;;                          /   \
      BTREE_MAKE_ROOT                           ;;                         o     1 
      BTREE_DEPTH)                              ;;                        / \      
    ))                                          ;;                       2  nil    

  (regression-test
   btree-depth-6-state
   "make root -> depth 6"
   (check-equal? (shorten-cell-strings (vm-stack->strings btree-depth-6-state 10 #t))
                 (list "stack holds 2 items"
                       "3  (rt)"
                       "NIL"))
   (inform-check-equal? (cpu-state-clock-cycles btree-depth-6-state)
                        11278)))

;; (define (btree-path-to-first node (path (list)))
;;   (cond [(btree-value? node) path]
;;         [else (btree-path-to-first (car node) (cons (cons -1 node) path))]))
(define BTREE_PATH_TO_FIRST ;; node :: result-path=nil -> path
  (list
   (label BTREE_PATH_TO_FIRST)
          (byte 1)
          (bc WRITE_TO_L0)                 ;; local0 = node
          (bc CALL) (word-ref BTREE_VALUE_P)
          (bc T_P_RET)  ;; [(btree-value? node) path]

   ;; [else (btree-path-to-first (car node) (cons (cons -1 node) path))]))
          (bc PUSH_L0)                     ;; node :: path
          (bc PUSH_I0)                       ;; 0 :: node :: path
                                                ;; (0 . node) :: path
          (bc COONS)                            ;; ((0 . node) . path)
          (bc PUSH_L0_CAR)                 ;; (car node) :: ((0 . node) . path)
          (bc TAIL_CALL)))

(module+ test #| path to first |#
  (define path-to-first-0-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_I2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc CALL) (word-ref BTREE_PATH_TO_FIRST)
       (bc BREAK))
      BTREE_MAKE_ROOT
      BTREE_VALUE_P
      BTREE_PATH_TO_FIRST)
     ))

  (regression-test
   path-to-first-0-state
   "make root -> path to first"
   (check-equal? (shorten-cell-strings (vm-list->strings path-to-first-0-state (peek-word-at-address path-to-first-0-state ZP_RT) '() #t))
                 '("(0 . (2 . NIL))")
                 "result is a path to the node with value 2: ((0 . (2 . NIL)))"))

  (define path-to-first-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_I2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)     ;;
       (bc PUSH_I1)                             ;;         o
       (bc SWAP)                                ;;       /   \      
       (bc CONS)                                ;;    ->0     o      
       (bc PUSH_I0)                             ;;          /   \
       (bc CONS)                                ;;         o     1  
       (bc CALL) (word-ref BTREE_PATH_TO_FIRST) ;;        / \       
       (bc BREAK))                              ;;       2  nil
      BTREE_MAKE_ROOT                           ;;
      BTREE_VALUE_P                             ;;
      BTREE_PATH_TO_FIRST)                      ;;
     ))                                         ;;

  (regression-test
   path-to-first-1-state
   "make tree -> path to first 2"
   (check-equal? (shorten-cell-strings (vm-list->strings path-to-first-1-state (peek-word-at-address path-to-first-1-state ZP_RT) '() #t))
                 '("(0 . (0 . ((2 . NIL) . 1)))")
                 "result is a path to the node with value 1: ((0 . (1 . ((2 . nil) . 1))")))


;; (define (btree-path-to-last  node (path (list)))
;;   (cond [(btree-value? node) path]
;;         [(empty? (cdr node)) (btree-path-to-last (car node) (cons (cons -1 node) path))]
;;         [else (btree-path-to-last (cdr node) (cons (cons 1 node) path))]))
(define BTREE_PATH_TO_LAST ;; node :: result-path=nil -> path
  (bc-resolve
   (flatten
    (list
     (label BTREE_PATH_TO_LAST)
            (byte 1)
            (bc WRITE_TO_L0)             ;; local0 = node
            (bc CALL) (word-ref BTREE_VALUE_P) ;; 
            (bc T_P_RET)                   ;;     [(btree-value? node) path]
  
      ;;     [(empty? (cdr node)) (btree-path-to-last (car node) (cons (cons -1 node) path))]
            (bc PUSH_L0_CDR)
            (bc NIL_P)
            (bc F_P_BRA) (bc-rel-ref ELSE_COND__BTREE_PATH_TO_LAST)
  
            (bc PUSH_L0)
            (bc PUSH_I0)
            (bc COONS)
            (bc PUSH_L0_CAR)
            (bc TAIL_CALL)
  
      (label ELSE_COND__BTREE_PATH_TO_LAST)
      ;;    [else (btree-path-to-last (cdr node) (cons (cons 1 node) path))]))
            (bc PUSH_L0)
            (bc PUSH_I1)
            (bc COONS)
            (bc PUSH_L0_CDR)
            (bc TAIL_CALL)))))

(module+ test #| path to last |#
  (define path-to-last-0-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)

       (bc PUSH_I2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)

       (bc CALL) (word-ref BTREE_PATH_TO_LAST)
       (bc BREAK))
      BTREE_PATH_TO_LAST
      BTREE_VALUE_P
      BTREE_MAKE_ROOT)))

  (regression-test
   path-to-last-0-state
   "make root -> path to last"
   (check-equal? (shorten-cell-strings (vm-list->strings path-to-last-0-state (peek-word-at-address path-to-last-0-state ZP_RT) '() #t))
                 '("(0 . (2 . NIL))")))

  (define path-to-last-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
        (bc PUSH_I2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)     ;;
       (bc PUSH_NIL)                            ;;         o
       (bc SWAP)                                ;;       /   \
       (bc CONS)                                ;;    ->0     o
       (bc PUSH_I0)                          ;;          /   \
       (bc CONS)                                ;;         o     nil
       (bc CALL) (word-ref BTREE_PATH_TO_LAST)  ;;        / \       
       (bc BREAK))                                ;;    -> 2  nil
      BTREE_MAKE_ROOT                           ;;
      BTREE_VALUE_P                             ;;
      BTREE_PATH_TO_LAST)                       ;;
     ))

  (regression-test
   path-to-last-1-state
   "make root -> path to last 2"
   (check-equal? (shorten-cell-strings (vm-list->strings path-to-last-1-state (peek-word-at-address path-to-last-1-state ZP_RT) '() #t))
                 '("(0 . (2 . NIL))"
                   "(0 . ((2 . NIL) . NIL))"
                   "(1 . (0 . ((2 . NIL) . NIL)))")))

  (define path-to-last-2-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_I2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)     ;;
       (bc PUSH_I1)                          ;;         o
       (bc SWAP)                                ;;       /   \
       (bc CONS)                                ;;      0     o
       (bc PUSH_I0)                          ;;          /   \
       (bc CONS)                                ;;         o  -> 1
       (bc CALL) (word-ref BTREE_PATH_TO_LAST)  ;;        / \       
       (bc BREAK))                                ;;       2  nil
      BTREE_MAKE_ROOT                           ;;
      BTREE_VALUE_P                             ;;
      BTREE_PATH_TO_LAST)                       ;;
     ))

  (regression-test
   path-to-last-2-state
   "make root -> path to last"
   (check-equal? (shorten-cell-string (vm-regt->string path-to-last-2-state #t))
                  (string-append
                   "((1 . ((2 . NIL) . 1))"
                   " . ((1 . (0 . ((2 . NIL) . 1)))"
                   " . NIL))"))))

;; (define (btree-node-for-path path)
;;   (cond [(empty? path) '()]
;;         [(= -1 (caar path)) (car (cdar path))]
;;         [(= 1 (caar path)) (cdr (cdar path))]
;;         [else (raise-user-error (format "btree path may only contain 1 | -1:" path))]))
(define BTREE_NODE_FOR_PATH ;; path -> node
  (bc-resolve
   (flatten
    (list
     (label BTREE_NODE_FOR_PATH)
            (byte 1) ;; locals
            (bc WRITE_TO_L0)
            (bc NIL_P_RET_L0_POP_1)
            (bc POP)

      (label LEFT_NODE_COND__BTREE_NODE_FOR_PATH)
      ;; [(= 0 (caar path)) (car (cdar path))]
            (bc PUSH_L0_CAR)
  
            (bc CAR)
            (bc I_Z_P)
            (bc F_P_BRA) (bc-rel-ref ELSE_COND__BTREE_NODE_FOR_PATH) 
  
            (bc PUSH_L0_CAR)
            (bc CADR)
            (bc RET)
  
      (label ELSE_COND__BTREE_NODE_FOR_PATH)
      ;; [else (cdr (cdar path))]  ;; no error handling
            (bc PUSH_L0_CAR)
            (bc CDDR)
            (bc RET)))))

(module+ test #| node for path |#
  (define node-for-path-0-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_I2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)
       (bc PUSH_I0)
       (bc CONS)
       (bc CONS)
       (bc CALL) (word-ref BTREE_NODE_FOR_PATH)
       (bc BREAK))
      BTREE_NODE_FOR_PATH
      BTREE_MAKE_ROOT
      BTREE_VALUE_P)))

  (regression-test
   node-for-path-0-state
   "make root -> node for path"
   (check-equal? (vm-stack->strings node-for-path-0-state)
                 (list "stack holds 2 items"
                       "int $0002  (rt)"
                       "ptr NIL")))

  (define node-for-path-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_I0)  ;; right
       (bc PUSH_I2)  ;; left
       (bc CONS)        ;; node
       (bc PUSH_I1)  ;; path selector
       (bc CONS)
       (bc CONS)
       (bc CALL) (word-ref BTREE_NODE_FOR_PATH)
       (bc BREAK))
      BTREE_NODE_FOR_PATH
      BTREE_MAKE_ROOT
      BTREE_VALUE_P)
     ))

  (regression-test
   node-for-path-1-state
   "make root -> node for path 2"
   (check-equal? (vm-stack->strings node-for-path-1-state)
                  (list "stack holds 2 items"
                        "int $0000  (rt)"
                        "ptr NIL"))))

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
(define BTREE_PREV ;; path -> path
  (bc-resolve
   (flatten
    (list
     (label BTREE_PREV)
            (byte 3)
  
            (bc WRITE_TO_L0)
            (bc NIL_P_RET_L0_POP_1)

     (label LEFT_NODE_COND__BTREE_PREV) 
      ;; [(= 0 (caar path))
            (bc CAAR)
            (bc I_Z_P)
            (bc F_P_BRA) (bc-rel-ref ELSE_COND__BTREE_PREV) 
  
     (label LOOP_FN__BTREE_PREV)
            (bc PUSH_L0_CDR)
            (bc WRITE_TO_L0)         ;; top-most-relevant = local0 = (cdr path) <- looping cdr
            (bc NIL_P_RET_L0_POP_1)
            (bc CAAR)
            (bc I_Z_P)
            (bc T_P_BRA) (bc-rel-ref LOOP_FN__BTREE_PREV) 
  
     (label END_LOOP__BTREE_PREV)
            (bc PUSH_L0)             ;; top-most-relevant
            (bc NIL_P_RET_L0_POP_1)
  
     (label CONSTRUCT_PATH__BTREE_PATH_TO_LAST)
            (bc CDR)                      ;; entry for construct path <-- 
            (bc PUSH_L0_CAR)         ;; top-most-relevant
            (bc CDR)
            (bc WRITE_TO_L1)         ;; local1 = (cdar top-most-relevant)
            (bc PUSH_I0)
            (bc COONS)
  
            (bc PUSH_NIL)
            (bc PUSH_L1_CAR)         ;; (cdar top-most-relevant)
            (bc CALL) (word-ref BTREE_PATH_TO_LAST)
  
            (bc CALL) (word-ref APPEND)
            (bc RET)
  
      (label ELSE_COND__BTREE_PREV)
      ;; [else
            (bc PUSH_L0)
            (bc GOTO) (bc-rel-ref CONSTRUCT_PATH__BTREE_PATH_TO_LAST)          ;; (-17) construct path -->
            ))))

(module+ test #| prev |#
  (define prev-0-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)                            ;; for call to path_to_first
       (bc PUSH_I2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)     ;; got the tree, now construct a path
       (bc CALL) (word-ref BTREE_PATH_TO_FIRST)
       (bc BNOP)
       (bc CALL) (word-ref BTREE_PREV)
       (bc BREAK))
      BTREE_PATH_TO_FIRST
      BTREE_MAKE_ROOT
      BTREE_PREV
      BTREE_PATH_TO_LAST
      BTREE_VALUE_P
      BTREE_NODE_P
      APPEND
      REVERSE)
     ))

  (regression-test
   prev-0-state
   "make root -> append"
   (check-equal? (vm-regt->string prev-0-state #t)
                 "ptr NIL"))

  (define prev-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)                            ;; for call to path_to_first
       (bc PUSH_I2)                             ;;    o
       (bc PUSH_I) (word $0003)                 ;;   / \
       (bc CONS)                                ;;  2   3
       (bc CALL) (word-ref BTREE_PATH_TO_FIRST) ;; ((0 . (2 . 3)) . NIL)
       (bc BNOP)
       (bc CALL) (word-ref BTREE_PREV)
       (bc BREAK))
      BTREE_PATH_TO_FIRST
      BTREE_MAKE_ROOT
      BTREE_PREV
      BTREE_PATH_TO_LAST
      BTREE_VALUE_P
      BTREE_NODE_P
      APPEND
      REVERSE)
     ))

  (regression-test
   prev-1-state
   "make root -> append 2"
   (check-equal? (vm-regt->string prev-1-state #t)
                 "ptr NIL"))

  (define prev-2-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)                            ;; for call to path_to_first
       (bc PUSH_I)  (word $0003)              ;;    o
       (bc PUSH_I2)                          ;;   / \
       (bc CONS)                                ;;  2   3
       (bc CALL) (word-ref BTREE_PATH_TO_LAST)  ;; ((1 . (2 . 3)) . NIL)
       (bc BNOP)
       (bc CALL) (word-ref BTREE_PREV)
       (bc BREAK))
      BTREE_PATH_TO_FIRST
      BTREE_MAKE_ROOT
      BTREE_PREV
      BTREE_PATH_TO_LAST
      BTREE_VALUE_P
      BTREE_NODE_P
      APPEND
      REVERSE)
     ))

  (regression-test
   prev-2-state
   "make root -> path to last -> prev"
   (check-equal? (shorten-cell-string (vm-regt->string prev-2-state #t))
                 "((0 . (2 . 3)) . NIL)"))

  (define prev-3-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)                            ;; for call to path_to_first
       (bc PUSH_I)  (word $0003)                ;;    o
       (bc PUSH_I2)                             ;;   / \
       (bc CONS)                                ;;  1   o  
       (bc PUSH_I1)                             ;;     / \
       (bc CONS)                                ;;    2   3
       (bc CALL) (word-ref BTREE_PATH_TO_LAST)  ;; ((1 . (2 . 3)) . ((1 . (1 . (2 . 3))) . NIL))
       (bc BNOP)
       (bc CALL) (word-ref BTREE_PREV)
       (bc BREAK))
      BTREE_PATH_TO_FIRST
      BTREE_MAKE_ROOT
      BTREE_PREV
      BTREE_PATH_TO_LAST
      BTREE_VALUE_P
      BTREE_NODE_P
      APPEND
      REVERSE)
     ))

  (regression-test
   prev-3-state
   "make root -> path to last -> prev 2"
   (check-equal? (shorten-cell-string (vm-regt->string prev-3-state #t))
                 "((0 . (2 . 3)) . ((1 . (1 . (2 . 3))) . NIL))"))

  
  (define prev-4-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)                            ;; for call to path_to_first
       (bc PUSH_I) (word $0004)                 ;;     o
       (bc PUSH_I) (word $0003)                 ;;    / \
       (bc PUSH_I2)                             ;;   1   o
       (bc CONS)                                ;;      / \
       (bc CONS)                                ;;     o   4
       (bc PUSH_I1)                             ;;    / \
       (bc CONS)                                ;;   2   3
       (bc CALL) (word-ref BTREE_PATH_TO_LAST)  ;; ((1 . ((2 . 3) . 4)) . ((1 . (1 . ((2 . 3) . 4))) . NIL))
       (bc DUP)
       (bc CALL) (word-ref BTREE_PREV)          ;; ((1 . (2 . 3)) . ((0 . ((2 . 3) . 4)) . ((1 . (1 . ((2 . 3) . 4))) . NIL)))
       (bc DUP)
       (bc CALL) (word-ref BTREE_PREV)          ;; ((0 . (2 . 3)) . ((0 . ((2 . 3) . 4)) . ((1 . (1 . ((2 . 3) . 4))) . NIL)))
       (bc DUP)
       (bc BNOP)
       (bc CALL) (word-ref BTREE_PREV)          ;; ((0 . (1 . ((2 . 3) . 4))) . NIL)))
       (bc BREAK))
      BTREE_PATH_TO_FIRST
      BTREE_MAKE_ROOT
      BTREE_PREV
      BTREE_PATH_TO_LAST
      BTREE_VALUE_P
      BTREE_NODE_P
      APPEND
      REVERSE)
     ))

  (regression-test
   prev-4-state
   "make root -> path to last -> prev -> prev -> prev"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings prev-4-state 10 #t))
                 (list "stack holds 5 items"
                       "((0 . (1 . ((2 . 3) . 4))) . NIL)  (rt)"
                       "((0 . (2 . 3)) . ((0 . ((2 . 3) . 4)) . ((1 . (1 . ((2 . 3) . 4))) . NIL)))"
                       "((1 . (2 . 3)) . ((0 . ((2 . 3) . 4)) . ((1 . (1 . ((2 . 3) . 4))) . NIL)))"
                       "((1 . ((2 . 3) . 4)) . ((1 . (1 . ((2 . 3) . 4))) . NIL))"
                       "NIL"))

   (inform-check-equal? (cpu-state-clock-cycles prev-4-state)
                        5942)))

;; optimization idea: NIL?_RET instead of NIL?, T_P_RET
(define REVERSE ;; list :: result=nil -> list
  (list
   (label REVERSE)
          (byte 1)
          (bc WRITE_TO_L0)     ;; local0 = list
          (bc NIL_P)
          (bc T_P_RET)           ;; return second parameter (result)
          (bc PUSH_L0_CAR) 
          (bc CONS)
          (bc PUSH_L0_CDR)
          (bc TAIL_CALL)))

(module+ test #| reverse |#
  (define reverse-0-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_I0)
       (bc CONS)
       (bc PUSH_I1)
       (bc CONS)
       (bc PUSH_I2)
       (bc CONS)
       (bc PUSH_IM1)
       (bc CONS)
       (bc PUSH_NIL)
       (bc SWAP)
       (bc BNOP)
       (bc CALL) (word-ref REVERSE)
       (bc BREAK))
      REVERSE)
     ))

  (regression-test
   reverse-0-state
   "list (-1 2 1 0) -> reverse"
   (check-equal? (shorten-cell-string
                  (vm-regt->string reverse-0-state #t))
                 "(0 . (1 . (2 . (3fff . NIL))))")
   (inform-check-equal? (cpu-state-clock-cycles reverse-0-state)
                        5001)))

(define APPEND ;; head-list :: tail-list -> list
  (bc-resolve
   (flatten
    (list
     (label APPEND)
            (byte 1)
            (bc PUSH_NIL)
            (bc SWAP)              ;; head-list :: NIL :: tail-list
            (bc CALL) (word-ref REVERSE)
  
      (label LOOP__APPEND)         ;; (reverse head-list) :: tail-list
            (bc WRITE_TO_L0)  ;; local0 = reversed list     <- loop
            (bc NIL_P)
            (bc T_P_RET)        ;; return second (which is tail-list)
            (bc PUSH_L0_CAR)  ;; (car (reversed head-list)) :: tail-list
            (bc CONS)              ;; ((car (reversed head-list)) . tail-list)
            (bc PUSH_L0_CDR)  ;;
            (bc GOTO) (bc-rel-ref LOOP__APPEND) ;; (-6) loop ->
   ))))

(module+ test #| append |#
  (define append-0-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_I0)
       (bc CONS)
       (bc PUSH_I1)
       (bc CONS)
       (bc PUSH_I2)
       (bc CONS)

       (bc PUSH_NIL)
       (bc PUSH_I) (word $0003)
       (bc CONS)
       (bc PUSH_I) (word $0004)
       (bc CONS)
       (bc PUSH_I) (word $0005)
       (bc CONS)

       (bc BNOP)
       (bc CALL) (word-ref APPEND)
       (bc BREAK))
      APPEND
      REVERSE)))

  (regression-test
   append-0-state
   "list (2 1 0), list (5 4 3) -> append"
   (check-equal? (shorten-cell-string (vm-regt->string append-0-state #t))
                 "(5 . (4 . (3 . (2 . (1 . (0 . NIL))))))")
   (inform-check-equal? (cpu-state-clock-cycles append-0-state)
                        7901)))


;; (define (btree-next path)
;;   (cond [(empty? path)
;;          '()]

;;         [(and (= -1 (caar path))
;;             (not (empty? (cddar path))))
;;          (define top-most-relevant path)
;;          (append (btree-path-to-first (cddar top-most-relevant))
;;                  (cons (cons 1 (cdar top-most-relevant))
;;                        (cdr top-most-relevant)))]

;;         [(or (and (= -1 (caar path))
;;                (empty? (cddar path)))
;;             (= 1 (caar path)))
;;          (define top-most-relevant (dropf (cdr path)
;;                                           (lambda (pe) (or (= 1 (car pe))
;;                                                      (empty? (cdr (cdr pe)))))))
;;          (if (empty? top-most-relevant)
;;              '()
;;              (append (btree-path-to-first (cddar top-most-relevant))
;;                      (cons (cons 1 (cdar top-most-relevant))
;;                            (cdr top-most-relevant))))]

;;         [else (raise-user-error "unknown case")]))
(define BTREE_NEXT  ;; path -> path
  (bc-resolve
   (flatten
     (list
      (label BTREE_NEXT)
             (byte 2)
             (bc WRITE_TO_L0)         ;; local0= path
             (bc NIL_P_RET_L0_POP_1)

      (label COND__BTREE_NEXT)
             (bc CAAR)
             (bc I_Z_P)
             (bc F_P_BRA) (bc-rel-ref ELSE_COND__BTREE_NEXT)  ;; !=0 => goto to else branch
   
             (bc PUSH_L0_CAR)
             (bc CDDR)
             (bc NIL_P)
             (bc F_P_BRA) (bc-rel-ref LEFT_NODE__BTREE_NEXT)   ;; not nil? => goto (and ...) branch
   
       (label ELSE_COND__BTREE_NEXT)
       ;; [else
             (bc PUSH_L0_CDR)         ;; (cdr path)
             (bc NIL_P)
             (bc F_P_BRA) (bc-rel-ref LOOP_FN__BTREE_NEXT) 

             (bc PUSH_NIL)
             (bc RET)

             ;; loop start
       (label LOOP_FN__BTREE_NEXT)
             (bc PUSH_L0_CDR)         ;; (cdr path) <-- loop entry
             (bc WRITE_TO_L0)         ;; local2= (list pe ...) 
             (bc CAAR)                      ;; (car pe)
             (bc I_Z_P)
             (bc F_P_BRA) (bc-rel-ref LOOP_FN__BTREE_NEXT) ;; next --> loop
             (bc PUSH_L0_CAR)         ;; pe
             (bc CDDR)                      ;; (cddr pe)
             (bc NIL_P)
             (bc T_P_BRA) (bc-rel-ref LOOP_FN__BTREE_NEXT) ;; next --> loop
   
             ;; inner loop done
             ;; local2 = top most relevant
   
       ;; [(and (= 0 (caar path))
       ;;       (not (empty? (cddar path))))
       (label LEFT_NODE__BTREE_NEXT)
   
             (bc PUSH_L0_CDR)         ;; (cdr top-most-relevant)
             (bc PUSH_L0_CAR)         
             (bc CDR)                      ;; (cdar top-most-relevant
             (bc WRITE_TO_L1)         ;; local1 = (cdar top-most-relevant)
             (bc PUSH_I1)
             (bc COONS)
   
             (bc PUSH_NIL)
             (bc PUSH_L1_CDR)         ;; (cddar top-most-relevant)
             (bc CALL) (word-ref BTREE_PATH_TO_FIRST)
   
             (bc CALL) (word-ref APPEND)
             (bc RET)))))

(module+ test #| next |#
  (define next-0-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)                            ;; for call to path_to_first
       (bc PUSH_I2)
       (bc CALL) (word-ref BTREE_MAKE_ROOT)     ;; got the tree, now construct a path
       (bc CALL) (word-ref BTREE_PATH_TO_FIRST) ;; ((0 . (2 . NIL)) . NIL)
       (bc BNOP)
       (bc CALL) (word-ref BTREE_NEXT)
       (bc BREAK))
      BTREE_PATH_TO_FIRST
      BTREE_MAKE_ROOT
      BTREE_NEXT
      BTREE_PATH_TO_LAST
      BTREE_VALUE_P
      BTREE_NODE_P
      APPEND
      REVERSE)
     ))

  (regression-test
   next-0-state
   "make-root -> path to first -> next"
   (check-equal? (vm-regt->string next-0-state #t)
                 "ptr NIL"))

  (define next-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)                            ;; for call to path_to_first
       (bc PUSH_I)  (word $0003)                ;;    o
       (bc PUSH_I2)                             ;;   / \
       (bc CONS)                                ;;  2   3
       (bc CALL) (word-ref BTREE_PATH_TO_FIRST) ;; ((0 . (2 . 3)) . NIL)
       (bc BNOP)
       (bc CALL) (word-ref BTREE_NEXT)
       (bc BREAK))
      BTREE_PATH_TO_FIRST
      BTREE_MAKE_ROOT
      BTREE_NEXT
      BTREE_PATH_TO_LAST
      BTREE_VALUE_P
      BTREE_NODE_P
      APPEND
      REVERSE)
     ))


  (regression-test
   next-1-state
   "-> path to first -> next"
   (check-equal? (shorten-cell-string (vm-regt->string next-1-state #t))
                "((1 . (2 . 3)) . NIL)"))

  (define next-2-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)                            ;; for call to path_to_first
       (bc PUSH_I)  (word $0003)                ;;    o
       (bc PUSH_I2)                             ;;   / \
       (bc CONS)                                ;;  2   3
       (bc CALL) (word-ref BTREE_PATH_TO_LAST)  ;; ((1 . (2 . 3)) . NIL)
       (bc BNOP)
       (bc CALL) (word-ref BTREE_NEXT)
       (bc BREAK))
      BTREE_PATH_TO_FIRST
      BTREE_MAKE_ROOT
      BTREE_NEXT
      BTREE_PATH_TO_LAST
      BTREE_VALUE_P
      BTREE_NODE_P
      APPEND
      REVERSE)
     ))

  (regression-test
   next-2-state
   "-> path to last -> next"
   (check-equal? (vm-regt->string next-2-state #t)
                 "ptr NIL"))

  (define next-3-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)                            ;; for call to path_to_first
       (bc PUSH_I)  (word $0003)                ;;    o
       (bc PUSH_I2)                             ;;   / \
       (bc CONS)                                ;;  1   o
       (bc PUSH_I1)                             ;;     / \
       (bc CONS)                                ;;    2   3
       (bc CALL) (word-ref BTREE_PATH_TO_FIRST) ;; ((1 . (2 . 3)) . ((1 . (1 . (2 . 3))) . NIL))
       (bc BNOP)
       (bc CALL) (word-ref BTREE_NEXT)
       (bc BREAK))
      BTREE_PATH_TO_FIRST
      BTREE_MAKE_ROOT
      BTREE_NEXT
      BTREE_PATH_TO_LAST
      BTREE_VALUE_P
      BTREE_NODE_P
      APPEND
      REVERSE)
     ))

  (regression-test
   next-3-state
   "path to first -> next"
   (check-equal? (shorten-cell-string (vm-regt->string next-3-state #t))
                 "((0 . (2 . 3)) . ((1 . (1 . (2 . 3))) . NIL))"))

  (define next-4-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)                            ;; for call to path_to_first
       (bc PUSH_I) (word $0004)                 ;;     o
       (bc PUSH_I) (word $0003)                 ;;    / \
       (bc PUSH_I2)                             ;;   1   o
       (bc CONS)                                ;;      / \
       (bc CONS)                                ;;     o   4
       (bc PUSH_I1)                             ;;    / \
       (bc CONS)                                ;;   2   3
       (bc CALL) (word-ref BTREE_PATH_TO_FIRST) ;;((0 . (1 . ((2 . 3) . 4))) . NIL)
       (bc DUP)
       (bc CALL) (word-ref BTREE_NEXT)          ;; ((0 . (2 . 3)) . ((0 . ((2 . 3) . 4)) . ((1 . (1 . ((2 . 3) . 4))) . NIL)))
       (bc DUP)
       (bc CALL) (word-ref BTREE_NEXT)          ;; ((1 . (2 . 3)) . ((0 . ((2 . 3) . 4)) . ((1 . (1 . ((2 . 3) . 4))) . NIL)))
       (bc DUP)
       (bc CALL) (word-ref BTREE_NEXT)          ;;  ((1 . ((2 . 3) . 4)) . ((1 . (1 . ((2 . 3) . 4))) . NIL))
       (bc DUP)
       (bc BNOP)
       (bc CALL) (word-ref BTREE_NEXT)          ;; NIL
       (bc BREAK))
      BTREE_PATH_TO_FIRST
      BTREE_MAKE_ROOT
      BTREE_NEXT
      BTREE_PATH_TO_LAST
      BTREE_VALUE_P
      BTREE_NODE_P
      APPEND
      REVERSE)
     ))

  (regression-test
   next-4-state
   "-> path to first -> next -> next -> next-> next"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings next-4-state 10 #t))
                 (list "stack holds 6 items"
                       "NIL  (rt)"
                       "((1 . ((2 . 3) . 4)) . ((1 . (1 . ((2 . 3) . 4))) . NIL))"
                       "((1 . (2 . 3)) . ((0 . ((2 . 3) . 4)) . ((1 . (1 . ((2 . 3) . 4))) . NIL)))"
                       "((0 . (2 . 3)) . ((0 . ((2 . 3) . 4)) . ((1 . (1 . ((2 . 3) . 4))) . NIL)))"
                       "((0 . (1 . ((2 . 3) . 4))) . NIL)"
                       "NIL"))

   (inform-check-equal? (cpu-state-clock-cycles next-4-state)
                        1870)))

;; replace new nodes up the tree, making the tree persistent
;; balanced: O(lg N), worst case O(N)
;; (define (recursive-rebuild-path-with path repl-node (result (list)))
;;   (cond [(empty? path) (reverse result)]
;;         [(= (caar path) -1)
;;          (define new-node (cons repl-node (cddar path)))
;;          (define new-pe
;;            (cons (caar path) new-node))
;;          (recursive-rebuild-path-with (cdr path) new-node (cons new-pe result))]
;;         [(= (caar path) 1)
;;          (define new-node (cons (cadar path) repl-node))
;;          (define new-pe
;;            (cons (caar path) new-node))
;;          (recursive-rebuild-path-with (cdr path) new-node (cons new-pe result))]
;;         [else (raise-user-error "unknown case")]))
(define BTREE_REC_REBUILD_PATH_WITH ;; (list path) :: repl-node :: result=nil -> (list path)
  (bc-resolve
   (flatten
    (list
     (label BTREE_REC_REBUILD_PATH_WITH)
            (byte 2)
            (bc WRITE_TO_L0)               ;; local0 = path

            ;; check cond
            (bc NIL_P)
            (bc F_P_BRA) (bc-rel-ref CHECK_COND__BTREE_REC_REBUILD_PATH_WITH) ;; check next cond expression
  
     (label DONE__BTREE_REC_REBUILD_PATH_WITH)
            (bc POP) ;; ignore repl-node
            (bc PUSH_NIL)
            (bc SWAP)
            (bc CALL) (word-ref REVERSE)
            (bc RET) ;; return result
  
     (label CHECK_COND__BTREE_REC_REBUILD_PATH_WITH)
            ;; check cond
            (bc PUSH_L0_CAR)       
            (bc CAR)
            (bc I_Z_P)                 ;; (== 0 (caar path)
            (bc F_P_BRA) (bc-rel-ref ELSE__BTREE_REC_REBUILD_PATH_WITH) ;; check next cond expression
  
            (bc PUSH_L0_CAR)        
            (bc CDDR)                     ;; (cddar path) :: repl-node :: result
            (bc SWAP)                    ;; repl-node :: (cddar path) :: result
            (bc CONS)                    ;; newnode = (repl-node . (cddar path)) :: result
  
      (label PREV_TAIL_CALL__BTREE_REC_REBUILD_PATH_WITH)
            (bc WRITE_TO_L1)        ;; local1 = new node  <-- goto target of else
            (bc PUSH_L0_CAR)
            (bc CAR)                     ;; (caar path) :: (repl-node . (cddar path)) :: result
                                         ;; new-pe = ((caar path) . new-node) :: result
  
            (bc COONS)                   ;; (new-pe . result)
            (bc PUSH_L1)            ;; new-node :: (new-pe . result)
            (bc PUSH_L0_CDR)        ;; (cdr path) :: new-node :: (new-pe . result)
            (bc TAIL_CALL)
  
     (label ELSE__BTREE_REC_REBUILD_PATH_WITH)
            ;; else
            (bc PUSH_L0_CAR)
            (bc CADR)
            (bc CONS)                    ;; ((cadar path) . repl-node) :: result
  
            (bc GOTO) (bc-rel-ref PREV_TAIL_CALL__BTREE_REC_REBUILD_PATH_WITH)             ;; -10
            ))))

(module+ test #| rec rebuild path with |#
  (define rec-rebuild-path-with-0-state
    (run-bc-wrapped-in-test
     (append
      (list
       ;; build path
       (bc PUSH_NIL)
       (bc PUSH_I) (word $0008)
       (bc PUSH_I) (word $0007)
       (bc PUSH_I) (word $0006)
       (bc PUSH_I) (word $0005)
       (bc CONS)                           ;; (5 . 6)
       (bc CONS)                           ;; ((5 . 6) . 7)
       (bc PUSH_I) (word $0003)
       (bc CONS)                           ;; (3 . ((5 . 6) . 7))
       (bc CONS)                           ;; ((3 . ((5 . 6) . 7)) . 8)
       (bc PUSH_I0)
       (bc CONS)                           ;; (0 . ((3 . ((5 . 6) . 7)) . 8))
       (bc CONS)                           ;; list

       (bc PUSH_NIL)
       (bc PUSH_I) (word $0007)
       (bc PUSH_I) (word $0006)
       (bc PUSH_I) (word $0005)
       (bc CONS)                           ;; (5 . 6)
       (bc CONS)                           ;; ((5 . 6) . 7)
       (bc PUSH_I) (word $0003)
       (bc CONS)                           ;; (3 . ((5 . 6) . 7))
       (bc PUSH_I1)
       (bc CONS)                           ;; (1 . (3 . ((5 . 6) . 7)))
       (bc CONS)                           ;; list

       (bc PUSH_NIL)
       (bc PUSH_I) (word $0007)
       (bc PUSH_I) (word $0006)
       (bc PUSH_I) (word $0005)
       (bc CONS)                           ;; (5 . 6)
       (bc CONS)                           ;; ((5 . 6) . 7)
       (bc PUSH_I0)
       (bc CONS)                           ;; (0 . ((5 . 6) . 7))
       (bc CONS)                           ;; list

       (bc PUSH_NIL)
       (bc PUSH_I) (word $0006)
       (bc PUSH_I) (word $0005)
       (bc CONS)                           ;; (5 . 6)
       (bc PUSH_I0)
       (bc CONS)                           ;; (0 . (5 . 6))
       (bc CONS)                           ;; list

       (bc CALL) (word-ref APPEND)
       (bc CALL) (word-ref APPEND)
       (bc CALL) (word-ref APPEND)

       (bc DUP)

       (bc PUSH_NIL) ;; result
       (bc SWAP)

       (bc PUSH_I) (word $0005)
       (bc PUSH_I) (word $0004)
       (bc CONS)     ;; repl-node
       (bc SWAP)

       (bc BNOP)
       (bc CALL) (word-ref BTREE_REC_REBUILD_PATH_WITH)
       (bc BREAK))
      BTREE_REC_REBUILD_PATH_WITH
      BTREE_VALUE_P
      APPEND
      REVERSE)
    ))

  (regression-test
   rec-rebuild-path-with-0-state
   "recursive rebuild path"
   (check-equal? (shorten-cell-strings
                   (vm-stack->strings rec-rebuild-path-with-0-state 10 #t))
                  (list "stack holds 3 items"
                        (string-append
                         "((0 . ((4 . 5) . 6))"
                         " . ((0 . (((4 . 5) . 6) . 7))"
                         " . ((1 . (3 . (((4 . 5) . 6) . 7)))"
                         " . ((0 . ((3 . (((4 . 5) . 6) . 7)) . 8))"
                         " . NIL))))  (rt)")

                        (string-append
                         "((0 . (5 . 6))"
                         " . ((0 . ((5 . 6) . 7))"
                         " . ((1 . (3 . ((5 . 6) . 7)))"
                         " . ((0 . ((3 . ((5 . 6) . 7)) . 8))"
                         " . NIL))))")

                        "NIL")

                  "replaces the node '5' with '(4 . 5)' all the way up to the root in this path")))

;; add value after the given path, returning the new path (with the tail holding the new root, because tree is persistent)
;; balanced: O(lg N), worst case O(N)
;; (define (btree-add-value-after value path)
;;   (cond [(empty? path) (raise-user-error "path may not be empty")]
;;         [(and (= -1 (caar path))
;;             (empty? (cddar path)))
;;          (define new-node (cons (cadar path) value))
;;          (cons
;;           (cons 1 new-node)
;;           (recursive-rebuild-path-with (cdr path) new-node))]
;;         [(and (= -1 (caar path))
;;             (not (empty? (cddar path))))
;;          (define new-right-node (cons value (cddar path)))
;;          (define repl-node (cons (cadar path) new-right-node))
;;          (cons
;;           (cons -1 new-right-node)
;;           (cons (cons 1 repl-node)
;;                 (recursive-rebuild-path-with (cdr path) repl-node)))]
;;         [(= 1 (caar path))
;;          (define new-right-node (cons (cddar path) value))
;;          (define repl-node (cons (cadar path) new-right-node))
;;          (cons
;;           (cons 1 new-right-node)
;;           (cons (cons 1 repl-node)
;;                 (recursive-rebuild-path-with (cdr path) repl-node)))]
;;         [else (raise-user-error "unknown case")]))
(define BTREE_ADD_VALUE_AFTER ;; value :: path -> path
  (bc-resolve
   (flatten
    (list
     (label BTREE_ADD_VALUE_AFTER)
            (byte 4)
            (bc POP_TO_L1)           ;; local1 = value
            (bc WRITE_TO_L0)         ;; local0 = path
  
            ;; cond (nil? path)
            (bc NIL_P_RET_L0_POP_1)   ;; return local0 if it is nil (thus return nil)
  
            ;; cond (and (= -1 (caar path)) (empty? (cddar path)))
            (bc CAAR)
            (bc I_Z_P)
            (bc F_P_BRA) (bc-rel-ref ELSE_COND__BTREE_ADD_VALUE_AFTER) ;; -> else cond
            (bc PUSH_L0_CAR)
            (bc CDDR)
            (bc NIL_P)
            (bc F_P_BRA) (bc-rel-ref NEXT_COND__BTREE_ADD_VALUE_AFTER) ;; -> next cond
  
            (bc PUSH_NIL)                ;; (list)
            (bc PUSH_L1)            ;; value :: (list)
            (bc PUSH_L0_CAR)
            (bc CADR)                    ;; (cadar path) :: value  :: (list)
            (bc CONS)                    ;; ((cadar path) . value)  :: (list)
            (bc WRITE_TO_L1)        ;; local1 = new-node (no longer value)!
  
            (bc PUSH_L0_CDR)        ;; (cdr path) :: new-node :: (list)
            (bc CALL) (word-ref BTREE_REC_REBUILD_PATH_WITH)
  
            (bc PUSH_L1)
            (bc PUSH_I1)
                                         ;; (1 . new-node) :: rec-rebuild
            (bc COONS)                   ;; ((1 . new-node) . rec-rebuild)
            (bc RET)
  
     (label NEXT_COND__BTREE_ADD_VALUE_AFTER)  
            ;; cond (and (= -1 (caar path)) (not (empty? (cddar path))))
            ;; cond already checked
            (bc PUSH_NIL)
  
            (bc PUSH_L0_CAR)
            (bc CDDR)                    ;; (cddar path) :: (list)
            (bc PUSH_L1)            ;; value :: (cddar path) :: (list)
            (bc CONS)                    ;; (value . (cddar path)) :: (list)
            (bc WRITE_TO_L1)        ;; local1 = new-right-node (no longer value)!
  
            (bc PUSH_I0)
     (label COMMON_RET__BTREE_ADD_VALUE_AFTER)
            (bc CONS)
            (bc POP_TO_L3)          ;; local3 = (0 . new-right-node)
  
            ;; entry for tail of else condition
            (bc PUSH_L1)            ;; new-right-node :: (list)    <--
  
            (bc PUSH_L0_CAR)        ;; 
            (bc CADR)                    ;; (cadar path) :: new-right-node :: (list)
            (bc CONS)                    ;; ((cadar path) . new-right-node) :: (list)
            (bc WRITE_TO_L2)        ;; local2= repl-node 
  
            (bc PUSH_L0_CDR)        ;; (cdr path) :: repl-node :: (list)
            (bc CALL) (word-ref BTREE_REC_REBUILD_PATH_WITH)
  
            (bc PUSH_L2)
            (bc PUSH_I1)
                                         ;; (1 . repl-node) :: rec-rebuild
            (bc COONS)                   ;; ((1 . repl-node) . rec-rebuild)
  
            (bc PUSH_L3)            ;; (0 . new-right-node) :: ((1 . repl-node) . rec-rebuild)
  
            (bc CONS)                    ;; ((0 . new-right-node) . ((1 . repl-node) . rec-rebuild))
            (bc RET)
  
     (label ELSE_COND__BTREE_ADD_VALUE_AFTER)
            ;; cond else
            (bc PUSH_NIL)
  
            (bc PUSH_L1)            ;; value :: (list)
            (bc PUSH_L0_CAR)
            (bc CDDR)                    ;; (cddar path) :: value :: (list)
            (bc CONS)                    ;; ((cddar path) . value) :: (list)
            (bc WRITE_TO_L1)        ;; local1 = new-right-node (no longer value)!
  
            (bc PUSH_I1)
 
            (bc GOTO) (bc-rel-ref COMMON_RET__BTREE_ADD_VALUE_AFTER)         ;; -24 -->
            ))))

(module+ test #| add after |#
  (define add-after-0-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)

       (bc PUSH_NIL)
       (bc PUSH_I) (word $0004)
       (bc CONS)
       (bc PUSH_I0)
       (bc CONS)
       (bc CONS)

       (bc DUP)

       (bc PUSH_I) (word $0005)
       (bc BNOP)

       (bc CALL) (word-ref BTREE_ADD_VALUE_AFTER)
       (bc BREAK))
      BTREE_ADD_VALUE_AFTER
      BTREE_REC_REBUILD_PATH_WITH
      BTREE_VALUE_P
      REVERSE)
     ))

  (regression-test
   add-after-0-state
   "path *4 -> add value 5 after 4"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings add-after-0-state 10 #t))
                 (list "stack holds 3 items"
                       "((1 . (4 . 5)) . NIL)  (rt)"
                       "((0 . (4 . NIL)) . NIL)"
                       "NIL")))

  (define add-after-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)

       (bc PUSH_I) (word $0006)
       (bc PUSH_I) (word $0004)
       (bc CONS)
       (bc PUSH_I0)
       (bc CONS)
       (bc CONS)

       (bc DUP)

       (bc PUSH_I) (word $0005)
       (bc BNOP)

       (bc CALL) (word-ref BTREE_ADD_VALUE_AFTER)
       (bc BREAK))
      BTREE_ADD_VALUE_AFTER
      BTREE_REC_REBUILD_PATH_WITH
      BTREE_VALUE_P
      REVERSE)
     ))

  (regression-test
   add-after-1-state
   "path *4 6 -> add value 5 after 4"
   (check-equal? (shorten-cell-strings
                   (vm-stack->strings add-after-1-state 10 #t))
                  (list "stack holds 3 items"
                        "((0 . (5 . 6)) . ((1 . (4 . (5 . 6))) . NIL))  (rt)"
                        "((0 . (4 . 6)) . NIL)"
                        "NIL")))

  (define add-after-2-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)

       (bc PUSH_I) (word $0007)
       (bc PUSH_I) (word $0006)
       (bc CONS)
       (bc PUSH_I) (word $0004)
       (bc CONS)
       (bc PUSH_I0)
       (bc CONS)
       (bc CONS)

       (bc DUP)

       (bc PUSH_I) (word $0005)
       (bc BNOP)

       (bc CALL) (word-ref BTREE_ADD_VALUE_AFTER)
       (bc BREAK)) 
      BTREE_ADD_VALUE_AFTER
      BTREE_REC_REBUILD_PATH_WITH
      BTREE_VALUE_P
      REVERSE)
     ))

  (regression-test
   add-after-2-state
   "path *4 (6 7) -> add value 5 after 4"
   (check-equal? (shorten-cell-strings
                   (vm-stack->strings add-after-2-state 10 #t))
                  (list "stack holds 3 items"
                        "((0 . (5 . (6 . 7))) . ((1 . (4 . (5 . (6 . 7)))) . NIL))  (rt)"
                        "((0 . (4 . (6 . 7))) . NIL)"
                        "NIL")))

  (define add-after-3-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)

       (bc PUSH_I) (word $0006)
       (bc PUSH_I) (word $0005)
       (bc CONS)
       (bc PUSH_I1)
       (bc CONS)
       (bc CONS)

       (bc DUP)

       (bc PUSH_I) (word $0007)
       (bc BNOP)

       (bc CALL) (word-ref BTREE_ADD_VALUE_AFTER)
       (bc BREAK))
      BTREE_ADD_VALUE_AFTER
      BTREE_REC_REBUILD_PATH_WITH
      BTREE_VALUE_P
      REVERSE)
     ))

  (regression-test
   add-after-3-state
   "path 5 *6 -> add value 7 after 6"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings add-after-3-state 10 #t))
                 (list "stack holds 3 items"
                       "((1 . (6 . 7)) . ((1 . (5 . (6 . 7))) . NIL))  (rt)"
                       "((1 . (5 . 6)) . NIL)"
                       "NIL")))


  (define add-after-4-state
    (run-bc-wrapped-in-test
     (append
      (list

       (bc PUSH_I) (word $0006)       ;; 6
       (bc PUSH_I) (word $0005)       ;; 5 :: 6
       (bc CONS)                        ;; (5 . 6)
       (bc DUP)                         ;; (5 . 6) :: (5 . 6)

       (bc PUSH_I) (word $0004)       ;; 4 :: (5 . 6) :: (5 . 6)
       (bc CONS)                        ;; (4 . (5 . 6)) :: (5 . 6)
       (bc DUP)                         ;; (4 . (5 . 6)) :: (4 . (5 . 6)) :: (5 . 6)

       (bc PUSH_I) (word $0003)       ;; 3 :: (4 . (5 . 6)) :: (4 . (5 . 6)) :: (5 . 6)
       (bc CONS)                        ;; (3 . (4 . (5 . 6))) :: (4 . (5 . 6)) :: (5 . 6)

       (bc PUSH_I1)                  ;; 1 :: (3 . (4 . (5 . 6))) :: (4 . (5 . 6)) :: (5 . 6)
       (bc CONS)                        ;; (1 . (3 . (4 . (5 . 6)))) :: (4 . (5 . 6)) :: (5 . 6)

       (bc PUSH_NIL)                    ;; NIL :: (1 . (3 . (4 . (5 . 6)))) :: (4 . (5 . 6)) :: (5 . 6)
       (bc SWAP)                        ;; (1 . (3 . (4 . (5 . 6)))) :: NIL :: (4 . (5 . 6)) :: (5 . 6)
       (bc CONS)                        ;; ((1 . (3 . (4 . (5 . 6)))) . NIL) :: (4 . (5 . 6)) :: (5 . 6)

       (bc SWAP)                        ;; (4 . (5 . 6)) :: ((1 . (3 . (4 . (5 . 6)))) . NIL) :: (5 . 6)
       (bc PUSH_I1)                  ;; 1 :: (4 . (5 . 6)) :: ((1 . (3 . (4 . (5 . 6)))) . NIL) :: (5 . 6)
       (bc CONS)                        ;; (1 . (4 . (5 . 6))) :: ((1 . (3 . (4 . (5 . 6)))) . NIL) :: (5 . 6)
       (bc CONS)                        ;; ((1 . (4 . (5 . 6))) . ((1 . (3 . (4 . (5 . 6)))) . NIL)) :: (5 . 6)

       (bc SWAP)                        ;; (5 . 6) :: ((1 . (4 . (5 . 6))) . ((1 . (3 . (4 . (5 . 6)))) . NIL))
       (bc PUSH_I1)                  ;; 1 :: (5 . 6) :: ((1 . (4 . (5 . 6))) . ((1 . (3 . (4 . (5 . 6)))) . NIL))
       (bc CONS)                        ;; (1 . (5 . 6)) :: ((1 . (4 . (5 . 6))) . ((1 . (3 . (4 . (5 . 6)))) . NIL))
       (bc CONS)                        ;; ((1 . (5 . 6)) . ((1 . (4 . (5 . 6))) . ((1 . (3 . (4 . (5 . 6)))) . NIL)))

       (bc DUP)

       (bc PUSH_I) (word $0007)
       (bc BNOP)

       (bc CALL) (word-ref BTREE_ADD_VALUE_AFTER)
       (bc BREAK))
      BTREE_ADD_VALUE_AFTER
      BTREE_REC_REBUILD_PATH_WITH
      BTREE_VALUE_P
      REVERSE)
     ))

  (regression-test
   add-after-4-state
   "path (5 *6) (4 *(5 6)) (3 *(4 (5 6))) -> add 7 after 6"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings add-after-4-state 10 #t))
                 (list "stack holds 3 items"
                       (string-append
                        "((1 . (6 . 7))"
                        " . ((1 . (5 . (6 . 7)))"
                        " . ((1 . (4 . (5 . (6 . 7))))"
                        " . ((1 . (3 . (4 . (5 . (6 . 7)))))"
                        " . NIL))))  (rt)")
                       "((1 . (5 . 6)) . ((1 . (4 . (5 . 6))) . ((1 . (3 . (4 . (5 . 6)))) . NIL)))"
                       "NIL"))))

;; (define (btree-add-value-before value path)
;;   (cond [(empty? path) (raise-user-error "path may not be empty")]
;;         [(and (= -1 (caar path))
;;             (empty? (cddar path)))
;;          (define new-node (cons value (cadar path)))
;;          (cons
;;           (cons -1 new-node)
;;           (recursive-rebuild-path-with (cdr path) new-node))]
;;         [(and (= -1 (caar path))
;;             (not (empty? (cddar path))))
;;          (define new-left-node (cons value (cadar path)))
;;          (define repl-node (cons new-left-node (cddar path)))
;;          (cons
;;           (cons -1 new-left-node)
;;           (cons (cons -1 repl-node)
;;                 (recursive-rebuild-path-with (cdr path) repl-node)))]
;;         [(= 1 (caar path))
;;          (define new-right-node (cons value (cddar path)))
;;          (define repl-node (cons (cadar path) new-right-node))
;;          (cons
;;           (cons -1 new-right-node)
;;           (cons (cons 1 repl-node)
;;                 (recursive-rebuild-path-with (cdr path) repl-node)))]
;;         [else (raise-user-error "unknown case")]))
(define BTREE_ADD_VALUE_BEFORE ;; value :: path -> path
  (bc-resolve
   (flatten
    (list
     (label BTREE_ADD_VALUE_BEFORE)
            (byte 4)
  
            (bc POP_TO_L1)           ;; local1 = value
            (bc WRITE_TO_L0)         ;; local0 = path
  
            (bc NIL_P_RET_L0_POP_1)
  
            (bc CAAR)
            (bc I_Z_P)
            (bc F_P_BRA) (bc-rel-ref ELSE_COND__BTREE_ADD_VALUE_BEFORE) ;; -> else cond

            ;; cond (and (= -1 (caar path)) (empty? (cddar path)))
            (bc PUSH_NIL)                ;; param 3 for rec-rebuild call
  
            (bc PUSH_L0_CAR)
            (bc CADR)                    ;; (cadar path)
            (bc PUSH_L1)
            (bc CONS)
            (bc WRITE_TO_L1)        ;; local1 = new-node
                                         ;; param 2 for rec-rebuild call
            
            (bc PUSH_L0_CAR)
            (bc CDDR)
            (bc NIL_P)
            (bc F_P_BRA) (bc-rel-ref NEXT_COND__BTREE_ADD_VALUE_BEFORE) ;; -> next option


            (bc PUSH_L0_CDR)        ;; param 3 for rec-rebuild call
            (bc CALL) (word-ref BTREE_REC_REBUILD_PATH_WITH)
  
            (bc PUSH_L1)
            (bc PUSH_I0)
                                         ;; (0 . new-node)
            (bc COONS)                   ;; ((0 . new-node) . rec-build)
            (bc RET)
  

     (label NEXT_COND__BTREE_ADD_VALUE_BEFORE)
  
            (bc PUSH_L0_CAR)
            (bc CDDR)
            (bc SWAP)

            (bc PUSH_I0)

     (label COMMON_RETURN__BTREE_ADD_VALUE_BEFORE)
            (bc POP_TO_L3)
            (bc CONS)
            (bc WRITE_TO_L2)       ;; local2 = repl-node
                                        ;; param 2 for rec-rebuild call
  
            (bc PUSH_L0_CDR)       ;; param 1 for rec-rebuild call

            (bc CALL) (word-ref BTREE_REC_REBUILD_PATH_WITH)
  
            (bc PUSH_L2)
            (bc PUSH_L3)
            (bc COONS)

            (bc PUSH_L1)
            (bc PUSH_I0)
            (bc COONS)
            (bc RET)
  
            ;; cond else
     (label ELSE_COND__BTREE_ADD_VALUE_BEFORE)
            (bc PUSH_NIL)                ;; param 3 for rec-rebuild call
  
            (bc PUSH_L0_CAR)
            (bc CDDR)
            (bc PUSH_L1)
            (bc CONS)
            (bc WRITE_TO_L1)       ;; local1 = new right node (no longer value)!
  
            (bc PUSH_L0_CAR)
            (bc CADR)

            (bc PUSH_I1)
            (bc GOTO) (bc-rel-ref COMMON_RETURN__BTREE_ADD_VALUE_BEFORE)
          ))))

(module+ test #| add value before |#
  (define add-before-0-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)

       (bc PUSH_NIL)
       (bc PUSH_I) (word $0006)
       (bc CONS)
       (bc PUSH_I0)
       (bc CONS)
       (bc CONS)

       (bc DUP)

       (bc PUSH_I) (word $0005)

       (bc CALL) (word-ref BTREE_ADD_VALUE_BEFORE)
       (bc BREAK))
      BTREE_ADD_VALUE_BEFORE
      BTREE_REC_REBUILD_PATH_WITH
      BTREE_VALUE_P
      REVERSE)))

  (regression-test
   add-before-0-state
   "path *6 -> add 5 before 6"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings add-before-0-state 10 #t))
                 (list "stack holds 3 items"
                       "((0 . (5 . 6)) . NIL)  (rt)"
                       "((0 . (6 . NIL)) . NIL)"
                       "NIL")))

  (define add-before-1-state
    (run-bc-wrapped-in-test
     (append
      (list

       (bc PUSH_NIL)
       (bc PUSH_I) (word $0006)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I) (word $0003)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I) (word $0007)
       (bc SWAP)
       (bc CONS)

       (bc PUSH_I0)
       (bc CONS)

       (bc PUSH_NIL)
       (bc SWAP)
       (bc CONS)

       (bc SWAP)
       (bc PUSH_I1)
       (bc CONS)
       (bc CONS)

       (bc SWAP)
       (bc PUSH_I0)
       (bc CONS)
       (bc CONS)

       (bc DUP)

       (bc PUSH_I) (word $0005)
       (bc BNOP)

       (bc CALL) (word-ref BTREE_ADD_VALUE_BEFORE)
       (bc BREAK))

      BTREE_ADD_VALUE_BEFORE
      BTREE_REC_REBUILD_PATH_WITH
      BTREE_VALUE_P
      REVERSE)))

  (regression-test
   add-before-1-state
   "path *6, (3 *(6)), (*(3 (6)) 7) -> add 5 before 6"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings add-before-1-state 10 #t))
                 (list "stack holds 3 items"
                       (string-append
                        "((0 . (5 . 6))"
                        " . ((1 . (3 . (5 . 6)))"
                        " . ((0 . ((3 . (5 . 6)) . 7))"
                        " . NIL)))  (rt)")
                       (string-append
                        "((0 . (6 . NIL))"
                        " . ((1 . (3 . (6 . NIL)))"
                        " . ((0 . ((3 . (6 . NIL)) . 7))"
                        " . NIL)))")
                       "NIL")
                 "replace null with value, make new node and replace all up to the root"))

  (define add-before-2-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)

       (bc PUSH_I) (word $0007)
       (bc PUSH_I) (word $0006)
       (bc CONS)
       (bc PUSH_I0)
       (bc CONS)

       (bc CONS)
       (bc DUP)

       (bc PUSH_I) (word $0005)
       (bc CALL) (word-ref BTREE_ADD_VALUE_BEFORE)
       (bc BREAK))
      BTREE_ADD_VALUE_BEFORE
      BTREE_REC_REBUILD_PATH_WITH
      BTREE_VALUE_P
      REVERSE)))

  (regression-test
   add-before-2-state
   "path (*6 7) -> add 5 before 6"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings add-before-2-state 10 #t))
                 (list "stack holds 3 items"
                       (string-append
                        "((0 . (5 . 6))"
                        " . ((0 . ((5 . 6) . 7))"
                        " . NIL))  (rt)")
                       (string-append
                        "((0 . (6 . 7)) . NIL)")
                       "NIL")
                 "replace old node 6 with (5 . 6)"))

  (define add-before-3-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_I) (word $0008)
       (bc PUSH_I) (word $0007)
       (bc PUSH_I) (word $0006)
       (bc CONS)                        ;; (6 . 7)
       (bc PUSH_I) (word $0003)
       (bc CONS)                        ;; (3 . (6 . 7))
       (bc CONS)                        ;; ((3 . (6 . 7)) . 8)
       (bc PUSH_I0)
       (bc CONS)                        ;; (0 . ((3 . (6 . 7)) . 8))
       (bc CONS)                        ;; ((0 . ((3 . (6 . 7)) . 8)) . NIL)

       (bc PUSH_I) (word $0007)
       (bc PUSH_I) (word $0006)
       (bc CONS)                        ;; (6 . 7)
       (bc PUSH_I) (word $0003)       
       (bc CONS)                        ;; (3 . (6 . 7))
       (bc PUSH_I1)                 
       (bc CONS)                        ;; (1 . (3 . (6 . 7)))
       (bc CONS)                        ;; ((1 . (3 . (6 . 7))) . ((0 . ((3 . (6 . 7)) . 8)) . NIL))

       (bc PUSH_I) (word $0007)       
       (bc PUSH_I) (word $0006)
       (bc CONS)                        ;; (6 . 7)
       (bc PUSH_I0)                  
       (bc CONS)                        ;; (0 . (6 . 7))
       (bc CONS)                        ;; ((0 . (6 . 7)) . ((1 . (3 . (6 . 7))) . ((0 . ((3 . (6 . 7)) . 8)) . NIL)))

       (bc DUP)
       (bc PUSH_I) (word $0005)

       (bc CALL) (word-ref BTREE_ADD_VALUE_BEFORE)
       
       (bc BREAK))
      BTREE_ADD_VALUE_BEFORE
      BTREE_REC_REBUILD_PATH_WITH
      BTREE_VALUE_P
      REVERSE)))

  (regression-test
   add-before-3-state
   "path (*6 7), (3 *(6 7)), (*(3 (6 7)) 8) -> add 5 before 6"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings add-before-3-state 10 #t))
                 (list "stack holds 3 items"
                       (string-append
                        "((0 . (5 . 6))"
                        " . ((0 . ((5 . 6) . 7))"
                        " . ((1 . (3 . ((5 . 6) . 7)))"
                        " . ((0 . ((3 . ((5 . 6) . 7)) . 8))"
                        " . NIL))))  (rt)")
                       (string-append
                        "((0 . (6 . 7))"
                        " . ((1 . (3 . (6 . 7)))"
                        " . ((0 . ((3 . (6 . 7)) . 8))"
                        " . NIL)))")
                       "NIL")
                 "replace old node 6 with (5 . 6)"))

  (define add-before-4-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)

       (bc PUSH_I) (word $0006)
       (bc PUSH_I) (word $0004)
       (bc CONS)
       (bc PUSH_I1)
       (bc CONS)
       (bc CONS)

       (bc DUP)
       (bc PUSH_I) (word $0005)
       (bc CALL) (word-ref BTREE_ADD_VALUE_BEFORE)
       (bc BREAK))
      BTREE_ADD_VALUE_BEFORE
      BTREE_REC_REBUILD_PATH_WITH
      BTREE_VALUE_P
      REVERSE)
     ))

  (regression-test
   add-before-4-state
   "path (4 *6) -> add 5 before 6"
   (check-equal? (shorten-cell-strings
                   (vm-stack->strings add-before-4-state 10 #t))
                  (list "stack holds 3 items"
                        (string-append
                         "((0 . (5 . 6))"
                         " . ((1 . (4 . (5 . 6)))"
                         " . NIL))  (rt)")
                        (string-append
                         "((1 . (4 . 6)) . NIL)")
                        "NIL")
                  "replace old node 6 with (5 . 6)"))

  (define add-before-5-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)

       (bc PUSH_I) (word $0008)
       (bc PUSH_I) (word $0007)
       (bc PUSH_I) (word $0006)
       (bc PUSH_I) (word $0004)
       (bc CONS)
       (bc CONS)
       (bc PUSH_I) (word $0003)
       (bc CONS)
       (bc CONS)
       (bc PUSH_I0)
       (bc CONS)
       (bc CONS)

       (bc PUSH_I) (word $0007)
       (bc PUSH_I) (word $0006)
       (bc PUSH_I) (word $0004)
       (bc CONS)
       (bc CONS)
       (bc PUSH_I) (word $0003)
       (bc CONS)
       (bc PUSH_I1)
       (bc CONS)
       (bc CONS)

       (bc PUSH_I) (word $0007)
       (bc PUSH_I) (word $0006)
       (bc PUSH_I) (word $0004)
       (bc CONS)
       (bc CONS)
       (bc PUSH_I0)
       (bc CONS)
       (bc CONS)

       (bc PUSH_I) (word $0006)
       (bc PUSH_I) (word $0004)
       (bc CONS)
       (bc PUSH_I1)
       (bc CONS)
       (bc CONS)

       (bc DUP)
       (bc PUSH_I) (word $0005)

       (bc BNOP)
       (bc CALL) (word-ref BTREE_ADD_VALUE_BEFORE)
       (bc BREAK))
      BTREE_ADD_VALUE_BEFORE
      BTREE_REC_REBUILD_PATH_WITH
      BTREE_VALUE_P
      REVERSE)
     ))

  (regression-test
   add-before-5-state
   "path (4 *6), (*(4 6) 7), (3 *((4 6) 7)), (*(3 *((4 6) 7)) 8) -> add 5 before 6"
   (inform-check-equal? (cpu-state-clock-cycles add-before-5-state)
                        17168)

   (check-equal? (shorten-cell-strings
                  (vm-stack->strings add-before-5-state 10 #t))
                 (list "stack holds 3 items"
                       (string-append
                        "((0 . (5 . 6))"
                        " . ((1 . (4 . (5 . 6)))"
                        " . ((0 . ((4 . (5 . 6)) . 7))"
                        " . ((1 . (3 . ((4 . (5 . 6)) . 7)))"
                        " . ((0 . ((3 . ((4 . (5 . 6)) . 7)) . 8))"
                        " . NIL)))))  (rt)")
                       (string-append
                        "((1 . (4 . 6))"
                        " . ((0 . ((4 . 6) . 7))"
                        " . ((1 . (3 . ((4 . 6) . 7)))"
                        " . ((0 . ((3 . ((4 . 6) . 7)) . 8))"
                        " . NIL))))")
                       "NIL")
                 "replace old node 6 with (5 . 6)")))

;; (define (btree<-nodes nodes (result (list)))
;;   (cond
;;     [(and (empty? nodes) (empty? result)) '()]
;;     [(and (empty? nodes)
;;         (not (empty? result))
;;         (empty? (cdr result)))
;;      (car result)]
;;     [(and (empty? nodes)
;;         (not (empty? result)))
;;      (btree<-nodes (reverse result))]
;;     [(empty? (cdr nodes))
;;      (btree<-nodes (cdr nodes) (cons (cons (car nodes) '()) result))]
;;     [else
;;      (btree<-nodes (cddr nodes)
;;                   (cons (cons (car nodes) (cadr nodes)) result))]))
(define BTREE_FROM_LIST ;; (list node) :: result=nil -> node
  (bc-resolve
   (flatten
    (list
     (label BTREE_FROM_LIST)
            (byte 2)
  
            (bc WRITE_TO_L1)
            (bc NIL_P)
            (bc F_P_BRA) (bc-rel-ref NODES_NOT_EMPTY__BTREE_FROM_LIST)
  
            ;; nodes empty
            (bc WRITE_TO_L0)
            (bc NIL_P_RET_L0_POP_1) ;; nodes empty && result empty -> return nil
            
  
     (label NODES_EMPTY_RESULT_NOT_EMPTY__BTREE_FROM_LIST)
            (bc CDR)
            (bc NIL_P)
            (bc F_P_BRA) (bc-rel-ref NODES_EMPTY_RESULT_CDR_NOT_EMPTY__BTREE_FROM_LIST)
  
            (bc PUSH_L0_CAR)
            (bc RET)
  
     (label NODES_EMPTY_RESULT_CDR_NOT_EMPTY__BTREE_FROM_LIST)
            (bc PUSH_NIL)                ;; param 2 for tail call = nil
            (bc PUSH_NIL)                ;; param 2 for reverse = nil
            (bc PUSH_L0)            ;; param 1 for reverse = result
            (bc CALL) (word-ref REVERSE) ;; param 1 for tail call = (reverse result)
            (bc TAIL_CALL)
            
  
     (label NODES_NOT_EMPTY__BTREE_FROM_LIST)
            (bc POP_TO_L0)
            (bc PUSH_L1_CDR)
            (bc NIL_P)
            (bc F_P_BRA) (bc-rel-ref ELSE__BTREE_FROM_LIST)
  
            (bc PUSH_L0)
            (bc PUSH_NIL)
            (bc PUSH_L1_CAR)
            (bc COONS)
            (bc PUSH_L1_CDR)
            (bc TAIL_CALL)
  
     (label ELSE__BTREE_FROM_LIST)
            (bc PUSH_L0)
            (bc PUSH_L1_CDR)
            (bc CAR)
            (bc PUSH_L1_CAR)
            (bc COONS)
            (bc PUSH_L1_CDR)
            (bc CDR)
            (bc TAIL_CALL)))))

(module+ test #| btree from list |#
  (define btree-from-list-1-state
    (run-bc-wrapped-in-test     
     (append
      (list       
       (bc PUSH_NIL)
       (bc PUSH_I) (word $0001)
       (bc CONS)

       (bc DUP)
       (bc PUSH_NIL)
       (bc SWAP)

       (bc BNOP)
       (bc CALL) (word-ref BTREE_FROM_LIST)
       (bc BREAK))
      BTREE_FROM_LIST
      REVERSE)
     ))

  (regression-test
   btree-from-list-1-state
   "list (1) -> btree"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings btree-from-list-1-state 10 #t))
                 (list "stack holds 3 items"
                       "(1 . NIL)  (rt)"
                       "(1 . NIL)"
                       "NIL")))

  (define btree-from-list-0-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_I) (word $0009)
       (bc CONS)
       (bc PUSH_I) (word $0008)
       (bc CONS)
       (bc PUSH_I) (word $0007)
       (bc CONS)
       (bc PUSH_I) (word $0006)
       (bc CONS)
       (bc PUSH_I) (word $0005)
       (bc CONS)
       (bc PUSH_I) (word $0004)
       (bc CONS)
       (bc PUSH_I) (word $0003)
       (bc CONS)
       (bc PUSH_I) (word $0002)
       (bc CONS)
       (bc PUSH_I) (word $0001)
       (bc CONS)

       (bc DUP)
       (bc PUSH_NIL)
       (bc SWAP)

       (bc BNOP)
       (bc CALL) (word-ref BTREE_FROM_LIST)
       (bc BREAK))
      BTREE_FROM_LIST
      REVERSE)))

  (regression-test
   btree-from-list-0-state
   "list (1 2 3 4 5 6 7 8 9) -> btree"
   (check-equal? (shorten-cell-strings
                   (vm-stack->strings btree-from-list-0-state 10 #t))
                  (list "stack holds 3 items"
                        "((((1 . 2) . (3 . 4)) . ((5 . 6) . (7 . 8))) . (((9 . NIL) . NIL) . NIL))  (rt)"
                        "(1 . (2 . (3 . (4 . (5 . (6 . (7 . (8 . (9 . NIL)))))))))"
                        "NIL")))

  (define btree-from-list-2-state
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

       (bc DUP)
       (bc PUSH_NIL)
       (bc SWAP)

       (bc BNOP)
       (bc CALL) (word-ref BTREE_FROM_LIST)
       (bc BREAK))
      BTREE_FROM_LIST
      REVERSE)))

  (regression-test
   btree-from-list-2-state
   "list (1 2 3 4) -> btree"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings btree-from-list-2-state 10 #t))
                 (list "stack holds 3 items"
                       "((1 . 2) . (3 . 4))  (rt)"
                       "(1 . (2 . (3 . (4 . NIL))))"
                       "NIL"))))

;; (define (btree->list node (btree-prefix (list)) (result (list)) )
;;   (cond [(and (empty? node)
;;             (not (empty? btree-prefix)))
;;          (btree->list (car btree-prefix) (cdr btree-prefix) result)]
;;         [(empty? node) result]
;;         [(btree-value? node) (btree->list '() btree-prefix (cons node result))]
;;         [(btree-node? node)
;;          (btree->list (cdr node) (cons (car node) btree-prefix) result)]
;;         [else (raise-user-error "unknown case")]))
(define BTREE_TO_LIST ;; node :: btree-prefix=nil :: result=nil -> (list node)
  (bc-resolve
   (flatten
    (list
     (label BTREE_TO_LIST)
            (byte 2)
            (bc WRITE_TO_L1)       ;; local1 = node
            (bc NIL_P)
            (bc F_P_BRA) (bc-rel-ref NODE_NOT_NIL__BTREE_TO_LIST)

            (bc WRITE_TO_L0)       ;; local_0 = btree-prefix
            (bc NIL_P)
            (bc T_P_RET)             ;; return result
                                        ;; result
            ;; Node Nil, Prefix Not Nil
            (bc PUSH_L0_CDR)       ;; (cdr btree-prefix) :: result
            (bc PUSH_L0_CAR)       ;; (car btree-prefix) :: (cdr btree-prefix) :: result
            (bc TAIL_CALL)

     (label NODE_NOT_NIL__BTREE_TO_LIST)

            (bc PUSH_L1)
            (bc CALL) (word-ref BTREE_VALUE_P)
            (bc F_P_BRA) (bc-rel-ref NO_BT_VALUE__BTREE_TO_LIST)

            (bc POP_TO_L0)         ;; local_0 = btree-prefix
                                        ;; result 
            (bc PUSH_L1)           ;; node :: result
            (bc CONS)                   ;; (cons node result) 
            (bc PUSH_L0)           ;; btree_prefix :: (cons node result)
            (bc PUSH_NIL)               ;; NIL :: btree-prefix :: (cons node result)
            (bc TAIL_CALL)

     (label NO_BT_VALUE__BTREE_TO_LIST)
                                        ;; btree-prefix :: result
            (bc PUSH_L1_CAR)       ;; (car node) :: btree-prefix :: result
            (bc CONS)                   ;; (cons (car node) btree-prefix) :: result
            (bc PUSH_L1_CDR)       ;; (cdr node) :: (cons (car node) btree-prefix) :: result
            (bc TAIL_CALL)))))

(module+ test #| btree to list |#
  (define btree-to-list-0-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_NIL)
       (bc PUSH_I) (word $0004)
       (bc CONS)
       (bc PUSH_NIL)
       (bc PUSH_I) (word $0003)
       (bc CONS)
       (bc CONS)
       (bc PUSH_NIL)
       (bc PUSH_I) (word $0002)
       (bc PUSH_I) (word $0001)
       (bc CONS)
       (bc CONS)
       (bc CONS)
       (bc CONS)

       (bc DUP)
       (bc PUSH_NIL)
       (bc SWAP)
       (bc PUSH_NIL)
       (bc SWAP)

       (bc BNOP)
       (bc CALL) (word-ref BTREE_TO_LIST)
       (bc BREAK))
      BTREE_TO_LIST
      BTREE_VALUE_P)
     ))

  (regression-test
   btree-to-list-0-state
   "btree 1 2 3 4 -> list"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings btree-to-list-0-state 10 #t))
                 (list "stack holds 3 items"
                       "(1 . (2 . (3 . (4 . NIL))))  (rt)"
                       "((((1 . 2) . NIL) . ((3 . NIL) . (4 . NIL))) . NIL)"
                       "NIL"))

   (inform-check-equal? (cpu-state-clock-cycles btree-to-list-0-state)
                        25639)))


;; (define (btree-remove-value-at path (result (list)) (old-prev (list)))
;;   (cond
;;     [(and (empty? path)
;;         (not (empty? old-prev)))
;;      (define node (btree-node-for-path result))
;;      (define prev-node-path (btree-prev result))
;;      (if (or (eq? node old-prev)
;;             (empty? prev-node-path))
;;          result
;;          prev-node-path)]
;;     [(empty? path) result]
;;     [(= 1 (caar path))
;;      (define new-node (cons (cadar path) '()))
;;      (btree-remove-value-at
;;       (list)
;;       (cons
;;        (cons -1 new-node)
;;        (recursive-rebuild-path-with (cdr path) new-node))
;;       old-prev)]
;;     [(and (= -1 (caar path))
;;         (not (empty? (cddar path))))
;;      (define new-node (cons (cddar path) '()))
;;      (btree-remove-value-at
;;       (list)
;;       (cons
;;        (cons -1 new-node)
;;        (recursive-rebuild-path-with (cdr path) new-node))
;;       old-prev)]
;;     [(and (= -1 (caar path))
;;         (empty? (cddar path))
;;         (empty? (cdr path)))
;;      '()]
;;     [(and (= -1 (caar path))
;;         (empty? (cddar path))
;;         (not (empty? (cdr path))))
;;      (define old-prev-node (btree-node-for-path (btree-prev path)))
;;      (btree-remove-value-at (cdr path) (list) old-prev-node)]
;;     [else (raise-user-error "unknown case")]))
(define BTREE_REMOVE_VALUE_AT ;; path :: result=nil :: old-prev=nil
  (bc-resolve
   (flatten
     (list
      (label BTREE_REMOVE_VALUE_AT)
             (byte 4)
             (bc WRITE_TO_L1)        ;; local_1= path

             (bc NIL_P)
             (bc F_P_BRA) (bc-rel-ref PATH_NOT_NIL__BTREE_REMOVE_VALUE_AT)

             (bc POP_TO_L0)
             ;; path empty
             (bc WRITE_TO_L2)          ;; old-prev
             (bc NIL_P_RET_L0_POP_1)
             (bc WRITE_L0)

             ;; path empty and old-prev not empty
             (bc CALL) (word-ref BTREE_PREV)
             (bc WRITE_TO_L3)              ;; (btree-prev result)
             (bc NIL_P_RET_L0_POP_1)        ;; since prev-node-opath is nil return result

             (bc WRITE_L0)                  ;; result
             (bc CALL) (word-ref BTREE_NODE_FOR_PATH)   ;; (btree-node-for-path result)
             (bc PUSH_L2)                          ;; old-prev
             (bc CELL_EQ_P)                               
             (bc F_P_BRA) (bc-rel-ref NEXT_RESULT__BTREE_REMOVE_VALUE_AT)
             (bc PUSH_L0)
             (bc RET)

      (label NEXT_RESULT__BTREE_REMOVE_VALUE_AT)
             (bc PUSH_L3)
             (bc RET)

      (label PATH_NOT_NIL__BTREE_REMOVE_VALUE_AT)
             (bc POP)                   ;; result (is discarded)


             (bc PUSH_L1_CAR)
             (bc CAR)                           ;; (caar path)
             (bc I_Z_P)
             (bc F_P_BRA) (bc-rel-ref CAAR_PATH_1__BTREE_REMOVE_VALUE_AT)

             ;; from now on (caar path) = 0
             (bc PUSH_L1_CAR)
             (bc CDDR)
             (bc NIL_P)
             (bc F_P_BRA) (bc-rel-ref CDDAR_PATH_NOT_NIL__BTREE_REMOVE_VALUE_AT)

             (bc POP)                           ;; discard old-prev

             ;; from now on (empty? (cddar path))
             (bc PUSH_L1_CDR)
             (bc NIL_P)
             (bc F_P_BRA) (bc-rel-ref CDR_PATH_NOT_NIL__BTREE_REMOVE_VALUE_AT)

             ;; (and (= -1 (caar path)) (empty? (cddar path)) (empty? (cdr path)))
             (bc PUSH_NIL)
             (bc RET)

      (label CDR_PATH_NOT_NIL__BTREE_REMOVE_VALUE_AT)
             ;; (and (= -1 (caar path)) (empty? (cddar path)) (not (empty? (cdr path))))
             (bc PUSH_L1)
             (bc CALL) (word-ref BTREE_PREV)
             (bc CALL) (word-ref BTREE_NODE_FOR_PATH)   ;; param 3 for tail-call
             (bc PUSH_NIL)                              ;; param 2 for tail-call
             (bc PUSH_L1_CDR)                      ;; param 1 for tail-call
             (bc TAIL_CALL)

      (label CDDAR_PATH_NOT_NIL__BTREE_REMOVE_VALUE_AT)
             ;; (and (= -1 (caar path)) (not (empty? (cddar path))))
                                                       ;; old-prev = param3 for tail call still on stack
             (bc PUSH_NIL)                             ;; param 3 form btree rec

             (bc PUSH_NIL)
             (bc PUSH_L1_CAR)
             (bc CDDR)

      (label COMMON_PREP_TC__BTREE_REMOVE_VALUE_AT)
             (bc CONS)
             (bc WRITE_TO_L0)      ;; new node

             (bc PUSH_L1_CDR)      ;; (cdr path)
             (bc CALL) (word-ref BTREE_REC_REBUILD_PATH_WITH)

             (bc PUSH_L0)
             (bc PUSH_I0)
             (bc COONS)                         ;; ((0 . new-local) . rec-rebuild)
                                                ;; param 2 for tail call
             (bc PUSH_NIL)                      ;; param 1 for tail call

             (bc TAIL_CALL)

      (label CAAR_PATH_1__BTREE_REMOVE_VALUE_AT)
                                                ;; old-prev = parameter 3 for btree-remove-value-at still on stack
             (bc PUSH_NIL)                      ;; parameter 3 for btree rec

             (bc PUSH_NIL)
             (bc PUSH_L1_CAR)
             (bc CADR)                          ;; (cadar path)
             ;; from here downwards same as previous cond! <- for (size) optimization
             (bc GOTO) (bc-rel-ref COMMON_PREP_TC__BTREE_REMOVE_VALUE_AT)
             ;; (bc CONS)
             ;; (bc WRITE_TO_L3)              ;; local3= ((cadar path) . NIL)
             ;; ;; parameter 2 for betree rec
             ;; (bc PUSH_L1_CDR)              ;; (cdr path) parameter 1 for btree rec
             ;; (bc CALL) (word-ref BTREE_REC_REBUILD_PATH_WITH)

             ;; (bc PUSH_L3)
             ;; (bc PUSH_I0)
             ;; (bc COONS)                         ;; ((0 . new-node) . rec-rebuild)
             ;; ;; param 2 for btree-remove-value-at
             ;; (bc PUSH_NIL)                       ;; param 1 for btree-remove-value-at
             ;; (bc TAIL_CALL)
             ))))

(module+ test #| remove value at |#
  (define dependecies-remove-value-at
    (append
     BTREE_REMOVE_VALUE_AT
     BTREE_NODE_FOR_PATH
     BTREE_PREV
     BTREE_REC_REBUILD_PATH_WITH
     BTREE_PATH_TO_FIRST
     BTREE_MAKE_ROOT
     BTREE_PATH_TO_LAST
     BTREE_VALUE_P
     BTREE_NODE_P
     APPEND
     REVERSE))


  (define remove-value-at-0-state
    (run-bc-wrapped-in-test
     (append
      (list

       (bc PUSH_NIL)
       (bc PUSH_NIL)
       (bc PUSH_I) (word $0008)
       (bc CONS)
       (bc PUSH_I0)
       (bc CONS)
       (bc CONS)

       (bc DUP)

       (bc PUSH_NIL)
       (bc SWAP)
       (bc PUSH_NIL)
       (bc SWAP)

       (bc BNOP)
       (bc CALL) (word-ref BTREE_REMOVE_VALUE_AT)
       (bc BREAK))
      dependecies-remove-value-at)))

  (regression-test
   remove-value-at-0-state
   "path *8 -> remove value at"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings remove-value-at-0-state 10 #t))
                 (list "stack holds 3 items"
                       "NIL  (rt)"
                       "((0 . (8 . NIL)) . NIL)"
                       "NIL")))

  (define remove-value-at-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_NIL)
       (bc PUSH_NIL)
       (bc CALL) (word-ref BTREE_REMOVE_VALUE_AT)
       (bc BREAK))
      dependecies-remove-value-at)))

  (regression-test
   remove-value-at-1-state
   "path nil -> remove value at"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings remove-value-at-1-state 10 #t))
                  (list "stack holds 2 items"
                        "NIL  (rt)"
                        "NIL")))

  (define remove-value-at-2-state
    (run-bc-wrapped-in-test
     (append
      (list

       (bc PUSH_NIL)
       (bc PUSH_I) (word $0009)
       (bc PUSH_I) (word $0008)
       (bc CONS)
       (bc PUSH_I0)
       (bc CONS)
       (bc CONS)

       (bc DUP)

       (bc PUSH_NIL)
       (bc SWAP)
       (bc PUSH_NIL)
       (bc SWAP)

       (bc BNOP)
       (bc CALL) (word-ref BTREE_REMOVE_VALUE_AT)
       (bc BREAK))
      dependecies-remove-value-at)
     ))

  (regression-test
   remove-value-at-2-state
   "path *8 9 -> remove value at"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings remove-value-at-2-state 10 #t))
                 (list "stack holds 3 items"
                       "((0 . (9 . NIL)) . NIL)  (rt)"
                       "((0 . (8 . 9)) . NIL)"
                       "NIL")))

  (define remove-value-at-3-state
    (run-bc-wrapped-in-test
     (append
      (list

       (bc PUSH_NIL)
       (bc PUSH_I) (word $0009)
       (bc PUSH_I) (word $0008)
       (bc CONS)
       (bc PUSH_I1)
       (bc CONS)
       (bc CONS)

       (bc DUP)

       (bc PUSH_NIL)
       (bc SWAP)
       (bc PUSH_NIL)
       (bc SWAP)

       (bc BNOP)
       (bc CALL) (word-ref BTREE_REMOVE_VALUE_AT)
       (bc BREAK))
      dependecies-remove-value-at)
     ))

  (regression-test
   remove-value-at-3-state
   "path 8 *9 -> remove value at"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings remove-value-at-3-state 10 #t))
                 (list "stack holds 3 items"
                       "((0 . (8 . NIL)) . NIL)  (rt)"
                       "((1 . (8 . 9)) . NIL)"
                       "NIL")))

  (define remove-value-at-4-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I) (word $0006)
       (bc PUSH_I) (word $0005)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I1)
       (bc CONS)
       (bc SWAP)

       (bc PUSH_I) (word $0007)
       (bc SWAP)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I0)
       (bc CONS)
       (bc SWAP)

       (bc PUSH_I) (word $0004)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I1)
       (bc CONS)
       (bc SWAP)

       (bc PUSH_I) (word $0008)
       (bc SWAP)
       (bc CONS)

       (bc PUSH_I0)
       (bc CONS)
       (bc PUSH_NIL)
       (bc SWAP)
       (bc CONS)


       (bc SWAP)
       (bc CONS)
       (bc SWAP)
       (bc CONS)
       (bc SWAP)
       (bc CONS)
       
       (bc DUP)

       (bc PUSH_NIL)
       (bc SWAP)
       (bc PUSH_NIL)
       (bc SWAP)

       (bc BNOP)
       (bc CALL) (word-ref BTREE_REMOVE_VALUE_AT)
       (bc BREAK))
      dependecies-remove-value-at)
     ))

  (regression-test
   remove-value-at-4-state
   "path 5 *6, (*(5 6) 7), (4 *((5 6) 7)), (*(4 ((5 6) 7)) 8) -> remove value at"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings remove-value-at-4-state 10 #t))
                 (list "stack holds 3 items"
                       (string-append
                        "((0 . (5 . NIL))"
                        " . ((0 . ((5 . NIL) . 7))"
                        " . ((1 . (4 . ((5 . NIL) . 7)))"
                        " . ((0 . ((4 . ((5 . NIL) . 7)) . 8))"
                        " . NIL))))  (rt)")
                       (string-append
                        "((1 . (5 . 6))"
                        " . ((0 . ((5 . 6) . 7))"
                        " . ((1 . (4 . ((5 . 6) . 7)))"
                        " . ((0 . ((4 . ((5 . 6) . 7)) . 8))"
                        " . NIL))))"
                        )
                       "NIL")))

  (define remove-value-at-5-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I) (word $0006)
       (bc PUSH_I) (word $0005)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I0)
       (bc CONS)
       (bc SWAP)

       (bc PUSH_I) (word $0007)
       (bc SWAP)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I0)
       (bc CONS)
       (bc SWAP)

       (bc PUSH_I) (word $0004)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I1)
       (bc CONS)
       (bc SWAP)

       (bc PUSH_I) (word $0008)
       (bc SWAP)
       (bc CONS)

       (bc PUSH_I0)
       (bc CONS)
       (bc PUSH_NIL)
       (bc SWAP)
       (bc CONS)


       (bc SWAP)
       (bc CONS)
       (bc SWAP)
       (bc CONS)
       (bc SWAP)
       (bc CONS)

       (bc DUP)

       (bc PUSH_NIL)
       (bc SWAP)
       (bc PUSH_NIL)
       (bc SWAP)

       (bc BNOP)
       (bc CALL) (word-ref BTREE_REMOVE_VALUE_AT)
       (bc BREAK))
      dependecies-remove-value-at)
     ))

  (regression-test
   remove-value-at-5-state
   "path *5 6, (*(5 6) 7), (4 *((5 6) 7)), (*(4 ((5 6) 7)) 8) -> remove at"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings remove-value-at-5-state 10 #t))
                 (list "stack holds 3 items"
                       (string-append
                        "((0 . (6 . NIL))"
                        " . ((0 . ((6 . NIL) . 7))"
                        " . ((1 . (4 . ((6 . NIL) . 7)))"
                        " . ((0 . ((4 . ((6 . NIL) . 7)) . 8))"
                        " . NIL))))  (rt)")
                       (string-append
                        "((0 . (5 . 6))"
                        " . ((0 . ((5 . 6) . 7))"
                        " . ((1 . (4 . ((5 . 6) . 7)))"
                        " . ((0 . ((4 . ((5 . 6) . 7)) . 8))"
                        " . NIL))))")
                       "NIL")))

  (define remove-value-at-6a-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_I) (word $0005)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I0)
       (bc CONS)
       (bc SWAP)

       (bc PUSH_I) (word $0007)
       (bc SWAP)
       (bc CONS)
       ;; (bc DUP)

       (bc PUSH_I0)
       (bc CONS)
       ;; (bc SWAP)

       ;; (bc PUSH_I) (word $0004)
       ;; (bc CONS)
       ;; (bc DUP)

       ;; (bc PUSH_I1)
       ;; (bc CONS)
       ;; (bc SWAP)

       ;; (bc PUSH_I) (word $0008)
       ;; (bc SWAP)
       ;; (bc CONS)

       ;; (bc PUSH_I0)
       ;; (bc CONS)

       (bc PUSH_NIL)
       (bc SWAP)
       (bc CONS)

       ;; (bc SWAP)
       ;; (bc CONS)
       ;; (bc SWAP)
       ;; (bc CONS)
       (bc SWAP)
       (bc CONS)

       (bc DUP)

       (bc PUSH_NIL)
       (bc SWAP)
       (bc PUSH_NIL)
       (bc SWAP)

       (bc BNOP)
       (bc CALL) (word-ref BTREE_REMOVE_VALUE_AT)
       (bc BREAK))
      dependecies-remove-value-at)
     ))

  (regression-test
   remove-value-at-6a-state
   "path *5, (*(5) 7) -> remove value at"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings remove-value-at-6a-state 10 #t))
                 (list "stack holds 3 items"
                       (string-append
                        "((0 . (7 . NIL))"
                        " . NIL)  (rt)")
                       (string-append
                        "((0 . (5 . NIL))"
                        " . ((0 . ((5 . NIL) . 7))"
                        " . NIL))")
                       "NIL")))

  (define remove-value-at-6b-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_I) (word $0005)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I0)
       (bc CONS)
       (bc SWAP)

       (bc PUSH_I) (word $0007)
       (bc SWAP)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I0)
       (bc CONS)
       (bc SWAP)

       (bc PUSH_I) (word $0004)
       (bc CONS)
       ;; (bc DUP)

       (bc PUSH_I1)
       (bc CONS)
       ;; (bc SWAP)

       ;; (bc PUSH_I) (word $0008)
       ;; (bc SWAP)
       ;; (bc CONS)

       ;; (bc PUSH_I0)
       ;; (bc CONS)

       (bc PUSH_NIL)
       (bc SWAP)
       (bc CONS)

       (bc SWAP)
       (bc CONS)
       ;; (bc SWAP)
       ;; (bc CONS)
       (bc SWAP)
       (bc CONS)

       (bc DUP)

       (bc PUSH_NIL)
       (bc SWAP)
       (bc PUSH_NIL)
       (bc SWAP)

       (bc BNOP)
       (bc CALL) (word-ref BTREE_REMOVE_VALUE_AT)
       (bc BREAK))
      dependecies-remove-value-at)
     ))

  (regression-test
   remove-value-at-6b-state
   "path *5, (*(5) 7), (4 *((5) 7)) -> remove value at"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings remove-value-at-6b-state 10 #t))
                 (list "stack holds 3 items"
                       (string-append
                        "((0 . (4 . (7 . NIL)))"
                        " . NIL)  (rt)")
                       (string-append
                        "((0 . (5 . NIL))"
                        " . ((0 . ((5 . NIL) . 7))"
                        " . ((1 . (4 . ((5 . NIL) . 7)))"
                        " . NIL)))")
                       "NIL")))

  (define remove-value-at-6c-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_I) (word $0005)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I0)
       (bc CONS)
       (bc SWAP)

       (bc PUSH_I) (word $0007)
       (bc SWAP)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I0)
       (bc CONS)
       (bc SWAP)

       (bc PUSH_I) (word $0004)
       (bc CONS)
       ;; (bc DUP)

       (bc PUSH_I1)
       (bc CONS)
       ;; (bc SWAP)

       ;; (bc PUSH_I) (word $0008)
       ;; (bc SWAP)
       ;; (bc CONS)

       ;; (bc PUSH_I0)
       ;; (bc CONS)

       (bc PUSH_NIL)
       (bc SWAP)
       (bc CONS)

       (bc SWAP)
       (bc CONS)
       ;; (bc SWAP)
       ;; (bc CONS)
       (bc SWAP)
       (bc CONS)

       (bc DUP)

       (bc BNOP)
       (bc CALL) (word-ref BTREE_PREV)
       (bc BREAK))
      dependecies-remove-value-at)
     ))

  (regression-test
   remove-value-at-6c-state
   "path *5, (*(5) 7), (4 *((5)7)) -> remove value at"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings remove-value-at-6c-state 10 #t))
                 (list "stack holds 3 items"
                       (string-append
                        "((0 . (4 . ((5 . NIL) . 7)))"
                        " . NIL)  (rt)")
                       (string-append
                        "((0 . (5 . NIL))"
                        " . ((0 . ((5 . NIL) . 7))"
                        " . ((1 . (4 . ((5 . NIL) . 7)))"
                        " . NIL)))")
                       "NIL")))

  (define remove-value-at-6d-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_I) (word $0007)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I0)
       (bc CONS)
       (bc SWAP)

       (bc PUSH_I) (word $0004)
       (bc CONS)
       ;; (bc DUP)

       (bc PUSH_I1)
       (bc CONS)

       (bc PUSH_NIL)
       (bc SWAP)
       (bc CONS)

       (bc SWAP)
       (bc CONS)

       (bc DUP)

       (bc BNOP)
       (bc CALL) (word-ref BTREE_PREV)
       (bc BREAK))
      dependecies-remove-value-at)
     ))

  (regression-test
   remove-value-at-6d-state
   "path *7, (4 *(7)) -> remove value at"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings remove-value-at-6d-state 10 #t))
                 (list "stack holds 3 items"
                       (string-append
                        "((0 . (4 . (7 . NIL)))"
                        " . NIL)  (rt)")
                       (string-append
                        "((0 . (7 . NIL))"
                        " . ((1 . (4 . (7 . NIL)))"
                        " . NIL))")
                       "NIL")))

  (define remove-value-at-6-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_I) (word $0005)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I0)
       (bc CONS)
       (bc SWAP)

       (bc PUSH_I) (word $0007)
       (bc SWAP)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I0)
       (bc CONS)
       (bc SWAP)

       (bc PUSH_I) (word $0004)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I1)
       (bc CONS)
       (bc SWAP)

       (bc PUSH_I) (word $0008)
       (bc SWAP)
       (bc CONS)

       (bc PUSH_I0)
       (bc CONS)
       (bc PUSH_NIL)
       (bc SWAP)
       (bc CONS)


       (bc SWAP)
       (bc CONS)
       (bc SWAP)
       (bc CONS)
       (bc SWAP)
       (bc CONS)

       (bc DUP)

       (bc PUSH_NIL)
       (bc SWAP)
       (bc PUSH_NIL)
       (bc SWAP)

       (bc BNOP)
       (bc CALL) (word-ref BTREE_REMOVE_VALUE_AT)
       (bc BREAK))
      dependecies-remove-value-at)
     ))

  (regression-test
   remove-value-at-6-state
   "path *5, (*(5) 7), (4 *((5) 7)), (*(4 ((5) 7)) 8) -> remove value at"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings remove-value-at-6-state 10 #t))
                 (list "stack holds 3 items"
                       (string-append
                        "((0 . (4 . (7 . NIL)))"
                        " . ((0 . ((4 . (7 . NIL)) . 8))"
                        " . NIL))  (rt)")
                       (string-append
                        "((0 . (5 . NIL))"
                        " . ((0 . ((5 . NIL) . 7))"
                        " . ((1 . (4 . ((5 . NIL) . 7)))"
                        " . ((0 . ((4 . ((5 . NIL) . 7)) . 8))"
                        " . NIL))))")
                       "NIL")))

  (define remove-value-at-7-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_I) (word $0005)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I0)
       (bc CONS)
       (bc SWAP)

       (bc PUSH_NIL)
       (bc SWAP)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I0)
       (bc CONS)
       (bc SWAP)

       (bc PUSH_NIL)
       (bc SWAP)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I0)
       (bc CONS)
       (bc SWAP)

       (bc PUSH_I) (word $0006)
       (bc SWAP)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I0)
       (bc CONS)
       (bc SWAP)
 
       (bc PUSH_I) (word $0004)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I1)
       (bc CONS)
       (bc SWAP)

       (bc PUSH_I) (word $0003)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I1)
       (bc CONS)
       (bc SWAP)

       (bc PUSH_I) (word $0007)
       (bc SWAP)
       (bc CONS)

       (bc PUSH_I0)
       (bc CONS)

       (bc PUSH_NIL)
       (bc SWAP)
       (bc CONS)


       (bc SWAP)
       (bc CONS)
       (bc SWAP)
       (bc CONS)
       (bc SWAP)
       (bc CONS)
       (bc SWAP)
       (bc CONS)
       (bc SWAP)
       (bc CONS)
       (bc SWAP)
       (bc CONS)

       (bc DUP)

       (bc PUSH_NIL)
       (bc SWAP)
       (bc PUSH_NIL)
       (bc SWAP)

       (bc BNOP)
       (bc CALL) (word-ref BTREE_REMOVE_VALUE_AT)
       (bc BREAK))
      dependecies-remove-value-at)
     ))

  ;; (require profile)
  ;; (profile-thunk remove-value-at-7-state)

  (regression-test
   remove-value-at-7-state
   "path *5, (*(5)), (*((5))), (*(((5))) 6), (4 *((((5))) 6)), (3 *(4 ((((5))) 6))) (*(3 (4 ((((5))) 6))) 7) -> remove value at"
   (inform-check-equal? (cpu-state-clock-cycles remove-value-at-7-state)
                        57197)
   (check-equal? (shorten-cell-strings (vm-stack->strings remove-value-at-7-state 10 #t))
                 (list "stack holds 3 items"
                       (string-append
                        "((0 . (4 . (6 . NIL)))"
                        " . ((1 . (3 . (4 . (6 . NIL))))"
                        " . ((0 . ((3 . (4 . (6 . NIL))) . 7))"
                        " . NIL)))  (rt)")
                       (string-append
                        "((0 . (5 . NIL))"
                        " . ((0 . ((5 . NIL) . NIL))"
                        " . ((0 . (((5 . NIL) . NIL) . NIL))"
                        " . ((0 . ((((5 . NIL) . NIL) . NIL) . 6))"
                        " . ((1 . (4 . ((((5 . NIL) . NIL) . NIL) . 6)))"
                        " . ((1 . (3 . (4 . ((((5 . NIL) . NIL) . NIL) . 6))))"
                        " . ((0 . ((3 . (4 . ((((5 . NIL) . NIL) . NIL) . 6))) . 7))"
                        " . NIL)))))))")
                       "NIL")
                 "if the node deleted has a prev, return that one and recursively replace up to root")))


(define BTREE_ROOT_FOR_PATH ;; path -> node
  (bc-resolve
   (flatten
    (list
     (label BTREE_ROOT_FOR_PATH)
            (byte 1)
            (bc WRITE_TO_L0)
            (bc NIL_P_RET_L0_POP_1)

            (bc CDR)
            (bc NIL_P)
            (bc T_P_BRA) (bc-rel-ref CDR_IS_NIL__BTREE_ROOT_FOR_PATH)
            (bc PUSH_L0_CDR)
            (bc TAIL_CALL)

     (label CDR_IS_NIL__BTREE_ROOT_FOR_PATH)
            (bc PUSH_L0_CAR)
            (bc CDR)
            (bc RET)))))

(module+ test #| btree root for path |#
  (define root-for-path-0-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc CALL) (word-ref BTREE_ROOT_FOR_PATH)
       (bc BREAK))
      BTREE_ROOT_FOR_PATH)))

  (regression-test
   root-for-path-0-state
   "root for path nil"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings root-for-path-0-state 10 #t))
                 (list "stack holds 2 items"
                       "NIL  (rt)"
                       "NIL")))

  (define root-for-path-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_NIL)
       (bc PUSH_NIL)
       (bc PUSH_I) (word $0005)
       (bc CONS)
       (bc PUSH_I0)
       (bc CONS)
       (bc CONS)
       (bc DUP)
       (bc CALL) (word-ref BTREE_ROOT_FOR_PATH)
       (bc BREAK))
      BTREE_ROOT_FOR_PATH)))

  (regression-test
   root-for-path-1-state
   "root for path *5"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings root-for-path-1-state 10 #t))
                 (list "stack holds 3 items"
                       "(5 . NIL)  (rt)"
                       "((0 . (5 . NIL)) . NIL)"
                       "NIL")))

  (define root-for-path-2-state
    (run-bc-wrapped-in-test
     (append      
      (list
       (bc PUSH_NIL)
       (bc PUSH_I) (word $0005)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I0)
       (bc CONS)
       (bc SWAP)

       (bc PUSH_I) (word $0007)
       (bc SWAP)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I0)
       (bc CONS)
       (bc SWAP)

       (bc PUSH_I) (word $0004)
       (bc CONS)
       (bc DUP)

       (bc PUSH_I1)
       (bc CONS)
       (bc SWAP)

       (bc PUSH_I) (word $0008)
       (bc SWAP)
       (bc CONS)

       (bc PUSH_I0)
       (bc CONS)
       (bc PUSH_NIL)
       (bc SWAP)
       (bc CONS)


       (bc SWAP)
       (bc CONS)
       (bc SWAP)
       (bc CONS)
       (bc SWAP)
       (bc CONS)

       (bc DUP)

       (bc BNOP)
       (bc CALL) (word-ref BTREE_ROOT_FOR_PATH)
       (bc BREAK))
      BTREE_ROOT_FOR_PATH)))

  (regression-test
   root-for-path-2-state
   "root for path *5, (*(5) 7), (4 *((5) 7)), (*(4 ((5) 7)) 8)"
   (check-equal? (shorten-cell-strings
                  (vm-stack->strings root-for-path-2-state 10 #t))
                 (list "stack holds 3 items"
                       "((4 . ((5 . NIL) . 7)) . 8)  (rt)"
                       (string-append ""
                                      "((0 . (5 . NIL))"
                                      " . ((0 . ((5 . NIL) . 7))"
                                      " . ((1 . (4 . ((5 . NIL) . 7)))"
                                      " . ((0 . ((4 . ((5 . NIL) . 7)) . 8))"
                                      " . NIL))))")
                       "NIL"))))

(define BTREE_REVERSE  ;; node -> node
  (list
   (label BTREE_REVERSE)
          (byte 0)

          (bc PUSH_NIL)
          (bc SWAP)
          (bc PUSH_NIL)
          (bc SWAP)
          (bc CALL) (word-ref BTREE_TO_LIST)
          (bc PUSH_NIL)
          (bc SWAP)
          (bc CALL) (word-ref REVERSE)
          (bc PUSH_NIL)
          (bc SWAP)
          (bc CALL) (word-ref BTREE_FROM_LIST)

          (bc RET)))

(define vm-btree
  (flatten
   (append
    REVERSE
    APPEND

    BTREE_MAKE_ROOT

    BTREE_VALUE_P
    BTREE_NODE_P
    BTREE_DEPTH

    BTREE_PATH_TO_LAST
    BTREE_PATH_TO_FIRST
    BTREE_NEXT
    BTREE_PREV

    BTREE_NODE_FOR_PATH

    BTREE_REC_REBUILD_PATH_WITH
    ;; BTREE_VALIDATE   

    BTREE_ADD_VALUE_BEFORE
    BTREE_ADD_VALUE_AFTER

    BTREE_TO_LIST
    BTREE_FROM_LIST

    BTREE_REMOVE_VALUE_AT

    BTREE_ROOT_FOR_PATH

    BTREE_REVERSE)))

(module+ test #| vm-btree |#
  (inform-check-equal? (bc-bytes (flatten vm-btree))
                508))
