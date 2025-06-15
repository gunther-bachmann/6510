#lang racket/base

#|
 this file is implementing fifo by using two stacks (in a struct, a cell-pair could be used too)

 this is foremost to test structures that have non atomic fields, pointing to other data,
 in this case lists

 (defs fifo
   (in list)
   (out list))

  create void -> FIFO
  enqueue FIFO :: T -> void
  dequeue FIFO -> T

|#

(require (only-in racket/list flatten))

(require "../6510.rkt")
(require "./vm-bc-ast.rkt")
(require (only-in "./bc-btree.rkt"
                  REVERSE))
(require (only-in "./vm-bc-resolver.rkt"
                  bc-resolve
                  bc-bytes))
(require (only-in "./vm-interpreter.rkt"
                  vm-interpreter
                  bc
                  ALLOC_ARA
                  PUSH_RA
                  GET_RA_AF_0
                  GET_RA_AF_1
                  GET_RA_AF_2
                  GET_RA_AF_3
                  SET_RA_AF_0
                  SET_RA_AF_1
                  SET_RA_AF_2
                  SET_RA_AF_3
                  CONS
                  CALL
                  RET
                  NIL_P
                  PUSH_NIL
                  PUSH_I
                  PUSH_B
                  GC_FL
                  ALLOC_A
                  F_P_RET_F
                  GET_AF_0
                  GET_AF_1
                  GET_AF_2
                  GET_AF_3
                  SET_AF_0
                  SET_AF_1
                  SET_AF_2
                  SET_AF_3
                  CELL_EQ_P
                  EXT
                  CAAR
                  CADR
                  CDAR
                  CDDR
                  COONS
                  POP
                  DUP
                  BNOP
                  I_Z_P
                  IINC
                  IMAX
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
                  WRITE_L3))

(module+ test #|  |#
  (require "../6510-test-utils.rkt")

  (require (only-in "./vm-interpreter-test-utils.rkt" run-bc-wrapped-in-test- vm-list->strings))
  (require (only-in "../cisc-vm/stack-virtual-machine.rkt" BRK))
  (require (only-in "../tools/6510-interpreter.rkt" cpu-state-clock-cycles peek memory-list))

  (require (only-in "./vm-inspector-utils.rkt"
                    shorten-cell-strings
                    shorten-cell-string
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
            (list (org #xa000))
            vm-interpreter))

  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (run-bc-wrapped-in-test- bc wrapped-code debug)))

(define FIFO_CREATE_N ;;  -> point struct
  (list
   (label FIFO_CREATE_N)
          (byte 1)
          (bc PUSH_B) (byte 2)  ;;                                stack: 2
          (bc ALLOC_ARA)        ;; RA = aptr -> [-,-]             stack: -
          (bc PUSH_NIL)         ;;                                stack: NIL
          (bc SET_RA_AF_0)      ;; RA = aptr -> [NIL, -]          stack: -
          (bc PUSH_NIL)         ;;                                stack: NIL
          (bc SET_RA_AF_1)      ;; RA = aptr -> [NIL, NIL]        stack: -
          (bc PUSH_RA)          ;;                                stack: aptr -> [NIL, NIL]
          (bc RET)))

(define FIFO_CREATE ;;  -> point struct
  (list
   (label FIFO_CREATE)
          (byte 1)
          (bc PUSH_NIL)         ;; stack: NIL
          (bc PUSH_NIL)         ;; stack: NIL :: NIL
          (bc PUSH_B) (byte 2)  ;; stack: 2 :: NIL :: NIL
          (bc ALLOC_A)          ;; stack: <point-struct-ptr> :: NIL :: NIL
          (bc WRITE_TO_L0)      ;; l0 = <point-struct-ptr> -> [-, -]
          (bc SET_AF_0)         ;; stack: NIL, <point-struct-ptr> -> [NIL, -]
          (bc PUSH_L0)          ;; stack: <point-struct-ptr> :: NIL
          (bc SET_AF_1)         ;; stack: -, <point-struct-ptr> -> [NIL, NIL]
          (bc PUSH_L0)          ;; stack: <point-struct-ptr>
          (bc RET)))

(module+ test #| fifo create |#
  (define fifo-create-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc CALL) (word-ref FIFO_CREATE)
       (bc BRK))
      FIFO_CREATE)
     ))

  (check-equal? (vm-stack->strings fifo-create-state)
                (list "stack holds 1 item"
                      (format "ptr[1] $~a04  (rt)" (format-hex-byte PAGE_AVAIL_0))))
  (check-equal? (vm-cell-at->string fifo-create-state (+ PAGE_AVAIL_0_W #x04) #f #t)
                "cell-array len=$02 [...]")
  (check-equal? (peek fifo-create-state(+ PAGE_AVAIL_0_W #x03))
                1
                "reference count is 1")
  (check-equal? (map (lambda (offset) (vm-cell-at->string fifo-create-state (+ PAGE_AVAIL_0_W offset) #f #t))
                     (list 06 08))
                (list "pair-ptr NIL" "pair-ptr NIL")
                "the first two elements of the array are decimal NIL, NIL"))

(define FIFO_ENQUEUE ;; FIFO :: T -> void
  (list
   (label FIFO_ENQUEUE)
          (byte 1)
          (bc WRITE_TO_L0)
          (bc GET_AF_0)
          (bc SWAP)
          (bc CONS)
          (bc PUSH_L0)
          (bc SET_AF_0)          
          (bc RET)))

(module+ test #| enqueue |#
  (define enqueue-state-1
    (run-bc-wrapped-in-test
     (append
      (list
       (bc CALL) (word-ref FIFO_CREATE)
       (bc DUP)
       (bc PUSH_I1)
       (bc SWAP)
       (bc CALL) (word-ref FIFO_ENQUEUE)
       (bc BRK))
      FIFO_CREATE
      FIFO_ENQUEUE)))

  (check-equal? (vm-stack->strings enqueue-state-1)
                (list "stack holds 1 item"
                      (format "ptr[1] $~a04  (rt)" (format-hex-byte PAGE_AVAIL_0))))

  (check-equal? (memory-list enqueue-state-1 (+ PAGE_AVAIL_0_W #x06) (+ PAGE_AVAIL_0_W #x09))
                (list #x05 PAGE_AVAIL_1 #x01 #x00)
                "first entry is a ptr to a list, second entry is NIL")

  (check-equal? (vm-list->strings enqueue-state-1 (+ PAGE_AVAIL_1_W #x05))
                (list "int $0001"))

  (define enqueue-state-2
    (run-bc-wrapped-in-test
     (append
      (list
       (bc CALL) (word-ref FIFO_CREATE)
       (bc DUP)
       (bc DUP)
       (bc PUSH_I1)
       (bc SWAP)
       (bc CALL) (word-ref FIFO_ENQUEUE)
       (bc PUSH_I2)
       (bc SWAP)
       (bc CALL) (word-ref FIFO_ENQUEUE)
       (bc BRK))
      FIFO_CREATE
      FIFO_ENQUEUE)))

  (check-equal? (vm-stack->strings enqueue-state-2)
                (list "stack holds 1 item"
                      (format "ptr[1] $~a04  (rt)" (format-hex-byte PAGE_AVAIL_0))))

  (check-equal? (memory-list enqueue-state-2 (+ PAGE_AVAIL_0_W #x06) (+ PAGE_AVAIL_0_W #x09))
                (list #x09 PAGE_AVAIL_1 #x01 #x00)
                "first entry is a ptr to a list, second entry is NIL")

  (check-equal? (vm-list->strings enqueue-state-2 (+ PAGE_AVAIL_1_W #x09))
                (list "int $0002" "int $0001")))

(define FIFO_DEQUEUE ;; FIFO -> T
  (bc-resolve  
   (flatten
    (list
     (label FIFO_DEQUEUE)
            (byte 2)
            (bc WRITE_TO_L0)
            (bc GET_AF_1)
            (bc WRITE_TO_L1)
            (bc NIL_P)
            (bc F_P_BRA) (bc-rel-ref OUT_IS_NOT_EMPTY)
  
            (bc PUSH_NIL)                   ;; result for reverse
            (bc PUSH_L0)
            (bc GET_AF_0)
            ;; exception if result is NIL!
  
            (bc CALL) (word-ref REVERSE)
            (bc POP_TO_L1) 
            ;; set 'in' to be empty
            (bc PUSH_NIL)
            (bc PUSH_L0)
            (bc SET_AF_0)
  
     (label OUT_IS_NOT_EMPTY)
            (bc PUSH_L1_CDR)
            (bc PUSH_L0)
            (bc SET_AF_1)
            (bc PUSH_L1_CAR)
            (bc RET)))))

(module+ test #| dequeue |#
  (define dequeue-state-1
    (run-bc-wrapped-in-test
     (append
      (list
       (bc CALL) (word-ref FIFO_CREATE)
       (bc DUP)
       (bc DUP)
       (bc PUSH_I1)
       (bc SWAP)
       (bc CALL) (word-ref FIFO_ENQUEUE)
       (bc PUSH_I2)
       (bc SWAP)
       (bc CALL) (word-ref FIFO_ENQUEUE)
       (bc CALL) (word-ref FIFO_DEQUEUE)
       (bc BRK))
      FIFO_CREATE
      FIFO_ENQUEUE
      FIFO_DEQUEUE
      REVERSE)))

  (check-equal? (vm-stack->strings dequeue-state-1)
                (list "stack holds 1 item"
                      "int $0001  (rt)")
                "1 -> FIFO, 2 -> FIFO, FIFO -> 1")

  (define dequeue-state-2
    (run-bc-wrapped-in-test
     (append
      (list
       (bc CALL) (word-ref FIFO_CREATE)
       (bc DUP)
       (bc DUP)
       (bc PUSH_I1)
       (bc SWAP)
       (bc CALL) (word-ref FIFO_ENQUEUE)
       (bc PUSH_I2)
       (bc SWAP)
       (bc CALL) (word-ref FIFO_ENQUEUE)
       (bc DUP)
       (bc CALL) (word-ref FIFO_DEQUEUE)
       (bc SWAP)
       (bc CALL) (word-ref FIFO_DEQUEUE)
       (bc BRK))
      FIFO_CREATE
      FIFO_ENQUEUE
      FIFO_DEQUEUE
      REVERSE)
     ))

  (check-equal? (vm-stack->strings dequeue-state-2)
                (list "stack holds 2 items"
                      "int $0002  (rt)"
                      "int $0001")
                "1 -> FIFO, 2 -> FIFO, FIFO -> 1, FIFO ->2"))

(module+ test #| memory fifo |#
  (define mem-fifo-state-1
    (run-bc-wrapped-in-test
     (append
      (list
       (bc CALL) (word-ref FIFO_CREATE)
       (bc DUP)
       (bc DUP)
       (bc PUSH_I1)
       (bc SWAP)
       (bc CALL) (word-ref FIFO_ENQUEUE)
       (bc PUSH_I2)
       (bc SWAP)
       (bc CALL) (word-ref FIFO_ENQUEUE)
       (bc DUP)
       (bc CALL) (word-ref FIFO_DEQUEUE)
       (bc SWAP)
       (bc DUP)
       (bc CALL) (word-ref FIFO_DEQUEUE)
       (bc SWAP)
       (bc POP)
       (bc EXT) (bc GC_FL)
       (bc BRK))
      FIFO_CREATE
      FIFO_ENQUEUE
      FIFO_DEQUEUE
      REVERSE)
     ;; #t
     ))

  (check-equal? (vm-stack->strings mem-fifo-state-1)
                (list "stack holds 2 items"
                      "int $0002  (rt)"
                      "int $0001")
                "1 -> FIFO, 2 -> FIFO, FIFO -> 1, FIFO ->2")
  (check-equal? (vm-page->strings mem-fifo-state-1 PAGE_AVAIL_1)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     0"
                      "next free slot: $05")
                "cell pair page is completely freed after gc")
  (check-equal? (vm-page->strings mem-fifo-state-1 PAGE_AVAIL_0)
                (list "page-type:      m1 page p0"
                      "previous page:  $00"
                      "slots used:     0"
                      "next free slot: $04")
                "page with array is completely freed after gc")

  (define mem-fifo-state-2
    (run-bc-wrapped-in-test
     (append
      (list
       (bc CALL) (word-ref FIFO_CREATE)
       (bc DUP)
       (bc DUP)
       (bc PUSH_I1)
       (bc SWAP)
       (bc CALL) (word-ref FIFO_ENQUEUE)
       (bc PUSH_I2)
       (bc SWAP)
       (bc CALL) (word-ref FIFO_ENQUEUE)
       (bc DUP)
       (bc CALL) (word-ref FIFO_DEQUEUE)
       (bc SWAP)
       (bc CALL) (word-ref FIFO_DEQUEUE)
       (bc EXT) (bc GC_FL)
       (bc BRK))
      FIFO_CREATE
      FIFO_ENQUEUE
      FIFO_DEQUEUE
      REVERSE)
     ))

  (check-equal? (vm-stack->strings mem-fifo-state-2)
                (list "stack holds 2 items"
                      "int $0002  (rt)"
                      "int $0001")
                "1 -> FIFO, 2 -> FIFO, FIFO -> 1, FIFO ->2")
  (check-equal? (vm-page->strings mem-fifo-state-2 PAGE_AVAIL_1)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     0"
                      "next free slot: $05")
                "cell pair page is completely freed after gc")
  (check-equal? (vm-page->strings mem-fifo-state-2 PAGE_AVAIL_0)
                (list "page-type:      m1 page p0"
                      "previous page:  $00"
                      "slots used:     0"
                      "next free slot: $04")
                "page with array is completely freed after gc (currently failing, return seems not to free locals as it should)"))
