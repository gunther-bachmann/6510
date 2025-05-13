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
(require "./bc-ast.rkt")
(require (only-in "./bc-resolver.rkt" bc-resolve bc-bytes))
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
                  PUSH_NIL
                  PUSH_LOCAL
                  PUSH_GLOBAL
                  PUSH_STRUCT_FIELD

                  POP_TO_LOCAL
                  POP_TO_GLOBAL))
(require [only-in "./vm-interpreter.rkt"
                  vm-interpreter
                  bc
                  PUSH_B
                  GC_FL
                  ALLOC_ARRAY
                  FALSE_P_RET_FALSE
                  GET_ARRAY_FIELD_0
                  GET_ARRAY_FIELD_1
                  GET_ARRAY_FIELD_2
                  GET_ARRAY_FIELD_3
                  SET_ARRAY_FIELD_0
                  SET_ARRAY_FIELD_1
                  SET_ARRAY_FIELD_2
                  SET_ARRAY_FIELD_3
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
(require (only-in "./vm-memory-manager.rkt" ZP_VM_PC shorten-cell-strings shorten-cell-string))
(require "../6510.rkt")
(require (only-in "../tools/6510-interpreter.rkt" memory-list))

(require (only-in "./vm-btree.rkt" REVERSE))

(module+ test #|  |#
  (require "../6510-test-utils.rkt")

  (require (only-in "./vm-interpreter-test-utils.rkt" run-bc-wrapped-in-test- vm-list->strings))
  (require (only-in "../cisc-vm/stack-virtual-machine.rkt" BRK))
  (require (only-in "../tools/6510-interpreter.rkt" cpu-state-clock-cycles peek))

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
            (list (org #xa000))
            vm-interpreter))

  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (run-bc-wrapped-in-test- bc wrapped-code debug)))

(define FIFO_CREATE ;;  -> point struct
  (list
   (label FIFO_CREATE)
          (byte 1)
          (bc PUSH_NIL)
          (bc PUSH_NIL)
          (bc PUSH_B) (byte 2)
          (bc ALLOC_ARRAY)
          (bc WRITE_TO_LOCAL_0)
          (bc SET_ARRAY_FIELD_0)
          (bc PUSH_LOCAL_0)
          (bc SET_ARRAY_FIELD_1)
          (bc PUSH_LOCAL_0)
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
          (bc WRITE_TO_LOCAL_0)
          (bc GET_ARRAY_FIELD_0)
          (bc SWAP)
          (bc CONS)
          (bc PUSH_LOCAL_0)
          (bc SET_ARRAY_FIELD_0)          
          (bc RET)))

(module+ test #| enqueue |#
  (define enqueue-state-1
    (run-bc-wrapped-in-test
     (append
      (list
       (bc CALL) (word-ref FIFO_CREATE)
       (bc DUP)
       (bc PUSH_INT_1)
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
       (bc PUSH_INT_1)
       (bc SWAP)
       (bc CALL) (word-ref FIFO_ENQUEUE)
       (bc PUSH_INT_2)
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
            (bc WRITE_TO_LOCAL_0)
            (bc GET_ARRAY_FIELD_1)
            (bc WRITE_TO_LOCAL_1)
            (bc NIL?)
            (bc FALSE_P_BRANCH) (bc-rel-ref OUT_IS_NOT_EMPTY)
  
            (bc PUSH_NIL)                   ;; result for reverse
            (bc PUSH_LOCAL_0)
            (bc GET_ARRAY_FIELD_0)
            ;; exception if result is NIL!
  
            (bc CALL) (word-ref REVERSE)
            (bc POP_TO_LOCAL_1) 
            ;; set 'in' to be empty
            (bc PUSH_NIL)
            (bc PUSH_LOCAL_0)
            (bc SET_ARRAY_FIELD_0)
  
     (label OUT_IS_NOT_EMPTY)
            (bc PUSH_LOCAL_1_CDR)
            (bc PUSH_LOCAL_0)
            (bc SET_ARRAY_FIELD_1)
            (bc PUSH_LOCAL_1_CAR)
            (bc RET)))))

(module+ test #| dequeue |#
  (define dequeue-state-1
    (run-bc-wrapped-in-test
     (append
      (list
       (bc CALL) (word-ref FIFO_CREATE)
       (bc DUP)
       (bc DUP)
       (bc PUSH_INT_1)
       (bc SWAP)
       (bc CALL) (word-ref FIFO_ENQUEUE)
       (bc PUSH_INT_2)
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
       (bc PUSH_INT_1)
       (bc SWAP)
       (bc CALL) (word-ref FIFO_ENQUEUE)
       (bc PUSH_INT_2)
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
       (bc PUSH_INT_1)
       (bc SWAP)
       (bc CALL) (word-ref FIFO_ENQUEUE)
       (bc PUSH_INT_2)
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
       (bc PUSH_INT_1)
       (bc SWAP)
       (bc CALL) (word-ref FIFO_ENQUEUE)
       (bc PUSH_INT_2)
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
