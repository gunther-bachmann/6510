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
(require (only-in "./vm-bc-opcode-definitions.rkt" bc))
(require (only-in "./vm-interpreter.rkt" vm-interpreter))

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


  (define PAGE_AVAIL_0 #x8a)
  (define PAGE_AVAIL_0_W #x8a00)
  (define PAGE_AVAIL_1 #x89)
  (define PAGE_AVAIL_1_W #x8900)

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
          (bc PUSH_B) (byte 2)  ;;                                stack: 2
          (bc ALLOC_ARA)        ;; RA = aptr -> [-,-]             stack: -
          (bc PUSH_NIL)         ;;                                stack: NIL
          (bc SET_RA_AF_0)      ;; RA = aptr -> [NIL, -]          stack: -
          (bc PUSH_NIL)         ;;                                stack: NIL
          (bc SET_RA_AF_1)      ;; RA = aptr -> [NIL, NIL]        stack: -
          (bc PUSH_RA)          ;;                                stack: aptr -> [NIL, NIL]
          (bc RET)))

(module+ test #| fifo create |#
  (define fifo-create-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc CALL) (word-ref FIFO_CREATE)
       (bc BREAK))
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
          (byte 0)
          (bc POP_TO_RA)
          (bc GET_RA_AF_0)
          (bc SWAP)
          (bc CONS)
          (bc SET_RA_AF_0)
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
       (bc BREAK))
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

  (inform-check-equal? (cpu-state-clock-cycles enqueue-state-1)
                       6806)

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
       (bc BREAK))
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
            (byte 1)
            (bc POP_TO_RA)      ;; RA = FIFO             stack: -
            (bc GET_RA_AF_1)    ;;                       stack: FIFO.out
            (bc WRITE_TO_L0)    ;; l0 = FIFO.out         stack: FIFO.out
            (bc NIL_P)          ;;                       stack: bool
            (bc F_P_BRA) (bc-rel-ref OUT_IS_NOT_EMPTY);; stack: -

            (bc PUSH_NIL)       ;; result for reverse    stack: NIL
            (bc GET_RA_AF_0)    ;;                       stack: FIFO.IN :: NIL
            ;; exception if result is NIL!

            (bc CALL) (word-ref REVERSE) ;;              stack: rev FIFO.IN
            (bc POP_TO_L0)      ;; l0 = ref FIFO.in      stack: -
            ;; set 'in' to be empty
            (bc PUSH_NIL)       ;;                       stack: NIL
            (bc SET_RA_AF_0)    ;;  FIFO.in = NIL        stack: -

     (label OUT_IS_NOT_EMPTY)
            (bc PUSH_L0_CDR)    ;;                       stack: cdr FIFO.out
            (bc SET_RA_AF_1)    ;; FIFO.out = cdr        stack: -
            (bc PUSH_L0_CAR)    ;;                       stack: car FIFO.out
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
       (bc BREAK))
      FIFO_CREATE
      FIFO_ENQUEUE
      FIFO_DEQUEUE
      REVERSE)
     ))

  (check-equal? (vm-stack->strings dequeue-state-1)
                (list "stack holds 1 item"
                      "int $0001  (rt)")
                "1 -> FIFO, 2 -> FIFO, FIFO -> 1")

  (inform-check-equal? (cpu-state-clock-cycles dequeue-state-1)
                       13046)

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
       (bc BREAK))
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
      (flatten
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
        (bc GC)
        (bc BREAK)))
      FIFO_CREATE
      FIFO_ENQUEUE
      FIFO_DEQUEUE
      REVERSE)
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
      (flatten
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
        (bc GC)
        (bc BREAK)))
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
