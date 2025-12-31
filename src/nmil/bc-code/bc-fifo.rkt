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

(require (only-in racket/list flatten)
         "../../6510.rkt"
         (only-in "./bc-btree.rkt"
                  REVERSE)
         "../vm-bc-ast.rkt"
         (only-in "../vm-bc-opcode-definitions.rkt" bc)
         (only-in "../vm-bc-resolver.rkt"
                  bc-resolve
                  bc-bytes)
         (only-in "../vm-interpreter.rkt" vm-interpreter))

(module+ test #|  |#
  (require "./test-utils.rkt"))

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

  (pcheck-equal? (vm-stack->strings fifo-create-state)
                (list "stack holds 2 items"
                      (format "ptr[1] $~a02  (rt)" (format-hex-byte PAGE_AVAIL_0))
                      "ptr NIL"))
  (pcheck-equal? (vm-slot->string fifo-create-state (+ PAGE_AVAIL_0_W #x02))
                "ptr[1] cell-array of len 2")
  (pcheck-equal? (peek fifo-create-state(+ PAGE_AVAIL_0_W #x02))
                1
                "reference count is 1")
  (pcheck-equal? (map (lambda (offset) (vm-cell-at->string fifo-create-state (+ PAGE_AVAIL_0_W offset) #f #t))
                     (list 04 06))
                (list "ptr NIL" "ptr NIL")
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
      FIFO_ENQUEUE)
     ))

  (pcheck-equal? (vm-stack->strings enqueue-state-1)
                (list "stack holds 2 items"
                      (format "ptr[1] $~a02  (rt)" (format-hex-byte PAGE_AVAIL_0))
                      "ptr NIL"))

  (pcheck-equal? (memory-list enqueue-state-1 (+ PAGE_AVAIL_0_W #x04) (+ PAGE_AVAIL_0_W #x07))
                (list #x08 PAGE_AVAIL_0 #x00 #x00)
                "first entry is a ptr to a list, second entry is NIL")

  (pcheck-equal? (vm-list->strings enqueue-state-1 (+ PAGE_AVAIL_0_W #x08))
                (list "int $0001"))

  (inform-check-equal? (cpu-state-clock-cycles enqueue-state-1)
                       7999)

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

  (pcheck-equal? (vm-stack->strings enqueue-state-2)
                (list "stack holds 2 items"
                      (format "ptr[1] $~a02  (rt)" (format-hex-byte PAGE_AVAIL_0))
                      "ptr NIL"))

  (pcheck-equal? (memory-list enqueue-state-2 (+ PAGE_AVAIL_0_W #x04) (+ PAGE_AVAIL_0_W #x07))
                (list #x0e PAGE_AVAIL_0 #x00 #x00)
                "first entry is a ptr to a list, second entry is NIL")

  (pcheck-equal? (vm-list->strings enqueue-state-2 (+ PAGE_AVAIL_0_W #x0e))
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

  (pcheck-equal? (vm-stack->strings dequeue-state-1)
                (list "stack holds 2 items"
                      "int $0001  (rt)"
                      "ptr NIL")
                "1 -> FIFO, 2 -> FIFO, FIFO -> 1")

  (inform-check-equal? (cpu-state-clock-cycles dequeue-state-1)
                       14577)

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

  (pcheck-equal? (vm-stack->strings dequeue-state-2)
                (list "stack holds 3 items"
                      "int $0002  (rt)"
                      "int $0001"
                      "ptr NIL")
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

  (pcheck-equal? (vm-stack->strings mem-fifo-state-1)
                (list "stack holds 3 items"
                      "int $0002  (rt)"
                      "int $0001"
                      "ptr NIL")
                "1 -> FIFO, 2 -> FIFO, FIFO -> 1, FIFO ->2")
  ;; (pcheck-equal? (vm-page->strings mem-fifo-state-1 PAGE_AVAIL_1)
  ;;               (list "page-type:      cell-pair page"
  ;;                     "previous page:  $00"
  ;;                     "slots used:     0"
  ;;                     "next free slot: $05")
  ;;               "cell pair page is completely freed after gc")
  (pcheck-equal? (vm-page->strings mem-fifo-state-1 PAGE_AVAIL_0)
                (list "page-type:      m1 page p0"
                      "previous page:  $00"
                      "slots used:     0"
                      "next free slot: $02")
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

  (pcheck-equal? (vm-stack->strings mem-fifo-state-2)
                (list "stack holds 3 items"
                      "int $0002  (rt)"
                      "int $0001"
                      "ptr NIL")
                "1 -> FIFO, 2 -> FIFO, FIFO -> 1, FIFO ->2")
  ;; (pcheck-equal? (vm-page->strings mem-fifo-state-2 PAGE_AVAIL_1)
  ;;               (list "page-type:      cell-pair page"
  ;;                     "previous page:  $00"
  ;;                     "slots used:     0"
  ;;                     "next free slot: $05")
  ;;               "cell pair page is completely freed after gc")
  (pcheck-equal? (vm-page->strings mem-fifo-state-2 PAGE_AVAIL_0)
                (list "page-type:      m1 page p0"
                      "previous page:  $00"
                      "slots used:     0"
                      "next free slot: $02")
                "page with array is completely freed after gc (currently failing, return seems not to free locals as it should)"))
