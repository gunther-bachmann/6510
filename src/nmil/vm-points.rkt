#lang racket/base

#|
 this file is implementing some functions around a
 (defs point
  (x int)
  (y int)
  (color string))


 xdist between two points:   x2-x1
 ydist between two points:   y2-y1
 equal (not comparing color)

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
(require (only-in "./vm-memory-manager.rkt" ZP_VM_PC cleanup-strings cleanup-string))
(require "../6510.rkt")
(require (only-in "../tools/6510-interpreter.rkt" memory-list))

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

(define POINT_CREATE ;; x :: y :: color -> point struct
  (list
   (label POINT_CREATE)
          (byte 1)
          (bc PUSH_BYTE) (byte 3)
          (bc ALLOC_ARRAY)
          (bc WRITE_TO_LOCAL_0)
          (bc SET_ARRAY_FIELD_0)
          (bc PUSH_LOCAL_0)
          (bc SET_ARRAY_FIELD_1)
          (bc PUSH_LOCAL_0)
          (bc SET_ARRAY_FIELD_2)
          (bc PUSH_LOCAL_0)
          (bc RET)))

(module+ test #| point create |#
  (define point-create-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_INT_0) ;; color is int0 (string will be implemented later)
       (bc PUSH_INT) (word 100)
       (bc PUSH_INT) (word 500)
       (bc CALL) (word-ref POINT_CREATE)
       (bc BRK))
      POINT_CREATE)
     ))

  (check-equal? (vm-stack->strings point-create-state)
                (list "stack holds 1 item"
                      (format "ptr[1] $~a04  (rt)" (format-hex-byte PAGE_AVAIL_0))))
  (check-equal? (vm-cell-at->string point-create-state (+ PAGE_AVAIL_0_W #x04) #f #t)
                "cell-array len=$03 [...]")
  (check-equal? (peek point-create-state(+ PAGE_AVAIL_0_W #x03))
                1
                "reference count is 1")
  (check-equal? (map (lambda (offset) (vm-cell-at->string point-create-state (+ PAGE_AVAIL_0_W offset) #f #t))
                     (list 06 08 10))
                (list "int $01f4" "int $0064" "int $0000")
                "the first three elements of the array are decimal 500, 100, 0")

  (define point-create-n-pop-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_INT_0) ;; color is int0 (string will be implemented later)
       (bc PUSH_INT) (word 100)
       (bc PUSH_INT) (word 500)
       (bc CALL) (word-ref POINT_CREATE)
       (bc POP)
       (bc BRK))
      POINT_CREATE)
     ))

  (check-equal? (vm-stack->strings point-create-n-pop-state)
                (list "stack is empty"))  
  (check-equal? (memory-list  point-create-n-pop-state (+ PAGE_AVAIL_0_W #x03) (+ PAGE_AVAIL_0_W #x04))
                (list #x00 #x0e)
                "reference count dropped to 0, next free is @0e"))

(define POINT_XDIST ;; point1 :: point2 -> int
  (list
   (label POINT_XDIST)
          (byte 0)
          (bc GET_ARRAY_FIELD_0)
          (bc SWAP)
          (bc GET_ARRAY_FIELD_0)
          (bc INT-)
          (bc RET)))

(module+ test #| point xdist |#
  (define point-xdist-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_INT_0) ;; color is int0 (string will be implemented later)
       (bc PUSH_INT) (word 250)
       (bc PUSH_INT) (word 600)
       (bc CALL) (word-ref POINT_CREATE)

       (bc PUSH_INT_0) ;; color is int0 (string will be implemented later)
       (bc PUSH_INT) (word 100)
       (bc PUSH_INT) (word 500)
       (bc CALL) (word-ref POINT_CREATE)

       (bc CALL) (word-ref POINT_XDIST)
       (bc BRK))
      POINT_CREATE
      POINT_XDIST)
     ))

  (check-equal? (vm-stack->strings point-xdist-1-state)
                (list "stack holds 1 item"
                      "int $0064  (rt)")))

(define POINT_YDIST ;; point1 :: point2 -> int
  (list
   (label POINT_YDIST)
          (byte 0)
          (bc GET_ARRAY_FIELD_1)
          (bc SWAP)
          (bc GET_ARRAY_FIELD_1)
          (bc INT-)
          (bc RET)))

(module+ test #| point ydist |#
  (define point-ydist-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_INT_0) ;; color is int0 (string will be implemented later)
       (bc PUSH_INT) (word 250)
       (bc PUSH_INT) (word 600)
       (bc CALL) (word-ref POINT_CREATE)

       (bc PUSH_INT_0) ;; color is int0 (string will be implemented later)
       (bc PUSH_INT) (word 100)
       (bc PUSH_INT) (word 500)
       (bc CALL) (word-ref POINT_CREATE)

       (bc CALL) (word-ref POINT_YDIST)
       (bc BRK))
      POINT_CREATE
      POINT_YDIST)
     ))

  (check-equal? (vm-stack->strings point-ydist-1-state)
                (list "stack holds 1 item"
                      "int $0096  (rt)")))

(define POINT_EQUAL ;; point1 :: point2 -> bool
  (list
   (label POINT_EQUAL)
          (byte 2)
          (bc WRITE_TO_LOCAL_0)
          (bc GET_ARRAY_FIELD_0)
          (bc SWAP)
          (bc WRITE_TO_LOCAL_1)
          (bc GET_ARRAY_FIELD_0)
          (bc CELL_EQ)
          (bc FALSE_P_RET_FALSE)
          (bc PUSH_LOCAL_0)
          (bc GET_ARRAY_FIELD_1)
          (bc PUSH_LOCAL_1)
          (bc GET_ARRAY_FIELD_1)
          (bc CELL_EQ)
          (bc RET)))

(module+ test #| point equal |#
  (define point-equal-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_INT_0) ;; color is int0 (string will be implemented later)
       (bc PUSH_INT) (word 100)
       (bc PUSH_INT) (word 500)
       (bc CALL) (word-ref POINT_CREATE)

       (bc PUSH_INT_0) ;; color is int0 (string will be implemented later)
       (bc PUSH_INT) (word 100)
       (bc PUSH_INT) (word 500)
       (bc CALL) (word-ref POINT_CREATE)

       (bc CALL) (word-ref POINT_EQUAL)
       (bc BRK))
      POINT_CREATE
      POINT_EQUAL)
     ))

  (check-equal? (vm-stack->strings point-equal-1-state)
                (list "stack holds 1 item"
                      "int $0001  (rt)"))

  (define point-equal-2-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_INT_0) ;; color is int0 (string will be implemented later)
       (bc PUSH_INT) (word 100)
       (bc PUSH_INT) (word 500)
       (bc CALL) (word-ref POINT_CREATE)

       (bc PUSH_INT_0) ;; color is int0 (string will be implemented later)
       (bc PUSH_INT) (word 100)
       (bc PUSH_INT) (word 499)
       (bc CALL) (word-ref POINT_CREATE)

       (bc CALL) (word-ref POINT_EQUAL)
       (bc BRK))
      POINT_CREATE
      POINT_EQUAL)
     ))

  (check-equal? (vm-stack->strings point-equal-2-state)
                (list "stack holds 1 item"
                      "int $0000  (rt)"))

  (define point-equal-3-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_INT_0) ;; color is int0 (string will be implemented later)
       (bc PUSH_INT) (word 199)
       (bc PUSH_INT) (word 500)
       (bc CALL) (word-ref POINT_CREATE)

       (bc PUSH_INT_0) ;; color is int0 (string will be implemented later)
       (bc PUSH_INT) (word 100)
       (bc PUSH_INT) (word 500)
       (bc CALL) (word-ref POINT_CREATE)

       (bc CALL) (word-ref POINT_EQUAL)
       (bc BRK))
      POINT_CREATE
      POINT_EQUAL)
     ))

  (check-equal? (vm-stack->strings point-equal-3-state)
                (list "stack holds 1 item"
                      "int $0000  (rt)")))

(module+ test #| memory check points |#
  (define mem-point-create-state-1
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_INT_0) ;; color is int0 (string will be implemented later)
       (bc PUSH_INT) (word 100)
       (bc PUSH_INT) (word 500)
       (bc CALL) (word-ref POINT_CREATE)
       (bc BRK))
      POINT_CREATE)
     ))

  (check-equal? (vm-cell-at->string mem-point-create-state-1 (+ PAGE_AVAIL_0_W #x04) #f #t)
                "cell-array len=$03 [...]")
  (check-equal? (peek mem-point-create-state-1 (+ PAGE_AVAIL_0_W #x03))
                1
                "reference count is 1")
  (check-equal? (map (lambda (offset) (vm-cell-at->string mem-point-create-state-1 (+ PAGE_AVAIL_0_W offset) #f #t))
                     (list 06 08 10))
                (list "int $01f4" "int $0064" "int $0000")
                "the first three elements of the array are decimal 500, 100, 0")

  (define mem-point-create-state-2
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_INT_0) ;; color is int0 (string will be implemented later)
       (bc PUSH_INT) (word 100)
       (bc PUSH_INT) (word 500)
       (bc CALL) (word-ref POINT_CREATE)
       (bc POP)
       (bc BRK))
      POINT_CREATE)
     ))

  (check-equal? (peek mem-point-create-state-2 (+ PAGE_AVAIL_0_W #x04))
                #x0e
                "next free slot on this page starts at $0e (eager collection of array)")
  (check-equal? (peek mem-point-create-state-2 (+ #xcf00 PAGE_AVAIL_0))
                #x04
                "first free slot on this page is at $04 (eager collection of array)")
  (check-equal? (peek mem-point-create-state-2 (+ PAGE_AVAIL_0_W #x03))
                0
                "reference count is 0"))
