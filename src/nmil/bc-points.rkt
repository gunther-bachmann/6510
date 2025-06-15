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
;; (require "./vm-bc-ast.rkt")
;; (require (only-in "./vm-bc-resolver.rkt" bc-resolve bc-bytes))

(require "../6510.rkt")
(require [only-in "./vm-interpreter.rkt"
                  vm-interpreter
                  bc
                  ALLOC_ARA
                  SWAP_RA_RB
                  PUSH_RA
                  GET_RA_AF_0
                  GET_RA_AF_1
                  POP_TO_RA
                  POP_TO_RB
                  POP_TO_RA_AF
                  CALL
                  RET
                  ISUB
                  PUSH_B
                  PUSH_I
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
                  WRITE_L3])
(require (only-in "../tools/6510-interpreter.rkt" memory-list))

(module+ test #|  |#
  (require "../6510-test-utils.rkt")

  (require (only-in "./vm-interpreter-test-utils.rkt" run-bc-wrapped-in-test- vm-list->strings))
  (require (only-in "../cisc-vm/stack-virtual-machine.rkt" BRK))
  (require (only-in "../tools/6510-interpreter.rkt" cpu-state-clock-cycles peek))

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

(define POINT_CREATE ;; x :: y :: color -> point struct
  (list
   (label POINT_CREATE)
          (byte 1)
          (bc PUSH_B) (byte 3)
          (bc ALLOC_ARA)
          (bc POP_TO_RA_AF)
          (bc POP_TO_RA_AF)
          (bc POP_TO_RA_AF)
          (bc PUSH_RA)
          (bc RET)))

(module+ test #| point create |#
  (define point-create-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 100)
       (bc PUSH_I) (word 500)
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
       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 100)
       (bc PUSH_I) (word 500)
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
          (bc GET_AF_0)    ;; pop_to_ra, get_ra_af_0
          (bc SWAP)
          (bc GET_AF_0)    ;; pop_to_ra, get_ra_af_0
          (bc ISUB)
          (bc RET)))

(module+ test #| point xdist |#
  (define point-xdist-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 250)
       (bc PUSH_I) (word 600)
       (bc CALL) (word-ref POINT_CREATE)

       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 100)
       (bc PUSH_I) (word 500)
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
          (bc GET_AF_1)
          (bc SWAP)
          (bc GET_AF_1)
          (bc ISUB)
          (bc RET)))

(module+ test #| point ydist |#
  (define point-ydist-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 250)
       (bc PUSH_I) (word 600)
       (bc CALL) (word-ref POINT_CREATE)

       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 100)
       (bc PUSH_I) (word 500)
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
          (byte 0)
          (bc POP_TO_RA)
          (bc POP_TO_RB)
          (bc GET_RA_AF_0)
          (bc SWAP_RA_RB)
          (bc GET_RA_AF_0)
          (bc CELL_EQ_P)
          (bc F_P_RET_F)
          (bc GET_RA_AF_1)
          (bc SWAP_RA_RB)
          (bc GET_RA_AF_1)
          (bc CELL_EQ_P)
          (bc RET)))

(module+ test #| point equal |#
  (define point-equal-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 100)
       (bc PUSH_I) (word 500)
       (bc CALL) (word-ref POINT_CREATE)

       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 100)
       (bc PUSH_I) (word 500)
       (bc CALL) (word-ref POINT_CREATE)

       (bc CALL) (word-ref POINT_EQUAL)
       (bc BRK))
      POINT_CREATE
      POINT_EQUAL)
     ))

  (check-equal? (vm-stack->strings point-equal-1-state)
                (list "stack holds 1 item"
                      "int $0001  (rt)"))

  (inform-check-equal? (cpu-state-clock-cycles point-equal-1-state)
                       8361)

  (define point-equal-2-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 100)
       (bc PUSH_I) (word 500)
       (bc CALL) (word-ref POINT_CREATE)

       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 100)
       (bc PUSH_I) (word 499)
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
       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 199)
       (bc PUSH_I) (word 500)
       (bc CALL) (word-ref POINT_CREATE)

       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 100)
       (bc PUSH_I) (word 500)
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
       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 100)
       (bc PUSH_I) (word 500)
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
       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 100)
       (bc PUSH_I) (word 500)
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
