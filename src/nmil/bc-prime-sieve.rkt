#lang racket/base

#|

implementation of the sieve of Eratosthenes

create an array of boolean values (native array)
1st implementation: use cell array with byte values (as booleans)
2nd implementation: native array, 1 byte =  boolean value
3rd implementation: native array, 1 byte = 8 boolean values

functions to implement access and usage of native array


- create native array with 0s
- 0 = is (possibly) prime
- set # = 2
- mark all multiples (not including itself) of # up to max as 1 (non-prime)
- scan array for next (possibly) prime number
- set # = number found (next is 3, then 5 (since 4 is marked), then 7 ...)
- if # > max end the loop
- else loop

test: check the content of the array
primes (ignore 0,1) up to 30


- - 2 3 . 5 . 7 . . . 11 . 13 . . . 17 . 19 . . . 23 . . . . . 29 .
0 0 0 0 1 0 1 0 1 1 1 0  1 0  1 1 1 0  1 0  1 1 1 0  1 1 1 1 1 0  1  <--  array/memory content to check!



- cell array operations: index = byte, read/write cells
  PUSH_CELLARR_FIELD    tos = index(byte) :: cell-array-ptr                 -> tos = value (cell)
  POP_TO_CELLARR_FIELD  tos = index(byte) :: value (cell) :: cell-array-ptr -> []
  ALLOC_CELLARR         tos = size != 0                                    -> tos = cell-array-ptr

- native array operations: index = byte, read/write byte
  PUSH_NATARR_FIELD     tos = index(byte) :: array-ptr                      -> tos = value (byte)
  POP_TO_NATARR_FIELD   tos = index(byte) :: value(byte) :: array-ptr       -> []
  ALLOC_NATARR          tos = size != 0                                    -> tos = array-ptr

  write native array ra @ index rt -> local (byte)?
  write local byte -> native array ra @ index rt?


|#


(require (only-in racket/list flatten))
(require "./vm-bc-ast.rkt")
(require (only-in "../ast/6510-resolver.rkt"  add-label-suffix))
(require (only-in "./vm-bc-resolver.rkt" bc-resolve bc-bytes))

(require [only-in "./vm-interpreter.rkt"
                  vm-interpreter
                  bc
                  ALLOC_ARA
                  POP_TO_RA_AF
                  B_GT_P
                  NZ_P_BRA
                  GOTO
                  BDEC
                  POP_TO_RAI
                  BADD
                  RET
                  BINC
                  B_LT_P
                  PUSH_RA
                  CALL
                  PUSH_I
                  PUSH_B
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
(require "../6510.rkt")
(require (only-in "../tools/6510-interpreter.rkt" memory-list))

(module+ test #|  |#
  (require "../6510-test-utils.rkt")

  (require (only-in "./vm-interpreter-test-utils.rkt" run-bc-wrapped-in-test- vm-list->strings))
  (require (only-in "../cisc-vm/stack-virtual-machine.rkt" BRK))
  (require (only-in "../tools/6510-interpreter.rkt" cpu-state-clock-cycles))

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


;; stack: size (up to number)
(define PRIME_SIEVE
  (bc-resolve
   (flatten
  (list
   (label PRIME_SIEVE)
          (byte 1) ;; locals
          (bc WRITE_TO_L0)              ;; l0 = size/max num (as byte)
          (bc ALLOC_ARA)                ;; RA = cell-array
          (bc PUSH_L0)
   (label LOOP_ARR_INIT__PRIME_SIEVE)
          (bc PUSH_I0)
          (bc POP_TO_RA_AF)
          (bc BDEC)
          (bc DUP)
          (bc NZ_P_BRA) (bc-rel-ref LOOP_ARR_INIT__PRIME_SIEVE)

          (bc PUSH_B) (byte 02)         ;;                       stack: 2
          (bc WRITE_TO_L1)
   (label LOOP_MARKING__PRIME_SIEVE)
          (bc PUSH_L1)                  ;;                       stack: 2 :: 2
          (bc BADD)                     ;;                       stack: 4
          (bc DUP)                      ;;                       stack: 4 :: 4
          (bc PUSH_L0)                  ;;                       stack: size :: 4 :: 4
          (bc B_GT_P)
          (bc T_P_BRA) (bc-rel-ref DONE_MARKING__PRIME_SIEVE) ;;
          (bc DUP)                      ;;                       stack: 4 :: 4
          (bc POP_TO_RAI)               ;;                       stack: 4
          (bc PUSH_I0)                  ;;                       stack: 0 :: 4
          (bc POP_TO_RA_AF)             ;;                       stack: 4
          (bc GOTO) (bc-rel-ref LOOP_MARKING__PRIME_SIEVE)

   (label DONE_MARKING__PRIME_SIEVE)
          (bc PUSH_L1)                  ;;                       stack: 2
          (bc BINC)                     ;;                       stack: 3
          (bc WRITE_TO_L1)              ;;                       stack: 3
          (bc DUP)                      ;;                       stack: 3 :: 3
          (bc PUSH_L0)                  ;;                       stack: size :: 3 :: 3
          (bc B_LT_P)                   ;;                       stack: < :: 3
          (bc T_P_BRA) (bc-rel-ref ALL_DONE__PRIME_SIEVE) ;;                  stack: 3
          (bc GOTO) (bc-rel-ref LOOP_MARKING__PRIME_SIEVE) ;;                  stack: 3


   (label ALL_DONE__PRIME_SIEVE)                     ;;                       stack: 20+
          (bc POP)
          (bc PUSH_RA)                  ;;                       stack: array-ptr
          (bc RET)))))

(module+ test #| primve-sieve |#
  (define prime-sieve-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_B) (byte 20)
       (bc CALL) (word-ref PRIME_SIEVE)
       (bc BRK))
      (list (org #x8F00))
      PRIME_SIEVE)
     #t)))
