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

(require (only-in "./vm-bc-opcode-definitions.rkt" bc))
(require [only-in "./vm-interpreter.rkt"
                  vm-interpreter
                  WRITE_RA
                  WRITE_TO_RBI
                  DEC_RBI_NZ_P_BRA
                  DEC_RAI
                  WRITE_TO_RAI
                  ALLOC_ARA
                  POP_TO_RA_AF
                  BSHR
                  PUSH_RA_AF
                  B_GE_P
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


;; init loop cnt (e.g. # -> RAI/BI/CI
;; execute loop body
;; jump until done     DEC RAI/BI/CI, NZ_P_BRA

;; stack: size (up to number)
(define PRIME_SIEVE ;; currently using 46 byte
  (bc-resolve
   (flatten
  (list
   (label PRIME_SIEVE)
          (byte 3) ;; locals
          (bc WRITE_TO_L0)              ;; l0 = size/max num (as byte)
          (bc WRITE_TO_RBI)
          (bc DUP)
          (bc BSHR)
          (bc POP_TO_L2)                ;; l2 = size/2
          (bc ALLOC_ARA)                ;; RA = cell-array
   (label LOOP_ARR_INIT__PRIME_SIEVE)
          (bc PUSH_I0)                  ;;                       stack: 0
          (bc POP_TO_RA_AF)             ;; (RA),RAI = 0          stack: -
          (bc DEC_RBI_NZ_P_BRA) (bc-rel-ref LOOP_ARR_INIT__PRIME_SIEVE)

          (bc PUSH_B) (byte 02)         ;;                       stack: 2
          (bc WRITE_TO_L1)              ;; l1 = current candidate

   (label LOOP_MARKING__PRIME_SIEVE)
          (bc PUSH_L1)                  ;;                       stack: 2 :: 2
          (bc BADD)                     ;;                       stack: 4
          (bc DUP)                      ;;                       stack: 4 :: 4
          (bc PUSH_L0)                  ;;                       stack: size :: 4 :: 4
          (bc B_LT_P)                   ;;                       stack: < :: 4
          (bc T_P_BRA) (bc-rel-ref DONE_MARKING__PRIME_SIEVE) ;; stack: 4

          (bc WRITE_TO_RAI)             ;;                       stack: 4
          (bc DEC_RAI)

          (bc PUSH_I1)                  ;;                       stack: 1 :: 4
          (bc POP_TO_RA_AF)             ;;                       stack: 4
          (bc GOTO) (bc-rel-ref LOOP_MARKING__PRIME_SIEVE)

   (label DONE_MARKING__PRIME_SIEVE)
          (bc WRITE_L1)                 ;;                       stack: 2

   (label NEXT_CAND__PRIME_SIEVE)
          (bc WRITE_TO_RAI)             ;;                       stack: 2
          (bc BINC)                     ;;                       stack: 3
          ;; check whether half of size is reached => done
          (bc DUP)                      ;;                       stack: 3 :: 3
          (bc PUSH_L2)                  ;;                       stack: size/2 :: 3 :: 3
          (bc B_LT_P)                   ;;                       stack: size/2 < 3 :: 3
          (bc T_P_BRA) (bc-rel-ref ALL_DONE__PRIME_SIEVE) ;;     stack: 3

          (bc PUSH_RA_AF)               ;;                       stack: x :: 3
          (bc T_P_BRA) (bc-rel-ref NEXT_CAND__PRIME_SIEVE) ;;    stack: 3

          (bc WRITE_TO_L1)              ;;                       stack: 3
          (bc DUP)                      ;;                       stack: 3 :: 3
          (bc PUSH_L0)                  ;;                       stack: size :: 3 :: 3
          (bc B_GE_P)                   ;;                       stack: < :: 3
          (bc T_P_BRA) (bc-rel-ref LOOP_MARKING__PRIME_SIEVE) ;; stack: 3

   (label ALL_DONE__PRIME_SIEVE)        ;;                       stack: 20+
          (bc WRITE_RA)                 ;;                       stack: array-ptr
          (bc RET)))))

(module+ test #| primve-sieve |#
  (define prime-sieve-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_B) (byte 40)
       (bc CALL) (word-ref PRIME_SIEVE)  ;; calc primes in the range of 1..40
       (bc BREAK))
      (list (org #x8700))
      PRIME_SIEVE)
     ))

  (inform-check-equal? (cpu-state-clock-cycles prime-sieve-state)
                       76494)

  (check-equal? (memory-list prime-sieve-state (+ PAGE_AVAIL_0_W 3) (+ PAGE_AVAIL_0_W 85))
                (list #x01 #x83 40
                      #x03 #x00  ;; 1 prime
                      #x03 #x00  ;; 2 prime
                      #x03 #x00  ;; 3 prime
                      #x03 #x01  ;; 4
                      #x03 #x00  ;; 5 prime
                      #x03 #x01  ;; 6
                      #x03 #x00  ;; 7 prime
                      #x03 #x01  ;; 8
                      #x03 #x01  ;; 9
                      #x03 #x01  ;; 10
                      #x03 #x00  ;; 11 prime
                      #x03 #x01  ;; 12
                      #x03 #x00  ;; 13 prime
                      #x03 #x01  ;; 14
                      #x03 #x01  ;; 15
                      #x03 #x01  ;; 16
                      #x03 #x00  ;; 17 prime
                      #x03 #x01  ;; 18
                      #x03 #x00  ;; 19 prime
                      #x03 #x01  ;; 20
                      #x03 #x01  ;; 21
                      #x03 #x01  ;; 22
                      #x03 #x00  ;; 23 prime
                      #x03 #x01  ;; 24
                      #x03 #x01  ;; 25
                      #x03 #x01  ;; 26
                      #x03 #x01  ;; 27
                      #x03 #x01  ;; 28
                      #x03 #x00  ;; 29 prime
                      #x03 #x01  ;; 30
                      #x03 #x00  ;; 31 prime
                      #x03 #x01  ;; 32
                      #x03 #x01  ;; 33
                      #x03 #x01  ;; 34
                      #x03 #x01  ;; 35
                      #x03 #x01  ;; 36
                      #x03 #x00  ;; 37 prime
                      #x03 #x01  ;; 38
                      #x03 #x01  ;; 39
                      #x03 #x01  ;; 40
                      )))
