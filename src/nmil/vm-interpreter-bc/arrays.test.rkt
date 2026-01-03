#lang racket/base

#|

  test of bytecode implementation of array commands

|#

(module+ test
  (require (only-in "../test-utils.rkt"
                    regression-test)
           (only-in "./arrays.rkt"
                    BC_DEC_RBI_NZ_P_BRA
                    BC_PUSH_AF
                    BC_POP_TO_AF
                    BC_ALLOC_ARA
                    BC_PUSH_RA)
           (only-in "./branch.rkt"
                    BC_T_P_BRA)
           (only-in "./push_const.rkt"
                    BC_PUSH_CONST_NUM_SHORT)
           (only-in "./push_n_pop.rkt"
                    BC_PUSH_B
                    BC_DUP
                    BC_SWAP)
           "./test-utils.rkt")

  (define relevant-opcode-definitions (filtered-opcode-definitions
                                       (list "BC_DEC_RBI_NZ_P_BRA"
                                             "BC_PUSH_AF"
                                             "BC_POP_TO_AF"
                                             "BC_ALLOC_ARA"
                                             "BC_PUSH_RA"

                                             "BC_DUP"
                                             "BC_SWAP"
                                             "BC_T_P_BRA"
                                             "BC_PUSH_INT1"
                                             "BC_PUSH_INT2"
                                             "BC_PUSH_B"
                                             "BC_BREAK")))

  (define (wrap-bytecode-for-test bc-to-wrap)
    (wrap-bytecode-for-bc-test
     bc-to-wrap
     relevant-opcode-definitions
     (list  BC_DEC_RBI_NZ_P_BRA
            BC_PUSH_AF
            BC_POP_TO_AF
            BC_ALLOC_ARA
            BC_PUSH_RA
            ;; ---
            BC_T_P_BRA
            BC_DUP
            BC_SWAP
            BC_PUSH_B
            BC_PUSH_CONST_NUM_SHORT)))

  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (run-bc-wrapped-in-test- bc wrapped-code debug)))


(module+ test #| push array field |#
  (define push-array-field-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_B) (byte 20)
       (bc ALLOC_ARA)
       (bc PUSH_RA)
       (bc DUP) ;; make sure to keep a reference to this array, otherwise it is freed!
       (bc PUSH_I1)
       (bc SWAP)
       (bc PUSH_B) (byte 1)
       (bc POP_TO_AF)

       (bc DUP)
       (bc PUSH_I2)
       (bc SWAP)
       (bc PUSH_B) (byte 10)
       (bc POP_TO_AF)

       (bc DUP)
       (bc DUP)
       (bc PUSH_B) (byte 1)
       (bc PUSH_AF)

       (bc SWAP)
       (bc PUSH_B) (byte 10)
       (bc PUSH_AF)))
     ))

  (regression-test
   push-array-field-state
   "alloc array -> pop 1 to array field 1 -> pop 2 to array field 10 -> push af 1 -> push af 10 "
   (check-equal? (memory-list push-array-field-state (+ PAGE_AVAIL_0_W 02) (+ PAGE_AVAIL_0_W 25))
                 (list 2      ;; refcnt = 2 (one reference on the stack, one in RA)
                       #x14   ;; page type = cell-array with 20 elements
                       0 0    ;; element 0
                       3 1
                       0 0
                       0 0
                       0 0
                       0 0
                       0 0
                       0 0
                       0 0
                       0 0
                       3 2   ;; element 10
                       ))
   (check-equal? (memory-list push-array-field-state (+ ZP_RA 0) (+ ZP_RA 1))
                 (list #x02 PAGE_AVAIL_0)
                 "RA holds a pointer to the array, too")
   (check-equal? (vm-stack->strings push-array-field-state)
                 (list "stack holds 4 items"
                       "int $0002  (rt)"
                       "int $0001"
                       (format "ptr[2] $~a02" (number->string PAGE_AVAIL_0 16))
                       "ptr NIL"))))

(module+ test #| pop to array field |#
  (define pop-to-array-field-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_B) (byte 20)
      (bc ALLOC_ARA)
      (bc PUSH_RA)
      (bc DUP) ;; make sure to keep a reference to this array, otherwise it is freed!
      (bc PUSH_I1)
      (bc SWAP)
      (bc PUSH_B) (byte 1)
      (bc POP_TO_AF))
     ))

  (regression-test
   pop-to-array-field-state
   "alloc array -> pop 1 to array field 1"
   (check-equal? (vm-stack->strings pop-to-array-field-state)
                 (list "stack holds 2 items"
                       (format "ptr[2] $~a02  (rt)" (number->string PAGE_AVAIL_0 16))
                       "ptr NIL"))
   (check-equal? (memory-list pop-to-array-field-state (+ ZP_RA 0) (+ ZP_RA 1))
                 (list #x02 PAGE_AVAIL_0)
                 "RA holds a pointer to the array, too")
   (check-equal? (memory-list pop-to-array-field-state (+ PAGE_AVAIL_0_W 02) (+ PAGE_AVAIL_0_W 7))
                 (list 2      ;; refcnt = 1 (one reference on the stack)
                       #x14   ;; page type + length of 20
                       0 0    ;; element 0
                       3 1)))) ;; element 1 = int 1
