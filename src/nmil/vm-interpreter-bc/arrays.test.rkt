#lang racket/base

#|

  test of bytecode implementation of array commands

|#

(module+ test
  (require (only-in "./arrays.rkt"
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
                                             "BC_PUSH_B")))

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
       (bc PUSH_AF)))))

  (check-equal? (memory-list push-array-field-state (+ PAGE_AVAIL_0_W 05) (+ PAGE_AVAIL_0_W 29))
                (list 2      ;; refcnt = 2 (one reference on the stack, one in RA)
                      #x83   ;; page type = m1p3 (slot size 49, used 20*2)
                      20     ;; number of elements
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
                (list #x06 PAGE_AVAIL_0)
                "RA holds a pointer to the array, too")
  (check-equal? (vm-stack->strings push-array-field-state)
                (list "stack holds 3 items"
                      "int $0002  (rt)"
                      "int $0001"
                      (format "ptr[2] $~a06" (number->string PAGE_AVAIL_0 16)))))

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

  (check-equal? (vm-stack->strings pop-to-array-field-state)
                (list "stack holds 1 item"
                      (format "ptr[2] $~a06  (rt)" (number->string PAGE_AVAIL_0 16))))
  (check-equal? (memory-list pop-to-array-field-state (+ ZP_RA 0) (+ ZP_RA 1))
                (list #x06 PAGE_AVAIL_0)
                "RA holds a pointer to the array, too")
  (check-equal? (memory-list pop-to-array-field-state (+ PAGE_AVAIL_0_W 05) (+ PAGE_AVAIL_0_W 11))
                (list 2      ;; refcnt = 1 (one reference on the stack)
                      #x83   ;; page type = m1p3 (slot size 49, used 20*2)
                      20     ;; number of elements
                      0 0    ;; element 0
                      3 1))) ;; element 1 = int 1
