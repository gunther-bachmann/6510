#lang racket/base

(module+ test
  (require "./vm-interpreter-bc.test-utils.rkt")
  (require (only-in "./vm-interpreter-bc.atom-num.rkt"
                    BC_BINC
                    BC_BDEC
                    BC_BADD))
  (require (only-in "./vm-interpreter-bc.push_n_pop.rkt" BC_PUSH_B))

  (define relevant-opcode-definitions (filtered-opcode-definitions
                                       (list "BC_BINC"
                                             "BC_BDEC"
                                             "BC_BADD"
                                             "BC_PUSH_B")))

  (define (wrap-bytecode-for-test bc-to-wrap)
    (wrap-bytecode-for-bc-test
     bc-to-wrap
     relevant-opcode-definitions
     (list  BC_BINC
            BC_BDEC
            BC_BADD
            ;; ---
            BC_PUSH_B)))

  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (run-bc-wrapped-in-test- bc wrapped-code debug)))


(module+ test #| binc |#
  (define binc-20
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_B) (byte #x14)
       (bc BINC)))))

  (check-equal? (vm-stack->strings binc-20)
                (list "stack holds 1 item"
                      "byte $15  (rt)")))

(module+ test #| bdec |#
  (define bdec-20
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_B) (byte #x14)
       (bc BDEC)))))

  (check-equal? (vm-stack->strings bdec-20)
                (list "stack holds 1 item"
                      "byte $13  (rt)")))

(module+ test #| badd |#
  (define badd-20-9
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_B) (byte #x14)
       (bc PUSH_B) (byte #x09)
       (bc BADD)))))

  (check-equal? (vm-stack->strings badd-20-9)
                (list "stack holds 1 item"
                      "byte $1d  (rt)")))
