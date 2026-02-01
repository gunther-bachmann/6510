#lang racket/base




;; execute benchmark function indexed by next byte
(module+ test #| test bc-bench |#
  (require "./test-utils.rkt"
           (only-in "./bench.rkt"
                  BC_BENCH)
           (only-in "./ext.rkt"
                    BC_EXT1_CMD)
           "../../6510.rkt")

  (define relevant-opcode-definitions (filtered-opcode-definitions
                                       (list "BC_BENCH" "BC_BREAK")))

  (define (wrap-bytecode-for-test bc-to-wrap)
    (wrap-bytecode-for-bc-test
     bc-to-wrap
     relevant-opcode-definitions
     (list BC_BENCH

           BC_EXT1_CMD)))

  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (run-bc-wrapped-in-test- bc wrapped-code debug)))

(module+ test #| bench |#
  (define bench-test
    (run-bc-wrapped-in-test
     (list
      (bc BENCH) (byte 00)) ;; wait for key press, writes it into ZP_RP
     #f))

  (check-equal? (memory-list bench-test ZP_RP)
                (list 1)))
