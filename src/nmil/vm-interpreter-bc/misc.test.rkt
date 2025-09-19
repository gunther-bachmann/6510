#lang racket/base


(module+ test
  (require (only-in "./misc.rkt"
                    BC_BNOP
                    BC_BREAK)
           "./test-utils.rkt")

  (define relevant-opcode-definitions (filtered-opcode-definitions
                                       (list "BC_BNOP"
                                             "BC_BREAK")))

  (define (wrap-bytecode-for-test bc-to-wrap)
    (wrap-bytecode-for-bc-test
     bc-to-wrap
     relevant-opcode-definitions
     (list  BC_BNOP)))

  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (run-bc-wrapped-in-test- bc wrapped-code debug)))

(module+ test #| nop |#
  (define nop-state
    (run-bc-wrapped-in-test
     (list (bc BNOP)
           (bc BNOP))))

  (inform-check-equal? (cpu-state-clock-cycles nop-state)
                27))

(module+ test #| bc_brk |#
  (define use-case-brk-state-after
    (run-bc-wrapped-in-test
     (list
      (bc BREAK))))

  (check-equal? (vm-next-instruction-bytes use-case-brk-state-after)
                (ast-bytes-cmd-bytes (bc BREAK))
                "stopped at byte code brk"))
