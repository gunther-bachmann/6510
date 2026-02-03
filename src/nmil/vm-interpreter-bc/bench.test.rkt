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
  (define (bench-test-code)
    (run-bc-wrapped-in-test
     (list
             (bc BENCH) (byte $05) ;; fill screen
             (bc BENCH) (byte $00) ;; wait for keypress
             (bc BENCH) (byte $01) ;; start timer
             ;; (bc BENCH) (byte $06) ;; scroll right 40 times <- quite expensive for the interpreter
             (bc BENCH) (byte $02) ;; stop timer
             (bc BENCH) (byte $03) ;; report timer
             (bc BENCH) (byte $00) ;; wait for keypress
             (bc BENCH) (byte $04) ;; do warmstart
             )
     #f))

  (define bench-test (bench-test-code))

  ;; (require profile)
  ;; (profile-thunk bench-test-code)

  (check-equal? (memory-list bench-test ZP_RP)
                (list 1)))
