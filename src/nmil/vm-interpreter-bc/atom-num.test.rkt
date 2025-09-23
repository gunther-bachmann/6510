#lang racket/base

(module+ test
  (require (only-in "./atom-num.rkt"
                    BC_BINC
                    BC_BDEC
                    BC_BADD
                    BC_IMAX
                    BC_IINC
                    BC_IADD
                    BC_ISUB)
           (only-in "./ext.rkt"
                    BC_EXT1_CMD)
           (only-in "./misc.rkt"
                    BC_BNOP)
           (only-in "./push_const.rkt"
                    BC_PUSH_CONST_NUM_SHORT)
           (only-in "./push_n_pop.rkt"
                    BC_PUSH_B
                    BC_PUSH_I)
           "./test-utils.rkt")

  (define relevant-opcode-definitions (filtered-opcode-definitions
                                       (list "BC_BINC"
                                             "BC_BDEC"
                                             "BC_BADD"
                                             "BC_PUSH_B"
                                             "BC_PUSH_INT0"
                                             "BC_PUSH_INT1"
                                             "BC_PUSH_INT2"
                                             "BC_PUSH_INTm1"
                                             "BC_PUSH_I"
                                             "BC_IINC"
                                             "BC_IMAX"
                                             "BC_IADD"
                                             "BC_BNOP"
                                             "BC_ISUB")))

  (define (wrap-bytecode-for-test bc-to-wrap)
    (wrap-bytecode-for-bc-test
     bc-to-wrap
     relevant-opcode-definitions
     (list  BC_BINC
            BC_BDEC
            BC_BADD
            BC_IMAX
            BC_IINC
            BC_IADD
            BC_ISUB
            ;; ---
            BC_EXT1_CMD
            BC_PUSH_B
            BC_PUSH_I
            BC_PUSH_CONST_NUM_SHORT
            BC_BNOP)))

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

(module+ test #| ext max-int |#
  (define max-int-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I2)
       (bc PUSH_I1)
       (bc IMAX)))))

  (check-equal? (vm-stack->strings max-int-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)"))

  (define max-int-2-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I1)
       (bc PUSH_I2)
       (bc IMAX)))))

  (check-equal? (vm-stack->strings max-int-2-state)
                (list "stack holds 1 item"
                      "int $0002  (rt)")))

(module+ test #| inc int |#
  (define inc-int-0-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I0)
       (bc IINC)))))

  (check-equal? (vm-stack->strings inc-int-0-state)
                (list "stack holds 1 item"
                      "int $0001  (rt)"))

  (define inc-int-1-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I) (byte 255) (byte 0)
       (bc IINC)))
     ))

  (check-equal? (vm-stack->strings inc-int-1-state)
                (list "stack holds 1 item"
                      "int $0100  (rt)"))

  (define inc-int-2-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_IM1)
       (bc IINC)))
     ))

  (check-equal? (vm-stack->strings inc-int-2-state)
                (list "stack holds 1 item"
                      "int $0000  (rt)"))

  (define inc-int-3-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I) (byte 255) (byte 05)
       (bc IINC)))
     ))

  (check-equal? (vm-stack->strings inc-int-3-state)
                (list "stack holds 1 item"
                      "int $0600  (rt)")))

(module+ test #| IADD |#
  (define (bc-int-plus-state a b)
    (define ra (if (< a 0) (+ #x2000 a) a))
    (define rb (if (< b 0) (+ #x2000 b) b))
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I) (ast-bytes-cmd '() (list (high-byte ra) (low-byte ra)))
      (bc PUSH_I) (ast-bytes-cmd '() (list (high-byte rb) (low-byte rb)))
      (bc IADD)
      (bc BREAK))))

  (define (bc-int-plus-expectation state c)
    (check-equal? (vm-stack->strings state)
                  (list "stack holds 1 item"
                        (format  "int $~a  (rt)" (word->hex-string (if (< c 0) (+ #x2000 c) c))))))

  ;; Execute this test only, if major change to int + have been done
  ;; (define _run-bc-int-plus-tests
  ;;   (for/list ([j '(-4096 -4095 -256 -255 -10 -5 -1 0 1 5 10 255 256 4095)])
  ;;     (for/list ([i '(-4096 -4095 -256 -255 -10 -5 -1 0 1 5 10 255 256 4095)])
  ;;       (bc-int-plus-expectation (bc-int-plus-state i j) (+ i j)))))

  (define use-case-int-plus-state-after
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I1)
      (bc PUSH_I2)
      (bc BNOP)
      (bc IADD)                      ;; byte code for INT_PLUS = 3
      (bc PUSH_I) (byte #xf0 #x04)   ;; push int #x4f0 (1264)
      (bc PUSH_I) (byte #x1f #x01)   ;; push int #x11f (287)
      (bc IADD)                      ;; byte code for INT_PLUS (+ #x04f0 #x011f) (1551 = #x060f)
      (bc PUSH_I1)
      (bc PUSH_IM1)
      (bc BNOP)                      ;; reset clock cycles
      (bc IADD)                      ;; byte code for INT_PLUS = 0
      (bc BREAK))))

  (inform-check-equal? (cpu-state-clock-cycles use-case-int-plus-state-after)
                       333)
  (check-equal? (vm-stack->strings use-case-int-plus-state-after)
                   (list "stack holds 3 items"
                         "int $0000  (rt)"
                         "int $060f"
                         "int $0003"
                         )))


(module+ test #| ISUB |#
  (define (bc-int-minus-state a b)
    (define ra (if (< a 0) (+ #x2000 a) a))
    (define rb (if (< b 0) (+ #x2000 b) b))
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I) (ast-bytes-cmd '() (list (high-byte ra) (low-byte ra)))
      (bc PUSH_I) (ast-bytes-cmd '() (list (high-byte rb) (low-byte rb)))
      (bc ISUB)
      (bc BREAK))))

  (define (bc-int-minus-expectation state c)
    (check-equal? (vm-stack->strings state)
                    (list "stack holds 1 item"
                          (format  "int $~a  (rt)" (word->hex-string (if (< c 0) (+ #x2000 c) c))))))

  ;; Execute this test only, if major change to int - have been done
  ;; (define _run-bc-int-minus-tests
  ;;   (for/list ([j '(-4096 -4095 -256 -255 -10 -5 -1 0 1 5 10 255 256 4095)])
  ;;     (for/list ([i '(-4096 -4095 -256 -255 -10 -5 -1 0 1 5 10 255 256 4095)])
  ;;       (bc-int-minus-expectation (bc-int-minus-state i j) (- j i)))))


  (define use-case-int-minus-state-after
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I1)
      (bc PUSH_I2)
      (bc BNOP)
      (bc ISUB)                      ;; byte code for INT_MINUS = 2 - 1 = 1
      (bc PUSH_I) (byte #xf0 #x04) ;; push int #x4f0 (1264)
      (bc PUSH_I) (byte #x1f #x01) ;; push int #x11f (287)
      (bc ISUB)                      ;; byte code for INT_MINUS (287 - 1264 = -977 = #x1c2f)
      (bc PUSH_I1)
      (bc PUSH_I0)
      (bc ISUB)                      ;; byte code for INT_MINUS => -1
      (bc BREAK))))                    ;; brk


   (inform-check-equal? (cpu-state-clock-cycles use-case-int-minus-state-after)
                        880)
    (check-equal? (vm-stack->strings use-case-int-minus-state-after)
                    (list "stack holds 3 items"
                          "int $1fff  (rt)"
                          "int $1c2f"
                          "int $0001")))
