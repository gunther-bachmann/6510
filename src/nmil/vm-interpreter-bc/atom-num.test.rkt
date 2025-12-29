#lang racket/base

(module+ test
  (require (only-in "./atom-num.rkt"
                    BC_BINC
                    BC_BDEC
                    BC_BADD
                    BC_IMAX
                    BC_IINC
                    BC_IADD
                    BC_ISUB
                    bc-atom-num-code)
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
                                             "BC_ISUB"
                                             "BC_BREAK")))

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
       (bc BINC)))
     ))

  (check-equal? (vm-stack-n->strings binc-20)
                (list "stack holds 2 items"
                      "byte $15  (rt)"
                      "ptr NIL")))

(module+ test #| bdec |#
  (define bdec-20
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_B) (byte #x14)
       (bc BDEC)))
     ))

  (check-equal? (vm-stack-n->strings bdec-20)
                (list "stack holds 2 items"
                      "byte $13  (rt)"
                      "ptr NIL")))

(module+ test #| badd |#
  (define badd-20-9
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_B) (byte #x14)
       (bc PUSH_B) (byte #x09)
       (bc BADD)))))

  (check-equal? (vm-stack-n->strings badd-20-9)
                (list "stack holds 2 items"
                      "byte $1d  (rt)"
                      "ptr NIL")))

(module+ test #| ext max-int |#
  (define max-int-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I2)
       (bc PUSH_I1)
       (bc IMAX)))
     ))

  (check-equal? (vm-stack-n->strings max-int-state)
                (list "stack holds 2 items"
                      "int $0002  (rt)"
                      "ptr NIL"))

  (define max-int-2-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I1)
       (bc PUSH_I2)
       (bc IMAX)))))

  (check-equal? (vm-stack-n->strings max-int-2-state)
                (list "stack holds 2 items"
                      "int $0002  (rt)"
                      "ptr NIL")))

(module+ test #| inc int |#
  (define inc-int-0-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I0)
       (bc IINC)))))

  (check-equal? (vm-stack-n->strings inc-int-0-state)
                (list "stack holds 2 items"
                      "int $0001  (rt)"
                      "ptr NIL"))

  (define inc-int-1-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I) (byte 255) (byte 0)
       (bc IINC)))
     ))

  (check-equal? (vm-stack-n->strings inc-int-1-state)
                (list "stack holds 2 items"
                      "int $0100  (rt)"
                      "ptr NIL"))

  (define inc-int-2-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_IM1)
       (bc IINC)))
     ))

  (check-equal? (vm-stack-n->strings inc-int-2-state)
                (list "stack holds 2 items"
                      "int $0000  (rt)"
                      "ptr NIL"))

  (define inc-int-3-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I) (byte 255) (byte 05)
       (bc IINC)))
     ))

  (check-equal? (vm-stack-n->strings inc-int-3-state)
                (list "stack holds 2 items"
                      "int $0600  (rt)"
                      "ptr NIL")))

(module+ test #| IADD |#
  (define (bc-int-plus-state a b)
    (define ra (if (< a 0) (+ #x2000 a) a))
    (define rb (if (< b 0) (+ #x2000 b) b))
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I) (ast-bytes-cmd '() (list (high-byte ra) (low-byte ra)))
      (bc PUSH_I) (ast-bytes-cmd '() (list (high-byte rb) (low-byte rb)))
      (bc IADD))))

  (define (bc-int-plus-expectation state c)
    (check-equal? (vm-stack-n->strings state)
                  (list "stack holds 2 items"
                        (format  "int $~a  (rt)" (word->hex-string (if (< c 0) (+ #x2000 c) c)))
                        "ptr NIL")))

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
      ;;(bc BNOP)
      (bc IADD)                      ;; byte code for INT_PLUS = 3
      (bc PUSH_I) (byte #xf0 #x04)   ;; push int #x4f0 (1264)
      (bc PUSH_I) (byte #x1f #x01)   ;; push int #x11f (287)
      (bc IADD)                      ;; byte code for INT_PLUS (+ #x04f0 #x011f) (1551 = #x060f)
      (bc PUSH_I1)
      (bc PUSH_IM1)
      (bc BNOP)                      ;; reset clock cycles
      (bc IADD)                      ;; byte code for INT_PLUS = 0
      )))

  (inform-check-equal? (cpu-state-clock-cycles use-case-int-plus-state-after)
                       84)
  (check-equal? (vm-stack-n->strings use-case-int-plus-state-after)
                   (list "stack holds 4 items"
                         "int $0000  (rt)"
                         "int $060f"
                         "int $0003"
                         "ptr NIL"
                         )))


(module+ test #| ISUB |#
  (define (bc-int-minus-state a b)
    (define ra (if (< a 0) (+ #x2000 a) a))
    (define rb (if (< b 0) (+ #x2000 b) b))
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I) (ast-bytes-cmd '() (list (high-byte ra) (low-byte ra)))
      (bc PUSH_I) (ast-bytes-cmd '() (list (high-byte rb) (low-byte rb)))
      (bc ISUB))))

  (define (bc-int-minus-expectation state c)
    (check-equal? (vm-stack-n->strings state)
                    (list "stack holds 2 item"
                          (format  "int $~a  (rt)" (word->hex-string (if (< c 0) (+ #x2000 c) c)))
                          "ptr NIL")))

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
      (bc ISUB)                      ;; byte code for INT_MINUS = 2 - 1 = 1
      (bc PUSH_I) (byte #xf0 #x04) ;; push int #x4f0 (1264)
      (bc PUSH_I) (byte #x1f #x01) ;; push int #x11f (287)
      (bc ISUB)                      ;; byte code for INT_MINUS (287 - 1264 = -977 = #x3c2f)
      (bc PUSH_I1)
      (bc PUSH_I0)
      (bc BNOP)
      (bc ISUB)                      ;; byte code for INT_MINUS => -1
      )))


   (inform-check-equal? (cpu-state-clock-cycles use-case-int-minus-state-after)
                        84)
    (check-equal? (vm-stack-n->strings use-case-int-minus-state-after)
                    (list "stack holds 4 items"
                          "int $3fff  (rt)"
                          "int $3c2f"
                          "int $0001"
                          "ptr NIL")))

(module+ test
  (inform-check-equal? (code-len bc-atom-num-code)
                       152
                       "code len for byte and int arithmetic"))
