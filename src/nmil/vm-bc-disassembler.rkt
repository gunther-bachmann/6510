#lang racket/base

(provide disassembler-byte-code--byte-count
         disassemble-byte-code)

#|

 disassembler for byte code

 |#

(require (only-in "../util.rkt"
                  bytes->int
                  format-hex-byte
                  format-hex-word)
         (only-in "./vm-bc-opcode-definitions.rkt"
                  bc-opcode-definitions
                  od-simple-bc?
                  od-simple-bc--byte-code
                  od-simple-bc--disassembler
                  od-simple-bc--byte-count
                  od-extended-bc?
                  od-extended-bc--sub-commands
                  find-dyn-opcode-def
                  get-dyn-opcode-simple-def
                  disassemble-od-simple-bc
                  byte-count-od-simple-bc))

;; get count of bytes belonging to the given bc command
(define (disassembler-byte-code--byte-count bc (bc_p1 0) (bc_p2 0))
  (define dyn-opcode-def (find-dyn-opcode-def bc))
  (cond
    [(and dyn-opcode-def (od-simple-bc? dyn-opcode-def))
     (byte-count-od-simple-bc dyn-opcode-def bc bc_p1)]
    [(and dyn-opcode-def (od-extended-bc? dyn-opcode-def))
     (add1 (byte-count-od-simple-bc
            (find-dyn-opcode-def bc_p1 (od-extended-bc--sub-commands dyn-opcode-def))
            bc_p1
            bc_p2))]
    [else (raise-user-error (format "unknown bc: ~a" (format-hex-byte bc)))]))                          ;; default is 1 byte (for regular byte code command)

(module+ test #| disassemble |#
  (require "../6510-test-utils.rkt")
  (check-equal? (disassembler-byte-code--byte-count #x00)
                1
                "push l0")

  (check-equal? (disassembler-byte-code--byte-count #x0c #x10 #x3f)
                3
                "push int $3f10")

  (check-equal? (disassembler-byte-code--byte-count #x08 #x01)
                2
                "int max"))

;; return disassembled string for bc (and byte 1, byte 2 thereafter)
(define (disassemble-byte-code bc (bc_p1 0) (bc_p2 0) #:labels (labels (hash)))
  (define dyn-opcode-def (get-dyn-opcode-simple-def bc bc_p1))
  (cond
    [dyn-opcode-def (disassemble-od-simple-bc dyn-opcode-def labels bc bc_p1 bc_p2)]
    [else "unknown bc"]))

(module+ test #| disassemble |#
  (require "../6510-test-utils.rkt")
  (check-equal? (disassemble-byte-code #x00)
                "PUSH_L0\t(push l0)")

  (check-equal? (disassemble-byte-code #x0c #x10 #x3f)
                "PUSH_I\t(push int $3f10)")

  (check-equal? (disassemble-byte-code #x08 #x01)
                "IMAX\t(int max)"))
