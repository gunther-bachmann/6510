#lang racket/base

#|

disassembler for byte code

|#
;; (require (only-in racket/list findf))
(require (only-in "../util.rkt" bytes->int format-hex-byte format-hex-word))
(require (only-in "../tools/6510-disassembler.rkt" info-for-label))

(require (only-in "./vm-bc-opcode-definitions.rkt"
                  bc-opcode-definitions
                  od-simple-bc?
                  od-simple-bc--byte-code
                  od-simple-bc--disassembler
                  od-simple-bc--byte-count
                  get-dyn-opcode-def
                  disassemble-od-simple-bc
                  byte-count-od-simple-bc))

(provide disassembler-byte-code--byte-count
         disassemble-byte-code)

;; get count of bytes belonging to the given bc command
(define (disassembler-byte-code--byte-count bc (bc_p1 0))
  (define dyn-opcode-def (get-dyn-opcode-def bc))
  (cond
    [dyn-opcode-def (byte-count-od-simple-bc dyn-opcode-def bc bc_p1)]
    [else 1]))                          ;; default is 1 byte (for regular byte code command)

;; return disassembled string for bc (and byte 1, byte 2 thereafter)
(define (disassemble-byte-code bc (bc_p1 0) (bc_p2 0) #:labels (labels (hash)))
  (define dyn-opcode-def (get-dyn-opcode-def bc))
  (cond
    [dyn-opcode-def (disassemble-od-simple-bc dyn-opcode-def labels bc bc_p1 bc_p2)]
    [else "unknown bc"]))

(module+ test #| disassemble |#
  (require "../6510-test-utils.rkt")
  (check-equal? (disassemble-byte-code #x00)
                "push l0")

  (check-equal? (disassemble-byte-code #x0c #x10 #x3f)
                "push int $3f10"))
