#lang racket/base

#|

Byte Code Opcodes are completely defined here to be able to quickly switch between
- param encoded byte code
- single byte code
- multiple byte code (extended byte code)
depending on number of usage to make it as compact as possible!

|#

(require racket/contract/base)
(require (only-in "../util.rkt" bytes->int format-hex-byte format-hex-word))

(provide bc-opcode-definitions
         (struct-out od-simple-bc)
         get-dyn-opcode-def)

(define-struct od-simple-bc
  (-label               ;; ast label into the implementation of this bc within the interpreter
   -byte-code           ;; the actual byte code
   -disassembler        ;; function to disassemble this byte code (and its operands)
   -byte-count)         ;; function returning the number of bytes used by this byte code and its operands
  #:transparent
  #:guard (struct-guard/c
           string?
           byte?
           (-> byte? byte? byte? string?)
           (-> byte? byte? byte?)))

(define bc-opcode-definitions
  (list
   (od-simple-bc "BC_PUSH_I" #x0c
                 (lambda (_bc bc-p1 bc-p2) (format "push int ~a" (format-hex-word (bytes->int bc-p1 bc-p2))))
                 (lambda (_bc _bc-p1) 3))))

(define (get-dyn-opcode-def bc)
  (findf (lambda (od)
           (and (od-simple-bc? od)
              (eq? od-simple-bc--byte-code bc)))
         bc-opcode-definitions))
