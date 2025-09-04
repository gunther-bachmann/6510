#lang racket/base

#|

Byte Code Opcodes are completely defined here to be able to quickly switch between
- param encoded byte code
- single byte code
- multiple byte code (extended byte code)
depending on number of usage to make it as compact as possible!

|#

(require racket/contract)
(require (only-in "../util.rkt" bytes->int format-hex-byte format-hex-word))
(require (only-in racket/list take drop))
(require (only-in "../ast/6510-command.rkt" ast-command? ast-unresolved-bytes-cmd ast-resolve-word-scmd))

(provide bc-opcode-definitions
         (struct-out od-simple-bc)
         get-dyn-opcode-def
         write-opcode-into-optable
         disassemble-od-simple-bc
         byte-count-od-simple-bc)

(define-struct od-simple-bc
  (-label               ;; ast label into the implementation of this bc within the interpreter
   -byte-code           ;; the actual byte code
   -disassembler        ;; function to disassemble this byte code (and its operands)
   -byte-count)         ;; function returning the number of bytes used by this byte code and its operands
  #:transparent
  #:guard (struct-guard/c
           string?
           byte?
           (or/c string? (-> byte? byte? byte? string?))
           (or/c byte? (-> byte? byte? byte?))))

(define bc-opcode-definitions
  (list
   (od-simple-bc "BC_PUSH_I" #x0c
                 (lambda (_bc bc-p1 bc-p2) (format "push int $~a" (format-hex-word (bytes->int bc-p1 bc-p2))))
                 3)
   (od-simple-bc "BC_PUSH_LOCAL_SHORT" #x00 "push l0" 1)
   (od-simple-bc "BC_PUSH_LOCAL_SHORT" #x02 "push l1" 1)
   (od-simple-bc "BC_PUSH_LOCAL_SHORT" #x04 "push l2" 1)
   (od-simple-bc "BC_PUSH_LOCAL_SHORT" #x06 "push l3" 1)))

(define (disassemble-od-simple-bc dyn-opcode-def bc bc_p1 bc_p2)
  (define df (od-simple-bc--disassembler dyn-opcode-def))
  (if (string? df)
      df
      (apply df (list bc bc_p1 bc_p2))))

(define (byte-count-od-simple-bc dyn-opcode-def bc bc_p1)
  (define df (od-simple-bc--byte-count dyn-opcode-def))
  (if (integer? df)
      df
      (apply df (list bc bc_p1))))

(define (get-dyn-opcode-def bc)
  (findf (lambda (od)
           (and (od-simple-bc? od)
              (eq? (od-simple-bc--byte-code od) bc)))
         bc-opcode-definitions))

(define/contract (write-opcode-into-optable optable byte-code label)
  (-> (listof ast-command?) byte? string? (listof ast-command?))
  (define idx (add1 (arithmetic-shift byte-code -1)))
  (append
   (take optable idx)
   (list (ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd label)))
   (drop optable (add1 idx))))
