#lang racket

(require "6510-alt-utils.rkt")

(provide one-op two-op no-op)

(module+ test
  (require rackunit))

(define (no-op stx addressings-defs)
  (datum->syntax
   stx
   (cond
     [(implicit-addressing? addressings-defs)
      `(no-operand-opcode 'implicit ',addressings-defs)]
     [#t (raise-addressing-error stx addressings-defs)])))

(module+ test #| no-op |#
  (check-equal?
   (syntax->datum (no-op #f #'((implicit . #x10))))
   '(no-operand-opcode 'implicit '((implicit . #x10)))))

(define (one-op stx addressings-defs op)
  (datum->syntax
   stx
   (cond
     [(accumulator-addressing? addressings-defs op)
      `(no-operand-opcode 'accumulator ',addressings-defs)]
     [(byte-addressing? 'zero-page addressings-defs op)
      `(zero-page-opcode ',addressings-defs ',op)]
     [(byte-addressing? 'relative addressings-defs op)
      `(relative-opcode ',addressings-defs ',op)]
     [(word-addressing? 'absolute addressings-defs op)
      `(absolute-opcode ',addressings-defs ',op)]
     [(immediate-addressing? addressings-defs op)
      `(immediate-opcode ',addressings-defs ',op)]
     [(indirect-x-addressing? addressings-defs op)
      `(indirect-x-opcode ',addressings-defs ',op)]
     [(indirect-addressing? addressings-defs op)
      `(indirect-opcode ',addressings-defs ',op)]
     [#t (raise-addressing-error stx addressings-defs)])))

(module+ test #|one-op|#
  (check-equal?
     (syntax->datum (one-op #f #'((accumulator . #x10)) #'A))
     (quote (no-operand-opcode 'accumulator '((accumulator . #x10))))))

(define (two-op stx addressings-defs op1 op2)
  (define ambiguous-indexed-addressing '((zero-page-x . ,x) (zero-page-y . ,y) (absolute-x . ,x) (absolute-y . ,y)))
  (datum->syntax
   stx
   (cond
     [(abs-or-zero-page-indexed-addressing? 
       ambiguous-indexed-addressing
       addressings-defs op1 op2)
      `(abs-or-zero-page-indexed-opcode
        ',ambiguous-indexed-addressing
        ',addressings-defs ',op1 ',op2)]
     [(zero-page-indexed-addressing? 'zero-page-x ',x addressings-defs op1 op2)
      `(zero-page-indexed-opcode 'zero-page-x ',addressings-defs ',op1)]
     [(zero-page-indexed-addressing? 'zero-page-y ',y addressings-defs op1 op2)
      `(zero-page-indexed-opcode 'zero-page-y ',addressings-defs ',op1)]
     [(absolute-indexed-addressing? 'absolute-x ',x addressings-defs op1 op2)
      `(absolute-indexed-opcode 'absolute-x ',addressings-defs ',op1)]
     [(absolute-indexed-addressing? 'absolute-y ',y addressings-defs op1 op2)             
      `(absolute-indexed-opcode 'absolute-y ',addressings-defs ',op1)]
     [(indirect-y-addressing? addressings-defs op1 op2)      
      `(indirect-y-opcode ',addressings-defs ',op1)]
     [#t (raise-addressing-error stx addressings-defs)])))
