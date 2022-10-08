#lang racket

(require "6510-alt-utils.rkt")

(provide one-op two-op no-op)

(module+ test
  (require rackunit))

(define (resyntax new-context stx)
  (datum->syntax new-context (syntax->datum stx)))

(define (no-op stx addressings-defs)
  (resyntax
   stx
   (cond
     [(implicit-addressing? addressings-defs)
      (with-syntax
        ((result `(no-operand-opcode 'implicit #',addressings-defs)))
        #'`,result)]
     [#t (raise-addressing-error stx addressings-defs)])))

(module+ test #| no-op |#
  (check-equal?
   (syntax->datum (no-op #f #'((implicit . #x10))))
   '`,(no-operand-opcode 'implicit #'((implicit . #x10)))))

(define (one-op stx addressings-defs op)
  (resyntax
   stx
   (cond
     [(accumulator-addressing? addressings-defs op)
      (with-syntax ((result `(no-operand-opcode 'accumulator #',addressings-defs)))
        #'`,result)]
     [(byte-addressing? 'zero-page addressings-defs op)
      (with-syntax ((result `(zero-page-opcode #',addressings-defs #',op)))
        #'`,result)]
     [(byte-addressing? 'relative addressings-defs op)
      (with-syntax ((result `(relative-opcode #',addressings-defs #',op)))
        #'`,result)]
     [(word-addressing? 'absolute addressings-defs op)
      (with-syntax ((result `(absolute-opcode #',addressings-defs #',op)))
        #'`,result)]
     [(immediate-addressing? addressings-defs op)
      (with-syntax ((result `(immediate-opcode #',addressings-defs #',op)))
        #'`,result)]
     [(indirect-x-addressing? addressings-defs op)
      (with-syntax ((result `(indirect-x-opcode #',addressings-defs #',op)))
        #'`,result)]
     [(indirect-addressing? addressings-defs op)
      (with-syntax ((result `(indirect-opcode #',addressings-defs #',op)))
        #'`,result)]
     [#t (raise-addressing-error stx addressings-defs)])))

(module+ test #|one-op|#
  (check-equal?
     (syntax->datum (one-op #f #'((accumulator . #x10)) #'A))
     (quote (quasiquote (unquote (no-operand-opcode 'accumulator #'((accumulator . #x10))))))))

(define (two-op stx addressings-defs op1 op2)
  (resyntax
   stx
   (cond
     [(zero-page-indexed-addressing? 'zero-page-x ',x addressings-defs op1 op2)
      (with-syntax ((result `(zero-page-indexed-opcode 'zero-page-x #',addressings-defs #',op1)))
        #'`,result)]
     [(zero-page-indexed-addressing? 'zero-page-y ',y addressings-defs op1 op2)
      (with-syntax ((result `(zero-page-indexed-opcode 'zero-page-y #',addressings-defs #',op1)))
        #'`,result)]
     [(absolute-indexed-addressing? 'absolute-x ',x addressings-defs op1 op2)
      (with-syntax ((result `(absolute-indexed-opcode 'absolute-x #',addressings-defs #',op1)))
        #'`,result)]
     [(absolute-indexed-addressing? 'absolute-y ',y addressings-defs op1 op2)
      (with-syntax ((result `(absolute-indexed-opcode 'absolute-y #',addressings-defs #',op1)))
        #'`,result)]
     [(indirect-y-addressing? addressings-defs op1 op2)
      (with-syntax ((result (quasiquote (indirect-y-opcode #',addressings-defs #',op1))))
        #'`,result)]
     [#t (raise-addressing-error stx addressings-defs)])))
