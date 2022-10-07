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
     [(implicit-addr-applies? addressings-defs)
      (with-syntax
        ((result `(opcode-for-addressing 'implicit #',addressings-defs)))
        #'`,result)]
     [#t (raise-addressing-error stx addressings-defs)])))

(module+ test #| no-op |#
  (check-equal?
   (syntax->datum (no-op #f #'((implicit . #x10))))
   '`,(opcode-for-addressing 'implicit #'((implicit . #x10)))))

(define (one-op stx addressings-defs op)
  (resyntax
   stx
   (cond
     [(accumulator-addr-applies? addressings-defs op)
      (with-syntax ((result `(opcode-for-addressing 'accumulator #',addressings-defs)))
        #'`,result)]
     [(byte-addr-applies? 'zero-page addressings-defs op)
      (with-syntax ((result `(opcode-for-zero-page-addr #',addressings-defs #',op)))
        #'`,result)]
     [(byte-addr-applies? 'relative addressings-defs op)
      (with-syntax ((result `(opcode-for-relative-addr #',addressings-defs #',op)))
        #'`,result)]
     [(word-addr-applies? 'absolute addressings-defs op)
      (with-syntax ((result `(opcode-for-absolute-addr #',addressings-defs #',op)))
        #'`,result)]
     [(immediate-addr-applies? addressings-defs op)
      (with-syntax ((result `(opcode-for-immediate-addr #',addressings-defs #',op)))
        #'`,result)]
     [(indirect-x-addr-applies? addressings-defs op)
      (with-syntax ((result `(opcode-for-indirect-x-addr #',addressings-defs #',op)))
        #'`,result)]
     [(indirect-addr-applies? addressings-defs op)
      (with-syntax ((result `(opcode-for-indirect-addr #',addressings-defs #',op)))
        #'`,result)]
     [#t (raise-addressing-error stx addressings-defs)])))

(module+ test #|one-op|#
  (check-equal?
     (syntax->datum (one-op #f #'((accumulator . #x10)) #'A))
     (quote (quasiquote (unquote (opcode-for-addressing 'accumulator #'((accumulator . #x10))))))))

(define (two-op stx addressings-defs op1 op2)
  (resyntax
   stx
   (cond
     [(zero-page-indexed-addr-applies? 'zero-page-x ',x addressings-defs op1 op2)
      (with-syntax ((result `(opcode-for-zero-page-indexed-addr 'zero-page-x #',addressings-defs #',op1)))
        #'`,result)]
     [(zero-page-indexed-addr-applies? 'zero-page-y ',y addressings-defs op1 op2)
      (with-syntax ((result `(opcode-for-zero-page-indexed-addr 'zero-page-y #',addressings-defs #',op1)))
        #'`,result)]
     [(absolute-indexed-addr-applies? 'absolute-x ',x addressings-defs op1 op2)
      (with-syntax ((result `(opcode-for-absolute-indexed-addr 'absolute-x #',addressings-defs #',op1)))
        #'`,result)]
     [(absolute-indexed-addr-applies? 'absolute-y ',y addressings-defs op1 op2)
      (with-syntax ((result `(opcode-for-absolute-indexed-addr 'absolute-y #',addressings-defs #',op1)))
        #'`,result)]
     [(indirect-y-addr-applies? addressings-defs op1 op2)
      (with-syntax ((result (quasiquote (opcode-for-indirect-y-addr #',addressings-defs #',op1))))
        #'`,result)]
     [#t (raise-addressing-error stx addressings-defs)])))
