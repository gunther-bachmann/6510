#lang racket
#|

 provide syntax for all addressing modes available to the 6510.

 usage: (define-opcode XYZ ((implicit   . #xff)
                            (zero-page  . #xfe)
                            (indirect-x . #xfd)
                            (indirect-y . #xfc)))

 defines an opcode 'XYZ' with implicit, zero-page, indirect-x and indirect-y addressing modes
 implicit  : XYZ
 zero-page : XYZ $10
 indirect-x: XYZ ($10,x)
 indirect-y: XYZ ($10),y

|#

(require (for-syntax "6510-addressing-utils.rkt"))
(require (for-syntax "6510-syntax-utils.rkt"))

(require "6510-addressing-utils.rkt")
(provide (all-from-out "6510-addressing-utils.rkt"))

(require "../ast/6510-command.rkt")

(provide define-opcode)

(module+ test
  (require rackunit)
  (begin-for-syntax
    (require rackunit)))

(define-for-syntax (meta-info? stx) 
  (let ([datum (syntax->datum stx)])
    (and (list? datum)
       (equal? (car datum)
               '#:line))))

(module+ test #| meta-info? |#
  (begin-for-syntax
    (check-true (meta-info? #'(#:line 1 #:org-cmd "some")))))

;; define an opcode with a list of addressing modes and their respecitve byte encoding
(define-syntax (define-opcode stx)
    (syntax-case stx ()
      ([_ mnemonic addressing-modes]
       (with-syntax ((nstx (make-id stx "~a" #'nstx)))
         #`(define-syntax (mnemonic nstx)
             (syntax-case nstx ()
               ([_]              (no-op nstx #'addressing-modes))
               ([_ op]           (if (meta-info? #'op)
                                     (no-op-w-meta nstx #'addressing-modes #'op)
                                     (one-op nstx #'addressing-modes #'op)))
               ([_ op1 op2]      (if (meta-info? #'op1)
                                     (one-op-w-meta nstx #'addressing-modes #'op2 #'op1)
                                     (two-op nstx #'addressing-modes #'op1 #'op2)))
               ([_ meta op1 op2] (if (meta-info? #'meta)
                                     (two-op nstx #'addressing-modes #'op1 #'op2)
                                     (raise-addressing-error nstx #'addressings-modes 2)))))))))

(module+ test #| define-opcode |#
  ;; used by macro
  (require (only-in "6510-addressing-utils.rkt"
                    no-operand-opcode
                    zero-page-opcode
                    indirect-x-opcode
                    indirect-y-opcode)) 

  (define-opcode XYZ ((implicit   . #xff)
                      (zero-page  . #xfe)
                      (indirect-x . #xfd)
                      (indirect-y . #xfc)))

  (check-equal? (XYZ)
                (ast-opcode-cmd '()  '(#xff)))
  (check-equal? (XYZ (#:line 17 #:org-cmd "xyz"))
                (ast-opcode-cmd '(#:line 17 #:org-cmd "xyz") '(#xff)))
  (check-exn exn:fail? (λ () (expand #'(XYZ $))))
  (check-equal? (XYZ $10)
                (ast-opcode-cmd '()  '(#xfe #x10)))
  (check-equal? (XYZ (#:line 17 #:org-cmd) $10)
                (ast-opcode-cmd '(#:line 17 #:org-cmd) '(#xfe #x10)))
  (check-exn exn:fail? (λ () (expand #'(XYZ no no))))
  (check-equal? (XYZ ($10,x))
                (ast-opcode-cmd '()  '(#xfd #x10)))
  (check-equal? (XYZ (#:line 17 #:org-cmd) ($10,x))
                (ast-opcode-cmd '(#:line 17 #:org-cmd) '(#xfd #x10)))
  (check-equal? (XYZ  ($10),y)
                (ast-opcode-cmd '()  '(#xfc #x10)))
  (check-equal? (XYZ (#:line 17 #:org-cmd) ($10),y)
                (ast-opcode-cmd '()  '(#xfc #x10)))
  (check-exn exn:fail? (λ () (expand #'(XYZ no ($10),y)))))

;; transform the given to a no-operand (implicit) command
(define-for-syntax (no-op stx addressings-defs)
  (datum->syntax
   stx
   (cond
     [(implicit-addressing? addressings-defs)         
      `(no-operand-opcode 'implicit ',addressings-defs)]
     [else (raise-addressing-error stx addressings-defs 0)])))

(module+ test #| no-op |#
  (begin-for-syntax
    (check-equal?
     (syntax->datum (no-op #f #'((implicit . #x10))))
     '(no-operand-opcode 'implicit '((implicit . #x10))))))

(define-for-syntax (no-op-w-meta stx addressings-defs meta)
  (datum->syntax
   stx
   (cond
     [(implicit-addressing? addressings-defs)
      `(no-operand-opcode-w-meta 'implicit ',addressings-defs ',meta)]
     [else (raise-addressing-error stx addressings-defs 0)])))

(module+ test #| no-op-w-meta |#
  (begin-for-syntax
    (check-equal?
     (syntax->datum (no-op-w-meta #f #'((implicit . #x10)) #'(#:line 17 #:org-cmd)))
     '(no-operand-opcode-w-meta 'implicit '((implicit . #x10)) '(#:line 17 #:org-cmd)))))

;; transform the given (having one operand) to a valid operand command given the possible addressing-modes
(define-for-syntax (one-op stx addressings-defs op)
  (define possible-ambiguous-addressing '(zero-page  absolute))
  (datum->syntax
   stx
   (cond
     [(abs-or-zero-page-addressing? possible-ambiguous-addressing addressings-defs op)
      `(abs-or-zero-page-opcode ',possible-ambiguous-addressing ',addressings-defs ',op)]
     [(accumulator-addressing? addressings-defs op)
      `(no-operand-opcode 'accumulator ',addressings-defs)]
     [(byte-addressing? 'zero-page addressings-defs op)
      `(zero-page-opcode ',addressings-defs ',op)]
     [(relative-addressing? addressings-defs op)
      `(relative-opcode ',addressings-defs ',op)]
     [(word-addressing? 'absolute addressings-defs op)
      `(absolute-opcode ',addressings-defs ',op)]
     [(immediate-addressing? addressings-defs op)
      `(immediate-opcode ',addressings-defs ',op)]
     [(indirect-x-addressing? addressings-defs op)
      `(indirect-x-opcode ',addressings-defs ',op)]
     [(indirect-addressing? addressings-defs op)
      `(indirect-opcode ',addressings-defs ',op)]
     [#t (raise-addressing-error stx addressings-defs 1)])))

(define-for-syntax (one-op-w-meta stx addressings-defs op meta)
  (define possible-ambiguous-addressing '(zero-page  absolute))
  (datum->syntax
   stx
   (cond
     ;; TODO: fix the other adressing modes, too (zero-page and accumulator already done)
     [(abs-or-zero-page-addressing? possible-ambiguous-addressing addressings-defs op)
      `(abs-or-zero-page-opcode-w-meta ',possible-ambiguous-addressing ',addressings-defs ',op ',meta)]
     [(accumulator-addressing? addressings-defs op)
      `(no-operand-opcode-w-meta 'accumulator ',addressings-defs ',meta)]
     [(byte-addressing? 'zero-page addressings-defs op)
      `(zero-page-opcode-w-meta ',addressings-defs ',op ',meta)]
     [(relative-addressing? addressings-defs op)
      `(relative-opcode-w-meta ',addressings-defs ',op ',meta)]
     [(word-addressing? 'absolute addressings-defs op)
      `(absolute-opcode-w-meta ',addressings-defs ',op)]
     [(immediate-addressing? addressings-defs op)
      `(immediate-opcode-w-meta ',addressings-defs ',op ',meta)]
     [(indirect-x-addressing? addressings-defs op)
      `(indirect-x-opcode-w-meta ',addressings-defs ',op ',meta)]
     [(indirect-addressing? addressings-defs op)
      `(indirect-opcode-w-meta ',addressings-defs ',op ',meta)]
     [else (raise-addressing-error stx addressings-defs 1)])))

(module+ test #|one-op|#
  (begin-for-syntax
    (check-equal?
     (syntax->datum (one-op #f #'((accumulator . #x10)) #'A))
     (quote (no-operand-opcode 'accumulator '((accumulator . #x10)))))
    (check-equal?
     (syntax->datum (one-op-w-meta #f #'((accumulator . #x10)) #'A '(#:line 17 #:cmd "add a")))
     (quote (no-operand-opcode-w-meta 'accumulator '((accumulator . #x10)) '(#:line 17 #:cmd "add a"))))))

;; transform the given (having two operands) to a vliad operand command given the possible addressing-modes 
(define-for-syntax (two-op stx addressings-defs op1 op2)
  (define possible-ambiguous-indexed-addressing
    '((zero-page-x . ,x) (zero-page-y . ,y) (absolute-x . ,x) (absolute-y . ,y)))
  (datum->syntax
   stx
   (cond
     [(abs-or-zero-page-indexed-addressing? possible-ambiguous-indexed-addressing addressings-defs op1 op2)
      `(abs-or-zero-page-indexed-opcode ',possible-ambiguous-indexed-addressing ',addressings-defs ',op1 ',op2)]
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
     [else (raise-addressing-error stx addressings-defs 2)])))
