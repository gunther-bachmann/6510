#lang racket/base

#|

resolve unresolved reference in byte code ast

|#

(require (rename-in  racket/contract [define/contract define/c]))
(require (only-in racket/list empty? flatten))
(require (only-in "./bc-ast.rkt"
                  bc-ast-rel-branch-reference
                  bc-ast-rel-branch-reference-label-ref
                  bc-ast-rel-branch-reference?
                  bc-cmd?))
(require (only-in "../ast/6510-command.rkt"
                  ast-label-def-cmd
                  ast-label-def-cmd?
                  ast-label-def-cmd-label
                  ast-bytes-cmd
                  ast-bytes-cmd?
                  ast-bytes-cmd-bytes
                  ast-unresolved-bytes-cmd?
                  ast-resolve-byte-scmd?
                  ast-resolve-word-scmd?
                  ast-unresolved-bytes-cmd-resolve-sub-command))

(provide bc-resolve)

(module+ test
  (require rackunit)
  (require "../6510.rkt")
  (require (only-in "./vm-interpreter.rkt" bc BNOP))
  (require (only-in "../cisc-vm/stack-virtual-machine.rkt" GOTO CALL)))


(define/c (bc-collect-labels bc-ast-cmds (offset 0) (label-hash (hash)))
  (->* [(listof bc-cmd?)] [integer? hash?] hash?)
  (cond
    ;; end condition, return result
    [(empty? bc-ast-cmds) label-hash]
    [(ast-label-def-cmd? (car bc-ast-cmds))
     (bc-collect-labels (cdr bc-ast-cmds)
                        offset
                        (hash-set label-hash (ast-label-def-cmd-label (car bc-ast-cmds)) offset))]
    [(ast-unresolved-bytes-cmd? (car bc-ast-cmds))
     (bc-collect-labels (cdr bc-ast-cmds)
                        (+ (cond
                             [(ast-resolve-word-scmd? (ast-unresolved-bytes-cmd-resolve-sub-command (car bc-ast-cmds))) 2]
                             [(ast-resolve-byte-scmd? (ast-unresolved-bytes-cmd-resolve-sub-command (car bc-ast-cmds))) 1]
                             [else (raise-user-error "unknown case")])
                           offset)
                        label-hash)]
    [(ast-bytes-cmd? (car bc-ast-cmds))
     (bc-collect-labels (cdr bc-ast-cmds)
                        (+ (length (ast-bytes-cmd-bytes (car bc-ast-cmds))) offset)
                        label-hash)]
    [else (raise-user-error "unknown condition")]))

(module+ test #| collect labels |#
  (check-equal? (bc-collect-labels
                 (flatten
                  (list
                   (label first)
                          (bc BNOP)
                   (label next)
                          (bc GOTO) (bc-ast-rel-branch-reference '() (list 0) "first")
                          (bc CALL) (word-ref yz)
                   (label last))))
                (hash "first" 0 "next" 1 "last" 6)))

(define/c (bc-resolve bc-ast-cmds)
  (-> (listof bc-cmd?) (listof bc-cmd?))
  (bc-resolve- bc-ast-cmds (bc-collect-labels bc-ast-cmds)))

(define/c (bc-resolve- bc-ast-cmds labels (offset 0) (result (list)))
  (->* [(listof bc-cmd?) hash?] [integer? (listof bc-cmd?)] (listof bc-cmd?))  
  [cond
    ;; end condition, return result
    [(empty? bc-ast-cmds) (reverse result)]
    ;; resolve relative label
    [(bc-ast-rel-branch-reference? (car bc-ast-cmds))
     (define rel (- (hash-ref labels
                              (bc-ast-rel-branch-reference-label-ref (car bc-ast-cmds)))
                    offset))
     (bc-resolve- (cdr bc-ast-cmds)
                 labels
                 (add1 offset)
                 (cons (ast-bytes-cmd
                        '()
                        (list (if (> 0 rel)
                                  (+ 257 rel) ;; 256 + 1 (+1 since the branch command has increment the offset already)
                                  (sub1 rel)))) ;; (-1 since the branch bytecode has incremented the offset already
                       result))]
    ;; take regular ast-bytes-cmds as is
    [(ast-bytes-cmd? (car bc-ast-cmds))
     (bc-resolve-
      (cdr bc-ast-cmds)
      labels
      (+ offset (length (ast-bytes-cmd-bytes (car bc-ast-cmds))))
      (cons (car bc-ast-cmds) result))]
    ;; take unresolved bytes as is
    [(ast-unresolved-bytes-cmd? (car bc-ast-cmds))
     (bc-resolve- (cdr bc-ast-cmds)
                 labels
                 (+ (cond
                      [(ast-resolve-word-scmd? (ast-unresolved-bytes-cmd-resolve-sub-command (car bc-ast-cmds))) 2]
                      [(ast-resolve-byte-scmd? (ast-unresolved-bytes-cmd-resolve-sub-command (car bc-ast-cmds))) 1]
                      [else (raise-user-error "unknown case")])
                    offset)
                 (cons (car bc-ast-cmds) result))]
    ;; ignore label definitions
    [(ast-label-def-cmd? (car bc-ast-cmds))
     (bc-resolve- (cdr bc-ast-cmds) labels offset result)]
    [else (raise-user-error "unknown case")]])

(module+ test #| resolve |#
  (define bc-cmds-unresolved
    (flatten
     (list
      (label first)
             (bc BNOP)
      (label next)
             (bc GOTO) (bc-ast-rel-branch-reference '() (list 0) "first")
             (bc GOTO) (bc-ast-rel-branch-reference '() (list 0) "last")
             (bc CALL) (word-ref yz)
      (label last))))

  (check-equal? (bc-resolve- bc-cmds-unresolved (bc-collect-labels bc-cmds-unresolved))
                (list
                 (ast-bytes-cmd '() (list BNOP))
                 (ast-bytes-cmd '() (list GOTO))
                 (ast-bytes-cmd '() '(255))
                 (ast-bytes-cmd '() (list GOTO))
                 (ast-bytes-cmd '() '(3))
                 (ast-bytes-cmd '() (list CALL))
                 (ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd "yz")))))
