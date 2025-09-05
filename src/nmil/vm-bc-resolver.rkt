#lang racket/base

#|

resolve unresolved reference in byte code ast

functions
- bc-resolve :: resolve references to labels in byte code commands
- bc-bytes   :: find length in bytes of byte code commands

|#

(require (rename-in  racket/contract [define/contract define/c]))
(require (only-in racket/list empty? flatten))
(require (only-in "./vm-bc-ast.rkt"
                  bc-ast-rel-branch-reference
                  bc-ast-rel-branch-reference-label-ref
                  bc-ast-rel-branch-reference?
                  bc-cmd?
                  bc-rel-ref))
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

(provide bc-resolve bc-bytes)

(module+ test
  (require rackunit)
  (require "../6510.rkt")
  (require (only-in "./vm-bc-opcode-definitions.rkt" bc))
  (require (only-in "./vm-interpreter.rkt"
                    BNOP
                    WRITE_TO_L0
                    T_P_BRA
                    PUSH_L0
                    CALL
                    GOTO)))


;; collect all labels in this list of BC-AST-CMDS
;; and build/extend the returned LABEL-HASH
;; with the labels and their relative offset
;; to the first command +OFFSET passed
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

;; resolve the given BC-AST-CMDS
;; collect labels first and resolve with the collected labels
(define/c (bc-resolve bc-ast-cmds)
  (-> (listof bc-cmd?) (listof bc-cmd?))
  (bc-resolve- bc-ast-cmds (bc-collect-labels bc-ast-cmds)))

;; inner function
;; resolve the given BC-AST-CMDS into the RESULT list
;; using the given LABELS hash adding OFFSET
(define/c (bc-resolve- bc-ast-cmds labels (offset 0) (result (list)))
  (->* [(listof bc-cmd?) hash?] [integer? (listof bc-cmd?)] (listof bc-cmd?))  
  [cond
    ;; end condition, return result
    [(empty? bc-ast-cmds) (reverse result)]
    ;; resolve relative label
    [(bc-ast-rel-branch-reference? (car bc-ast-cmds))
     (define label (bc-ast-rel-branch-reference-label-ref (car bc-ast-cmds)))
     (define rel (- (hash-ref labels label) offset))
     (define bytes (list (if (> 0 rel)
                             (+ 257 rel) ;; 256 + 1 (+1 since the branch command has increment the offset already)
                             (sub1 rel))))  ;; (-1 since the branch bytecode has incremented the offset already
     (bc-resolve- (cdr bc-ast-cmds)
                 labels
                 (add1 offset)
                 (cons (ast-bytes-cmd '() bytes)
                       result))]
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
    ;; take regular ast-bytes-cmds as is
    [(ast-bytes-cmd? (car bc-ast-cmds))
     (bc-resolve-
      (cdr bc-ast-cmds)
      labels
      (+ offset (length (ast-bytes-cmd-bytes (car bc-ast-cmds))))
      (cons (car bc-ast-cmds) result))]
    ;; ignore label definitions
    [(ast-label-def-cmd? (car bc-ast-cmds))
     (bc-resolve- (cdr bc-ast-cmds) labels offset (cons (car bc-ast-cmds) result))]
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
                 (ast-label-def-cmd '() "first")
                 (ast-bytes-cmd '() (list BNOP))
                 (ast-label-def-cmd '() "next")
                 (ast-bytes-cmd '() (list GOTO))
                 (ast-bytes-cmd '() '(255))
                 (ast-bytes-cmd '() (list GOTO))
                 (ast-bytes-cmd '() '(3))
                 (ast-bytes-cmd '() (list CALL))
                 (ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd "yz"))
                 (ast-label-def-cmd '() "last")))


  (define bc-cmds-unresolved-2
    (flatten
     (list
      (label BTREE_VALIDATE)
             (byte 2) ;; locals (0 = node, 1 = car/cdr
             (bc WRITE_TO_L0)
             (bc CALL) (word-ref BTREE_NODE_P)
             (bc T_P_BRA) (bc-rel-ref IS_PAIR__BTREE_VALIDATE);; (byte 7) ;; jump to is-pair
             (bc PUSH_L0)
             (bc CALL) (word-ref BTREE_VALUE_P)
             (bc T_P_BRA) (byte 20) ;; jump to is-value
             (byte 2)               ;; BRK error, passed parameter is neither value nor node!

      (label IS_PAIR__BTREE_VALIDATE))))

  (check-equal? (bc-collect-labels bc-cmds-unresolved-2)
                (hash "BTREE_VALIDATE" 0 "IS_PAIR__BTREE_VALIDATE" 14))
  (check-equal? (bc-resolve bc-cmds-unresolved-2)
                (list
                 (ast-label-def-cmd '() "BTREE_VALIDATE")
                 (ast-bytes-cmd '() '(2))
                 (ast-bytes-cmd '() '(48))
                 (ast-bytes-cmd '() '(104))
                 (ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd "BTREE_NODE_P"))
                 (ast-bytes-cmd '() '(24))
                 (ast-bytes-cmd '() '(7))
                 (ast-bytes-cmd '() '(0))
                 (ast-bytes-cmd '() '(104))
                 (ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd "BTREE_VALUE_P"))
                 (ast-bytes-cmd '() '(24))
                 (ast-bytes-cmd '() '(20))
                 (ast-bytes-cmd '() '(2))
                 (ast-label-def-cmd '() "IS_PAIR__BTREE_VALIDATE"))))

;; calculate the number of bytes the given list of BC-CMDS actually take
;; useful for calculating the length of a sequence of byte code commands
(define/c (bc-bytes bc-cmds (result 0))
  (-> (listof bc-cmd?) integer?)
  (cond [(empty? bc-cmds) result] ;; done? return collected count
        [(ast-unresolved-bytes-cmd? (car bc-cmds)) ;; unresolved byte command depend on referenc len (word/byte)
         (define bytes-count
           (cond
             [(ast-resolve-word-scmd? (ast-unresolved-bytes-cmd-resolve-sub-command (car bc-cmds))) 2]
             [(ast-resolve-byte-scmd? (ast-unresolved-bytes-cmd-resolve-sub-command (car bc-cmds))) 1]
             [else (raise-user-error "unknown case")]))
         (bc-bytes (cdr bc-cmds) (+ result bytes-count))]
        [(ast-bytes-cmd? (car bc-cmds)) ;; ast-bytes-cmd simply counts the length of bytes in this command
         (define bytes-count (length (ast-bytes-cmd-bytes (car bc-cmds))))
         (when (= 0 bytes-count) (raise-user-error "suspicious byte count 0"))
         (bc-bytes (cdr bc-cmds) (+ result bytes-count))]
        [(ast-label-def-cmd? (car bc-cmds)) ;; labels count as 0
         (bc-bytes (cdr bc-cmds) result)]
        [else (raise-user-error "unknown case")]))
