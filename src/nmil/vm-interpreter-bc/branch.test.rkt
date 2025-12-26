#lang racket/base

(module+ test
  (require (only-in "./branch.rkt"
                       BC_T_P_BRA
                       BC_F_P_BRA
                       BC_GOTO)
            (only-in "./push_const.rkt"
                     BC_PUSH_CONST_NUM_SHORT)
             "./test-utils.rkt")

  (define relevant-opcode-definitions (filtered-opcode-definitions
                                       (list "BC_PUSH_INT0"               ;; PUSH_I0
                                             "BC_PUSH_INT1"               ;; ..
                                             "BC_PUSH_INT2"               ;; ..
                                             "BC_PUSH_INTm1"              ;; PUSH_IM1
                                             "BC_BREAK"
                                             "BC_T_P_BRA"
                                             "BC_F_P_BRA"
                                             "BC_GOTO")))

  (define (wrap-bytecode-for-test bc-to-wrap)
    (wrap-bytecode-for-bc-test
     bc-to-wrap
     relevant-opcode-definitions
     (list BC_T_P_BRA
           BC_F_P_BRA
           BC_GOTO
           ;; ---
           BC_PUSH_CONST_NUM_SHORT)))

  (define (run-bc-wrapped-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (run-bc-wrapped-in-test- bc wrapped-code debug)))

(module+ test #| branch true |#
  (define branch-true-0-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I1)
      (bc T_P_BRA) (byte 2)
      (bc PUSH_I1)
      (bc BREAK)
      (bc PUSH_I2))))
  (check-equal? (vm-stack-n->strings branch-true-0-state)
                (list "stack holds 2 items"
                      "int $0002  (rt)"
                      "ptr NIL"))

  (define branch-true-1-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I1)
      (bc T_P_BRA) (byte $75)
      (bc BREAK)
      (org-align #x78)
      (bc PUSH_I2))))
  (check-equal? (vm-stack-n->strings branch-true-1-state)
                (list "stack holds 2 items"
                      "int $0002  (rt)"
                      "ptr NIL"))

  (define branch-true-2-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I1)
       (bc T_P_BRA) (byte $7d)
       (bc BREAK)
       (org-align #x80)
       (bc PUSH_I1)
       (bc T_P_BRA) (byte $6d)
       (bc BREAK)
       (org-align #xf0)
       (bc PUSH_I1)
       ;; 80f1
       (bc T_P_BRA) (byte $0d)
       (build-list 13 (lambda (_i) (bc BREAK)))
       ;; now at 8100
       (bc PUSH_I2)))
   ))
  (check-equal? (vm-stack-n->strings branch-true-2-state)
                (list "stack holds 2 items"
                      "int $0002  (rt)"
                      "ptr NIL"))

  (define branch-true-3-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I1)
       (bc T_P_BRA) (byte $7d)
       (bc BREAK)
       (org-align #x80)
       (bc PUSH_I1)
       ;; now at 8081
       (bc T_P_BRA) (byte $6d)
       ;; 8083
       (bc BREAK)
       (org-align #xf0)
       (bc PUSH_I1)
       ;; now at 80f1
       (bc T_P_BRA) (byte $0e)
       (build-list 14 (lambda (_i) (bc BREAK)))
       ;; now at 8102
       (bc PUSH_I2)))
   ))
  (check-equal? (vm-stack-n->strings branch-true-3-state)
                (list "stack holds 2 items"
                      "int $0002  (rt)"
                      "ptr NIL"))

  (define branch-true-4-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I1)
       (bc T_P_BRA) (byte 3)
       (bc BREAK)
       (bc PUSH_I2)
       (bc BREAK)
       (bc PUSH_I1)
       (bc T_P_BRA) (byte $fd)))
     ))
  (check-equal? (vm-stack-n->strings branch-true-4-state)
                (list "stack holds 2 items"
                      "int $0002  (rt)"
                      "ptr NIL"))

  (define branch-true-5-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I1)
       (bc T_P_BRA) (byte $7d)
       (bc BREAK)
       (org-align #x80)
       (bc PUSH_I1)
       ;; now at 8081
       (bc T_P_BRA) (byte $6d)
       ;; 8083
       (bc BREAK)
       (org-align #xf0)
       (bc PUSH_I1)
       ;; now at 80f1
       (bc T_P_BRA) (byte $0e)
       (build-list 12 (lambda (_i) (bc BREAK)))
       ;; 80ff
       (bc PUSH_I2)
       ;; 8100
       (bc BREAK)
       ;; now at 8101
       (bc PUSH_I1)
       (bc T_P_BRA) (byte $fd)))
     ))
  (check-equal? (vm-stack-n->strings branch-true-5-state)
                (list "stack holds 2 items"
                      "int $0002  (rt)"
                      "ptr NIL")))

(module+ test #| branch true |#
  (define branch-false-0-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc F_P_BRA) (byte 2)
      (bc PUSH_I0)
      (bc BREAK)
      (bc PUSH_I2))
     ))
  (check-equal? (vm-stack-n->strings branch-false-0-state)
                (list "stack holds 2 items"
                      "int $0002  (rt)"
                      "ptr NIL"))

  (define branch-false-1-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc F_P_BRA) (byte $75)
      (bc BREAK)
      (org-align #x78)
      (bc PUSH_I2))))
  (check-equal? (vm-stack-n->strings branch-false-1-state)
                (list "stack holds 2 items"
                      "int $0002  (rt)"
                      "ptr NIL"))

  (define branch-false-2-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I0)
       (bc F_P_BRA) (byte $7d)
       (bc BREAK)
       (org-align #x80)
       (bc PUSH_I0)
       (bc F_P_BRA) (byte $6d)
       (bc BREAK)
       (org-align #xf0)
       (bc PUSH_I0)
       ;; 80f1
       (bc F_P_BRA) (byte $0d)
       (build-list 13 (lambda (_i) (bc BREAK)))
       ;; now at 8100
       (bc PUSH_I2)))
   ))
  (check-equal? (vm-stack-n->strings branch-false-2-state)
                (list "stack holds 2 items"
                      "int $0002  (rt)"
                      "ptr NIL"))

  (define branch-false-3-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I0)
       (bc F_P_BRA) (byte $7d)
       (bc BREAK)
       (org-align #x80)
       (bc PUSH_I0)
       ;; now at 8081
       (bc F_P_BRA) (byte $6d)
       ;; 8083
       (bc BREAK)
       (org-align #xf0)
       (bc PUSH_I0)
       ;; now at 80f1
       (bc F_P_BRA) (byte $0e)
       (build-list 14 (lambda (_i) (bc BREAK)))
       ;; now at 8102
       (bc PUSH_I2)))
   ))
  (check-equal? (vm-stack-n->strings branch-false-3-state)
                (list "stack holds 2 items"
                      "int $0002  (rt)"
                      "ptr NIL"))

  (define branch-false-4-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I0)
       (bc F_P_BRA) (byte 3)
       (bc BREAK)
       (bc PUSH_I2)
       (bc BREAK)
       (bc PUSH_I0)
       (bc F_P_BRA) (byte $fd)))
     ))
  (check-equal? (vm-stack-n->strings branch-false-4-state)
                (list "stack holds 2 items"
                      "int $0002  (rt)"
                      "ptr NIL"))

  (define branch-false-5-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I0)
       (bc F_P_BRA) (byte $7d)
       (bc BREAK)
       (org-align #x80)
       (bc PUSH_I0)
       ;; now at 8081
       (bc F_P_BRA) (byte $6d)
       ;; 8083
       (bc BREAK)
       (org-align #xf0)
       (bc PUSH_I0)
       ;; now at 80f1
       (bc F_P_BRA) (byte $0e)
       (build-list 12 (lambda (_i) (bc BREAK)))
       ;; 80ff
       (bc PUSH_I2)
       ;; 8100
       (bc BREAK)
       ;; now at 8101
       (bc PUSH_I0)
       (bc F_P_BRA) (byte $fd)))
     ))
  (check-equal? (vm-stack-n->strings branch-false-5-state)
                (list "stack holds 2 items"
                      "int $0002  (rt)"
                      "ptr NIL")))

(module+ test #| goto |#
  (define goto-0-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc GOTO) (byte 2)
      (bc PUSH_IM1)
      (bc BREAK)
      (bc PUSH_I1))))
  (check-equal? (vm-stack-n->strings goto-0-state)
                (list "stack holds 3 items"
                      "int $0001  (rt)"
                      "int $0000"
                      "ptr NIL"))

  (define goto-1-state
    (run-bc-wrapped-in-test
     (list
      (bc PUSH_I0)
      (bc GOTO) (byte $75)
      (bc BREAK)
      (org-align #x78)
      (bc PUSH_I1))
     ))
  (check-equal? (vm-stack-n->strings goto-1-state)
                (list "stack holds 3 items"
                      "int $0001  (rt)"
                      "int $0000"
                      "ptr NIL"))

  (define goto-2-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I0)
       (bc GOTO) (byte $7d)
       (bc BREAK)
       (org-align #x80)
       (bc PUSH_I1)
       (bc GOTO) (byte $6d)
       (bc BREAK)
       (org-align #xf0)
       (bc PUSH_I2)
       ;; 80f1
       (bc GOTO) (byte $0d)
       (build-list 13 (lambda (_i) (bc BREAK)))
       ;; now at 8100
       (bc PUSH_IM1)))
   ))
  (check-equal? (vm-stack-n->strings goto-2-state)
                (list "stack holds 5 items"
                      "int $3fff  (rt)"
                      "int $0002"
                      "int $0001"
                      "int $0000"
                      "ptr NIL"))

  (define goto-3-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I0)
       (bc GOTO) (byte $7d)
       (bc BREAK)
       (org-align #x80)
       (bc PUSH_I1)
       ;; now at 8081
       (bc GOTO) (byte $6d)
       ;; 8083
       (bc BREAK)
       (org-align #xf0)
       (bc PUSH_I2)
       ;; now at 80f1
       (bc GOTO) (byte $0e)
       (build-list 14 (lambda (_i) (bc BREAK)))
       ;; now at 8102
       (bc PUSH_IM1)))
   ))
  (check-equal? (vm-stack-n->strings goto-3-state)
                (list "stack holds 5 items"
                      "int $3fff  (rt)"
                      "int $0002"
                      "int $0001"
                      "int $0000"
                      "ptr NIL"))

  (define goto-4-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I0)
       (bc GOTO) (byte 3)
       (bc BREAK)
       (bc PUSH_I1)
       (bc BREAK)
       (bc GOTO) (byte $fe)))
     ))
  (check-equal? (vm-stack-n->strings goto-4-state)
                (list "stack holds 3 items"
                      "int $0001  (rt)"
                      "int $0000"
                      "ptr NIL"))

  (define goto-5-state
    (run-bc-wrapped-in-test
     (flatten
      (list
       (bc PUSH_I0)
       (bc GOTO) (byte $7d)
       (bc BREAK)
       (org-align #x80)
       (bc PUSH_I1)
       ;; now at 8081
       (bc GOTO) (byte $6d)
       ;; 8083
       (bc BREAK)
       (org-align #xf0)
       (bc PUSH_I2)
       ;; now at 80f1
       (bc GOTO) (byte $0e)
       (build-list 12 (lambda (_i) (bc BREAK)))
       ;; 80ff
       (bc PUSH_I0)
       ;; 8100
       (bc BREAK)
       ;; now at 8101
       (bc PUSH_IM1)
       (bc GOTO) (byte $fd)))
     ))
  (check-equal? (vm-stack-n->strings goto-5-state)
                (list "stack holds 6 items"
                      "int $0000  (rt)"
                      "int $3fff"
                      "int $0002"
                      "int $0001"
                      "int $0000"
                      "ptr NIL")))
