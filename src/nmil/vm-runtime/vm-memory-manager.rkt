#lang racket/base

(provide
 VM_INIT_MEMORY_MANAGER
 vm-memory-manager-code)


#|

 define memory management

 |#


(require "../../6510.rkt"
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function)
         (only-in "./vm-call-frame.rkt"
                  VM_INIT_CALL_FRAME_STACK
                  vm-call-frame-code)
         (only-in "./vm-cell-array.rkt"
                  vm-cell-array-code)
         (only-in "./vm-cell-stack.rkt"
                  INIT_EVLSTK_TAIL
                  vm-cell-stack-code)
         (only-in "./vm-lists.rkt"
                  vm-list-code)
         (only-in "./vm-m1-slots.rkt"
                  vm-m1-slot-code)
         (only-in "./vm-memory-map.rkt"
                  VM_MEMORY_MANAGEMENT_CONSTANTS)
         (only-in "./vm-native-array.rkt"
                  vm-native-array-code)
         (only-in "./vm-pages.rkt"
                  VM_INIT_PAGE_MEMORY_MANAGER
                  vm-pages-code)
         (only-in "./vm-register-functions.rkt"
                  vm-register-functions-code))


(module+ test #| test |#

    (require
           "../../6510-test-utils.rkt"
           (only-in "../../ast/6510-relocator.rkt" code-len)
           "./vm-memory-manager-test-utils.rkt"
           (only-in "./vm-memory-map.rkt"
                    ZP_TEMP)))


(define-vm-function VM_INIT_MEMORY_MANAGER
  (list
                (JSR VM_INIT_PAGE_MEMORY_MANAGER_N20)
                (JSR INIT_EVLSTK_TAIL)
                (JMP VM_INIT_CALL_FRAME_STACK)))


(define vm-memory-manager-code
  (append vm-register-functions-code
          vm-pages-code
          vm-m1-slot-code
          vm-cell-stack-code
          vm-cell-array-code
          vm-native-array-code
          vm-call-frame-code
          vm-list-code
          VM_INIT_MEMORY_MANAGER
          VM_MEMORY_MANAGEMENT_CONSTANTS
          ))

(module+ test #| code len |#
  (inform-check-equal? (code-len vm-memory-manager-code)
                       2227))
