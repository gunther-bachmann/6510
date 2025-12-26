#lang racket/base

(provide
 VM_INITIALIZE_MEMORY_MANAGER)


#|

 define memory management

 |#


(require "../../6510.rkt"
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function)
         (only-in "./vm-call-frame.rkt"
                  VM_INITIALIZE_CALL_FRAME
                  vm-call-frame-code)
         (only-in "./vm-cell-array-n.rkt"
                  vm-cell-array-code)
         (only-in "./vm-cell-stack.rkt"
                  INIT_CELLSTACK
                  vm-cell-stack-code)
         (only-in "./vm-lists-n.rkt"
                  vm-list-code)
         (only-in "./vm-m1-slots-n.rkt"
                  vm-m1-slot-code)
         (only-in "./vm-memory-map.rkt"
                  VM_MEMORY_MANAGEMENT_CONSTANTS)
         (only-in "./vm-native-array.rkt"
                  vm-native-array-code)
         (only-in "./vm-pages-n.rkt"
                  VM_INITIALIZE_PAGE_MEMORY_MANAGER_N
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


(define-vm-function VM_INITIALIZE_MEMORY_MANAGER
  (list
   (JSR VM_INITIALIZE_PAGE_MEMORY_MANAGER_N)
   (JSR INIT_CELLSTACK)
   (JMP VM_INITIALIZE_CALL_FRAME)))


(define vm-memory-manager-code
  (append vm-register-functions-code
          vm-pages-code
          vm-m1-slot-code
          vm-cell-stack-code
          vm-cell-array-code
          vm-call-frame-code
          vm-list-code
          VM_INITIALIZE_MEMORY_MANAGER
          VM_MEMORY_MANAGEMENT_CONSTANTS
          ))

(module+ test #| code len |#
  (inform-check-equal? (code-len vm-memory-manager-code)
                       2008))
