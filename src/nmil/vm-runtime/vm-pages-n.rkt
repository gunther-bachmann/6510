#lang racket

#|

  define all functions, data and constants for generic page level management

|#

;; (provide
;;  VM_INITIALIZE_MEMORY_MANAGER_N     ;; initialize memory management (must be called before first allocation)
;; )

(require "../../6510.rkt"
         (only-in "../../ast/6510-resolver.rkt"
                  add-label-suffix)
         (only-in "./vm-memory-map.rkt"
                  TAGGED_NIL
                  ZP_RP
                  ZP_RA
                  ZP_RB
                  ZP_TEMP
                  ZP_RT
                  VM_MEMORY_MANAGEMENT_CONSTANTS))


(module+ test
  (require  "../../6510-test-utils.rkt"
            (only-in "../../tools/6510-interpreter.rkt"
                     peek)
            (only-in "../../util.rkt"
                     format-hex-byte)
            "./vm-memory-manager-test-utils.rkt"
            (only-in "./vm-register-functions.rkt"
                     WRITE_INT_AY_TO_RT))


  (define PAGE_AVAIL_0 #x8d)      ;; high byte of first page available for allocation
  (define PAGE_AVAIL_0_W #x8d00)  ;; word (absolute address) of first page available
  (define PAGE_AVAIL_1 #x8c)      ;; high byte of second page available for allocation
  (define PAGE_AVAIL_1_W #x8c00)  ;; word (absolute address) of second page available

  ;; (define test-runtime
  ;;   (append
  ;;    VM_INITIALIZE_MEMORY_MANAGER_N))
  )

(define-syntax (define-vm-function stx)
  (syntax-case stx ()
    [(define-vm-function name code-list)
     #'(define name
         (add-label-suffix "__" (string-append "__" (symbol->string 'name))
                           (append (list name) code-list)))]))

;; (define-vm-function VM_INITIALIZE_MEMORY_MANAGER_N
;;   (list (RTS)))

;; (define VM_INITIALIZE_MEMORY_MANAGER_N
;;   (add-label-suffix
;;    "__" "VM_INITIALIZE_MEMORY_MANAGER_N__"
;;    (list
;;     (label VM_INITIALIZE_MEMORY_MANAGER_N))))
