#lang racket

#|

  define all functions, data and constants for generic page level management


  page layout
  | offset | content
  |--------|--------------------
  | +00    | page-type
  | +01    | # of free slots / # of used slots (optional, since this offset is not available on specialized cell-pair pages)
  | +02    | beginning of first slot
  |  :     |
  | +fe    | offset to first free slot (0 if there is no free slot)
  | +ff    | ptr to prev page of this type (0 if this page is full || head and tail of the PPAGE_FREE_LIST)
             if the page is free and has no type (uninitialized), it points to the next free page

  if a slot is part of the on page free list of slots, its first payload byte points to the next free slot!

  | page-type | description
  |-----------|--------------------
  | 0000 0000 | unknown or uninitialized
  | 0001 0001 | page with code
  | 0001 0010 | cell stack page
  | 0001 0011 | call stack page
  | 0010 xxxx | m1 page with profile xxxx


|#

(provide
 VM_INITIALIZE_PAGE_MEMORY_MANAGER_N     ;; initialize page memory management (must be called before first allocation)
 VM_ALLOCATE_NEW_PAGE_N                  ;; get a page from the free list and adjust the free list accordingly (actually pop)
 VM_DEALLOCATE_PAGE_N                    ;; return a page to the free list (actually push)
)

(require "../../6510.rkt"
         (only-in "../../ast/6510-resolver.rkt"
                  add-label-suffix)
         (only-in "./vm-memory-map.rkt"
                  ZP_PAGE_REG
                  ZP_PAGE_FREE_LIST
                  ZP_TEMP
                  VM_MEMORY_MANAGEMENT_CONSTANTS))

(module+ test
  (require  "../../6510-test-utils.rkt"
            (only-in "../../tools/6510-interpreter.rkt"
                     peek
                     memory-list
                     peek-word-at-address)
            "./vm-memory-manager-test-utils.rkt")

  (define test-runtime
    (append
     VM_INITIALIZE_PAGE_MEMORY_MANAGER_N
     VM_MEMORY_MANAGEMENT_CONSTANTS
     VM_ALLOCATE_NEW_PAGE_N
     VM_DEALLOCATE_PAGE_N
     (list (label VM_INITIALIZE_MEMORY_MANAGER) (RTS))))
  )

;; syntactic sugar to define a vm function, without label at start of impl.
(define-syntax-rule (define-vm-function-wo-label name code-list)
  (define name
    (add-label-suffix
     "__" (string-append "__" (symbol->string (quote name)))
     code-list) ))

;; syntactic sugar to define a vm function
(define-syntax-rule (define-vm-function name code-list)
  (define-vm-function-wo-label name
    (append
      (list (label name))
      code-list)))

;; pop a page off the free list (if available, else out of memory error)
;;
;; input:  -
;; output: X = allocated page
;;         ZP_PAGE_FREE_LIST = points to new head of list
;; usage:
(define-vm-function VM_ALLOCATE_NEW_PAGE_N (list))

(module+ test #| vm-allocate-new-page-n (incomplete) |#
  (define vm-allocate-new-page-n-01
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (LDX !$20)
     (JSR VM_INITIALIZE_PAGE_MEMORY_MANAGER_N)

     ;; now allocate a new page
     (JSR VM_ALLOCATE_NEW_PAGE_N)
     (STX ZP_PAGE_REG+1)  ;; save for later check
     ))

  (skip
   "! TODO: implement page allocation"
   (check-equal? (peek vm-allocate-new-page-n-01 ZP_PAGE_REG)
                 #xcf
                 "first page should be $cf"))

  (skip
   "! TODO: implement page allocation"
   (check-equal? (peek vm-allocate-new-page-n-01 ZP_PAGE_FREE_LIST)
                     #xce
                     "head of free list now is $ce")))

;; return page to free list
;; make sure to mark page as uninitialized (in $00) and adjust previous page ptr in $ff
;;
;; input:  x = page
;; output: ZP_PAGE_FREE_LIST = page
;; usage:
(define-vm-function VM_DEALLOCATE_PAGE_N (list))

(module+ test #| vm-deallocate-page-n (incomplete) |#
  (skip
   "! TODO: implement page deallocation"
   (check-equal? (peek vm-deallocate-page-n-01 ZP_PAGE_FREE_LIST)
                 #xcf
                 "first free page should be $cf (again)")))

;; initialize all pages available for memory management
;; memory layout:
;; pages: 00   .. 07  are reserved
;;        08   .. kk-1  reserved for code [runtime comes first, then program to be executed]
;;        kk   .. 9f  available for page memory management
;;        a0   .. bf  reserved by basic
;;        c0   .. cf  available for page memory management
;;        d0   .. df  reserved by i/o and char rom
;;        e0   .. ff  reserved by kernal
;; initialize variables for page memory management
;;   ZP_PAGE_FREE_LIST = $cf
;;
;; each page (starting from cf downward), is initialized to page type 0 (offset 0)
;; and to point to the next free page (offset $ff) in the list down to kk
;; the last page in the list (at kk) has its next ptr set to 0
;;
;; input:  X = kk-1 (kk first page available for page memory management behind the code)
;; output: ZP_PAGE_FREE_LIST = cf
;;         ZP_PAGE (byte)   = 0
;;         ZP_PAGE+1 (byte) = kk-1
;;         A = $00
;;         Y = $ff
;;         X = kk-1
;; uses:   A, X, Y, ZP_TEMP (word), ZP_PAGE_REG (word)
;;
;; THIS CODE IS EXECUTED ONLY OPEN INITIALIZATION AND COULD BE DISCARDED,
;; ONCE THE PROGRAM IS TERMINATED. IN CASE OF ADDITIONAL PROGRAM LOADS, THIS
;; - MAY STILL HAVE ITS USES THOUGH
;; - MORE REGIONS ARE POSSIBLE, THOUGH
(define-vm-function
  VM_INITIALIZE_PAGE_MEMORY_MANAGER_N
  (list
          (STX ZP_TEMP+1) ;; for later comparison

          (LDA !$00)
          (STA ZP_PAGE_REG) ;; initialize page reg

          (LDX !$cf)     ;; x = $cf
          (STX ZP_PAGE_FREE_LIST) ;; cf = first page available for allocation

          (LDY !$bf)     ;; y = $bf
          (LDA !$9f)     ;; last page at c0 points to 9f as next free page
          (JSR init_page_up_to__) ;; init c0..cf

          (TAX)           ;; x = $9f
          (LDY ZP_TEMP+1) ;; y = kk+1
          (LDA !$00)      ;; last page will point to 00, being the last page of the free list
          ;; (JMP init_page_up_to__) ;; not necessary, since we go there anyhow
          ;; init kk+1..9f

          ;; a = page to be next for the list page initialized!
          ;; x = page to start from
          ;; y = down to this page not including!
   (label init_page_up_to__)
          (PHA)
          (STY ZP_TEMP)

   (label loop_page_range__)
          (STX ZP_PAGE_REG+1)
          (LDY !$00)
          (TYA)
          (STA (ZP_PAGE_REG),y) ;; @00 = 0 (page type: uninitialized)
          (DEY)                 ;; now $FF
          (DEX)
          (TXA)
          (STA (ZP_PAGE_REG),y) ;; @ff = # (next free page)
          (CPX ZP_TEMP)
          (BNE loop_page_range__)

          (PLA) ;; get the page to be registered as next for the last page initialized
          (STA (ZP_PAGE_REG),y) ;; to set last page within the range to point to A
          (RTS)
   ))

(module+ test #| vm-initialize-page-memory-manager |#
  (define vm-initialize-page-memory-manager-01
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (LDX !$20)
     (JSR VM_INITIALIZE_PAGE_MEMORY_MANAGER_N)
     (STA $0200) ;; save for later check
     (STX $0201)
     (STY $0202)))

  (check-equal? (memory-list vm-initialize-page-memory-manager-01 #x0200 #x0202)
                (list #x00 #x20 #xff)
                "registers are filled with values guaranteed by function")

  (check-equal? (peek-word-at-address vm-initialize-page-memory-manager-01 ZP_PAGE_REG)
                #x2100)

  (check-equal? (peek vm-initialize-page-memory-manager-01 ZP_PAGE_FREE_LIST)
                #xcf
                "first free page is $cf")

  (check-equal? (peek vm-initialize-page-memory-manager-01 #xcf00)
                0
                "type of page $cf is 0")
  (check-equal? (peek vm-initialize-page-memory-manager-01 #xcfff)
                #xce
                "next free page after $cf is $ce")

  (check-equal? (peek vm-initialize-page-memory-manager-01 #xc000)
                0
                "type of page $c0 is 0")
  (check-equal? (peek vm-initialize-page-memory-manager-01 #xc0ff)
                #x9f
                "next free page after $c0 is $9f")

  (check-equal? (peek vm-initialize-page-memory-manager-01 #x9f00)
                0
                "type of page $9f is 0")
  (check-equal? (peek vm-initialize-page-memory-manager-01 #x9fff)
                #x9e
                "next free page after $9f is $9e")

  (check-equal? (peek vm-initialize-page-memory-manager-01 #x2100)
                0
                "type of page $20 is 0")
  (check-equal? (peek vm-initialize-page-memory-manager-01 #x21ff)
                #x00
                "next free page after $20 is $00, that is, there is no free list after $20"))
