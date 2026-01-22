#lang racket

(provide
 VM_INIT_PAGE_MEMORY_MANAGER   ;; initialize page memory management (must be called before first allocation)
 VM_ALLOCATE_NEW_PAGE          ;; get a page from the free list and adjust the free list accordingly (actually pop)
 VM_DEALLOCATE_PAGE            ;; return a page to the free list (actually push)

 vm-pages-code)

#|

  define all functions, data and constants for generic page level management


  page layout
  | offset | content
  |--------|--------------------
  | +00    | page-type (0 if not initialized, yet)
  | +01    | # of used slots  (not written if not initialized)
  | +02    | beginning of first slot
  |  :     |
  | +fe    | offset to first free slot (0 if there is no free slot) (not written if not initialized)
  | +ff    | ptr to prev page of this type (0 if this page is full || head and tail of the PPAGE_FREE_LIST)
             if the page is free and has no type (uninitialized), it points to the next free page

  if a slot is part of the on page free list of slots, its first payload byte points to the next free slot!

  | page-type | description
  |-----------|--------------------
  | 0000 0000 | unknown or uninitialized
  | 0001 0001 | page with code
  | 0001 0010 | cell stack page
  | 0001 0011 | call frame page
  | 0010 xxxx | m1 page with profile xxxx


 |#

(require "../../6510.rkt"
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function-wol
                  define-vm-function)
         (only-in "./vm-memory-map.rkt"
                  ZP_PAGE_REG
                  ZP_PAGE_FREE_LIST
                  ZP_TEMP
                  ZP_INC_COLLECTIBLE_LIST
                  VM_MEMORY_MANAGEMENT_CONSTANTS
                  ZP_PROFILE_PAGE_FREE_LIST
                  ZP_PAGE_FREE_SLOTS_LIST))

(module+ test
  (require  "../../6510-test-utils.rkt"
            (only-in "../../ast/6510-relocator.rkt" code-len)
            (only-in "../../tools/6510-interpreter.rkt"
                     peek
                     memory-list
                     peek-word-at-address)
            (only-in "../test-utils.rkt"
                     regression-test)
            "./vm-memory-manager-test-utils.rkt")

  (define PAGE_AVAIL_0 #xcf)
  (define PAGE_AVAIL_0_W #xcf00)

  (define test-runtime
    (append
     VM_INIT_PAGE_MEMORY_MANAGER
     VM_MEMORY_MANAGEMENT_CONSTANTS
     VM_ALLOCATE_NEW_PAGE
     VM_DEALLOCATE_PAGE
     (list (label VM_INIT_MEMORY_MANAGER) (RTS)))))

;; pop a page off the free list (if available, else out of memory error)
;;
;; input:  ZP_PAGE_FREE_LIST, A = PAGE TYPE
;; usage:  A, X, Y, ZP_PAGE_REG, ZP_PAGE_FREE_LIST
;; output: X = allocated page
;;         Y = $FF
;;         A = next free page
;;         ZP_PAGE_REG+1 = allocated page
;;         ZP_PAGE_FREE_LIST = points to new head of list
;; usage:
(define-vm-function
  VM_ALLOCATE_NEW_PAGE
  (list
          (LDX ZP_PAGE_FREE_LIST)       ;; get current head of page free list into X
          (BEQ outof_memory__)          ;; 0? then out of memory error
          (STX ZP_PAGE_REG+1)           ;; store page as high byte
          (LDY !$00)
          (STA (ZP_PAGE_REG),y)         ;; store page type
          (DEY)                         ;; offset to next free page on this page
          (LDA (ZP_PAGE_REG),y)         ;; load next free page (after this)
          (STA ZP_PAGE_FREE_LIST)       ;; store as new head of page free list
          (RTS)
   (label outof_memory__)
          (BRK)))

(module+ test #| vm-allocate-new-page-n (incomplete) |#
  (define vm-allocate-new-page-n-01
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (LDX !$20)
     (JSR VM_INIT_PAGE_MEMORY_MANAGER)

     ;; now allocate a new page
     (JSR VM_ALLOCATE_NEW_PAGE)))

  (regression-test
   vm-allocate-new-page-n-01
   "init -> allocate new page"
   (check-equal? (peek vm-allocate-new-page-n-01 (+ 1 ZP_PAGE_REG))
                 #xcf
                 "first page should be $cf")
   (check-equal? (peek vm-allocate-new-page-n-01 ZP_PAGE_FREE_LIST)
                 #xce
                 "head of free list now is $ce")))

;; return page to free list
;; make sure to mark page as uninitialized (in $00) and adjust previous page ptr in $ff
;;
;; input:  X = page
;; output: ZP_PAGE_FREE_LIST = page (now free)
;;         A = previous free page
;;         X = page (now free)
;;         Y = $00
;;         ZP_PAGE_REG = page (now free)
;; usage:  A, X, Y, ZP_PAGE_REG, ZP_PAGE_FREE_LIST
(define-vm-function
  VM_DEALLOCATE_PAGE
  (list
                (LDA ZP_PAGE_FREE_LIST)   ;; load old head of page free list
                (STX ZP_PAGE_REG+1)       ;; store x (page to free) into page register
                (LDY !$ff)                ;; offset to place where linked next free should be
                (STA (ZP_PAGE_REG),y)     ;; store old page as link into the page about to be freed
                (INY)                     ;; y := 0
                (TYA)                     ;; A := 0
                (STA (ZP_PAGE_REG),y)     ;; store page type 0 into page @00
                (STX ZP_PAGE_FREE_LIST)   ;; store this page as new head of the page free list
                (RTS)))

(module+ test #| vm-deallocate-page-n (incomplete) |#
  (define vm-deallocate-page-n-01
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
            ;; given
            (JSR VM_INIT_PAGE_MEMORY_MANAGER_N20)
            (JSR VM_ALLOCATE_NEW_PAGE)
            (fill-page-with PAGE_AVAIL_0  #xcc)

            ;; when
            (JSR VM_DEALLOCATE_PAGE)))

  (check-equal? (peek vm-deallocate-page-n-01 ZP_PAGE_FREE_LIST)
                PAGE_AVAIL_0
                "first free page should be PAGE_AVAIL_0 (again)")
  (check-equal? (peek vm-deallocate-page-n-01 (peek-word-at-address vm-deallocate-page-n-01 ZP_PAGE_REG))
                #x00
                "first byte of freed page should be $00 (untyped) again")
  (check-equal? (peek vm-deallocate-page-n-01 (+ #xff (peek-word-at-address vm-deallocate-page-n-01 ZP_PAGE_REG)))
                (- PAGE_AVAIL_0 1)
                "last byte of freed page should be $ce, the previous free page again"))

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
(define-vm-function-wol
  VM_INIT_PAGE_MEMORY_MANAGER
  (list
   (label VM_INIT_PAGE_MEMORY_MANAGER_N20)
          (LDX !$30)
   (label VM_INIT_PAGE_MEMORY_MANAGER)
          (STX ZP_TEMP+1) ;; for later comparison

          (LDX !$05) ;; last index of profile
          (LDA !$00) ;; page = 0 => no element in the list

          (STA ZP_INC_COLLECTIBLE_LIST)   ;; init ptr to 0
          (STA ZP_INC_COLLECTIBLE_LIST+1) ;;

   (label init_profile_pages__)
          (STA ZP_PROFILE_PAGE_FREE_LIST,x)
          (STA ZP_PAGE_FREE_SLOTS_LIST,x)
          (DEX)
          (BPL init_profile_pages__)

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
     (JSR VM_INIT_PAGE_MEMORY_MANAGER)
     (STA $0200) ;; save for later check
     (STX $0201)
     (STY $0202)))

  (regression-test
   vm-initialize-page-memory-manager-01
   "initialize all pages for memory management, CFxx .. page A, skipping A000-BFFF"
   (check-equal?
    (memory-list vm-initialize-page-memory-manager-01 #x0200 #x0202)
    (list #x00 #x20 #xff)
    "registers are filled with values guaranteed by function")

   (check-equal?
    (memory-list vm-initialize-page-memory-manager-01 ZP_INC_COLLECTIBLE_LIST (+ 1 ZP_INC_COLLECTIBLE_LIST))
    (list #x00 #x00)
    "head of incrementally collective cell arrays is 0")

   (check-equal?
    (memory-list vm-initialize-page-memory-manager-01 ZP_PROFILE_PAGE_FREE_LIST (+ ZP_PROFILE_PAGE_FREE_LIST 5))
    (list #x00 #x00 #x00 #x00 #x00 #x00)
    "the next free page already initialized to the profiles are all $00 => none available")

   (check-equal?
    (memory-list vm-initialize-page-memory-manager-01 ZP_PAGE_FREE_SLOTS_LIST (+ ZP_PAGE_FREE_SLOTS_LIST 5))
    (list #x00 #x00 #x00 #x00 #x00 #x00)
    "the next page with slots of the given profiles are all $00 => none available")

   (check-equal?
    (peek-word-at-address vm-initialize-page-memory-manager-01 ZP_PAGE_REG)
    #x2100
    "page register is set to the page before the last one allocatable")

   (check-equal?
    (peek vm-initialize-page-memory-manager-01 ZP_PAGE_FREE_LIST)
    PAGE_AVAIL_0
    "first free page is PAGE_AVAIL_0")

   (check-equal?
    (peek vm-initialize-page-memory-manager-01 PAGE_AVAIL_0_W)
    0
    "type of page $cf is 0")

   (check-equal?
    (peek vm-initialize-page-memory-manager-01 (+ PAGE_AVAIL_0_W #xff))
    (- PAGE_AVAIL_0 1)
    "next free page after $cf is $ce")

   (check-equal?
    (peek vm-initialize-page-memory-manager-01 #xc000)
    0
    "type of page $c0 is 0")

   (check-equal?
    (peek vm-initialize-page-memory-manager-01 #xc0ff)
    #x9f
    "next free page after $c0 is $9f"))

  (check-equal?
   (peek vm-initialize-page-memory-manager-01 #x9f00)
   0
   "type of page $9f is 0")

  (check-equal?
   (peek vm-initialize-page-memory-manager-01 #x9fff)
   #x9e
   "next free page after $9f is $9e")

  (check-equal?
   (peek vm-initialize-page-memory-manager-01 #x2100)
   0
   "type of page $20 is 0")

  (check-equal?
   (peek vm-initialize-page-memory-manager-01 #x21ff)
   #x00
   "next free page after $20 is $00, that is, there is no free list after $20"))

(define vm-pages-code
  (append
   VM_INIT_PAGE_MEMORY_MANAGER
   VM_ALLOCATE_NEW_PAGE
   VM_DEALLOCATE_PAGE))

(module+ test #| module code len |#
  (inform-check-equal? (code-len vm-pages-code)
                       109))
