#lang racket/base

(require "../6510.rkt")
(require "../ast/6510-calc-opcode-facades.rkt")
(require (only-in "../ast/6510-resolver.rkt" commands->bytes))
(require (only-in "../ast/6510-assembler.rkt" assemble))
(require (only-in racket/list flatten take))
(require (only-in "../tools/6510-disassembler.rkt" disassemble-bytes))

(module+ test
  (require "../6510-test-utils.rkt")
  (require (only-in "../tools/6510-interpreter.rkt" memory-list run-interpreter cpu-state-accumulator)))

;; constants that are used by the assembler code
(define VM_MEMORY_MANAGEMENT_CONSTANTS
  (list
   ;; page type list-pair-cells
   (byte-const PT_LIST_PAIR_CELLS  #x00)
   (byte-const PT_CALL_STACK       #x01)
   (byte-const NEXT_FREE_PAGE_PAGE #xcf) ;; cf00..cfff is a byte array, mapping each page idx to the next free page idx, 00 = no next free page for the given page
   ))

;; jump table  page-type->allocation method
(define VM_ALLOC_PAGE_JUMP_TABLE
  (flatten ;; necessary because word ref creates a list of ast-byte-codes ...
   (list
    (label VM_ALLOC_PAGE__LIST_PAIR_CELLS_JT)
           (word-ref VM_ALLOC_PAGE__LIST_PAIR_CELLS)
    (label VM_ALLOC_PAGE__CALL_STACK_JT)
           (word-ref VM_ALLOC_PAGE__CALL_STACK))))

;; initialize memory management (paging)
;; - setup 'next free page' information, basically initializing the whole page with zeros
;;
;; destroys: A Y
(define VM_INITIALIZE_MM_PAGE
  (flatten
   (list
    (label VM_INITIALIZE_MM_PAGE)
           (LDA !0)
           (TAY)
    (label VM_INITIALIZE_MM_PAGE__LOOP)
           ;; highbyte of this address should be using the constant NEXT_FREE_PAGE_PAGE
           ;; (STA $cf00,y) ;; encoded directly in the next couple of bytes
           (byte 153 0) (byte-ref NEXT_FREE_PAGE_PAGE)
           (INY)
           (BNE VM_INITIALIZE_MM_PAGE__LOOP)
           (RTS))))

;; parameter:
;;   A = page-type to allocate (see constants)
;;       (00 = fixed slot size list-pair-cells,
;;        01 = variable slot size native arrays for call-stack)
;; result:
;;   A = allocated page
(define VM_ALLOC_PAGE
  (list
   (label VM_ALLOC_PAGE)
          (ASL A)
          (TAY)
          (LDA VM_ALLOC_PAGE__LIST_PAIR_CELLS_JT+1,y) ;; high byte from jump table
          (STA VM_ALLOC_PAGE__JT+2)                   ;; write high byte of jump target
          (LDA VM_ALLOC_PAGE__LIST_PAIR_CELLS_JT,y)   ;; low byte from jump table
          (STA VM_ALLOC_PAGE__JT+1)                   ;; write low byte of jump target
   (label VM_ALLOC_PAGE__JT)
          (JMP $0000)                                 ;; jump to
   ))

;; (JSR VM_ALLOC_PAGE__LIST_PAIR_CELLS)      ;; direct calls (if type is statically known)
;; (JSR VM_ALLOC_PAGE__CALL_STACK)

;; allocate a list pair cells page (initialized with free list etc)
;; parameter: (none)
;; result: A = allocated list pair cells page
(define VM_ALLOC_PAGE__LIST_PAIR_CELLS
  (list (label VM_ALLOC_PAGE__LIST_PAIR_CELLS)
        (byte-const TEMP_TYPE #xfa)
        (byte-const PAGE #xfc)
        (word-const PAGE_USE_BITMAP #xc000)
        (word-const OUT_OF_MEMORY_ERROR #xc100)

      ;; ALLOCATE_NEW_PAGE: ;; Y = page type (0 list-cell-pair ,1 cell, 2 float), return A = page idx
        (STY TEMP_TYPE)
        (LDY !$1F)
        (label  CHECK_PAGE_BITMAP)
        (LDA PAGE_USE_BITMAP,y)
        (CMP !$FF)
        (BNE FP_FOUND)
        (DEY)
        (BPL CHECK_PAGE_BITMAP)
        (JMP OUT_OF_MEMORY_ERROR) ;; TODO if there are enqueued ref count decs, check those first

        (label FP_FOUND)
        (PHA)
        (TYA)
        (ASL A)
        (ASL A)
        (ASL A)
              ;; now a has the topmost 5 bits set
        (STA PAGE)
              ;; now get the lower three bits
        (LDX !$07)
        (PLA) ;; get bit map
        (label SHIFT_OUT)
        (LSR)
        (BCC UNSET_BIT_FOUND)
        (DEX)
        (BPL SHIFT_OUT)
        (label UNSET_BIT_FOUND)
        (TXA)
        (ORA PAGE) ;; combine with bits
        (STA PAGE)

        (LDA PAGE_USE_BITMAP,y)
        (ORA BITS,x)
              ;; make sure to set the bit of allocated page in page bitmap
        (STA PAGE_USE_BITMAP,y)
        (LDA PAGE)
        (RTS)

        (label BITS)
        (byte #b10000000
               #b01000000
               #b00100000
               #b00010000
               #b00001000
               #b00000100
               #b00000010
               #b00000001)

        (label ALT_UNSET_BUT_FOUND) ;; 4 bytes less, mean 28 cycles more
        (TXA)
        (ORA PAGE) ;; combine with bits
        (STA PAGE)

        (LDA !$01)
        (label SHIFT_AGAIN)
        (ASL A)
        (DEX)
        (BPL SHIFT_AGAIN)
        (ROR)
        (ORA PAGE_USE_BITMAP,y)
        (STA PAGE_USE_BITMAP,y)
        (LDA PAGE)
        (RTS)
))

;; allocate a list pair cells page (initialized with free list etc)
;; parameter: (none)
;; result: A = allocated call stack page
(define VM_ALLOC_PAGE__CALL_STACK
  (list (label VM_ALLOC_PAGE__CALL_STACK)
               (RTS)))

(module+ test #| VM_ALLOC_PAGE |#
  (define org #x2068)
  (define program (append (list)
                          VM_MEMORY_MANAGEMENT_CONSTANTS
                          VM_INITIALIZE_MM_PAGE
                          ;; VM_ALLOC_PAGE_JUMP_TABLE
                          ;; VM_ALLOC_PAGE
                          ;; VM_ALLOC_PAGE__LIST_PAIR_CELLS
                          ;; VM_ALLOC_PAGE__CALL_STACK
                          ))
  (check-equal? (disassemble-bytes org (take (assemble org program) 10))
                (list "LDA #$00"
                      "TAY"
                      "STA $cf00,y"
                      "INY"
                      "BNE $fa"
                      "RTS")))
