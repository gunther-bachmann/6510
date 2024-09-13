#lang racket/base

(require "../6510.rkt")
(require (only-in "../ast/6510-assembler.rkt" assemble assemble-to-code-list translate-code-list-for-basic-loader))
(require (only-in racket/list flatten take))

(module+ test
  (require "../6510-test-utils.rkt")
  (require (only-in racket/port open-output-nowhere))
  (require (only-in "../tools/6510-disassembler.rkt" disassemble-bytes))
  (require (only-in "../tools/6510-debugger.rkt" run-debugger-on)))
(require (only-in "../tools/6510-interpreter.rkt" 6510-load 6510-load-multiple initialize-cpu run-interpreter run-interpreter-on memory-list cpu-state-accumulator))


;; test one roundtrip:

;; use case: allocate, free, reallocate small list of cell-pairs, keeping tail because ptr was copied (ref count incremented)
;; - allocate cell-pair
;; - set cell1 to an int
;; - allocate cell-pair
;; - set cell2 to allocated cell-pair
;; - increment ref count of cell2 (thus it must be kept, when head is decremented)
;; - set cell1 to an int
;; - (set cell2 to nil) => (equivalent to '(1 2))
;; - decrement list head (cell-pair) <- should put the head of the list on the free tree, keeping the pointer to the next cell-pair
;; - allocate cell-pair <- should recycle the top of the free tree, decrement ref count of cell2, since ref count cell2 is not dropping to 0, top of tree is set to 0

;; Method index + description
;; DATA
;;  VM_MEMORY_MANAGEMENT_CONSTANTS :: constants that are used by the assembler code
;;  VM_ALLOC_PAGE_JUMP_TABLE       :: jump table  page-type->allocation method
;;  VM_INITIAL_MM_REGS             :: (initial data for) the memory management registers
;;  VM_FREE_PAGE_BITMAP            :: bitmap indicating free pages (0) or allocated pages (1)
;;
;; CODE (FULL PAGE)
;;  VM_INITIALIZE_MM_PAGE          :: initialize memory management (paging)
;;  VM_ALLOC_PAGE                  :: INCOMPLETE! allocate page (of any kind)
;;  VM_ALLOC_PAGE__LIST_CELL_PAIRS :: allocate a complete new page and initialize it to hold reference counted cell-pairs
;;  VM_FREE_PAGE                   :: free the given page (may then be allocated again via VM_ALLOC_PAGE*
;;  VM_ALLOC_PAGE__PAGE_UNINIT     :: allocate page (without initialization for specific type)
;;  VM_ALLOC_PAGE__CALL_STACK      :: INCOMPLETE! allocate page for call stack usage
;;
;; CODE
;;  VM_ALLOC_CELL_PAIR_ON_PAGE     :: allocate a cell-pair on given page, auto allocate new page if full
;;  VM_REFCOUNT_DECR               :: INCOMPLETE: dispatch to type specific decrement of ref count
;;  VM_REFCOUNT_DECR_CELL_PAIR     :: decrement ref count for cell-pair, mark as free if ref count drops to 0 (calls VM_FREE_CELL_PAIR)
;;  VM_REFCOUNT_INCR_CELL_PAIR     :: increments ref count for cell-pair
;;  VM_FREE_NON_ATOMIC             :: INCOMPLETE: free a non atomic cell (e.g. cell-ptr, cell-pair, float, array/struct)
;;  VM_ALLOC_CELL_PAIR             :: allocate a cell-pair (reuse marked free, allocate new if no reuse possible)
;;  VM_FREE_CELL_PAIR              :: mark cell-pair as free, tail call free on cell1 (which is used for free tree)
;;  VM_ADD_CELL_PAIR_TO_ON_PAGE_FREE_LIST :: add the given cell-pair to its free list on its page (cell1 and cell2 must not point to anything), refcount is set to 0

;; constants that are used by the assembler code
(define VM_MEMORY_MANAGEMENT_CONSTANTS
  (list
   ;; page type list-pair-cells
   (byte-const PT_LIST_PAIR_CELLS        #x00) ;; page type: list pair cells
   (byte-const PT_CALL_STACK             #x01) ;; page type: call stack
   (byte-const PT_CODE                   #x02) ;; page type: code
   (byte-const NEXT_FREE_PAGE_PAGE       #xcf) ;; cf00..cfff is a byte array, mapping each page idx to the next free page idx, 00 = no next free page for the given page
   (byte-const FREE_PAGE_BITMAP_SIZE     #x20) ;; size of the free page bitmap (bit set == used, bit unset == free)
   (word-const VM_FREE_PAGE_BITMAP       #xced0) ;; location: free page bitmap (ced0..ceff)
   (word-const VM_FREE_SLOT_FOR_PAGE     #xcf00) ;; location: table of first free slot for each page
   (word-const TAGGED_INT_0              #x0000)
   (word-const TAGGED_NIL                #x0200)
   (byte-const ZP_PTR #xfb)
   (byte-const ZP_PTR2 #xfd)
   ))

;; y =1 for cell1, = 3 for cell 2
;; zp_ptr = cell-pair
(define VM_CELL_PAIR_SET_INT_0
  (list
   (label VM_CELL_PAIR_SET_INT_0)
          (LDA !>TAGGED_INT_0)
          (STA (ZP_PTR),y)
          (LDA !<TAGGED_INT_0)
          (DEY)
          (STA (ZP_PTR),y)
          (RTS)))

;; y =1 for cell1, = 3 for cell 2
;; zp_ptr = cell-pair
(define VM_CELL_PAIR_SET_NIL
  (list
   (label VM_CELL_PAIR_SET_NIL)
          (LDA !>TAGGED_NIL)
          (STA (ZP_PTR),y)
          (LDA !<TAGGED_NIL)
          (DEY)
          (STA (ZP_PTR),y)
          (RTS)))

;; jump table  page-type->allocation method
(define VM_ALLOC_PAGE_JUMP_TABLE
  (flatten ;; necessary because word ref creates a list of ast-byte-codes ...
   (list
    (label VM_ALLOC_PAGE__LIST_CELLS_PAIRS_JT)
           (word-ref VM_ALLOC_PAGE__LIST_CELL_PAIRS)
    (label VM_ALLOC_PAGE__CALL_STACK_JT)
           (word-ref VM_ALLOC_PAGE__CALL_STACK))))

;; initial data for the memory management registers
;; put into memory @ #xced0 - len (currently 3)
(define VM_INITIAL_MM_REGS
  (list
   (label VM_INITIAL_MM_REGS)

   (label VM_FREE_CELL_PAGE) ;; cell page with free cells
          (byte $00)
   (label VM_FREE_CALL_STACK_PAGE) ;; call stack page with free space
          (byte $00)
   (label VM_FREE_CODE_PAGE) ;; code page with free space
          (byte $00)
   (label VM_FREE_CELL_PAIR_PAGE) ;; cell page with free cells
          (byte $00) ;; none
   (label VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH) ;; what is the highest page to start searching for a free page
          (byte $1f) ;; safe to start with $1F is index
   (label VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
          (word $0000) ;; if high byte is 0, the tree is empty!
   ))

;; put into memory @ #xced0
(define VM_FREE_PAGE_BITMAP
  (list
   (label VM_FREE_PAGE_BITMAP)
          (byte #b11111111)     ;; mem 0000-07ff is unavailable
          (byte #b11111111)     ;; mem 0800-0fff is unavailable
          (byte #b11111111)     ;; mem 1000-17ff is unavailable
          (byte #b11111111)     ;; mem 1800-1fff is unavailable
          (byte #b00000011)     ;; mem 2000-21ff is unavailable, 2200-27ff is free
          (byte #b00000000)     ;; mem 2800-2fff is free
          (byte #b00000000)     ;; mem 3000-37ff is free
          (byte #b00000000)     ;; mem 3800-3fff is free
          (byte #b00000000)     ;; mem 4000-47ff is free
          (byte #b00000000)     ;; mem 4800-4fff is free
          (byte #b00000000)     ;; mem 5000-57ff is free
          (byte #b00000000)     ;; mem 5800-5fff is free
          (byte #b00000000)     ;; mem 6000-67ff is free
          (byte #b00000000)     ;; mem 6800-6fff is free
          (byte #b00000000)     ;; mem 7000-77ff is free
          (byte #b00000000)     ;; mem 7800-7fff is free
          (byte #b00000000)     ;; mem 8000-87ff is free
          (byte #b00000000)     ;; mem 8800-8fff is free
          (byte #b00000000)     ;; mem 9000-97ff is free
          (byte #b11100000)     ;; mem 9800-9cff is free, 9d00..9dff = first free code page, 9e00..9eff stack page, 9f00..9fff cell page
          (byte #b11111111)     ;; mem A000-A7ff is unavailable
          (byte #b11111111)     ;; mem A800-Afff is unavailable
          (byte #b11111111)     ;; mem B000-B7ff is unavailable
          (byte #b11111111)     ;; mem B800-Bfff is unavailable
          (byte #b00000000)     ;; mem C000-C7ff is free
          (byte #b11000000)     ;; mem C800-Cdff is free, ce00-ceff = other memory management registers + bitmap, cf00-cfff =  used by next free page mapping
          (byte #b11111111)     ;; mem D000-D7ff is unavailable
          (byte #b11111111)     ;; mem D800-Dfff is unavailable
          (byte #b11111111)     ;; mem E000-E7ff is unavailable
          (byte #b11111111)     ;; mem E800-Efff is unavailable
          (byte #b11111111)     ;; mem F000-F7ff is unavailable
          (byte #b11111111)     ;; mem F800-Ffff is unavailable
          ))


;; initialize memory management (paging)
;; - setup 'next free page' information, basically initializing the whole page with zeros
;;
;; destroys: A Y
(define VM_INITIALIZE_MM_PAGE
  (flatten
   (list
    (label VM_INITIALIZE_MM_PAGE)

           ;; initialize NEXT_FREE_PAGE_PAGE (256 byte)
           (LDA !0)
           (TAY)
    (label VM_INITIALIZE_MM_PAGE__LOOP)
           ;; highbyte of this address should be using the constant NEXT_FREE_PAGE_PAGE
           ;; (STA $cf00,y) ;; encoded directly in the next couple of bytes
           (byte 153 0) (byte-ref NEXT_FREE_PAGE_PAGE)
           (INY)
           (BNE VM_INITIALIZE_MM_PAGE__LOOP)

           (RTS))))

;; INCOMPLETE!
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
          (LDA VM_ALLOC_PAGE__LIST_CELL_PAIRS_JT+1,y) ;; high byte from jump table
          (STA VM_ALLOC_PAGE__JT+2)                   ;; write high byte of jump target
          (LDA VM_ALLOC_PAGE__LIST_CELL_PAIRS_JT,y)   ;; low byte from jump table
          (STA VM_ALLOC_PAGE__JT+1)                   ;; write low byte of jump target
   (label VM_ALLOC_PAGE__JT)
          (JMP $0000)                                 ;; jump to
   ))

;; (JSR VM_ALLOC_PAGE__LIST_PAIR_CELLS)      ;; direct calls (if type is statically known)
;; (JSR VM_ALLOC_PAGE__CALL_STACK)


;; memory layout of a cell-pairs page
;; offset content
;; --------------
;; 00     unused
;; 01     ref-count for cell-pair at 04 (cell-pair 0)
;; 02     ref-count for cell-pair at 08 (cell-pair 1)
;; 03     ref-count for cell-pair at 0C (cell-pair 2)
;; 04..07  cell-pair 0
;; 08..0b  cell-pair 1
;; 0c..0f  cell-pair 2
;; 10     ref-count for cell-pair at 40 (cell-pair 3)
;; 11     ref-count for cell-pair at 44 (cell-pair 4)
;; ..3F   ref-count for cell-pair at fc (cell-pair 50)
;; 40     cell-pair 3
;; 44     cell-pair 4
;; ..fc   cell-pair 50
;;
;; VM_FREE_SLOT_FOR_PAGE + pageidx: holds the index within the page of the first free cell-pair on that page (0 = no free cell-pair on this page)
;; the free cell-pair holds in byte 0 of the cell-pair the offset of the next free cell-pair (0 = no other free cell-pair)
;;
;; allocate a complete new page and initialize it to hold reference counted cell-pairs
;; connect all cell-pairs in a free-list
;; also set the first free slot of this allocated page (in VM_FREE_SLOT_FOR_PAGE + pageidx)
(define VM_ALLOC_PAGE__LIST_CELL_PAIRS
  (list
   (label VM_ALLOC_PAGE__LIST_CELL_PAIRS)
          (JSR VM_ALLOC_PAGE__PAGE_UNINIT)
          ;; now initialize page in A

          (STA ZP_PTR+1)
          (TAY) ;; page used as idx
          (LDA !$04) ;; first free slot (after initialization)
          (STA VM_FREE_SLOT_FOR_PAGE,y)

          ;; first write all reference count fields (zero)
          (LDY !$01)
          (STY ZP_PTR)
          (LDY !$02)
          (LDA !$00) ;; reference count initialized with 0

   (label FIRST_RC_BLOCK__VM_ALLOC_PAGE__LIST_CELL_PAIRS)
          (STA (ZP_PTR),y)
          (DEY)
          (BPL FIRST_RC_BLOCK__VM_ALLOC_PAGE__LIST_CELL_PAIRS)

          (LDY !$10)
          (STY ZP_PTR)
          (LDY !$2F)

   (label SECOND_RC_BLOCK__VM_ALLOC_PAGE__LIST_CELL_PAIRS)
          (STA (ZP_PTR),y)
          (DEY)
          (BPL SECOND_RC_BLOCK__VM_ALLOC_PAGE__LIST_CELL_PAIRS)

          (STA ZP_PTR) ;; clear lowbyte of ptr
          ;; write all cell-pairs to point to next free one
          (LDA !$04)
   (label FIRST_CELL_PAIR_BLOCK__VM_ALLOC_PAGE__LIST_CELL_PAIRS)
          (TAY)
          (CLC)
          (ADC !$04)
          (STA (ZP_PTR),y)
          (CMP !$0C)
          (BMI FIRST_CELL_PAIR_BLOCK__VM_ALLOC_PAGE__LIST_CELL_PAIRS)

          ;; write last pointer in this block to next free cell pair (in next block)
          (TAY)
          (LDA !$40)
          (STA (ZP_PTR),y)

   (label SECOND_CELL_PAIR_BLOCK__VM_ALLOC_PAGE__LIST_CELL_PAIRS)
          (TAY)
          (CLC)
          (ADC !$04)
          (STA (ZP_PTR),y)
          (CMP !$FC)
          (BNE SECOND_CELL_PAIR_BLOCK__VM_ALLOC_PAGE__LIST_CELL_PAIRS)

          (RTS)))


;; whether a page is free or used is kept in VM_FREE_PAGE_BITMAP
;; each bit represents one page 1 = used, 0 = free
;; VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH keeps the max idx to start looking for a page that is free
;; parameter: a = page
;; result: (none)
(define VM_FREE_PAGE
  (list
   (label VM_FREE_PAGE)
          (PHA)

          (LSR)
          (LSR)
          (LSR)
          (CMP VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH) ;; check for max of highest page #
          (BMI VM_FREE_PAGE__CONTINUE)
          (STA VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH) ;; store new max

   (label VM_FREE_PAGE__CONTINUE)
          (TAY) ;; index into bitmap
          (PLA)
          (AND !$07)
          (TAX)
          (LDA VM_FREE_PAGE_BITMAP,y)
          (EOR BITS,x) ;; clear bit that must be set because this page was in use
          (STA VM_FREE_PAGE_BITMAP,y)
          (RTS)))

;; allocate a page (completely uninitialized), just the page, update the memory page bitmap (VM_FREE_PAGE_BITMAP)
;; parameter: (none)
;; result: A = allocated free page (uninitialized)
;; uses: A, Y, X, one stack value
(define VM_ALLOC_PAGE__PAGE_UNINIT
  (list
   (label VM_ALLOC_PAGE__PAGE_UNINIT)
          (byte-const TEMP_TYPE #xfa)
          (byte-const PAGE #xfc)

          (word-const OUT_OF_MEMORY_ERROR #xc100)

          ;; use a sensible place to start looking for free page, from top to bottom
          (LDY VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH)


  (label  CHECK_PAGE_BITMAP)
          (LDA VM_FREE_PAGE_BITMAP,y)
          (CMP !$FF) ;; all pages used?
          (BNE FP_FOUND)
          (DEY)
          (BPL CHECK_PAGE_BITMAP)
          (JMP OUT_OF_MEMORY_ERROR) ;; TODO if there are enqueued ref count decs, check those first

   (label FP_FOUND)
          (PHA)
          (TYA) ;; Y = index into bitmap -> A
          (STA VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH) ;; remember last page allocated (<- since always allocating form top -> bottom) this is the new top
          (ASL A)
          (ASL A)
          (ASL A)
          ;; now A has the topmost 5 bits set to the free page
          (STA PAGE)
          ;; now get the lower three bits (look for first bit not set)
          (LDX !$07)
          (PLA) ;; get bit map byte again

   (label SHIFT_OUT)
          (ASL A)
          (BCC UNSET_BIT_FOUND)
          (DEX)
          (BPL SHIFT_OUT)
          (BRK) ;; should never get here, there must be at least one bit not set

   (label UNSET_BIT_FOUND)
          (TXA)      ;; get the index of the bit into A (can be of value 0..7) => lower 3 bits
          (ORA PAGE) ;; combine with bits already found for page
          (STA PAGE) ;; store the full page idx

          ;; mark this page as used in the bitmap!
          (LDA VM_FREE_PAGE_BITMAP,y) ;; y should still hold the index into the bitmap
          (ORA BITS,x)                ;; x should still hold the index of the bit in the bitmap byte
          ;; make sure to set the bit of allocated page in page bitmap
          (STA VM_FREE_PAGE_BITMAP,y)
          (LDA PAGE) ;; return the new page in A
          (RTS)

   (label BITS)
          (byte #b00000001
                #b00000010
                #b00000100
                #b00001000
                #b00010000
                #b00100000
                #b01000000
                #b10000000)

   (label ALT_UNSET_BIT_FOUND) ;; 4 bytes less, mean 28 cycles more (calc BITS by shifting $01 x-times
          (TXA)
          (ORA PAGE) ;; combine with bits
          (STA PAGE)

          (LDA !$01)
          (label SHIFT_AGAIN)
          (ASL A)
          (DEX)
          (BPL SHIFT_AGAIN)
          (ROR)
          (ORA VM_FREE_PAGE_BITMAP,y)
          (STA VM_FREE_PAGE_BITMAP,y)
          (LDA PAGE)
          (RTS)
          ))



;; allocate a list pair cells page (initialized with free list etc)
;; parameter: (none)
;; result: A = allocated call stack page
(define VM_ALLOC_PAGE__CALL_STACK
  (list (label VM_ALLOC_PAGE__CALL_STACK)
               (RTS)))

;; page is in a
;; resulting ptr is in ZP_PTR
;; first free element is adjusted
;; ---
;; allocate a cell-pair from this page (if page has no free cell-pairs, a new page is allocated and is used to get a free cell-pair!)
;;
(define VM_ALLOC_CELL_PAIR_ON_PAGE
  (list
   (label ALLOC_NEW_PAGE_PREFIX__VM_ALLOC_CELL_PAIR_ON_PAGE)
          (JSR VM_ALLOC_PAGE__LIST_CELL_PAIRS)
          (LDA ZP_PTR+1)
          (STA VM_FREE_CELL_PAIR_PAGE)

   (label VM_ALLOC_CELL_PAIR_ON_PAGE) ;; <-- real entry point of this function
          (STA ZP_PTR+1) ;; safe as highbyte of ptr
          (TAX)
          (LDA VM_FREE_SLOT_FOR_PAGE,x)
          (BEQ ALLOC_NEW_PAGE_PREFIX__VM_ALLOC_CELL_PAIR_ON_PAGE) ;; allocate new page first

   (label CELL_ON_THIS_PAGE__VM_ALLOC_CELL_PAIR_ON_PAGE_)
          (STA ZP_PTR)
          (LDY !$00)
          (LDA (ZP_PTR),y) ;; next free cell
          (STA VM_FREE_SLOT_FOR_PAGE,x)
          (RTS)))

;; find out what kind of cell zp_ptr points to.
;;   in case of cell-pair,
(define VM_REFCOUNT_DECR
  (list
   (label VM_REFCOUNT_DECR)
          (LDA ZP_PTR+1)
          (LSR)
          (BCS DECR_CELL_PTR__VM_REFCOUNT_DECR)
          (LSR)
          (BCS DECR_CELL_PAIR__VM_REFCOUNT_DECR)
          ;; check other types of cells
          (RTS)

   (label DECR_CELL_PAIR__VM_REFCOUNT_DECR)
          (JMP VM_REFCOUNT_DECR_CELL_PAIR)
   (label DECR_CELL_PTR__VM_REFCOUNT_DECR)
          ;; implement VM_REFCOUNT_DECR_CELL_PTR
          (RTS)))

;; input: cell ptr in ZP_PTR
;; decrement ref count, if 0 deallocate
(define VM_REFCOUNT_DECR_CELL_PAIR
  (list
   (label VM_REFCOUNT_DECR_CELL_PAIR)
          (LDA ZP_PTR+1)
          (STA PAGE__VM_REFCOUNT_DECR_CELL_PAIR+2) ;; store high byte (page) into dec-command high-byte (thus +2 on the label)
          (LDA ZP_PTR)
          (LSR)
          (LSR)
          (TAX)
   (label PAGE__VM_REFCOUNT_DECR_CELL_PAIR)
          (DEC $c000,x) ;; c0 is overwritten by page (see above)
          (BNE DONE__VM_REFCOUNT_DECR_CELL_PAIR)
          (JMP VM_FREE_CELL_PAIR) ;; free delayed
   (label DONE__VM_REFCOUNT_DECR_CELL_PAIR)
          (RTS)))

(define VM_REFCOUNT_INCR_CELL_PAIR
  (list
   (label VM_REFCOUNT_INCR_CELL_PAIR)
          (LDA ZP_PTR+1)
          (STA PAGE__VM_REFCOUNT_INCR_CELL_PAIR+2) ;; store high byte (page) into inc-command high-byte (thus +2 on the label)
          (LDA ZP_PTR)
          (LSR)
          (LSR)
          (TAX)
   (label PAGE__VM_REFCOUNT_INCR_CELL_PAIR)
          (INC $c000,x) ;; c0 is overwritten by page (see above)
          (RTS)))

;; TODO: Free nonatomic (is cell-ptr, cell-pair)
;; parameter: zp_ptr
(define VM_FREE_NON_ATOMIC
  (list
   (label VM_FREE_NON_ATOMIC)
          (RTS)))

;; result: zp_ptr = free cell-pair
;; try to reuse root of free tree: use root but make sure to deallocate cell2 of the root (since this might still point to some data)
;; if no free tree available, find page with free cells (VM_FREE_CELL_PAIR_PAGE)
;; if no free cell page is available, allocate a new page and used the first free slot there
;; NOTE: the cell-pair is not initialized (cell1 and/or cell2 may contain old data that needs to be overwritten!)
(define VM_ALLOC_CELL_PAIR
  (list
   (label VM_ALLOC_CELL_PAIR)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          (BNE REUSE_CELL_PAIR__VM_ALLOC_CELL_PAIR)
          ;; no cell-pair to free available

          ;; get a cell-pair on the given page (or allocate a new page)
          (LDA VM_FREE_CELL_PAIR_PAGE) ;; TODO
          (JMP VM_ALLOC_CELL_PAIR_ON_PAGE)

   (label REUSE_CELL_PAIR__VM_ALLOC_CELL_PAIR)
          ;; put root of free tree into zp_ptr (and copy in TEMP_PTR of this function)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          (STA ZP_PTR+1)
          (STA TEMP_PTR__VM_ALLOC_CELL_PAIR+1)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
          (STA ZP_PTR)
          (STA TEMP_PTR__VM_ALLOC_CELL_PAIR)

          ;; set new tree root for free tree to original cell1
          (LDY !$01)
          (LDA (ZP_PTR),y)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          (DEY)
          (LDA (ZP_PTR),y)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)

          ;; check whether cell2 is atomic or ptr
          (LDY !$03)
          (LDA (ZP_PTR),y)
          (AND !$03)
          (BEQ CELL2_IS_ATOMIC__VM_ALLOC_CELL_PAIR) ;; no need to do further deallocation

          ;; write cell2 into zp_ptr
          (LDA (ZP_PTR),y)
          (PHA)
          (DEY)
          (LDA (ZP_PTR),y)
          (STA ZP_PTR)
          (PLA)
          (STA ZP_PTR+1)
          (JSR VM_REFCOUNT_DECR)

   (label CELL2_IS_ATOMIC__VM_ALLOC_CELL_PAIR)
          ;; restore zp_ptr to the cell-pair to be reused
          (LDA TEMP_PTR__VM_ALLOC_CELL_PAIR+1)
          (STA ZP_PTR+1)
          (LDA TEMP_PTR__VM_ALLOC_CELL_PAIR)
          (STA ZP_PTR)

          (RTS)

   (label TEMP_PTR__VM_ALLOC_CELL_PAIR)
          (word $0000)))

;; cell ptr is in ZP_PTR
;; -----
;; put the cell-pair itself as new root to the free-tree
;; put the old free-tree into cell1
;; tail call free on old cell1 in this cell-pair (if not atomic, if atomic no tail call)
;; result: this cell-pair is the new root of the free-tree for cell-pairs with:
;;              cell1 = old free tree root, cell2 = non-freed (yet) original cell
(define VM_FREE_CELL_PAIR
  (list
   (label VM_FREE_CELL_PAIR)

          ;; check cell1
          (LDY !$01)
          (LDA (ZP_PTR),y) ;; HIGHBYTE OF FIRST cell1
          (AND !$03)
          (BEQ CELL_1_ATOMIC__VM_FREE_CELL_PAIR)
          ;; make sure to call free on cell1 (could be any type of cell)
          ;; remember ZP_PTR

          ;; store cell1 into TEMP_PTR__VM_FREE_CELL_PAIR (for later tail call of free)
          (LDA (ZP_PTR),y)
          (STA TEMP_PTR__VM_FREE_CELL_PAIR+1)
          (DEY)
          (LDA (ZP_PTR),y)
          (STA TEMP_PTR__VM_FREE_CELL_PAIR)

   (label CELL_1_ATOMIC__VM_FREE_CELL_PAIR)
          ;; cell1 is atomic and can thus be discarded (directly)

          ;; simply add this cell-pair as head to free tree
          ;; set cell1 to point to old root
          (LDY !$01)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          (STA (ZP_PTR),y)
          (DEY)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
          (STA (ZP_PTR),y)
          ;; set new root to point to cell-pair
          (LDA ZP_PTR+1)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          (LDA ZP_PTR)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)

          ;; write original cell1 -> zp_ptr
          (LDA TEMP_PTR__VM_FREE_CELL_PAIR+1)
          (BEQ DONE__VM_FREE_CELL_PAIR)
          (STA ZP_PTR+1)
          (LDA TEMP_PTR__VM_FREE_CELL_PAIR)
          (STA ZP_PTR)

          (LDA !$00)
          (STA TEMP_PTR__VM_FREE_CELL_PAIR+1) ;; mark temp_ptr as clear

          (JMP VM_FREE_NON_ATOMIC) ;; chain call

   (label DONE__VM_FREE_CELL_PAIR)
          (RTS)

   (label TEMP_PTR__VM_FREE_CELL_PAIR)
          (word $0000)))

;; zp_ptr = pointer to cell-pair that is added to the free list on its page
(define VM_ADD_CELL_PAIR_TO_ON_PAGE_FREE_LIST
  (list
   (label VM_ADD_CELL_PAIR_TO_ON_PAGE_FREE_LIST)
          (LDX ZP_PTR+1)
          (LDA VM_FREE_SLOT_FOR_PAGE,x) ;; old first free on page
          (LDY !$00)
          (STA (ZP_PTR),y) ;; set old free to next free on this very cell
          (LDA ZP_PTR) ;; load idx within page
          (STA VM_FREE_SLOT_FOR_PAGE,x) ;; set this cell as new first free cell on page

          ;; clear refcount, too (should have been done already)
          (LSR)
          (LSR)
          (TAY);; y now pointer to refcount byte
          (LDA !$00)
          (STA ZP_PTR) ;; modify pointer such that zp_ptr points to beginning of page
          (STA (ZP_PTR),y) ;; clear refcount byte, too
          (RTS)))

(define VM_COPY_PTR2_TO_PTR1
  (list
   (label VM_COPY_PTR2_TO_PTR1)
          (LDA ZP_PTR2+1)
          (STA ZP_PTR+1)
          (LDA ZP_PTR2)
          (STA ZP_PTR)
          (RTS)))

(define VM_COPY_PTR1_TO_PTR2
  (list
   (label VM_COPY_PTR1_TO_PTR2)
          (LDA ZP_PTR+1)
          (STA ZP_PTR2+1)
          (LDA ZP_PTR)
          (STA ZP_PTR2)
          (RTS)))

(module+ test #| vm-program <- complete page memory management |#
  (define vm-program (append VM_MEMORY_MANAGEMENT_CONSTANTS
                             VM_INITIALIZE_MM_PAGE
                          ;; VM_ALLOC_PAGE_JUMP_TABLE
                          ;; VM_ALLOC_PAGE

                          VM_ALLOC_PAGE__LIST_CELL_PAIRS
                          VM_FREE_PAGE
                          VM_ALLOC_PAGE__PAGE_UNINIT
                          VM_ALLOC_PAGE__CALL_STACK
                          VM_ALLOC_CELL_PAIR_ON_PAGE
                          VM_REFCOUNT_DECR
                          VM_REFCOUNT_DECR_CELL_PAIR
                          VM_REFCOUNT_INCR_CELL_PAIR
                          VM_FREE_NON_ATOMIC
                          VM_ALLOC_CELL_PAIR
                          VM_FREE_CELL_PAIR
                          VM_CELL_PAIR_SET_NIL
                          VM_CELL_PAIR_SET_INT_0
                          VM_COPY_PTR2_TO_PTR1
                          VM_COPY_PTR1_TO_PTR2

                          (list (org #xcec0))
                          VM_INITIAL_MM_REGS
                          (list (org #xced0))
                          VM_FREE_PAGE_BITMAP
                          )))

(module+ test #| use case 1 allocate, free, reallocate single cell-pair |#
   ;; use case: 1 allocate, free, reallocate single cell-pair
   ;; a- allocate cell-pair (auto increment?) [zp_ptr = (nil . nil), free-tree-top = nil, rc(zp_ptr) = 1]
   ;; a- (increment ref count of cell-pair)   [zp_ptr = (nil . nil), free-tree-top = nil, rc(zp_ptr) = 1]
   ;; a- set cell1 and cell2 to int cells (equivalent to '(1 . 2)) [zp_ptr = (1 . 2), free-tree-top = nil, rc(zp_ptr) = 1]
   ;; b- decrement ref count of cell-pair <- should put the cell-pair on the free tree [zp_ptr = x, free-tree-top = (nil . 2), rc((nil . 2)) = 0]
   ;; c- allocate cell-pair (again) <- should recycle the top of the free tree [z_ptr = (nil . nil), free-tree-top = nil, rc((nil . nil)) = 1]

  (define use-case-1-a-code (list (org #xc000)
                  (JSR VM_INITIALIZE_MM_PAGE)
                  (JSR VM_ALLOC_CELL_PAIR)
                  (JSR VM_REFCOUNT_INCR_CELL_PAIR)
                  ;; set cell2 to int 0
                  (LDY !$03)
                  (JSR VM_CELL_PAIR_SET_INT_0)
                  ;; set cell1 to int 0
                  (DEY)
                  (JSR VM_CELL_PAIR_SET_INT_0)))
  (define use-case-1-a
    (append use-case-1-a-code
            (list (BRK))
            vm-program))

  (define use-case-1-a-state-before (6510-load-multiple (initialize-cpu) (assemble-to-code-list use-case-1-a)))
  (define use-case-1-a-state-after  (parameterize ([current-output-port (open-output-nowhere)]) (run-interpreter-on use-case-1-a-state-before)))

  (check-equal? (memory-list use-case-1-a-state-after #xfb #xfc)
                '(#x04 #xcd)
                "zp_ptr -> $cd04 = first free cell-pair on page $cd after initialization")
  (check-equal? (memory-list use-case-1-a-state-after #xcd01 #xcd01)
                '(#x01)
                "refcount for first cell-pair allocated = 1")
  (check-equal? (memory-list use-case-1-a-state-after #xcd04 #xcd07)
                '(#x00 #x00 #x00 #x00)
                "cell1=int0 cell2=int0")
  (check-equal? (memory-list use-case-1-a-state-after #xcfcd #xcfcd)
                '(#x08)
                "next free cell-pair on page $cd is at $08")

  (define use-case-1-b-code
    (append use-case-1-a-code
            (list (JSR VM_REFCOUNT_DECR_CELL_PAIR))))
  (define use-case-1-b
    (append use-case-1-b-code
            (list (BRK))
            vm-program))

  (define use-case-1-b-state-before (6510-load-multiple (initialize-cpu) (assemble-to-code-list use-case-1-b)))
  (define use-case-1-b-state-after  (parameterize ([current-output-port (open-output-nowhere)]) (run-interpreter-on use-case-1-b-state-before)))

  (check-equal? (memory-list use-case-1-b-state-after #xcec5 #xcec6) ;;
                '(#x04 #xcd )
                "root of free tree is cell-pair at $cd04")
  (check-equal? (memory-list use-case-1-b-state-after #xcd01 #xcd01)
                '(#x00)
                "refcount for cell-pair freed = 0")
  (check-equal? (memory-list use-case-1-b-state-after #xcfcd #xcfcd)
                '(#x08)
                "next free cell-pair on page $cd is still $08")

  (define use-case-1-c-code
    (append use-case-1-b-code
            (list
             ;; clear zp_ptr just to make sure
             (LDA !$00)
             (STA $fb)
             (STA $fc)
             (JSR VM_ALLOC_CELL_PAIR))))
  (define use-case-1-c
    (append use-case-1-c-code
            (list (BRK))
            vm-program))

  (define use-case-1-c-state-before (6510-load-multiple (initialize-cpu) (assemble-to-code-list use-case-1-c)))
  (define use-case-1-c-state-after  (parameterize ([current-output-port (open-output-nowhere)]) (run-interpreter-on use-case-1-c-state-before)))
  (check-equal? (memory-list use-case-1-c-state-after #xfb #xfc) ;;
                '(#x04 #xcd )
                "allocated cell-pair is reused cell-pair of free tree")
  (check-equal? (memory-list use-case-1-c-state-after #xcec5 #xcec6) ;;
                '(#x00 #x00 )
                "root of free tree is initial again")
  (check-equal? (memory-list use-case-1-c-state-after #xcd01 #xcd01)
                '(#x00)
                "refcount for (reused) cell-pair = 1")
  (check-equal? (memory-list use-case-1-c-state-after #xcfcd #xcfcd)
                '(#x08)
                "next free cell-pair on page $cd is still $08"))

(module+ test #| use case: allocate, free, reallocate small list of cell-pairs |#
  ;; use case: allocate, free, reallocate small list of cell-pairs
  ;; - allocate cell-pair [A(nil . nil), free-tree-top = nil, rc(A) = 1]
  ;; - set cell1 to an int [A(1 . nil), free-tree-top = nil, rc(A) = 1]
  ;; - allocate cell-pair [A(1 . B(nil . nil)), free-tree-top = nil, rc(A) = 1, rc(B) = 1]
  ;; - set cell2 to allocated cell-pair
  ;; - set cell1 to an int [A(1 . B(2 . nil)), free-tree-top = nil, rc(A) = 1, rc(B) = 1]
  ;; - (set cell2 to nil) => (equivalent to '(1 2))
  ;; - decrement list head (cell-pair) <- should put the head of the list on the free tree, keeping the pointer to the next cell-pair
  ;;                                  [A(nil . B(2 . nil)), free-tree-top = A, rc(A) = 0, rc(B) = 1]
  ;; - allocate cell-pair <- should recycle the top of the free tree, decrement ref count of cell2, top of free tree will be set to cell2 and
  ;;                        [NA(nil . nil), free-tree-top = B, rc(A) = 1, rc(B) = 0]
    (define use-case-2-a-code (list (org #xc000)
                  (JSR VM_INITIALIZE_MM_PAGE)
                  (JSR VM_ALLOC_CELL_PAIR)
                  (JSR VM_REFCOUNT_INCR_CELL_PAIR)
                  ;; set cell2 to nil
                  (LDY !$03)
                  (JSR VM_CELL_PAIR_SET_NIL)
                  ;; set cell1 to int 0
                  (DEY)
                  (JSR VM_CELL_PAIR_SET_INT_0)
                  (JSR VM_COPY_PTR1_TO_PTR2)
                  (JSR VM_ALLOC_CELL_PAIR)
                  (JSR VM_REFCOUNT_INCR_CELL_PAIR)
                  ;; set cell2 to zp_ptr2->
                  (LDY !$03)
                  (LDA ZP_PTR,y) ;; load from zp_ptr2+1 ( = zp_ptr+3)
                  (STA (ZP_PTR),y) ;; write into high byte of cell2
                  (DEY)
                  (LDA ZP_PTR,y) ;; load from zp_ptr2 ( = zp_ptr+2)
                  (STA (ZP_PTR),y) ;; write into low byte of cell2
                  (DEY)
                  (JSR VM_CELL_PAIR_SET_INT_0) ;; set cell1 to int 0
                  ))
  (define use-case-2-a
    (append use-case-2-a-code
            (list (BRK))
            vm-program))

  (define use-case-2-a-state-before (6510-load-multiple (initialize-cpu) (assemble-to-code-list use-case-2-a)))
  ;; (run-debugger-on use-case-2-a-state-before)
  (define use-case-2-a-state-after  (parameterize ([current-output-port (open-output-nowhere)]) (run-interpreter-on use-case-2-a-state-before)))
  (check-equal? (memory-list use-case-2-a-state-after #xfb #xfe)
                '(#x08 #xcd #x04 #xcd)
                "zp_ptr -> $cd08, zp_ptr2 -> $cd04 = first two free cell-pairs on page $cd after initialization")
  (check-equal? (memory-list use-case-2-a-state-after #xcd04 #xcd0b)
                '(#x00 #x00 #x00 #x02 #x00 #x00 #x04 #xcd )
                "cell-pairs contain (int0 . -> next cell), (int 0 . nil)")
)
