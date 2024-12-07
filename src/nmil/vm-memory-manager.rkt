#lang racket/base
;; [[pdfview:~/Downloads/Small memory software patterns for limited memory systems.2001.pdf::261++0.00][Small memory software patterns for limited memory systems.2001.pdf: Page 261]]

#|

implementation of basic memory primitives for the native 6510 assembler implementation of mil.

primitives are e.g. allocation of cell-pair(s)
evalation stack operations
call frame primitives etc.

|#


;; IDEA: use the following idea in more situations:
;;       store high byte in one page
;;       and store low byte in another page (same index)
;;       32 bit values may as well be spread of 4 pages, storing all at one index!
;;       e.g: cell-value-stack (since cells are always 16 bit)
;;            store lowbyte in page I
;;            store highbyte in page J
;;       advantage: use same index (for pop/push inc/dec only once)
;;                  doubles the number of objects before new allocation is needed
;;                  e.g. push a value onto the stack:
;;                       ZP_CS_LB_PAGE (cell-stack page of low bytes) (ZP_CS_LB_PAGE-1 contains 0) such that ZP_CS_LB_PAGE-1 can be used as ptr
;;                       ZP_CS_HB_PAGE (cell-stack page of high bytes) (ZP_CS_HB_PAGE-1 contains 0) such that ZP_CS_LB_PAGE-1 can be used as ptr
;;                       ZP_CS_IDX is the current tos
;;
;;                       ;; PUSH A/X onto stack
;;                       LDY ZP_CS_IDX
;;                       INY                       ;; just one increment
;;                       STA (ZP_CS_LB_PAGE-1),y
;;                       STX (ZP_CS_HB_PAGE-1),y
;;                       STY ZP_CS_IDX             ;; store new tos idx
;;                       ;; that's it
;;
;;       are there any advantages to store cells in this way?
;;       where does the reference counting byte go in that case (maybe just into another page?)
;;       ==> cell-ptr's could be stored in 2+1 pages, lowbytes, highbytes and refcounts
;;       ==> cell-pair-ptr's could be store in 4+1 pages, lowbyte car, highbyte car, lowbyte cdr, highbyte cdr, refcounts
;;       allocation of these "pages" will then

;; IDEA: programs/processes have their own allocation pages => terminating a process means, all pages allocated by the process can be freed
;;       alternative: shared, process allocates using shared pages, terminating the process will free all entries (not the pages), possibly leading to pages, not freed, because some slots remain allocated.
;;       - each process has (a copy of) the following
;;         VM_FREE_CELL_PAIR_PAGE                 (1b)
;;         VM_FREE_CODE_PAGE                      (1b)
;;         VM_FREE_CALL_STACK_PAGE                (1b)
;;         VM_FREE_CELL_PAGE                      (1b)
;;         VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE    (2b)
;;         VM_FREE_M1_PAGE_P0         (P0..P3)    (4b) 
;;         VM_LIST_OF_FREE_CELLS                  (2b)
;;       - each process running needs (a copy of) the following interpreter values
;;         (lots of these values are restored when returning from a function, maybe this can be used to not copy too much during process switch (after function or on function call)
;;         ZP_CELL_STACK_TOS                      (1b)
;;         ZP_VM_PC                               (2b)
;;         ZP_PARAMS_PTR                          (2b)
;;         ZP_LOCALS_PTR                          (2b)
;;         ZP_CELL_STACK_BASE_PTR                 (2b)
;;         ZP_CALL_FRAME                          (2b)
;;         ZP_RT                                  (2b)
;;
;;
;; IDEA: don't do any ref counting on register (RT, RA)
;;       inc ref count of cell, pointed to by RT if pushed on cell-stack (only if RT holds a pointer, of course) 
;;       dec ref count of cell, pointed to by TOS, if popped from cell-stack (into RT), only if TOS (then RT) holds a pointer
;;       additionally, if a pointer is written into a heap allocated object (e.g. cell-ptr, cell-pair-ptr, cell-m1-ptr), then the pointed to cells ref count is incremented
;;                     if a cell is written into a heap allocated object, overwriting a pointer, the pointed to cells ref count is decremented
;;                     if a heap allocated object is collected (ref count drops to 0), all referenced cells ref count is decremented
;;       is this enough?
;;
;; decision:
;;   RT = 00 (low byte) is equivalent to RT is empty!
;;   use new pointer tagging scheme (makes tagged-low-byte obsolete):            examples (low, then high byte):
;;     (new) zzzz zzz0 = cell-ptr (no change on cell-ptr pages)                  0000 001[0]    1100 1101   cd02 (first allocated slot in cell-ptr page)
;;     (new) xxxx xx01 = cell-pair-ptr (change on cell-pair-ptr pages!)          0000 01[01]    1100 1101   cd05 (first allocated slot in a cell-pair-ptr page)
;;     (new) 0iii ii11 = int-cell (no direct adding of highbyte possible)        [0]000 10[11]  0001 1000   0218 (decimal 2*256+16+8 = 536) <- high byte comes first in this special int encoding
;;     (new) 1111 1111 = byte-cell (char|bool|bcd digits)                        [1111 1111]    0000 0001   01  <- payload is in high byte
;;     (new) 1000 0011 = cell-array-header                                       [1000 0011]    0000 0100   04 cells in array
;;     (new) 1000 0111 = cell-native-array-header                                [1000 0111]    0000 1000   08 bytes in array 
;;
;;     (new) cell-pair-ptr page layout 
;;                       00     #b01xx xxxx page type + number of used slots
;;                       01     ref-count cell-pair at 05 (cell-pair 0)
;;                       02     ref-count cell-pair at 09 (cell-pair 1)
;;                       03..04  unused (2)
;;                       05..08  cell-pair 0
;;                       09..0c  cell-pair 1
;;                       0d..0f  unused (3)
;;                       10     ref-count for cell-pair at 40 (cell-pair 2)
;;                       11     ref-count for cell-pair at 44 (cell-pair 3)
;;                       ..3e    ref-count for cell-pair at f9 (cell-pair 48)
;;                       3f..40  unused (2)
;;                       41..44  cell-pair 2
;;                       45..48  cell-pair 3
;;                       ...
;;                       f9..fc  cell-pair 48
;;                       fd..fe  unused (2)
;;                       ff     previous page of this type
;;
;;   implementation steps:
;;   - change int detection and calculation
;;   - change cell-array-header + cell-native-array-header detection (if applicable an existent)
;;
;; DONE: if cell-ptr and cell-pair-ptr would use the bytes as is (with having to separately hold a tagged low byte),
;;       additionally masking out the tagged byte + copying during stack push and pop would not be necessary
;;       problem: lda (zp_rt),y must then point to a cell or a cell-pair
;;                if cell-ptr lowbyte has at bit0 a 0 this would work
;;                however, cell-pair-ptr (to be able to differentiate from cell-ptr) would have to set bit0 to 1 and this lda (zp_rt),y would point to a different location
;;                => cell-pair pages need to be organized differently (or cell pages)
;;                   current memory layout
;;                   page type: cell-pairs page (its actually randomly growing, fixed slot size (4b), ref counted page)
;;                   memory layout of a cell-pairs page (refcount @ ptr >> 2) 51 cells
;;                   offset content
;;                   00     #b01xx xxxx page type + number of used slots
;;                   01     ref-count for cell-pair at 04 (cell-pair 0)
;;                   02     ref-count for cell-pair at 08 (cell-pair 1)
;;                   03     ref-count for cell-pair at 0C (cell-pair 2)
;;                   04..07  cell-pair 0
;;                   08..0b  cell-pair 1
;;                   0c..0f  cell-pair 2
;;                   10     ref-count for cell-pair at 40 (cell-pair 3)
;;                   11     ref-count for cell-pair at 44 (cell-pair 4)
;;                   ..3e   ref-count for cell-pair at fc (cell-pair 49)
;;                   3f    unused
;;                   40     cell-pair 3
;;                   44     cell-pair 4
;;                   ..fb  cell-pair 49
;;                   fc..fe unused
;;                   ff    previous page of this type
;;
;;                   old c004 = cell pair ptr,
;;                   new zzzz zzz0 = cell-ptr, xxxx xx01 = cell-pair-ptr (looses one cell-pair), 0iii ii11 = int (no direct adding of highbyte possible)
;;                       00
;;                       01     ref count cell pair at 05 (cellpair0)
;;                       02     ref-count for cell-pair at 08 (cell-pair 1)
;;                       03..04  unused
;;                       05..08  cell-pair 0
;;                       09..0c  cell-pair 1
;;                       0d..0f  unused
;;                       10     ref-count for cell-pair at 40 (cell-pair 2)
;;                       11     ref-count for cell-pair at 44 (cell-pair 3)
;;                       ..3e   ref-count for cell-pair at fc (cell-pair 48)
;;                       3f..40 unused
;;                       41     cell-pair 2
;;                       45     cell-pair 3
;;                       ..fc   cell-pair 48
;;                       fd..fe unused
;;                       ff    previous page of this type
;;
;;                => alternative cell pages
;;                   current memory layout
;;                   page type cell page (slot size 2b) (refcount @ ptr >> 1) 84 cells (85th slot is used for previous page pointer)
;;                   offset content
;;                   00     #b1zzz zzzz page type + number of used slots
;;                   01     ref-count for cell at 02 (cell 0)
;;                   02..03 cell 0
;;                   04     ref-count for cell at 08 (cell 1)
;;                   ...
;;                   07     ref-count for cell at 08 (cell 4)
;;                   08..09 cell 1
;;                   ...
;;                   0e..0f cell 4
;;                   10    ref-count for cell at 20 (cell 5)
;;                   ...
;;                   1f    ref-count for cell at 20 (cell 20)
;;                   20..21 cell 5
;;                   ...
;;                   3e..3f cell 20
;;                   40..7e ref-count for cell at 80..fc (cell 21..83)
;;                   7f    unused
;;                   80..fd cell 21..83
;;                   fe    unused
;;                   ff    previous page of this type
;;
;;                   old c002 = cell ptr,
;;                   new: zzzz zzz1 = cell-ptr (looses 3 cells), xxxx xx00 = cell-pair ptr (uses most compact layout), 0iii ii10 = int (no direct adding of highbyte possible!
;;                   new page layout
;;                       00        page type
;;                       01        ref count cell0
;;                       02        unused?
;;                       c003..c004 cell0
;;                       05         ref count cell1
;;                       ..
;;                       07         ref count cell3
;;                       0b..0c     cell 1 1011
;;                       0d..0e     cell 2 1101
;;                       10         ref count 3
;;                       11         ref count 4
;;                       ...
;;                       1f         unused?
;;                       21..22      cell3 0010 0001
;;                       ...
;;                       3d..3e      cell17 0011 1101
;;                       3f         unused?
;;                       40..7e      ref count for cell 81..fe (cell 18..80)
;;                       7f..80      unused?
;;                       81..82      cell 18 1000 0001
;;                       83..fc      cells 19..79
;;                       fd..fe      cell 80 1111 1101 .. 1111 1110
;;                       ff         previous page of this type
;;
;; DONE: tos is always a register held in zp (e.g. now zp_ptr, future zp_rt)
;;       have additional "registers", capable of holding cells zp_ra, zp_rb ...
;;       push zp_rt on stack only if necessary => operations working on one value only do no push/pop actions
;;         e.g. (car a-list), a-list is in zp_rt, car replaces zp_rt with the head of a-list, no stack op necessary!
;;              (push-int-0), pushes zp_rt, putting int-0 into zp_rt
;;              empty stack does now mean: no value on the stack and no value in zp_rt
;;              pop: fill zp_rt with new tos, popping it off the call-frame stack
;;              pop last item:  discard zp_rt (and mark stack as empty)
;;              push on empty stack: write pushed into zp_rt
;;              push non empty stack: push zp_rt onto the stack in the call-frame and write pushed value into zp_rt
;;              (cons a-val a-list): move a-val (from zp_rt) to zp_ra, pop (filling zp_rt with a-list) execute cons, result in zp_rt
;;       BENEFIT: - less actual pushes of values into the call-frame stack (e.g. car none at all)
;;                - call-frame stack size is always 1 item smaller!
;;                - maybe some harmonization of zp register usage?
;;       DRAWBACK: additional full/empty stack detection complexity (is it really complex? <- check before optimization)
;;                     <- ideas to prevent that (NONE IMPLEMENTED YET)
;;                        - statically compile first bytecode pushing into the stack
;;                          - with prefix byte code [adds 1 byte to each function]
;;                          - into specific byte code directly writing into zp_rt [wastes available byte codes])
;;                          this could collide with tail call recursion
;;                          upon function call change behavior such that first push will not copy zp_rt into stack (pop must be changed too)
;;                          and all subsequent calls do (e.g. change jump target, rechanging it to regular behavior)
;;                          pop might work accordingly (last actual stack manipulation will change pop/push target)
;;                        - require always 1 additional dummy local (before first actual stack entry)
;;                          this will allow to not have any special local but will loose the benefit of reduced stack size!
;;
;;       common operations (should be derived from byte-code functions):
;;         start with car, cdr, cons, push: local/param/const, int+/-, call, tail-call?
;;         e.g. zp_rt interpret as cell-pair-ptr, write, cellX of cell-pair into zp_rt again (or some other register?) <- used for car/cdr
;;              zp_rt interpret as cell-ptr, write cell pointed to into zp_rt again (or some other register)?
;;              copy zp_rt to other register (and vice versa)
;;              copy call-frame stack value @ idx into cellX of cell-pair, pointed to by zp_rt
;;              write zp_rt -> local / param of this function
;;              copy local/param -> cellX of cell-apri in zp_rt
;;              copy call-frame stack value @ idx into array pointed to by zp_rt
;;
;;       possible implementation steps:
;;         implement in parallel to existing solution
;;
;;
;; DONE: no memory bitmap, use free slot bytes to encode whether page is free or not
;; this would reduce complexity in finding free pages, free blocks of pages etc.
;; (since free slots may never hold the value 00, 01, fe, ff, these values can be used to encode the state of the page
;;  e.g. 00 = allocated but full page (0 allows BEQ to be used easily to check whether page is full during slot allocation!)
;;       01 = system page (unavailable for memory management)
;;       ... = allocated with free slots
;;       fe = ???
;;       ff = free page,
;;
;; code pages - granularity: modules
;; each module is loaded as a whole, modules should be unloadable, relocatable
;; modules are restricted to max 256 (loaded)?
;; loading a module does
;;   load all required modules (recursive until topmost module is found) <- no circles allowed
;;   resolve required modules functions/variables to ids <- must have been loaded
;;   assign ids to all functions/variables in this module
;;   patch own loaded bytecode to use (required modules or own) functions/variable ids (<- module needs patch table)
;; unloading a module does
;; relocating a module does

;; static calling a function does (e.g. w/i a module)
;;   allocate call-frame (#params + #locals is known)
;;   save current exec state->call frame
;;   jump to bytecode of function called (location is known)

;; dynamic calling a function does
;;   resolve id to bytecode location (16-bit->16-bit translation)
;;   get #params
;;   get #locals (max)
;;   allocate call-frame
;;   save current exec state->call frame
;;   jump to bytecode of function called

;; return from function does
;;   pop call frame (restoring saved exec state)

;; idea: trace byte code execution
;; idea: collect metrics of calls


;; naming



;; naming: atomic cell
;;         cell                      :: 16 bit value (finest granular memory managed block)
;;         atomic cell               :: a cell that has no followup value and is complete in itself (currently int-cell, byte-cell, cell-ptr, cell-pair-ptr)
;;         cell-ptr                  :: an atomic cell, lowest bit of low byte is set, points to a cell (of any type)
;;                                     lowbyte: #bxxxx xxx0
;;                                     highbyte = page
;;         cell-pair                 :: a pair of two cells, cell0 and cell1 (in memory: 00 lowbyte-cell0, 01 highbyte-cell0, 02 lowbyte-cell1, 03 highbyte-cell1),
;;                                     each cell can be of any atomic cell
;;         cell-pair-ptr             :: an atomic cell, second lowest bit is set, lowest bit is unset, points to a cell-pair
;;                                     lowbyte: #bxxxx xx01 
;;                                     highbyte = page
;;         int-cell                  :: an atomic cell having 13 bit as payload
;;                                     lowbyte mask: #b0xxx xx11, xxxxx = high bits of int
;;                                     highbyte = lowbyte of int
;;         byte-cell (char|bool)     :: an atomic cell having one byte as payload
;;                                     lowbyte mask: #b1111 1111
;;                                     highbyte = payload
;;         complex cell              :: a cell that functions as header for followup values (follows directly in memory)
;;                                     complex cells cannot be pushed on the stack, they can only be pointed to by cell-ptr!
;;         (cell-structure-header    :: a complex cell that defines a structure)
;;         cell-array-header         :: a complex cell that defines an array, defining the number of cells in the second byte
;;                                     a structure is an array of cells
;;                                     lowbyte mask: #b1000 0011
;;                                     highbyte: #of cells in this array
;;                                     n*2 bytes with cells <- each cell needs to be gc'ed separately
;;         cell-native-array-header  :: a complex cell that defines an array of bytes
;;                                     a string is an native array of bytes
;;                                     lowbyte mask: #b1000 0111
;;                                     highbyte: #of bytes in this array
;;                                     n bytes with byte payloads <- no gc of this necessary (holds plain values, no pointers, no cells)
;;         (cell-float-header        :: a complex cell that defines a float)
;;         page                      :: 256 byte memory managed unit, holding slots
;;         slot                      :: a fixed size portion of memory on a page (sizes are 2=cell, 4=cell=pair, 8 ...), only one size per page is allowed
;;         ref count                 :: byte counting how many pointers to this value exist, there can be pointer to pointers
;;         cell-stack aka eval-stack :: stack of cells. ZP_CELL_STACK_TOS holds the index, ZP_CELL_STACK_BASE_PTR holds the pointer to the base
;;                                                                      [RT]         RT is the top of the stack (even though not on the stack memory wise)
;;                                                                  n*2 [cell n]     
;;                                                                      ...
;;                                                                  02  [cell 1]
;;                                      ZP_CELL_STACK_BASE_PTR -->  00  [cell 0]
;;                                      Each cell on the stack is organzed as 00 highbyte, 01 lowbyte, 02 ... next entry <- highbyte comes first
;;                                      ZP_CELL_STACK_TOS points to the lowbyte of the current element below RT (cell n), = n*2+1

;; naming: m1 page px       :: page for slots with ref count at -1 position, with profile x (0..3) <- defines size and payload start offset
;;         call-frame page  :: page for call-frames (stack organized, no ref counting etc.)
;;         cell-pairs page  :: page for cell-pairs, (lowbyte) lsr x 2 to get ref count position
;;         cell page        :: page for cells, (lowbyte) lsr x 1 to get ref count position (last cell unusable)
;;         [s8 page          :: page for slots of size <=8, (lowbyte) lsr x 3 to get ref count position] optional
;;         fid->loc page    :: page that maps a function id to a location of first byte code
;;         code page        :: page holding byte code (and function meta data, module meta data?)
;;         constants page   :: page holding constants (not ref counted)
;;         page block       :: a number of consecutive pages allocated/freed as a block, allowing for larger memory objects (having less wasted bytes (e.g. for call-frames)?)

;; DONE: keep allocated #slots to detect empty pages (# drops to zero)
;; DONE: page 00 = page mod byte
;;            1xxx xxxx = (cell page) page with cells (slots of byte 2), xxxxxx = number of used cells 0..127 (actually only 85 possible)
;;            01yy yyyy = (cell-pairs page) page with cell-pairs (slots of byte 4) yyyyy = number of cells used 0..63 (actually only 51 possible)
;;            [001z zzzz = (s8 page) page with slots of (max) size 8 byte, zzzz = number of slots used 0..31 (actually only possible)]
;;            0001 0000 = (m1 page p0) page with buckets type 0 (byte at offset 02: holds the number of used slots)
;;            0001 0001 = (m1 page p1) page with buckets type 1 (byte at offset 02: holds the number of used slots)
;;            0001 0010 = (m1 page p2) page with buckets type 2 (byte at offset 02: holds the number of used slots)
;;            0001 0011 = (m1 page p3) page with buckets type 3 (byte at offset 02: holds the number of used slots)
;;            0001 1000 = (call-frame page) (stack organized, full+free detection already implemented)
;;            0001 1001 = (fid->loc page) page with 16 bit values (starting at $02), filled without gaps, next slot = offset to free, no ref counting
;;            0001 1010 = (code page) page with byte code and function meta data <- filled without gaps, next slot = offset to free, no ref counting
;;       page 01 = (code page, m1 page px, call-frame page) previous page of same type (<- currently only for pages with buckets and call-frame pages)
;;       page 02 = (m1 page px, s8 page) number of used slots
;;       page ff = (cell-pairs and cell page) previous page (in case of cell page = last cell stays unused!!)

;; existing: array of first free slot on the respective page (per page) (uses only the lower 7 bits) : uses 256 bytes cf00..cfff (idea to keep this data on the page itself?)
;;           each page points to the previous page (initially in allocation order)                   : uses 1 byte on page
;;           each page type points to the head of the (free) page list of this type                  : 1 byte per type (currently 8) [current free]
;; new:      head of list of pages that are completely free                                          : 1 byte per type

;; idea: allocate:
;;   during alloc (full): current page is full, find next non full (remove all fulls from this list from here on), set found non-full to current free
;;                        if no free is found, check list of completely free pages of this type,
;;                        if none is found, allocate new page (don't link with any full page!)
;;                        if none is left for allocation, check free list of other types
;;                        +- first free page pointer (points to full page, because it just got full)
;;                        [Ax]->[Bx]->[Cx]->[D-]->[E ]->[Fx]->[G-]
;;                   =>   remove full pages (their pointers must be cleared!) until first non full is found (or new pages is allocated)
;;                        +- first free page pointer
;;                        [D-]->[E ]->[Fx]->[G-]
;;
;;   during free (full->non-full):  if already part of the free list, do nothing, if not, add it as head of the free list
;;   during free (non-full->empty): naive: free,
;;          idea: keep number of free pages per type, only free pages > than minimum
;;          idea: move this behind at the end of the list (if it is the head),
;;          idea: keep list of completely free pages to speed up allocation of this type (since page needs no initialization)

;; worst case scenario:
;;   each bucket allocates until n pages are filled, then on each page all but 1 slots are freed => lots of pages with just one slot used
;;   (hopefully uncommon) Since no relocation of entries is possible, this "page"-level fragmentation is possible, even if unlikely

;; to keep a list of pages with free slots, each time a slot is not full (free slot offset != $00), it should not have full pages before it
;; => (alloc) a page getting full should be put behind the pages which have free slots
;; => (free) a page not full anymore should be moved before the full ones
;; algorithm:
;;   during alloc: page getting full (can be anywhere in the list) swaps down the list until the next is either $00 (no previous) or full itself
;;   during free:  page getting non-full (can be anywhere in the list) is put at the head of the free-page list
;;                 (or: optimization:) if the first is a non full page: right behind that one
;;                  -> this allows for a page that was "freshly" allocated to fill up before a page that has only one free slot is preferred
;;   during free:  a page that is left empty, is removed from the list and returned to the free pages
;;                 except if it is the last list of this type in the free list, then it is kept  (optimization: introduce a lower bound?, e.g. always keep 4+ free cell-pair pages to speed up allocation)

;; page attributes:
;;  - stack growing :: data will be allocated/deallocated as stack
;;  - randomly growing :: data will allocate/deallocate randomly
;;  - variable slot size :: slots have any size within the same page
;;  - fixed slot size :: slots have one size (or smaller) within this page
;;  - ref counted :: slots are ref counted and deallocated if ref count drops to 0 (used only in randomly growing pages)

;; valid combinations:
;;   stack growing, variable slot size :: used e.g. for call frame (stack)
;;   randomly growing, fixed slot size, ref counted  :: used e.g. for cell-pairs, cells, structures?
;;   randomly growing (but no deallocation), variable slot size, no ref couting :: permanent byte code routines

;; measures to ensure ease of allocation/deallocation etc.
;;   stack growing
;;     keep backward pointer to previous slot / page (for pop)
;;     keep first free slot on page (for push)
;;   randomly growing
;;     keep first free slot on page (for alloc)
;;     keep an easy way to get to the next free slot/add a freed slot to the existing free slots
;;   ref counted
;;     keep ref count per slot (on page), with an easy way to get the offset from the slot offset
;;   fixed slot size
;;     free/used slots can be kept in a bitmap (but also in a free list)
;;   variable slot size
;;     free/used slots are kept in a list

;; current page types and measures
;;   cell-pair-page
;;     - keep first free slot
;;     - keep free slots in a linked list
;;     - ref count is achieved by lsr x 2 (fixed size 4 bytes => 1 byte ref count)
;;   call-frame-page
;;     - keep first free slot
;;     - keep backward pointer to previous slot
;;     - keep id of previous page
;;   perma-bytecode-page
;;     - keep first free slot
;;     - keep id of previous page
;;     - if next allocation does not fit, check previous pages
;;   ref-counted-fixed-slot-page (cell-pair-page should be a special case for this)

;; invariant: any slot must start on an even memory location (since bit0 is used as tag for a pointer)
;; invariant: any cell-pair slot must start on a memory location divisable by 4 (since bit0 and bit1 is used as tag for the pointer)

;; ---------------------------------------- call-frame page
;; page type: call-frame page (its actually stack growing, variable slot size page)
;; => allocation/deallocation is always done on tos
;;    no need for a free list (stack structure is coded into the stack pages)
;;    need for max size left
;; memory layout of call frame page (organized in stack)
;;  00 : #b0001 0000 page type call-frame
;;  01 : previous page (just high byte), 00 for first stack page
;;  02 : first frame payload byte 0
;;  ... : first frame payload byte size-1
;;  free-1 : size of (prev) frame
;;  free : next payload
;; ...ff :
;;
;; VM_FIRST_FREE_SLOT_ON_PAGE + pageidx: holds free-idx (initially 02) <- points to the first free byte (-1 = size of previous)

;; OLD OUTDATED: SEE TOP OF FILE-------------------------------------- cell-pairs page
;; page type: cell-pairs page (its actually randomly growing, fixed slot size (4b), ref counted page)
;; memory layout of a cell-pairs page (refcount @ ptr >> 2) 51 cells
;; offset content
;; 00     #b01xx xxxx page type + number of used slots
;; 01     ref-count for cell-pair at 04 (cell-pair 0)
;; 02     ref-count for cell-pair at 08 (cell-pair 1)
;; 03     ref-count for cell-pair at 0C (cell-pair 2)
;; 04..07  cell-pair 0
;; 08..0b  cell-pair 1
;; 0c..0f  cell-pair 2
;; 10     ref-count for cell-pair at 40 (cell-pair 3)
;; 11     ref-count for cell-pair at 44 (cell-pair 4)
;; ..3e   ref-count for cell-pair at fc (cell-pair 50)
;; 3f    unused (1)
;; 40     cell-pair 3
;; 44     cell-pair 4
;; ..fb   cell-pair 50
;; fc..fe unused (3)
;; ff    previous page of this type
;;
;; VM_FIRST_FREE_SLOT_ON_PAGE + pageidx: holds the index within the page of the first free cell-pair on that page (0 = no free cell-pair on this page)
;; the free cell-pair holds in byte 0 of the cell-pair the offset of the next free cell-pair (0 = no other free cell-pair)
;;

;; ---------------------------------------- cell page
;; page type cell page (slot size 2b) (refcount @ ptr >> 1) 84 cells (85th slot is used for previous page pointer)
;; offset content
;; 00     #b1zzz zzzz page type + number of used slots
;; 01     ref-count for cell at 02 (cell 0)
;; 02..03 cell 0
;; 04     ref-count for cell at 08 (cell 1)
;; ...
;; 07     ref-count for cell at 0e (cell 4)
;; 08..09 cell 1
;; ...
;; 0e..0f cell 4
;; 10    ref-count for cell at 20 (cell 5)
;; ...
;; 1f    ref-count for cell at 3e (cell 20)
;; 20..21 cell 5
;; ...
;; 3e..3f cell 20
;; 40..7e ref-count for cell at 80..fc (cell 21..83)
;; 7f    unused (1)
;; 80..fd cell 21..83
;; fe    unused (1)
;; ff    previous page of this type

;; ---------------------------------------- s8 page
;; page type slot size 8 (refcount @ ptr >> 3) 28 cells
;; offset content
;; 00     #b001x xxxx  page type + number of used slots
;; 01     previous page
;; 02..03 unused
;; 04..1f refcount cell 0..27
;; 20..27  -> 04 (cell 0)
;; ...
;; f8..ff -> 1f (cell 27)

;; ---------------------------------------- m1 page p0
;; page type slot size 16!  (refcount @ ptr-1) 14 slots
;; math: first entry $04, refcount @ -1, next slot += $12, slot-size = $11 (17)
;; offset content
;; 00     #b0001 0000 page type bucket with slot size 17 (either use this or the one above)
;; 01     previous page
;; 02     number of slots used
;; 03     refcount slot0
;; 04..14 slot0
;; 15    refcount slot1
;; 16..26 slot1
;; 27    refcount slot2
;; 28..38 slot2
;; ...
;; ed    refcount slot13
;; ee..fe slot13
;; ff    unused


;; ---------------------------------------- m1 page p1
;; page type slot size 29! (refcount @ ptr-1) 8 slots total
;; math: first entry $02, refcount @ -1, next slot += $1e, slot-size = $1d (29)
;; offset content
;; 00     #b0001 0001 page type bucket + slot size 29
;; 01     previous page
;; 02     number of used slots
;; 03..0f unused
;; 0f     refcount slot0
;; 10..2c slot0
;; 2d     refcount slot1
;; 2e..4a slot1
;; 4b     refcount slot2
;; 4c..68 slot2
;; ...
;; e1    refcount slot7
;; e2..fe slot7
;; ff    unused

;; ---------------------------------------- m1 page p2
;; page type slot size 49! (refcount @ ptr-1) 5 slots total
;; math: first entry $02, refcount @ -1, next slot += $32, slot-size = $31
;; offset content
;; 00     #b0001 0010
;; 01     previous page
;; 02     # of slots used
;; 03..04 unused
;; 05     refcount slot0
;; 06..36 slot0
;; 37     refcount slot1
;; 38..68 slot1
;; 69     refcount slot2
;; 6a..9a slot2
;; 9b     refcount slot3
;; 9c..cc slot3
;; cd     refcount slot4
;; ce..fe slot4
;; ff    unused

;; ----------------------------------------m1 page p3
;; page type slot size 83! (refcount @ ptr-1) 3 slots total
;; math: first entry $02, refcount @ -1, next slot += $54, slot-size = $53
;; offset content
;; 00     #b001 0011
;; 01     previous page
;; 02     number of slots used
;; 03     refcount slot0
;; 04..56 slot0
;; 57     refcount slot1
;; 58..aa slot1
;; ab     refcount slot2
;; ac..fe slot2
;; ff     unused

;; DONE: zero page cell stack may not be beneficial (can be put in regular memory)
;;       LDA zeropage,x  consumes 4 clocks,  LDA (zeropage),y consumes 6 clocks => no significant speedup that would justify the amount of copying
;;       LDA zeropage and LDA absolute differs in speed => storing registers not accessed through index makes sense
;;       indirect addressing can only be done on zp => zp_ptr and zp_ptr2 make sense, too

;;       call frame is created by call, but there is not need to copy (portions of) the eval stack
;;       the call frame allocates space for locals and that's it (it's still a stack, so push/pop is supported)
;;       alternative to tos: function entry tos + index
;;         upon function entry, the fe tos is written (once) and a separate index is held in zero page
;;         such that fe0-tos + index = actual tos => push pop work in index only (restriction value stack w/i one page to 128 cells)
;;       alternative continuous stack space: allocate pages, but how? function call will fix the parameters => that part does not change
;;       within one function, one page is enough, but an almost full page could be a problem:
;;         one solution: page grows until function call, then, if page is free enough continues with the given page, if page is not enough allocated new one
;;         minimum free space = 16 cells? = 32 byte => 256 - 32 = 224 (roughly) are available, can be used in combination with call-frame
;;      zp_vm_params -> [params]            <- actually previous eval stack
;;        callframe:    -----               <- new allocation starts here
;;                      [old zp_vm_pc]      <- pointer to code to return to (zp_vm_pc)
;;                      [old zp_vm_locals]  <- used to restore zp_vm_locals
;;                      [old zp_vm_params]  <- used to restore zp_vm_params
;;      zp_vm_locals -> [locals]            <- fixed number of slots kept for function execution
;;         zp_vm_tos -> [eval-stack]        <- size must fit into page (16 cells) <- could later be derived from actual function implementation
;;                                            (part of the eval stack is then again [params] for the next function called)
;; call to function:
;;      new zp_vm_params = zp_vm_tos - 2 * n-params (of the function called)
;;      allocated call frame [call-back] = zp_vm_pc
;;      new zp_vm_locals = allocated call frame + 6
;;      new zp_vm_tos    = allocated call frame + 2 * (n-locals + 3)  (of the function called)
;;      zp_vm_pc         = function called
;;
;; return from function:
;;      zp_vm_params     = (new) zp_vm_locals - 2 * (n-params + 3) (of function returned to)
;;      zp_vm_locals     = ? <- must be saved too
;;      zp_vm_tos        = (old) zp_vm_params - 2
;;      zp_vm_pc         = call-frame [call-back] = ((old) zp_vm_locals-6)
;;
;; NEW STACK STRUCTURE:
;;      ZP_CELL_STACK_BASE_PTR -> points to the start of the stack of the current (executing) function
;;      ZP_CELL_STACK_TOS      -> offset for ZP_CELL_STACK_BASE_PTR, pointing to the tagged (low) byte, ff = empty, 01 = 1 el on stack, 03 = 2 el on stack ...
;;                                LDA (ZP_CELL_STACK_BASE_PTR),ZP_CELL_STACK_TOS   = tagged low byte
;;                                LDA (ZP_CELL_STACK_BASE_PTR),ZP_CELL_STACK_TOS-1 = high byte
;;                                => pushing is done in reverse order (push high byte first, then push tagged low byte)
;;                             reading tos:
;;                                LDY ZP_CELL_STACK_TOS;
;;                                LDA (ZP_CELL_STACK_BASE_PTR),y  ;; gets tagged low byte
;;                                DEY
;;                                LDA (ZP_CELL_STACK_BASE_PTR),y  ;; gets untagged high byte
;;
;; IDEA: use larger block page allocation (e.g. not used one page but 4 pages as a block => less waste

(require (only-in racket/format ~a))

(require "../6510.rkt")
(require (only-in "../ast/6510-assembler.rkt" assemble assemble-to-code-list translate-code-list-for-basic-loader))
(require (only-in "../tools/6510-interpreter.rkt" peek-word-at-address cpu-state-clock-cycles))
(require (only-in "../ast/6510-calc-opcode-facades.rkt" LDA-immediate))
(require (only-in "../util.rkt" bytes->int))
(require (only-in racket/list flatten take empty? drop make-list))

(module+ test
  (require "../6510-test-utils.rkt")
  (require (only-in racket/port open-output-nowhere))
  (require (only-in "../tools/6510-disassembler.rkt" disassemble-bytes))
  (require (only-in "../tools/6510-debugger.rkt" run-debugger-on))
  (require (only-in "../ast/6510-command.rkt" ast-label-def-cmd? ast-label-def-cmd-label))

  (define (remove-labels-for code labels-to-remove (result (list)))
    (cond
      [(empty? code) (reverse result)]
      [else
       (define cmd (car code))
       (cond
         [(ast-label-def-cmd? cmd)
          (define label (ast-label-def-cmd-label cmd))
          (if (findf (lambda (label-cmd) (string=? label (ast-label-def-cmd-label label-cmd))) labels-to-remove)
              (remove-labels-for (cdr code) labels-to-remove result)
              (remove-labels-for (cdr code) labels-to-remove (cons cmd result)))]
         [else (remove-labels-for (cdr code) labels-to-remove (cons cmd result))])]))

  (define (wrap-code-for-test bc (mocked-code-list (list)))
    (append (list (org #xc000)
                  (JSR VM_INITIALIZE_MEMORY_MANAGER))
            bc
            (list (BRK))
            (remove-labels-for vm-memory-manager (filter (lambda (ast-cmd) (ast-label-def-cmd? ast-cmd)) mocked-code-list))))

  (define (run-code-in-test bc (debug #f) #:mock (mocked-code-list (list)))
    (define wrapped-code (wrap-code-for-test bc mocked-code-list))
    (define state-before
      (6510-load-multiple (initialize-cpu)
                          (assemble-to-code-list wrapped-code)))
    (if debug
        (run-debugger-on state-before)
        (parameterize ([current-output-port (open-output-nowhere)])
          (run-interpreter-on state-before)))))

(require (only-in "../tools/6510-interpreter.rkt" 6510-load 6510-load-multiple initialize-cpu run-interpreter run-interpreter-on memory-list cpu-state-accumulator peek))

(provide vm-memory-manager
         vm-cell-at-nil?
         vm-call-frame->strings
         vm-stack->strings
         vm-cell-at->string
         ast-const-get
         vm-page->strings
         vm-regt->string
         vm-rega->string
         vm-deref-cell-pair-w->string
         vm-deref-cell-pair->string
         format-hex-byte
         format-hex-word

         VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE

         ZP_RT
         ZP_VM_PC
         ZP_VM_FUNC_PTR
         ZP_LOCALS_PTR
         ZP_PARAMS_PTR
         ZP_CELL_STACK_TOS
         ZP_CELL_STACK_BASE_PTR)

;; write out the cells that are marked as reallocatable
(define (vm-cell-pair-free-tree->string state)
  (define cell-pair-root (peek-word-at-address state #xcec5))
  (cond
    [(= 0 cell-pair-root) "root is initial"]
    [else
     (format "cell-pair $~a -> [ ~a . ~a ]"
             (format-hex-word cell-pair-root)
             (vm-cell-w->string (peek-word-at-address state cell-pair-root))
             (vm-cell-w->string (peek-word-at-address state (+ 2 cell-pair-root))))]))

;; write call stack status
(define (vm-call-frame->strings state)
  (list (format "call-frame:       $~a" (format-hex-word (peek-word-at-address state ZP_CALL_FRAME)))
        (format "program-counter:  $~a" (format-hex-word (peek-word-at-address state ZP_VM_PC)))
        (format "function-ptr:     $~a" (format-hex-word (peek-word-at-address state ZP_VM_FUNC_PTR)))
        (format "params start@:    $~a" (format-hex-word (peek-word-at-address state ZP_PARAMS_PTR)))
        (format "locals start@:    $~a" (format-hex-word (peek-word-at-address state ZP_LOCALS_PTR)))
        (format "cell-stack start: $~a" (format-hex-word (peek-word-at-address state ZP_CELL_STACK_BASE_PTR)))))

;; write a status string of a memory page
(define (vm-page->strings state page)
  (define page-type-enc (peek state (bytes->int 0 page)))
  (define next-free-slot (peek state (bytes->int page #xcf)))
  (define page-type
    (cond
      [(= #x10 (bitwise-and #xf8 page-type-enc))
       (format "m1 page p~a" (bitwise-and #x03 page-type-enc))]
      [(= #x80 (bitwise-and #x80 page-type-enc))
       "cell page"]
      [(= #x40 (bitwise-and #xc0 page-type-enc))
       "cell-pair page"]
      [(= #x20 (bitwise-and #xe0 page-type-enc))
       "s8 page"]
      [(= #x18 page-type-enc)
       "call-frame page"]
      [else (raise-user-error "unknown page type")]))
  (define previous-page
    (cond
      [(not (= 0 (bitwise-and #xc0 page-type-enc)))
       (peek state (bytes->int #xff page))]
      [else (peek state (bytes->int 1 page))]))
  (define slots-used
    (cond
      [(= #x10 (bitwise-and #xf0 page-type-enc))
       (peek state (bytes->int 2 page))]
      [(= #x80 (bitwise-and #x80 page-type-enc))
       (bitwise-and #x7f page-type-enc)]
      [(= #x40 (bitwise-and #xc0 page-type-enc))
       (bitwise-and #x3f page-type-enc)]
      [(= #x20 (bitwise-and #xe0 page-type-enc))
       (bitwise-and #x1f page-type-enc)]
      [else 0]
      ))
  (cond [(= #x18 page-type-enc)
         (list (format "page-type:      ~a" page-type)
               ;; (format "previous page:  $~a" (format-hex-byte previous-page))
               (format "stack frame:    $~a" (format-hex-word (peek-word-at-address state ZP_CALL_FRAME))))]
        [else
         (list (format "page-type:      ~a" page-type)
               (format "previous page:  $~a" (format-hex-byte previous-page))
               (format "slots used:     ~a" slots-used)
               (format "next free slot: $~a" (format-hex-byte next-free-slot)))]))

;; produce strings describing the current cell-stack status
(define (vm-stack->strings state)
  (define stack-tos-idx (peek state ZP_CELL_STACK_TOS))
  (cond
    [(and (regt-empty? state) (> stack-tos-idx #xf0)) (list "stack is empty")]
    [else
     (define stack-ptr (peek-word-at-address state ZP_CELL_STACK_BASE_PTR))
     (define stack (memory-list state stack-ptr (+ (bitwise-and #xff (+ 1 stack-tos-idx)) stack-ptr)))
     (define stack-values (if (regt-empty? state) '()  stack))
     (define stack-item-no (+ (/ (bitwise-and #xff (add1 stack-tos-idx)) 2)
                              (if (regt-empty? state) 0 1)))
     (define stack-strings (reverse (map (lambda (pair) (vm-cell->string (cdr pair) (car pair))) (pairing stack-values))))
     (cons (format "stack holds ~a ~a" stack-item-no (if (= 1 stack-item-no) "item" "items"))
           (if (regt-empty? state)
               "stack is empty"
               (cons (format "~a  (rt)" (vm-regt->string state)) stack-strings)))]))

;; make a list of adjacent pairs
(define (pairing list (paired-list '()))
  (if (< (length list) 2)
      (reverse paired-list)
      (pairing (drop list 2) (cons `(,(car list) . ,(cadr list)) paired-list))))

(module+ test #| pairing |#
  (check-equal? (pairing '())
                '())
  (check-equal? (pairing '(1 2 3 4 5 6))
                '((1 . 2) (3 . 4) (5 . 6))))

;; format a hexadecimal byte
(define (format-hex-byte byte)
  (~a (number->string byte 16) #:width 2 #:align 'right #:pad-string "0"))

(define (format-hex-word word)
  (~a (number->string word 16) #:width 4 #:align 'right #:pad-string "0"))

(define (low-byte word) (bitwise-and #xff word))
(define (high-byte word) (arithmetic-shift (bitwise-and #xff00 word) -8))

;; write the car, cdr cell of the cell-pair at word in memory
(define (vm-deref-cell-pair-w->string state word)
  (define derefed-word-car (peek-word-at-address state word))
  (define derefed-word-cdr (peek-word-at-address state (+ 2 word)))
  (format "(~a . ~a)"
          (vm-cell-w->string derefed-word-car)
          (vm-cell-w->string derefed-word-cdr)))

(define (vm-deref-cell-w->string state word)
  (define derefed-word (peek-word-at-address state word))
  (format "~a" (vm-cell-w->string derefed-word)))

;; write the car, cdr cell of the cell-pair at low/high in memory
(define (vm-deref-cell-pair->string state low high)  
  (vm-deref-cell-pair-w->string state (bytes->int low high)))

(define (vm-deref-cell->string state low high)
  (vm-deref-cell-w->string state (bytes->int low high)))

;; write decoded cell described by word
(define (vm-cell-w->string word)
  (vm-cell->string (low-byte word) (high-byte word)))

;; write decoded cell described by low high
;; the low 2 bits are used for pointer tagging
(define (vm-cell->string low high)
  (cond
    [(= 0 low) "empty"]
    [(= 0 (bitwise-and #x01 low)) (format "cell-ptr $~a~a"
                                          (format-hex-byte high)
                                          (format-hex-byte (bitwise-and #xfe low)))]
    [(and (= 1 (bitwise-and #x03 low)) (= high 0)) "cell-pair-ptr NIL"]
    [(= 1 (bitwise-and #x03 low)) (format "cell-pair-ptr $~a~a"
                                          (format-hex-byte high)
                                          (format-hex-byte (bitwise-and #xfd low)))]
    [(= 3 (bitwise-and #x83 low)) (format "cell-int $~a~a"
                                          (format-hex-byte (arithmetic-shift low -2))
                                          (format-hex-byte high))]
    [(= TAG_BYTE_BYTE_CELL (bitwise-and #xff low)) (format "cell-byte $~a" (format-hex-byte high))]
    ;; TODO: a structure has a special value + follow bytes
    ;; (= ? (bitwise-and #xfc low)) e.g. #x04 = structure, high byte = number of fields
    ;; the following number of fields * cells cannot be structure cells, but only atomic or pointer cells
    [else "?"]))


(define (regt-empty? state)
  (= 0 (peek state ZP_RT)))

(define (vm-cell-at-nil? state loc)
  (= TAGGED_NIL (peek-word-at-address state loc)))

(define (vm-cell-at->string state loc (rev-endian #f))
  (vm-cell-w->string (peek-word-at-address state loc rev-endian)))

;; write string of current RT
(define (vm-regt->string state)
  (vm-cell->string
   (peek state ZP_RT)
   (peek state (add1 ZP_RT))))

;; get the actual refcount of a cell-pair-ptr
(define (vm-refcount-cell-pair-ptr state cell-pair-ptr)
  (define lowb  (low-byte cell-pair-ptr))
  (define highb (high-byte cell-pair-ptr))
  (define refc  (arithmetic-shift lowb -2))
  (peek state (bytes->int refc highb) ))

;; get the actual refcount of a cell-pair-ptr
(define (vm-refcount-cell-ptr state cell-ptr)
  (define lowb  (low-byte cell-ptr))
  (define highb (high-byte cell-ptr))
  (define refc  (arithmetic-shift lowb -1))
  (peek state (bytes->int refc highb) ))

;; write string of current RA
(define (vm-rega->string state)
  (vm-cell->string
   (peek state ZP_RA)
   (peek state (add1 ZP_RA))))

(module+ test #| vm-cell->string |#
  (check-equal? (vm-cell->string #xc4 #xc0)
                "cell-ptr $c0c4")
  (check-equal? (vm-cell->string #xc1 #xc0)
                "cell-pair-ptr $c0c1")
  (check-equal? (vm-cell->string #x7b #x15)
                "cell-int $1e15")
  (check-equal? (vm-cell->string TAG_BYTE_BYTE_CELL #x15)
                "cell-byte $15"))

(define (vm-cells->strings byte-list (result (list)))
  (if (empty? byte-list)
      (reverse result)
      (vm-cells->strings
       (cddr byte-list)
       (cons (vm-cell->string (car byte-list)
                             (cadr byte-list))
             result))))

(module+ test #| vm-cells->strings |#
  (check-equal? (vm-cells->strings '(#x01 #x00 #x03 #x01))
                '("cell-pair-ptr NIL" "cell-int $0001")))

(module+ test #| vm-stack->string |#
  (define test-vm_stack_to_string-a-code
    (list (JSR VM_CELL_STACK_PUSH_NIL_R)
          (JSR VM_CELL_STACK_PUSH_NIL_R)
          (LDA !$01)
          (LDX !$03)
          (JSR VM_CELL_STACK_PUSH_INT_R)
          (JSR VM_CELL_STACK_PUSH_NIL_R)))

  (define test-vm_stack_to_string-a-state-after
    (run-code-in-test test-vm_stack_to_string-a-code))

  (check-equal? (vm-stack->strings test-vm_stack_to_string-a-state-after)
                '("stack holds 4 items"
                  "cell-pair-ptr NIL  (rt)"
                  "cell-int $0301"
                  "cell-pair-ptr NIL"
                  "cell-pair-ptr NIL")))

;; constants that are used by the assembler code
(define VM_MEMORY_MANAGEMENT_CONSTANTS
  (list
   ;; highest bit 0 and the lowest 2 bits are reserved for int, cell-ptr and cell-pair-ptr
   ;; => 32 values still available
   (byte-const TAG_BYTE_BYTE_CELL        $ff)
   (byte-const TAG_BYTE_CELL_ARRAY       $80)
   (byte-const TAG_BYTE_NATIVE_ARRAY     $84)

   (byte-const NEXT_FREE_PAGE_PAGE       $cf)   ;; cf00..cfff is a byte array, mapping each page idx to the next free page idx, 00 = no next free page for the given page
   (word-const VM_FIRST_FREE_SLOT_ON_PAGE     $cf00) ;; location: table of first free slot for each page

   (word-const TAGGED_INT_0              $0003)
   (word-const TAGGED_BYTE0              $00ff)
   (word-const TAGGED_NIL                $0001) ;; tag indicates cell-pair-ptr

   ;; d9..da free to use

   (byte-const ZP_CELL_STACK_TOS         $db) ;; byte (fe = empty stack, 0 = first element, 2 = second element, 4 = third element ...)

   ;; ZP_TEMP may be used as pointer (in combination with ZP_TEMP2)
   (byte-const ZP_TEMP                   $dc) ;; may not be used after sub calls (just within a routine without jsr)
   (byte-const ZP_TEMP2                  $dd) ;; may not be used after sub calls (just within a routine without jsr)

   ;; the following ten bytes need to be continuous, since they are saved into the call frame!
   (byte-const ZP_VM_PC                  $de) ;; program counter (ptr to currently executing byte code)
   (byte-const ZP_VM_FUNC_PTR            $e0) ;; pointer to the currently running function
   (byte-const ZP_PARAMS_PTR             $e2) ;; pointer to first parameter in call-frame
   (byte-const ZP_LOCALS_PTR             $e4) ;; pointer to first local in call-frame
   (byte-const ZP_CELL_STACK_BASE_PTR    $e6) ;; e6..e7 (pointer to the base of the eval stack of the currently running function (+ZP_CELL_STACK_TOS => pointer to tos of the call-frame, in register mode, actual TOS is ZP_RT!)

   (byte-const ZP_CALL_FRAME             $f1) ;; f1..f2 <- may be not needed (zp_locals_ptr always = zp_call_frame + 6)

   ;; implementation using registers
   ;; register T = top of stack, used as main register for operations, could be a pointer to a cell or an atomic cell. if it is a pointer to a cell, the low byte here is without tag bits => (zp_rt) points to the cell
   (byte-const ZP_RT                     $fb) ;; fb = low byte (without tag bits), fc = high byte,   tagged low byte is held in ZP_RT_TAGGED_LB
   ;; register A
   (byte-const ZP_RA                     $fd) ;; fd = low byte (without tag bits), fe = high byte,   tagged low byte is held in ZP_RA_TAGGED_LB
   ;; currently no need to register B (maybe someday)
   ))

;; extract constant definition in assembler into racket constant
(define (ast-const-get ast-commands key)
  (when (empty? ast-commands)
    (raise-user-error (format "key ~a not found in list of ast commands" key)))
  (define ast-command (car ast-commands))
  (cond
    [(and (ast-const-byte-cmd? ast-command)
        (string=? key (ast-const-byte-cmd-label ast-command)))
     (ast-const-byte-cmd-byte ast-command)]
    [(and (ast-const-word-cmd? ast-command)
        (string=? key (ast-const-word-cmd-label ast-command)))
     (ast-const-word-cmd-word ast-command)]
    [else (ast-const-get (cdr ast-commands) key)]))

(define ZP_RT                   (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_RT"))
(define ZP_RA                   (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_RA"))
(define ZP_CALL_FRAME           (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CALL_FRAME"))
(define ZP_CELL_STACK_BASE_PTR  (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CELL_STACK_BASE_PTR"))
(define ZP_CELL_STACK_TOS       (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CELL_STACK_TOS"))
(define ZP_TEMP                 (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_TEMP"))
(define ZP_VM_PC                (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_VM_PC"))
(define ZP_VM_FUNC_PTR          (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_VM_FUNC_PTR"))
(define ZP_LOCALS_PTR           (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_LOCALS_PTR"))
(define ZP_PARAMS_PTR           (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_PARAMS_PTR"))
(define TAG_BYTE_BYTE_CELL      (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAG_BYTE_BYTE_CELL"))
(define TAG_BYTE_CELL_ARRAY     (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAG_BYTE_CELL_ARRAY"))
(define TAG_BYTE_NATIVE_ARRAY   (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAG_BYTE_NATIVE_ARRAY"))
(define TAGGED_NIL              (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAGGED_NIL"))



;; input:  x  (00 = RT, 02 = RA)
;; output: Rx
;; NO PUSH IS DONE, Rx IS SIMPLY OVERWRITTEN!
(define VM_WRITE_NIL_TO_Rx
  (list
   (label VM_WRITE_NIL_TO_RA)
          (LDX !$02) ;; index 2 => RA
          (BNE VM_WRITE_NIL_TO_Rx)
   (label VM_WRITE_NIL_TO_RT)
          (LDX !$00) ;; index 0 => RT
   (label VM_WRITE_NIL_TO_Rx)
          (LDA !<TAGGED_NIL)
          (STA ZP_RT,x)
          (LDA !>TAGGED_NIL)
          (STA ZP_RT+1,x)
          (RTS)))

;; input: A = lowbyte of int (0..255), written into high byte of cell register RT
;;        Y = highbyte (0.31), written into lowbyte and tagged lowbyte of cell register
;;        X = (0 = RT, 2 = RA)
;; output: Rx = cell-int
(define VM_WRITE_INT_AY_TO_Rx
  (list
   (label VM_WRITE_INTm1_TO_RA)
          (LDX !$02) ;; index 2 => RA
          (BNE VM_WRITE_INTm1_TO_Rx)
   (label VM_WRITE_INTm1_TO_RT)
          (LDX !$00) ;; index 0 => RT
   (label VM_WRITE_INTm1_TO_Rx)
          (LDA !$ff) ;; int lowbyte = ff
          (LDY !$7f) ;; #b[0]111 11[11] = $1f for int high byte
          (BNE VM_WRITE_AY_TO_Rx)


   (label VM_WRITE_INT1_TO_RA)
          (LDX !$02) ;; index 2 => RA
          (BNE VM_WRITE_INT1_TO_Rx)
   (label VM_WRITE_INT1_TO_RT)
          (LDX !$00) ;; index 0 => RT
   (label VM_WRITE_INT1_TO_Rx)
          (LDA !$01)
          (BNE VM_WRITE_INT_A_TO_Rx)

   (label VM_WRITE_INT0_TO_RA)
          (LDX !$02) ;; index 2 => RA
          (BNE VM_WRITE_INT0_TO_Rx)
   (label VM_WRITE_INT0_TO_RT)
          (LDX !$00) ;; index 0 => RT
   (label VM_WRITE_INT0_TO_Rx)
          (LDA !$00)
          (BEQ VM_WRITE_INT_A_TO_Rx)

   (label VM_WRITE_INT_A_TO_RA)
          (LDX !$02) ;; index 2 => RA
          (BNE VM_WRITE_INT_A_TO_Rx)
   (label VM_WRITE_INT_A_TO_RT)
          (LDX !$00) ;; index 0 => RT
   (label VM_WRITE_INT_A_TO_Rx)
          (LDY !$03) ;; #b[0]000 00[11] = high byte of int  0
   (label VM_WRITE_AY_TO_Rx)
          (STY ZP_RT,x)
          (STA ZP_RT+1,x)
          (RTS)

   (label VM_WRITE_INT_AY_TO_RA)
          (LDX !$02) ;; index 2 => RA
          (BNE VM_WRITE_INT_AY_TO_Rx)
   (label VM_WRITE_INT_AY_TO_RT)
          (LDX !$00) ;; index 0 => RT
   (label VM_WRITE_INT_AY_TO_Rx)
          (STA ZP_RT+1,x)
          (TYA)      ;; #b???x xxxx
          (SEC)
          (ROL)      ;; #b??xx xxx1
          (SEC)
          (ROL)      ;; #b?xxx xx11
          (AND !$7f) ;; #xb0xxx xx11 (mask out top bit!)
          (STA ZP_RT,x) ;; encoded tagged byte of int goes into first memory cell, even though it is the high-byte part of int
          (RTS)))

(module+ test #| vm_write_int_ay_to_rx |#
  (define vm-write-int-ay-to-rx-code
    (list
     (LDA !$01)
     (LDY !$02)
     (LDX !$00)
     (JSR VM_WRITE_INT_AY_TO_Rx)))

  (define vm-write-int-ay-to-rx-state
    (run-code-in-test vm-write-int-ay-to-rx-code))

  (check-equal? (vm-regt->string vm-write-int-ay-to-rx-state)
                "cell-int $0201")

  (define vm-write-int-ay-to-rx2-code
    (list
     (LDA !$01)
     (LDY !$02)
     (LDX !$02)
     (JSR VM_WRITE_INT_AY_TO_Rx)))

  (define vm-write-int-ay-to-rx2-state
    (run-code-in-test vm-write-int-ay-to-rx2-code))

  (check-equal? (vm-rega->string vm-write-int-ay-to-rx2-state)
                "cell-int $0201")

  (define vm-write-int-ay-to-rt-code
    (list
     (LDA !$01)
     (LDY !$02)
     (JSR VM_WRITE_INT_AY_TO_RT)))

  (define vm-write-int-ay-to-rt-state
    (run-code-in-test vm-write-int-ay-to-rt-code))

  (check-equal? (vm-regt->string vm-write-int-ay-to-rt-state)
                "cell-int $0201")

  (define vm-write-int-ay-to-ra-code
    (list
     (LDA !$01)
     (LDY !$02)
     (JSR VM_WRITE_INT_AY_TO_RA)))

  (define vm-write-int-ay-to-ra-state
    (run-code-in-test vm-write-int-ay-to-ra-code))

  (check-equal? (vm-rega->string vm-write-int-ay-to-ra-state)
                "cell-int $0201"))

;; input:  RT
;;         RA must be cell-pair-ptr
;; output: cell1 of cell-pair pointed to be RA is set to RT
(define VM_WRITE_RT_TO_CELLy_RA
  (list
   (label VM_WRITE_RT_TO_CELL1_RA)
          (LDY !$02) ;; offset 2 for cell1
          (BNE VM_WRITE_RT_TO_CELLy_RA)

   (label VM_WRITE_RT_TO_CELL0_RA)
          (LDY !$00) ;; offset 0 for cell0

   ;; ----------------------------------------
   (label VM_WRITE_RT_TO_CELLy_RA)
          (LDA ZP_RT)
          (STA (ZP_RA),y)
          (INY)
          (LDA ZP_RT+1)
          (STA (ZP_RA),y)
          (RTS)))

(module+ test #| vm-write-rt-to-celly-ra |#
  (define vm_write_rt_to_celly_ra_code
    (list
     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)
     (JSR VM_CP_RT_TO_RA)
     (LDA !$01)
     (LDY !$10)
     (JSR VM_WRITE_INT_AY_TO_RT)
     (LDY !$00)
     (JSR VM_WRITE_RT_TO_CELLy_RA)
     (LDA !$10)
     (LDY !$01)
     (JSR VM_WRITE_INT_AY_TO_RT)
     (LDY !$02)
     (JSR VM_WRITE_RT_TO_CELLy_RA)))

  (define vm_write_rt_to_celly_ra_state
    (run-code-in-test vm_write_rt_to_celly_ra_code))

  (check-equal? (vm-regt->string vm_write_rt_to_celly_ra_state)
                "cell-int $0110")

  (check-equal? (vm-rega->string vm_write_rt_to_celly_ra_state)
                "cell-pair-ptr $cc05")

  (check-equal? (vm-deref-cell-pair-w->string vm_write_rt_to_celly_ra_state #xcc05)
                "(cell-int $1001 . cell-int $0110)"))

;; input:  RT
;;         RA must be cell-pair-ptr
;; output: cell1 of cell-pair pointed to be RA is set to RT
(define VM_WRITE_RA_TO_CELLy_RT
  (list
   (label VM_WRITE_RA_TO_CELL1_RT)
          (LDY !$02)
          (BNE VM_WRITE_RA_TO_CELLy_RT)

   (label VM_WRITE_RA_TO_CELL0_RT)
          (LDY !$00)

   ;; ----------------------------------------
   (label VM_WRITE_RA_TO_CELLy_RT)
          (LDA ZP_RA)
          (STA (ZP_RT),y)
          (INY)
          (LDA ZP_RA+1)
          (STA (ZP_RT),y)
          (RTS)))

(module+ test #| vm-write-ra-to-celly-rt |#
  (define vm_write_ra_to_celly_rt_code
    (list
     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)
     (LDA !$01)
     (LDY !$10)
     (JSR VM_WRITE_INT_AY_TO_RA)
     (LDY !$00)
     (JSR VM_WRITE_RA_TO_CELLy_RT)
     (LDA !$10)
     (LDY !$01)
     (JSR VM_WRITE_INT_AY_TO_RA)
     (LDY !$02)
     (JSR VM_WRITE_RA_TO_CELLy_RT)))

  (define vm_write_ra_to_celly_rt_state
    (run-code-in-test vm_write_ra_to_celly_rt_code))

  (check-equal? (vm-rega->string vm_write_ra_to_celly_rt_state)
                "cell-int $0110")

  (check-equal? (vm-regt->string vm_write_ra_to_celly_rt_state)
                "cell-pair-ptr $cc05")

  (check-equal? (vm-deref-cell-pair-w->string vm_write_ra_to_celly_rt_state #xcc05)
                "(cell-int $1001 . cell-int $0110)"))

;; input:  cell-stack (TOS)
;;         RA (must be a cell-pair ptr
;;         y = (0 = cell0, 2 = cell1)
;; output: cell-stack (one value less)
;;         cell0 of RA is set
(define VM_POP_FSTOS_TO_CELLy_RT
  (list
   (label VM_POP_FSTOS_TO_CELL1_RT)
          (LDY !$03)
          (BNE VM_POP_FSTOS_TO_CELLy_RT__UCY)

   (label VM_POP_FSTOS_TO_CELL0_RT)
          (LDY !$00)

   ;; ----------------------------------------
   (label VM_POP_FSTOS_TO_CELLy_RT)
          (INY)
   (label VM_POP_FSTOS_TO_CELLy_RT__UCY)
          (STY ZP_TEMP)
          (LDY ZP_CELL_STACK_TOS)
          (LDA (ZP_CELL_STACK_BASE_PTR),y)
          (TAX)
          (DEY)
          (LDA (ZP_CELL_STACK_BASE_PTR),y)
          (LDY ZP_TEMP)
          (STA (ZP_RT),y)
          (DEY)
          (TXA)
          (STA (ZP_RT),y)
          (DEC ZP_CELL_STACK_TOS)
          (DEC ZP_CELL_STACK_TOS)
          (RTS)))

(module+ test #| vm-pop-fstos-to-celly-rt |#
  (define vm-pop-fstos-to-celly-rt-code
    (list
     (JSR VM_CELL_STACK_PUSH_INT_1_R)
     (JSR VM_CELL_STACK_PUSH_INT_m1_R)
     (JSR VM_CELL_STACK_PUSH_INT_1_R)
     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)
     (LDY !$00)
     (JSR VM_POP_FSTOS_TO_CELLy_RT)
     (LDY !$02)
     (JSR VM_POP_FSTOS_TO_CELLy_RT)
     ))

  (define vm-pop-fstos-to-celly-rt-state
    (run-code-in-test vm-pop-fstos-to-celly-rt-code))

  (check-equal? (vm-stack->strings vm-pop-fstos-to-celly-rt-state)
                (list "stack holds 1 item"
                      "cell-pair-ptr $cc05  (rt)"))
  (check-equal? (vm-deref-cell-pair-w->string vm-pop-fstos-to-celly-rt-state #xcc05)
                "(cell-int $1fff . cell-int $0001)"))

;; input:  RA
;; output: RT (copy of RA)
(define VM_CP_RA_TO_RT
  (list
   (label VM_CP_RA_TO_RT)
   (label VM_CP_RT_TO_RA__VALUE) ;;just value, no tagged byte
          (LDA ZP_RA)
          (STA ZP_RT)
          (LDA ZP_RA+1)
          (STA ZP_RT+1)
          (RTS)))

(module+ test #| vm-cp-rt-to-ra |#
  (define vm-cp-ra-to-rt-code
    (list
     (JSR VM_WRITE_INT1_TO_RA)
     (JSR VM_CP_RA_TO_RT)))

  (define vm-cp-ra-to-rt-state
    (run-code-in-test vm-cp-ra-to-rt-code))

  (check-equal? (vm-rega->string vm-cp-ra-to-rt-state)
                "cell-int $0001")
  (check-equal? (vm-regt->string vm-cp-ra-to-rt-state)
                "cell-int $0001"))

;; input:  RT
;; output: RA (copy of RT)
(define VM_CP_RT_TO_RA
  (list
   (label VM_CP_RT_TO_RA)
   (label VM_CP_RA_TO_RT__VALUE) ;;just value, no tagged byte
          (LDA ZP_RT)
          (STA ZP_RA)
          (LDA ZP_RT+1)
          (STA ZP_RA+1)
          (RTS)))

(module+ test #| vm-cp-rt-to-ra |#
  (define vm-cp-rt-to-ra-code
    (list
     (JSR VM_WRITE_INT1_TO_RT)
     (JSR VM_CP_RT_TO_RA)))

  (define vm-cp-rt-to-ra-state
    (run-code-in-test vm-cp-rt-to-ra-code))

  (check-equal? (vm-rega->string vm-cp-rt-to-ra-state)
                "cell-int $0001")
  (check-equal? (vm-regt->string vm-cp-rt-to-ra-state)
                "cell-int $0001"))

;; input:  Y - 0 (cell0), 2 (cell1)
;;         RT (must be cell-pair ptr)
;; output: RT
(define VM_WRITE_RT_CELLy_TO_RT
  (list
   (label VM_WRITE_RT_CELL1_TO_RT)
          (LDY !$02)
          (BNE VM_WRITE_RT_CELLy_TO_RT)

   (label VM_WRITE_RT_CELL0_TO_RT)
          (LDY !$00)

   ;; ----------------------------------------
   (label VM_WRITE_RT_CELLy_TO_RT)
          (LDA (ZP_RT),y)
          (TAX)
          (INY)
          (LDA (ZP_RT),y)
          (STA ZP_RT+1)
          (STX ZP_RT)
          (RTS)))

(module+ test #| vm-write-rt-celly-to-rt |#
  (define vm-write-rt-celly-to-rt-code
    (list
     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)
     (LDA !$01)
     (LDY !$10)
     (JSR VM_WRITE_INT_AY_TO_RA)
     (LDY !$00)
     (JSR VM_WRITE_RA_TO_CELLy_RT)
     (LDA !$10)
     (LDY !$01)
     (JSR VM_WRITE_INT_AY_TO_RA)
     (LDY !$02)
     (JSR VM_WRITE_RA_TO_CELLy_RT)
     (JSR VM_WRITE_RT_CELL0_TO_RT)))

  (define vm-write-rt-celly-to-rt-state
    (run-code-in-test vm-write-rt-celly-to-rt-code))

  (check-equal? (vm-regt->string vm-write-rt-celly-to-rt-state)
                "cell-int $1001")

  (check-equal? (vm-deref-cell-pair-w->string vm-write-rt-celly-to-rt-state #xcc05)
                "(cell-int $1001 . cell-int $0110)"))

;; input:  Y - 0 (cell0), 2 (cell1)
;;         RT (must be cell-pair ptr)
;; output: RA
(define VM_WRITE_RT_CELLy_TO_RA
  (list
   (label VM_WRITE_RT_CELL1_TO_RA)
          (LDY !$02)
          (BNE VM_WRITE_RT_CELLy_TO_RA)

   (label VM_WRITE_RT_CELL0_TO_RA)
          (LDY !$00)

   ;; ----------------------------------------
   (label VM_WRITE_RT_CELLy_TO_RA)
          (LDA (ZP_RT),y)
          (STA ZP_RA)
          (INY)
          (LDA (ZP_RT),y)
          (STA ZP_RA+1)
          (RTS)))

(module+ test #| vm-write-rt-celly-to-ra |#
  (define vm-write-rt-celly-to-ra-code
    (list
     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)
     (LDA !$01)
     (LDY !$10)
     (JSR VM_WRITE_INT_AY_TO_RA)
     (JSR VM_WRITE_RA_TO_CELL0_RT)
     (LDA !$10)
     (LDY !$01)
     (JSR VM_WRITE_INT_AY_TO_RA)
     (JSR VM_WRITE_RA_TO_CELL1_RT)
     (JSR VM_WRITE_INT0_TO_RA) ;; clear ra
     (JSR VM_WRITE_RT_CELL0_TO_RA)))

  (define vm-write-rt-celly-to-ra-state
    (run-code-in-test vm-write-rt-celly-to-ra-code))

  (check-equal? (vm-rega->string vm-write-rt-celly-to-ra-state)
                "cell-int $1001")

  (check-equal? (vm-deref-cell-pair-w->string vm-write-rt-celly-to-ra-state #xcc05)
                "(cell-int $1001 . cell-int $0110)"))

;; input:  call-frame stack, RT
;; output: call-frame stack << RT
;; uses:   A Y
;; CHECK STACK PAGE OVERFLOW
(define VM_CELL_STACK_JUST_PUSH_RT
  (list
   (label VM_CELL_STACK_PUSH_RT_IF_NONEMPTY)
          (LDY ZP_RT)
          ;; if RT empty?  = $00 
          (BEQ DONE__VM_CELL_STACK_JUST_PUSH_RT)        ;; then no push

   ;; ----------------------------------------
   (label VM_CELL_STACK_JUST_PUSH_RT)
          (LDY ZP_CELL_STACK_TOS)

          ;; check that stack pointer does not run out of bound (over the page)
          (CPY !$fd) ;; marks the end of page, for pushes at least!
          [BNE NO_ERROR__VM_CELL_STACK_JUST_PUSH_RT]

          (BRK)

   (label NO_ERROR__VM_CELL_STACK_JUST_PUSH_RT)
          (LDA ZP_RT+1)
          (INY)
          (STA (ZP_CELL_STACK_BASE_PTR),y)       ;; write high byte! 

          (LDA ZP_RT)
          (INY)
          (STA (ZP_CELL_STACK_BASE_PTR),y)      ;; write low byte 

          (STY ZP_CELL_STACK_TOS)      ;; set new tos

   (label DONE__VM_CELL_STACK_JUST_PUSH_RT)
          (RTS)))

(module+ test #| vm-cell-stack-just-push-rt |#
  (define vm-cell-stack-just-push-rt-code
    (list     
     (JSR VM_WRITE_INTm1_TO_RT)
     (JSR VM_CELL_STACK_JUST_PUSH_RT)))

  (define vm-cell-stack-just-push-rt-state
    (run-code-in-test vm-cell-stack-just-push-rt-code))

  (check-equal? (vm-stack->strings vm-cell-stack-just-push-rt-state)
                (list "stack holds 2 items"
                      "cell-int $1fff  (rt)"
                      "cell-int $1fff")))

;; push a cell onto the stack (that is push the RegT, if filled, and write the value into RegT)
;; input: call-frame stack, RT
;;        A = high byte,
;;        X = tagged low
;; output: call-frame stack, RT
(define VM_CELL_STACK_PUSH_R
  (list

   ;; ints are saved high byte first, then low byte !!!!
   ;; X = high byte of int (max 31 = $1f) (stored in low byte (tagged) position)
   ;; A = low byte of int (0..255) (stored in high byte (untagged) position)
   (label VM_CELL_STACK_PUSH_INT_R)         ;; idea: can be optimized since it is known that this is an atomic value
          (TAY)
          (TXA)
          (SEC)
          (ROL)
          (SEC)
          (ROL)
          (AND !$7f)           ;; mask out top bit
          (TAX)
          (TYA)
          (CLC)
          (BCC VM_CELL_STACK_PUSH_R)

   (label VM_CELL_STACK_PUSH_INT_m1_R)
          (LDA !$ff) ;; 1f << 2
          (LDX !$7f)
          (BNE VM_CELL_STACK_PUSH_R)

   (label VM_CELL_STACK_PUSH_INT_2_R)
          (LDA !$02)
          (LDX !$03)
          (BNE VM_CELL_STACK_PUSH_R)

   (label VM_CELL_STACK_PUSH_INT_1_R)
          (LDA !$01)
          (LDX !$03)
          (BNE VM_CELL_STACK_PUSH_R)

   (label VM_CELL_STACK_PUSH_INT_0_R)
          (LDA !$00)
          (LDX !$03)
          (BNE VM_CELL_STACK_PUSH_R)

   ;; push NIL (cell-pair-ptr)           ;; idea: can be optimized since it is known that this is cell-pair-ptr
   (label VM_CELL_STACK_PUSH_NIL_R)
          (LDX !<TAGGED_NIL)
          (LDA !>TAGGED_NIL)

   ;; push a cell
   ;; A = high byte
   ;; X = tagged low byte
   (label VM_CELL_STACK_PUSH_R)
          (PHA)
          (JSR VM_CELL_STACK_PUSH_RT_IF_NONEMPTY) ;; uses A and Y
          (PLA)

   (label VM_WRITE_AX_TO_RT)
          (STX ZP_RT)          
          (STA ZP_RT+1)
          (RTS)))

(module+ test #| vm_cell_stack_push_r (basically on write into rt, since stack is completely empty) |#

  (define vm_cell_stack_push_r_int0_code
    (list
     (JSR VM_CELL_STACK_PUSH_INT_0_R)))

  (define vm_cell_stack_push_r_int0_state
    (run-code-in-test vm_cell_stack_push_r_int0_code))

  (check-equal? (vm-regt->string vm_cell_stack_push_r_int0_state)
                "cell-int $0000")
  (check-equal? (memory-list vm_cell_stack_push_r_int0_state ZP_RT (add1 ZP_RT))
                (list #x03 #x00))

  (define vm_cell_stack_push_r_int1_code
    (list
     (JSR VM_CELL_STACK_PUSH_INT_1_R)))

  (define vm_cell_stack_push_r_int1_state
    (run-code-in-test vm_cell_stack_push_r_int1_code))

  (check-equal? (vm-regt->string vm_cell_stack_push_r_int1_state)
                "cell-int $0001")
  (check-equal? (memory-list vm_cell_stack_push_r_int1_state ZP_RT (add1 ZP_RT))
                (list #x03 #x01))

  (define vm_cell_stack_push_r_intm1_code
    (list
     (JSR VM_CELL_STACK_PUSH_INT_m1_R)))

  (define vm_cell_stack_push_r_intm1_state
    (run-code-in-test vm_cell_stack_push_r_intm1_code))

  (check-equal? (vm-regt->string vm_cell_stack_push_r_intm1_state)
                "cell-int $1fff")
  (check-equal? (memory-list vm_cell_stack_push_r_intm1_state ZP_RT (add1 ZP_RT))
                (list #x7f #xff))

  (define vm_cell_stack_push_r_nil_code
    (list
     (JSR VM_CELL_STACK_PUSH_NIL_R)))

  (define vm_cell_stack_push_r_nil_state
    (run-code-in-test vm_cell_stack_push_r_nil_code))

  (check-equal? (vm-regt->string vm_cell_stack_push_r_nil_state)
                "cell-pair-ptr NIL")
  (check-equal? (memory-list vm_cell_stack_push_r_nil_state ZP_RT (add1 ZP_RT))
                (list #x01 #x00))


  (define vm_cell_stack_push_r_cell_ptr_code
    (list
     (LDX !$03)
     (LDA !$ce)
     (JSR VM_CELL_STACK_PUSH_R)))

  (define vm_cell_stack_push_r_cell_ptr_state
    (run-code-in-test vm_cell_stack_push_r_cell_ptr_code))

  (check-equal? (vm-regt->string vm_cell_stack_push_r_cell_ptr_state)
                "cell-int $00ce")
  (check-equal? (memory-list vm_cell_stack_push_r_cell_ptr_state ZP_RT (add1 ZP_RT))
                (list #x03 #xce))

  (define vm_cell_stack_push_r_cell_pair_ptr_code
    (list
     (LDX !$05)
     (LDA !$ce)
     (JSR VM_CELL_STACK_PUSH_R)))

  (define vm_cell_stack_push_r_cell_pair_ptr_state
    (run-code-in-test vm_cell_stack_push_r_cell_pair_ptr_code))

  (check-equal? (vm-regt->string vm_cell_stack_push_r_cell_pair_ptr_state)
                "cell-pair-ptr $ce05")
  (check-equal? (memory-list vm_cell_stack_push_r_cell_pair_ptr_state ZP_RT (add1 ZP_RT))
                (list #x05 #xce)))

(module+ test #| vm_cell_stack_push_r (push rt, and write rt) |#
  (define vm_cell_stack_push_r_push1_code
    (list
     (JSR VM_CELL_STACK_PUSH_INT_m1_R)
     (JSR VM_CELL_STACK_PUSH_INT_1_R)
     ))

  (define vm_cell_stack_push_r_push1_state
    (run-code-in-test vm_cell_stack_push_r_push1_code))

  (check-equal? (vm-stack->strings vm_cell_stack_push_r_push1_state)
                (list "stack holds 2 items"
                      "cell-int $0001  (rt)"
                      "cell-int $1fff"))

  (check-equal? (memory-list vm_cell_stack_push_r_push1_state ZP_RT (add1 ZP_RT))
                (list #x03 #x01))

  (define vm_cell_stack_push_r_push2_code
    (list
     (JSR VM_CELL_STACK_PUSH_INT_m1_R)
     (JSR VM_CELL_STACK_PUSH_INT_1_R)
     (JSR VM_CELL_STACK_PUSH_NIL_R)
     ))

  (define vm_cell_stack_push_r_push2_state
    [run-code-in-test vm_cell_stack_push_r_push2_code])

  (check-equal? (vm-stack->strings vm_cell_stack_push_r_push2_state)
                (list "stack holds 3 items"
                      "cell-pair-ptr NIL  (rt)"
                      "cell-int $0001"
                      "cell-int $1fff"))

  (check-equal? (memory-list vm_cell_stack_push_r_push2_state ZP_RT (add1 ZP_RT))
                (list #x01 #x00)))

;; pop cell from stack (that is, discard RegT, move tos of call-frame stack into RegT (if available))
;; input: call-frame stack, RT
;; output: call-frame stack reduced by`1, RT <- popped value
;; NO GC CHECKS!
(define VM_CELL_STACK_POP_R
  (list
   (label VM_CELL_STACK_POP_R)
          ;; optional: stack marked empty? => error: cannot pop from empty stack!
          ;; (LDY !$00)
          ;; (BEQ ERROR_NO_VALUE_ON_STACK)

          ;; is call-frame stack empty? => mark stack as empty and return | alternatively simply write NIL into RT
          (LDY ZP_CELL_STACK_TOS)
          (BMI WRITE_00_TO_RT) ;; which effectively clears the RT
          ;; pop value from call-frame stack into RT!
          (LDA (ZP_CELL_STACK_BASE_PTR),y) ;; tagged low byte
          (STA ZP_RT)


          ;; (optional) quick check for atomic cells [speeds up popping atomic cells, slows popping cell-ptr, slight slows popping cell-pair-ptr
          ;; (AND !$03)
          ;; (BEQ WRITE_TOS_TO_RT__VM_CELL_STACK_POP_R)
          ;; (TXA)

          ;; do pointer checks now
          (DEY)
          (LDA (ZP_CELL_STACK_BASE_PTR),y) ;; high byte
          (STA ZP_RT+1) 
          (DEY)
          (STY ZP_CELL_STACK_TOS)
          (RTS)

   (label WRITE_00_TO_RT)
          ;; mark RT as empty
          (LDA !$00)
          (STA ZP_RT)
          (STA ZP_RT+1)
          (RTS)))

(module+ test #| vm_cell_stack_pop_r (just one value) |#
  (define vm_cell_stack_pop3_r_code
    (list
     (JSR VM_CELL_STACK_PUSH_INT_1_R)
     (JSR VM_CELL_STACK_PUSH_INT_m1_R)
     (JSR VM_CELL_STACK_PUSH_INT_0_R)
     (JSR VM_CELL_STACK_POP_R)
     ))

  (define vm_cell_stack_pop3_r_state
    (run-code-in-test vm_cell_stack_pop3_r_code))

  (check-equal? (vm-stack->strings vm_cell_stack_pop3_r_state)
                (list "stack holds 2 items"
                      "cell-int $1fff  (rt)"
                      "cell-int $0001"))

  (check-equal? (memory-list vm_cell_stack_pop3_r_state ZP_RT (add1 ZP_RT))
                (list #x7f #xff))

  (define vm_cell_stack_pop2_r_code
    (list
     (JSR VM_CELL_STACK_PUSH_INT_1_R)
     (JSR VM_CELL_STACK_PUSH_INT_m1_R)
     (JSR VM_CELL_STACK_PUSH_INT_0_R)
     (JSR VM_CELL_STACK_POP_R)
     (JSR VM_CELL_STACK_POP_R)))

  (define vm_cell_stack_pop2_r_state
    (run-code-in-test vm_cell_stack_pop2_r_code))

  (check-equal? (vm-stack->strings vm_cell_stack_pop2_r_state)
                (list "stack holds 1 item"
                      "cell-int $0001  (rt)"))

  (define vm_cell_stack_pop1_r_code
    (list
     (JSR VM_CELL_STACK_PUSH_INT_1_R)
     (JSR VM_CELL_STACK_PUSH_INT_m1_R)
     (JSR VM_CELL_STACK_PUSH_INT_0_R)
     (JSR VM_CELL_STACK_POP_R)
     (JSR VM_CELL_STACK_POP_R)
     (JSR VM_CELL_STACK_POP_R)))

  (define vm_cell_stack_pop1_r_state
    (run-code-in-test vm_cell_stack_pop1_r_code))

  (check-equal? (vm-stack->strings vm_cell_stack_pop1_r_state)
                (list "stack is empty"))

  (check-equal? (memory-list vm_cell_stack_pop1_r_state ZP_RT (add1 ZP_RT))
                (list #x00 #x00)))

(module+ test #| vm_cell_stack_push_nil_r |#
  (define test-vm_cell_stack_push_nil-a-state-after
    (run-code-in-test
     (list (JSR VM_CELL_STACK_PUSH_NIL_R))))

  (check-equal? (vm-regt->string test-vm_cell_stack_push_nil-a-state-after)
                "cell-pair-ptr NIL")

  (define test-vm_cell_stack_push_nil-b-state-after
    (run-code-in-test
     (list (JSR VM_CELL_STACK_PUSH_NIL_R) ;; 1
           (JSR VM_CELL_STACK_PUSH_NIL_R) ;;
           (JSR VM_CELL_STACK_PUSH_NIL_R) ;; 3
           (JSR VM_CELL_STACK_PUSH_NIL_R) ;;
           (JSR VM_CELL_STACK_PUSH_NIL_R) ;; 5
           (JSR VM_CELL_STACK_PUSH_NIL_R) ;;
           (JSR VM_CELL_STACK_PUSH_NIL_R) ;; 7
           (JSR VM_CELL_STACK_PUSH_NIL_R)))) ;; 8

  (check-equal? (vm-stack->strings test-vm_cell_stack_push_nil-b-state-after)
                '("stack holds 8 items"
                  "cell-pair-ptr NIL  (rt)"
                  "cell-pair-ptr NIL"
                  "cell-pair-ptr NIL"
                  "cell-pair-ptr NIL"
                  "cell-pair-ptr NIL"
                  "cell-pair-ptr NIL"
                  "cell-pair-ptr NIL"
                  "cell-pair-ptr NIL")))

(module+ test #| vm_cell_push_int_r |#
  (define test-vm_cell_stack_push_int-a-state-after
    (run-code-in-test
     (list (JSR VM_CELL_STACK_PUSH_INT_m1_R)
           (LDA !$00) ;; -4096
           (LDX !$10)
           (JSR VM_CELL_STACK_PUSH_INT_R)
           (JSR VM_CELL_STACK_PUSH_INT_1_R)
           (JSR VM_CELL_STACK_PUSH_INT_0_R)
           (LDA !$ff) ;; 4095
           (LDX !$0f)
           (JSR VM_CELL_STACK_PUSH_INT_R))))

  (check-equal? (vm-regt->string test-vm_cell_stack_push_int-a-state-after)
                "cell-int $0fff")
  (check-equal? (vm-stack->strings test-vm_cell_stack_push_int-a-state-after)
                '("stack holds 5 items"
                  "cell-int $0fff  (rt)"
                  "cell-int $0000"
                  "cell-int $0001"
                  "cell-int $1000"
                  "cell-int $1fff")))

;; initial data for the memory management registers
;; put into memory @ #xced0 - len (currently 3)
(define VM_INITIAL_MM_REGS
  (list
   (label VM_INITIAL_MM_REGS)

   ;; $cec0
   (label VM_FREE_CELL_PAGE) ;; page with free cells
          (byte $00)
   ;; $cec1
   (label VM_FREE_CALL_STACK_PAGE) ;; call stack page with free space
          (byte $00) ;; initial -> first allocation will allocate a new page
   ;; $cec2
   (label VM_FREE_CODE_PAGE) ;; code page with free space
          (byte $00)

   ;; $cec3
   (label VM_FREE_CELL_PAIR_PAGE) ;; page with free cell-pairs
          (byte $00) ;; none

   ;; $cec4
   (label VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH) ;; what is the highest page to start searching for a free page
          (byte $cd) ;; safe to start with $cd is index

   ;; $cec5
   (label VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE) ;; tree of cell-pairs that are unused but still allocated (reusable)
          (word $0000) ;; if high byte is 0, the tree is empty!
          ;; this queue holds only cell-pairs, cell0 is always the pointer to the next in queue of this free cells
          ;; cell1 is left untouched => may still hold live references => to reuse a cell-pair of this queue,
          ;; cell1 must be checked (if ptr, decr ref count and possibly free, else ignore)

   ;; $cec7
   (label VM_FREE_M1_PAGE_P0)
          (byte $00) ;; cell page with free slots for m1 page p0 pages
          (byte $00) ;; cell page with free slots for m1 page p1 pages
          (byte $00) ;; cell page with free slots for m1 page p2 pages
          (byte $00) ;; cell page with free slots for m1 page p3 pages

   ;; $cecb..cecc
   (label VM_LIST_OF_FREE_CELLS) ;; list of cells that are unused but still allocated (reusable)
          (word $0000)

   ;; $cecd..$cecf (unused)
   ))

(define VM_LIST_OF_FREE_CELLS               #xcecb)
(define VM_FREE_CELL_PAIR_PAGE              #xcec3)
(define VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE #xcec5)

(define VM_PAGE_SLOT_DATA
  (list
   (label VM_PAGE_SLOT_DATA)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem 0000-07ff is unavailable (zero page, stack ... screen)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem 0800-0fff is unavailable (start of basic ram)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem 1000-17ff is unavailable
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem 1800-1fff is unavailable
          (byte $01 $01 $ff $ff  $ff $ff $ff $ff)     ;; mem 2000-27ff 2000-21ff is unavailable, 2200-27ff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 2800-2fff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 3000-37ff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 3800-3fff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 4000-47ff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 4800-4fff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 5000-57ff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 5800-5fff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 6000-67ff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 6800-6fff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 7000-77ff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 7800-7fff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 8000-87ff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 8800-8fff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 9000-97ff is free
          (byte $ff $ff $ff $ff  $ff $01 $01 $01)     ;; mem 9800-9cff is free, 9d00..9dff = first free code page, 9e00..9eff stack page, 9f00..9fff cell page
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem A000-A7ff is unavailable (C64 BASIC)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem A800-Afff is unavailable (C64 BASIC)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem B000-B7ff is unavailable (C64 BASIC)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem B800-Bfff is unavailable (C64 BASIC)
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem c000-c7ff is free
          (byte $ff $ff $ff $ff  $ff $ff $01 $01)     ;; mem c800-cdff is free, ce00-ceff = other memory management registers + bitmap, cf00-cfff =  used by next free page mapping
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem D000-D7ff is unavailable (C64 I/O)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem D800-Dfff is unavailable (C64 I/O)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem E000-E7ff is unavailable (C64 KERNAL)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem E800-Efff is unavailable (C64 KERNAL)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem F000-F7ff is unavailable (C64 KERNAL)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)))  ;; mem F800-Ffff is unavailable (C64 KERNAL)

;; initialize memory management (paging)
;; - setup 'next free page' information, basically initializing the whole page with zeros
;; - setup cell stack (to empty)
;;
;; destroys: A Y
(define VM_INITIALIZE_MEMORY_MANAGER
  (flatten
   (list
    (label VM_INITIALIZE_MEMORY_MANAGER)

           ;; initialize NEXT_FREE_PAGE_PAGE (256 byte)
           (LDA !$ff)
           (TAY)
    (label VM_INITIALIZE_MEMORY_MANAGER__LOOP)
           ;; highbyte of this address should be using the constant NEXT_FREE_PAGE_PAGE
           ;; (STA $cf00,y) ;; encoded directly in the next couple of bytes
           ;; (car (ast-opcode-cmd-bytes (STA $cf00,y)))
           (byte 153 0) (byte-ref NEXT_FREE_PAGE_PAGE)
           (INY)
           (BNE VM_INITIALIZE_MEMORY_MANAGER__LOOP)

           ;; initialize cell stack
           (LDA !$20)
           (JSR VM_ALLOC_CALL_FRAME)
           (TYA)
           (STA ZP_PARAMS_PTR)
           (CLC)
           (ADC !$08)
           (STA ZP_LOCALS_PTR)
           (STA ZP_CELL_STACK_BASE_PTR)
           (STX ZP_PARAMS_PTR+1)
           (STX ZP_LOCALS_PTR+1)
           (STX ZP_CELL_STACK_BASE_PTR+1)

           (LDX !$ff)          ;; negative and iny will produce 0
           (STX ZP_CELL_STACK_TOS)
           (LDX !$00)
           (STX ZP_RT) ;; set RT to hold no value
           (RTS))))

;; page type cell page (slot size 2b) (refcount @ ptr >> 1) 84 cells (85th slot is used for previous page pointer)
;; offset content
;; 00     #b1zzz zzzz page type + number of used slots
;; 01     ref-count for cell at 02 (cell 0)
;; 02..03 cell 0  (@2 = 8 = next free cell)
;; 04     ref-count for cell at 08 (cell 1)
;; ...
;; 07     ref-count for cell at 08 (cell 4)
;; 08..09 cell 1 (@08 = 0a = next free cell)
;; ...
;; 0e..0f cell 4 (@0e = 20 = next free cell
;; 10    ref-count for cell at 20 (cell 5)
;; ...
;; 1f    ref-count for cell at 20 (cell 20)
;; 20..21 cell 5 (@20 = 22 = next free cell)
;; ...
;; 3e..3f cell 20 (@3e = 80 = next free cell)
;; 40..7e ref-count for cell at 80..fc (cell 21..83)
;; 7f    unused
;; 80..fd cell 21..83
;; fe    unused
;; ff    previous page of this type
;;
;; input:  none
;; output: A = allocated page (of type cell page)
;;         vm_free_cell_page is new head of the list
;;         the page is initialized with each cell pointing to the next free cell on this page (0 marks the end)
;; uses: ZP_TEMP, ZP_TEMP2
(define VM_ALLOC_PAGE_FOR_CELLS
  (list
   (label VM_ALLOC_PAGE_FOR_CELLS)
          (JSR VM_ALLOC_PAGE__PAGE_UNINIT) ;; page is in A

          (STA ZP_TEMP+1)
          (TAY)
          (LDA !$02)
          (STA VM_FIRST_FREE_SLOT_ON_PAGE,y) ;; set slot @02 as the first free slot

          (LDA !$03)
          (STA BLOCK_LOOP_COUNT__VM_ALLOC_PAGE_FOR_CELLS) ;; how many blocks do we have (3)

          (LDA !$00)
          (STA ZP_TEMP)

          (LDY !$01)
          (LDX !$01)
          (STX LOOP_COUNT__VM_ALLOC_PAGE_FOR_CELLS)

          ;; option: optimization: maybe clearing the whole page would be faster (and shorter) for setting all refcounts to 0?
   (label LOOP_REF_COUNT__VM_ALLOC_PAGE_FOR_CELLS)
          (STA (ZP_TEMP),y) ;; refcount set to 0
          (INY)
          (DEX)
          (BNE LOOP_REF_COUNT__VM_ALLOC_PAGE_FOR_CELLS)
          (LDA LOOP_COUNT__VM_ALLOC_PAGE_FOR_CELLS)
          (ASL A)
          (ASL A) ;; times 4
          (STA LOOP_COUNT__VM_ALLOC_PAGE_FOR_CELLS)
          (TAX)
          (TAY) ;;
          (LDA !$00)
          (DEC BLOCK_LOOP_COUNT__VM_ALLOC_PAGE_FOR_CELLS)
          (BPL LOOP_REF_COUNT__VM_ALLOC_PAGE_FOR_CELLS)

          ;; initialize the free list of the cells (first byte in a cell = offset to next free cell)
          (LDA !$02)
          (STA BLOCK_LOOP_COUNT__VM_ALLOC_PAGE_FOR_CELLS) ;; how many blocks do we have (3, but the first block is written separately)

          ;; block 1
          (LDY !$02)
          (LDA !$08)
          (STA LOOP_COUNT__VM_ALLOC_PAGE_FOR_CELLS)
          (STA (ZP_TEMP),y)

          ;; block 2
          (TAY)
          (LDX !$04)
          (LDA !$0a)
          (DEX) ;; one loop less

          ;; blocks and their numbers
          ;; iterations offset next free (
          ;; #01        02     <- 08
          ;; #04        08..0f <- 0a.. last= 20   ->  # = prev offset*2, offset = prev offset*4
          ;; #10        20..3f <- 22.. last= 80
          ;; #40        80..7d <- 82.. last= 00

   (label LOOP_NEXT_FREE__VM_ALLOC_PAGE_FOR_CELLS)
          (STA (ZP_TEMP),y)
          (TAY)
          (CLC)
          (ADC !$02)
          (DEX)
          (BNE LOOP_NEXT_FREE__VM_ALLOC_PAGE_FOR_CELLS)

          ;; block n+1
          ;; write last entry
          (LDA LOOP_COUNT__VM_ALLOC_PAGE_FOR_CELLS)
          (ASL A)
          (TAX)
          (ASL A)
          (STA LOOP_COUNT__VM_ALLOC_PAGE_FOR_CELLS)
          (STA (ZP_TEMP),y)
          (TAY)
          (CLC)
          (ADC !$02)
          (DEX)
          (DEC BLOCK_LOOP_COUNT__VM_ALLOC_PAGE_FOR_CELLS)
          (BPL LOOP_NEXT_FREE__VM_ALLOC_PAGE_FOR_CELLS)

          ;; write last entry
          (LDA !$00)
          (LDY !$fc) ;; fc..fd is the last cell, fe..ff is unusable (since ff holds the previous page)
          (STA (ZP_TEMP),y)

          (LDY !$ff)
          (LDA VM_FREE_CELL_PAGE) ;; store last free cell page in $ff
          (STA (ZP_TEMP),y)

          ;; store page type in byte 0
          (LDY !$00)
          (LDA !$80)
          (STA (ZP_TEMP),y)

          (LDA ZP_TEMP+1) ;; page
          (STA VM_FREE_CELL_PAGE) ;; store allocated page as new free cell page

          (RTS)

   (label LOOP_COUNT__VM_ALLOC_PAGE_FOR_CELLS)
          (byte $00)
   (label BLOCK_LOOP_COUNT__VM_ALLOC_PAGE_FOR_CELLS)
          (byte $00)))

(module+ test #| vm_alloc_page__cell |#
  (define test-alloc-page--cell-code
    (list
            ;; fill page with cc
            (LDX !$00)
            (LDA !$77)
     (label FILL_PAGE__TEST_ALLOC_PAGE__CELL)
            (STA $cc00,x)
            (DEX)
            (BNE FILL_PAGE__TEST_ALLOC_PAGE__CELL)

            ;; now do allocation and write structure data into the page
            (JSR VM_ALLOC_PAGE_FOR_CELLS)))

  (define test-alloc-page--cell-state-after
    (run-code-in-test test-alloc-page--cell-code))

  (check-equal? (memory-list test-alloc-page--cell-state-after #xcc00 #xcc0f)
                (list #x80
                      #x00       ;; ref count cell 0 (@2)
                      #x08 #x77  ;; cell0: next free @8
                      #x00       ;; refcount cell1 (@8)
                      #x00       ;; refcount cell2 (@a)
                      #x00       ;; refcount cell3 (@c)
                      #x00       ;; refcount cell4 (@e)
                      #x0a #x77  ;; cell1: next free @10
                      #x0c #x77  ;; cell2: next free @12
                      #x0e #x77  ;; cell3: next free @14
                      #x20 #x77  ;; cell4: next free @32
                      ))
  (check-equal? (memory-list test-alloc-page--cell-state-after #xcc10 #xcc1f)
                (make-list #x10 #x0))
  (check-equal? (memory-list test-alloc-page--cell-state-after #xcc20 #xcc27)
                (list #x22 #x77  ;; cell5: next free @34
                      #x24 #x77  ;; cell6: next free @36
                      #x26 #x77  ;; cell7: next free @38
                      #x28 #x77  ;; cell8: next free @40
                      ))
  (check-equal? (memory-list test-alloc-page--cell-state-after #xcc38 #xcc3f)
                (list #x3a #x77  ;; cell17: next free @58
                      #x3c #x77  ;; cell18: next free @60
                      #x3e #x77  ;; cell19: next free @62
                      #x80 #x77  ;; cell20: next free @128
                      ))
  (check-equal? (memory-list test-alloc-page--cell-state-after #xcc40 #xcc7e)
                (make-list #x3f #x0)
                "refcounts are all zero")
  (check-equal? (memory-list test-alloc-page--cell-state-after #xcc80 #xcc87)
                (list #x82 #x77  ;; cell21: next free @130
                      #x84 #x77  ;; cell22: next free @132
                      #x86 #x77  ;; cell23: next free @134
                      #x88 #x77  ;; cell24: next free @136
                      ))
  (check-equal? (memory-list test-alloc-page--cell-state-after #xccf8 #xccff)
                (list #xfa #x77  ;; cell: next free @250
                      #xfc #x77  ;; cell: next free @252
                      #x00 #x77  ;; cell: next free 0
                      #x00       ;; unused
                      #x00       ;; pointer to previous page
                      )))


  ;; cell-pair page layout  (new layout with cell-pair-ptr having bit0 always set and bit1 always unset!)
  ;; offset  content
  ;; ---------------
  ;; 00      #b01xx xxxx page type + number of used slots
  ;; 01      ref-count cell-pair at 05 (cell-pair 0)
  ;; 02      ref-count cell-pair at 09 (cell-pair 1)
  ;; 03..04   unused (2)
  ;; 05..08   cell-pair 0     (#b0000 01[01] >> 2 = #b0000 0001)
  ;; 09..0c   cell-pair 1     (#b0000 10[01] >> 2 = #b0000 0010)
  ;; 0d..0f   unused (3)
  ;; 10      ref-count for cell-pair at 40 (cell-pair 2)
  ;; 11      ref-count for cell-pair at 44 (cell-pair 3)
  ;; ..3e     ref-count for cell-pair at f9 (cell-pair 48)
  ;; 3f..40   unused (2)
  ;; 41..44   cell-pair 2     (#b0100 00[01] >> 2 = #b0001 0000)
  ;; 45..48   cell-pair 3     (#b0100 01[01] >> 2 = #b0001 0001)
  ;; ...
  ;; f9..fc   cell-pair 48    (#b1111 10[01] >> 2 = #b0011 1110)
  ;; fd..fe   unused (2)
  ;; ff      previous page of this type
  ;;
  ;; VM_FIRST_FREE_SLOT_ON_PAGE + pageidx: holds the index within the page of the first free cell-pair on that page (0 = no free cell-pair on this page)
  ;; the free cell-pair holds in byte 0 of the cell-pair the offset of the next free cell-pair (0 = no other free cell-pair)
  ;;
  ;; allocate a complete new page and initialize it to hold reference counted cell-pairs
  ;; connect all cell-pairs in a free-list
  ;; also set the first free slot of this allocated page (in VM_FIRST_FREE_SLOT_ON_PAGE + pageidx)
  ;;
  ;; input:  -
  ;; output: X = page allocated (for cell-pairs)
  ;; usage:  A, X, Y, ZP_TEMP, ZP_TEMP2
(define VM_ALLOC_PAGE_FOR_CELL_PAIRS
    (list
     (label VM_ALLOC_PAGE_FOR_CELL_PAIRS)
            (JSR VM_ALLOC_PAGE__PAGE_UNINIT)
            ;; now initialize page in A

            (TAX)

            (STA ZP_TEMP+1)
            (TAY) ;; page used as idx
            (LDA !$05) ;; offset to first free slot (after initialization)
            (STA VM_FIRST_FREE_SLOT_ON_PAGE,y)

            (LDY !$00)
            (STY ZP_TEMP)
            (LDA !$40) ;; page type cell-pairs w/ 0 slots allocated
            (STA (ZP_TEMP),y)

            (LDY !$ff)
            (LDA VM_FREE_CELL_PAIR_PAGE)
            (STA (ZP_TEMP),y) ;; previous page of this type is (@$ff = )

            (STX VM_FREE_CELL_PAIR_PAGE)
            
            ;; first write all reference count fields (zero)
            ;; block0 (two ref counts)
            (LDY !$01)
            (LDA !$00) ;; reference count initialized with 0
            (STA (ZP_TEMP),y) ;; @ 01
            (INY)
            (STA (ZP_TEMP),y) ;; @ 02

            (LDY !$10)
            (STY ZP_TEMP)
            (LDY !$2F)

     (label SECOND_RC_BLOCK__VM_ALLOC_PAGE_FOR_CELL_PAIRS)
            (STA (ZP_TEMP),y)
            (DEY)
            (BPL SECOND_RC_BLOCK__VM_ALLOC_PAGE_FOR_CELL_PAIRS)

            (STA ZP_TEMP) ;; clear lowbyte of ptr

            ;; write all cell-pairs to point to next free one
            (LDY !$05)
            (LDA !$09)
            (STA (ZP_TEMP),y) ;; @05 <- 09
            (TAY)
            (LDA !$41)
            (STA (ZP_TEMP),y) ;; @09 <- 41

     (label SECOND_CELL_PAIR_BLOCK__VM_ALLOC_PAGE_FOR_CELL_PAIRS)
            (TAY)
            (CLC)
            (ADC !$04)
            (STA (ZP_TEMP),y)
            (CMP !$F9)
            (BNE SECOND_CELL_PAIR_BLOCK__VM_ALLOC_PAGE_FOR_CELL_PAIRS)

            (TAY)
            (LDA !$00)
            (STA (ZP_TEMP),y) ;; last cell points to 0

            (RTS)))

(module+ test #| vm_alloc_page_for_cell_pairs |#
  (define vm-alloc-page-for-cell-pairs-code
    (list
     (LDA !$a0)
     (STA VM_FREE_CELL_PAIR_PAGE) 
     (JSR VM_ALLOC_PAGE_FOR_CELL_PAIRS)
     (STX ZP_RT) ;; to test read out actual page
     ))

  (define vm-alloc-page-for-cell-pairs-state
    (run-code-in-test vm-alloc-page-for-cell-pairs-code))

  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state ZP_RT ZP_RT)
                (list #xcc)
                "page cc was allocated")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state #xcc00 #xcc02)
                (list #b01000000 #x00 #x00)
                "page type is #b01000000 and refcounts cell0 and cell1 are both 0")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state #xcc05 #xcc05)
                (list #x09)
                "cell0 first byte points to next free (09)")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state #xcc09 #xcc09)
                (list #x41)
                "cell1 first byte points to next free (41)")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state #xcc10 #xcc3e)
                (make-list #x2f #x00)
                "refcounts are all 0 (in block 2)")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state #xcc41 #xcc41)
                (list #x45)
                "cell1 first byte points to next free (45)")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state #xcc45 #xcc45)
                (list #x49)
                "cell2 first byte points to next free (49)")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state #xccf5 #xccf5)
                (list #xf9)
                "cell47 first byte points to next free (f9)")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state #xccf9 #xccf9)
                (list #x00)
                "last cell first byte points to 0 (no next free)")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state #xccff #xccff)
                (list #xa0)
                "last byte on page points to previous free page of cell-pairs"))

;; whether a page is free or used is kept in the 256 bytes starting at VM_FIRST_FREE_SLOT_ON_PAGE
;; each byte represents one page
;;   00 = allocated (used) but no free slots
;;   01 = system page, not available for memory management
;;   ff = free page (not allocated yet)
;; VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH  (255..0) keeps the max idx to start looking for a page that is free
;; parameter: a = page
;; result: (none)
(define VM_FREE_PAGE
  (list
   (label VM_FREE_PAGE)
          (CMP VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH)
          (BMI NO_CHANGE__VM_FREE_PAGE)
          (STA VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH)

   (label NO_CHANGE__VM_FREE_PAGE)
          (TAY)
          (LDA !$ff) ;; free/unallocated page
          (STA VM_FIRST_FREE_SLOT_ON_PAGE,y)
          (RTS)))

;; does a linear search for the next free page
;; allocate a page (completely uninitialized), just the page, update the memory page status in VM_FIRST_FREE_SLOT_ON_PAGE
;; parameter: (none)
;; result: A = allocated free page (uninitialized)
;; uses: A, Y,
(define VM_ALLOC_PAGE__PAGE_UNINIT
  (list
   (label VM_ALLOC_PAGE__PAGE_UNINIT)
          (LDY VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH)

   (label LOOP__VM_ALLOC_PAGE__PAGE_UNINIT)
          (LDA VM_FIRST_FREE_SLOT_ON_PAGE,y)
          (DEY)
          (BEQ OUT_OF_MEMORY__VM_ALLOC_PAGE__PAGE_UNINIT) ;; cannot be lower then 1!!
          (CMP !$ff)
          (BNE LOOP__VM_ALLOC_PAGE__PAGE_UNINIT)

          ;; found page marked unallocated ($ff)
          (INY) ;; restore original index
          (LDA !$00) ;; mark as initially full but allocated
          (STA VM_FIRST_FREE_SLOT_ON_PAGE,y)
          (TYA)
          (STA VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH) ;; set index for next search
          (RTS)

   (label OUT_OF_MEMORY__VM_ALLOC_PAGE__PAGE_UNINIT)
          (BRK)
          ))

(module+ test #| vm-free-page and vm-alloc-page--page-uninit |#
  (define vm-free-page-code
    (list
     (JSR VM_ALLOC_PAGE__PAGE_UNINIT) ;; page is in A ($cc)
     (PHA)
     (JSR VM_ALLOC_PAGE__PAGE_UNINIT) ;; page is in A ($cb)
     (JSR VM_WRITE_INT_A_TO_RT)
     (PLA)
     (JSR VM_FREE_PAGE )
     (JSR VM_ALLOC_PAGE__PAGE_UNINIT) ;; allocated page should be $cc again
     (JSR VM_WRITE_INT_A_TO_RA)))

  (define vm-free-page-state
    (run-code-in-test vm-free-page-code))

  (check-equal? (vm-rega->string vm-free-page-state)
                "cell-int $00cc")
  (check-equal? (vm-regt->string vm-free-page-state)
                "cell-int $00cb"))

;; allocate a cell-pair from this page (if page has no free cell-pairs, a new page is allocated and is used to get a free cell-pair!)
;; this will not check the free cell-pair tree!
;; input:  A : page to allocate cell-pair on (a new page is allocated, if this page does not have any free cell-pairs)
;; output: ZP_RT
;; WARNING: ZP_RT IS OVERWRITTEN !! NO PUSH INTO THE CELL-STACK IS DONE!
(define VM_ALLOC_CELL_PAIR_ON_PAGE_A_INTO_RT
  (list
   (label ALLOC_NEW_PAGE_PREFIX__VM_ALLOC_CELL_PAIR_ON_PAGE_A_INTO_RT)
          (JSR VM_ALLOC_PAGE_FOR_CELL_PAIRS)
          (TXA)

   ;; ----------------------------------------
   (label VM_ALLOC_CELL_PAIR_ON_PAGE_A_INTO_RT) ;; <-- real entry point of this function
          (STA ZP_RT+1) ;; safe as highbyte of ptr
          (TAX)
          (LDA VM_FIRST_FREE_SLOT_ON_PAGE,x)
          (BEQ ALLOC_NEW_PAGE_PREFIX__VM_ALLOC_CELL_PAIR_ON_PAGE_A_INTO_RT) ;; allocate new page first

   (label CELL_ON_THIS_PAGE__VM_ALLOC_CELL_PAIR_ON_PAGE_A_INTO_RT)
          (STA ZP_RT)
          (LDY !$00)
          (LDA (ZP_RT),y) ;; next free cell
          (STA VM_FIRST_FREE_SLOT_ON_PAGE,x)

          ;; increase the slot number used on this page
          (STX INC_CMD__VM_ALLOC_CELL_PAIR_ON_PAGE_A_INTO_RT+2) ;; overwrite $c0 (page in following INC command)
   (label INC_CMD__VM_ALLOC_CELL_PAIR_ON_PAGE_A_INTO_RT)
          (INC $c000)
          (RTS)))

(module+ test #| vm-alloc-cell-pair-on-page-a-into-rt |#
  (define vm-alloc-cell-pair-on-page-a-into-rt-code
    (list
     (JSR ALLOC_NEW_PAGE_PREFIX__VM_ALLOC_CELL_PAIR_ON_PAGE_A_INTO_RT)))

  (define vm-alloc-cell-pair-on-page-a-into-rt-state
    (run-code-in-test vm-alloc-cell-pair-on-page-a-into-rt-code))

  (check-equal? (vm-page->strings vm-alloc-cell-pair-on-page-a-into-rt-state #xcc)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $09"))
  (check-equal? (vm-regt->string vm-alloc-cell-pair-on-page-a-into-rt-state)
                "cell-pair-ptr $cc05"))

(module+ test #| vm-alloc-cell-pair-on-page-a-into-rt 2 times|#
  (define vm-alloc-cell-pair-on-page-a-into-rt-code2
    (list
     (JSR ALLOC_NEW_PAGE_PREFIX__VM_ALLOC_CELL_PAIR_ON_PAGE_A_INTO_RT)
     (LDA !$cc)
     (JSR VM_ALLOC_CELL_PAIR_ON_PAGE_A_INTO_RT)))

  (define vm-alloc-cell-pair-on-page-a-into-rt-state2
    (run-code-in-test vm-alloc-cell-pair-on-page-a-into-rt-code2))

  (check-equal? (vm-page->strings vm-alloc-cell-pair-on-page-a-into-rt-state2 #xcc)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $41"))
  (check-equal? (vm-regt->string vm-alloc-cell-pair-on-page-a-into-rt-state2)
                "cell-pair-ptr $cc09"))

;; find out what kind of cell zp_rt points to,
;; then call the right decrement refcounts function
;; input:  ZP_RT
;; output: the right refcount is decremented
;;         (in case of m1 pages, @ZP_RT-1)
;;         (in case of cell pages @ZP_RT>>1)
;;         (in case of cell-pair pages @ZP_RT>>2)
;;         (in case of 8s pages @ZP_RT>>3)
(define VM_REFCOUNT_DECR_RT
  (list
   (label VM_REFCOUNT_DECR_RT)
          (LDA ZP_RT)
          (TAY)
          (LSR)
          (BCC DECR_CELL_PTR__VM_REFCOUNT_DECR_RT)
          (LSR)
          (BCC DECR_CELL_PAIR__VM_REFCOUNT_DECR_RT)
          ;; check other types of cells
          (CPY !TAG_BYTE_CELL_ARRAY)
          (BEQ DECR_CELL_ARRAY__VM_REFCOUNT_DECR_RT)
          (CPY !TAG_BYTE_NATIVE_ARRAY)
          (BEQ DECR_NATIVE_ARRAY__VM_REFCOUNT_DECR_RT)

          ;; unknown object type (or atomic value that cannot be ref counted and MUST NOT END UP in ZP_RT)
          (BRK)

   (label DECR_CELL_ARRAY__VM_REFCOUNT_DECR_RT)
   (label DECR_NATIVE_ARRAY__VM_REFCOUNT_DECR_RT)
          ;; (JMP VM_REFCOUNT_DECR_RT__M1_SLOT_PTR)
          (BRK)

   (label DECR_CELL_PAIR__VM_REFCOUNT_DECR_RT)
          (JMP VM_REFCOUNT_DECR_RT__CELL_PAIR_PTR)

   (label DECR_CELL_PTR__VM_REFCOUNT_DECR_RT)
          (JMP VM_REFCOUNT_DECR_RT__CELL_PTR)))

;; find out what kind of cell zp_rt points to,
;; then call the right decrement refcounts function
;; input:  ZP_RT
;; output: the right refcount is decremented
;;         (in case of m1 pages, @ZP_RT-1)
;;         (in case of cell pages @ZP_RT>>1)
;;         (in case of cell-pair pages @ZP_RT>>2)
;;         (in case of 8s pages @ZP_RT>>3)
(define VM_REFCOUNT_INCR_RT
  (list
   (label VM_REFCOUNT_INCR_RT)
          (LDA ZP_RT)
          (TAY)
          (LSR)
          (BCC INCR_CELL_PTR__VM_REFCOUNT_INCR_RT)
          (LSR)
          (BCC INCR_CELL_PAIR__VM_REFCOUNT_INCR_RT)
          ;; check other types of cells
          (CPY !TAG_BYTE_CELL_ARRAY)
          (BEQ INCR_CELL_ARRAY__VM_REFCOUNT_INCR_RT)
          (CPY !TAG_BYTE_NATIVE_ARRAY)
          (BEQ INCR_NATIVE_ARRAY__VM_REFCOUNT_INCR_RT)

          ;; unknown object type (or atomic value that cannot be ref counted and MUST NOT END UP in ZP_RT)
          (BRK)

   (label INCR_CELL_ARRAY__VM_REFCOUNT_INCR_RT)
   (label INCR_NATIVE_ARRAY__VM_REFCOUNT_INCR_RT)
          ;; (JMP VM_REFCOUNT_INCR_RT__M1_SLOT_PTR)
          (BRK)

   (label INCR_CELL_PAIR__VM_REFCOUNT_INCR_RT)
          (JMP VM_REFCOUNT_INCR_RT__CELL_PAIR_PTR)

   (label INCR_CELL_PTR__VM_REFCOUNT_INCR_RT)
          (JMP VM_REFCOUNT_INCR_RT__CELL_PTR)))

(module+ test #| vm-refcount-decr-rt |#
  (define vm-refcount-decr-rt-code
    (list
     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)
     (JSR VM_REFCOUNT_INCR_RT)
     (JSR VM_REFCOUNT_INCR_RT)))

  (define vm-refcount-decr-rt-state
    (run-code-in-test vm-refcount-decr-rt-code))

  (check-equal? (vm-refcount-cell-pair-ptr vm-refcount-decr-rt-state #xcc05)
                2)

  (define vm-refcount-decr-rt-code2
    (list
     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)
     (JSR VM_REFCOUNT_INCR_RT)
     (JSR VM_REFCOUNT_INCR_RT)
     (JSR VM_REFCOUNT_DECR_RT)))

  (define vm-refcount-decr-rt-state2
    (run-code-in-test vm-refcount-decr-rt-code2))

  (check-equal? (vm-refcount-cell-pair-ptr vm-refcount-decr-rt-state2 #xcc05)
                1))

;; input: cell-pair ptr in ZP_RT
;; decrement ref count, if 0 deallocate
(define VM_REFCOUNT_DECR_RT__CELL_PAIR_PTR
  (list
   (label VM_REFCOUNT_DECR_RT__CELL_PAIR_PTR)
          (LDA ZP_RT+1)
          (STA PAGE__VM_REFCOUNT_DECR_RT__CELL_PAIR_PTR+2) ;; store high byte (page) into dec-command high-byte (thus +2 on the label)
          (LDA ZP_RT)
          (LSR)
          (LSR)
          (TAX)
   (label PAGE__VM_REFCOUNT_DECR_RT__CELL_PAIR_PTR)
          (DEC $c000,x) ;; c0 is overwritten by page (see above)
          (BNE DONE__VM_REFCOUNT_DECR_RT__CELL_PAIR_PTR)
          (JMP VM_FREE_CELL_PAIR_PTR_IN_RT) ;; free delayed
   (label DONE__VM_REFCOUNT_DECR_RT__CELL_PAIR_PTR)
          (RTS)))

;; input: cell-pair ptr in ZP_RT
;; increment ref count
(define VM_REFCOUNT_INCR_RT__CELL_PAIR_PTR
  (list
   (label VM_REFCOUNT_INCR_RT__CELL_PAIR_PTR)
          (LDA ZP_RT+1)
          (STA PAGE__VM_REFCOUNT_INCR_RT__CELL_PAIR_PTR+2) ;; store high byte (page) into inc-command high-byte (thus +2 on the label)
          (LDA ZP_RT)
          (LSR)
          (LSR)
          (TAX)
   (label PAGE__VM_REFCOUNT_INCR_RT__CELL_PAIR_PTR)
          (INC $c000,x) ;; c0 is overwritten by page (see above)
          (RTS)))

(module+ test #| vm-refcount-mmcr-rt--cell-pair-ptr |#
  (define vm-refcount-mmcr-rt--cell-pair-ptr-code
    (list
     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)
     (JSR VM_REFCOUNT_INCR_RT__CELL_PAIR_PTR)
     (JSR VM_REFCOUNT_INCR_RT__CELL_PAIR_PTR)))

  (define vm-refcount-mmcr-rt--cell-pair-ptr-state
    (run-code-in-test vm-refcount-mmcr-rt--cell-pair-ptr-code))

  (check-equal? (vm-regt->string vm-refcount-mmcr-rt--cell-pair-ptr-state)
                "cell-pair-ptr $cc05")
  (check-equal? (vm-refcount-cell-pair-ptr vm-refcount-mmcr-rt--cell-pair-ptr-state #xcc05)
                2)

  (define vm-refcount-mmcr-rt--cell-pair-ptr-code2
    (list
     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)
     (JSR VM_REFCOUNT_INCR_RT__CELL_PAIR_PTR)
     (JSR VM_REFCOUNT_INCR_RT__CELL_PAIR_PTR)
     (JSR VM_REFCOUNT_DECR_RT__CELL_PAIR_PTR)))

  (define vm-refcount-mmcr-rt--cell-pair-ptr-state2
    (run-code-in-test vm-refcount-mmcr-rt--cell-pair-ptr-code2))

  (check-equal? (vm-regt->string vm-refcount-mmcr-rt--cell-pair-ptr-state2)
                "cell-pair-ptr $cc05")
  (check-equal? (vm-refcount-cell-pair-ptr vm-refcount-mmcr-rt--cell-pair-ptr-state2 #xcc05)
                1))

;; input: cell ptr in ZP_RT
;; decrement ref count, if 0 deallocate
(define VM_REFCOUNT_DECR_RT__CELL_PTR
  (list
   (label VM_REFCOUNT_DECR_RT__CELL_PTR)
          (LDA ZP_RT+1)
          (STA PAGE__VM_REFCOUNT_DECR_RT__CELL_PTR+2) ;; store high byte (page) into dec-command high-byte (thus +2 on the label)
          (LDA ZP_RT)
          (LSR)
          (TAX)
   (label PAGE__VM_REFCOUNT_DECR_RT__CELL_PTR)
          (DEC $c000,x) ;; c0 is overwritten by page (see above)
          (BNE DONE__VM_REFCOUNT_DECR_RT__CELL_PTR)
          (JMP VM_FREE_CELL_PTR_IN_RT) ;; free delayed
   (label DONE__VM_REFCOUNT_DECR_RT__CELL_PTR)
          (RTS)))

;; input: cell ptr in ZP_RT
;; increment ref count
(define VM_REFCOUNT_INCR_RT__CELL_PTR
  (list
   (label VM_REFCOUNT_INCR_RT__CELL_PTR)
          (LDA ZP_RT+1)
          (STA PAGE__VM_REFCOUNT_INCR_RT__CELL+2) ;; store high byte (page) into inc-command high-byte (thus +2 on the label)
          (LDA ZP_RT) ;; get cell offset
          (LSR)       ;; get offset to ref count of this cell
          (TAX)       ;; -> index 
   (label PAGE__VM_REFCOUNT_INCR_RT__CELL)
          (INC $c000,x) ;; c0 is overwritten by page (see above)
          (RTS)))

(module+ test #| vm-refcount-mmcr-rt--cell-pair-ptr |#
  (define vm-refcount-mmcr-rt--cell-ptr-code
    (list
     (JSR VM_ALLOC_CELL_PTR_TO_RT)
     (JSR VM_REFCOUNT_INCR_RT__CELL_PTR)
     (JSR VM_REFCOUNT_INCR_RT__CELL_PTR)))

  (define vm-refcount-mmcr-rt--cell-ptr-state
    (run-code-in-test vm-refcount-mmcr-rt--cell-ptr-code))

  (check-equal? (vm-regt->string vm-refcount-mmcr-rt--cell-ptr-state)
                "cell-ptr $cc02")
  (check-equal? (vm-refcount-cell-ptr vm-refcount-mmcr-rt--cell-ptr-state #xcc02)
                2)

  (define vm-refcount-mmcr-rt--cell-ptr-code2
    (list
     (JSR VM_ALLOC_CELL_PTR_TO_RT)
     (JSR VM_REFCOUNT_INCR_RT__CELL_PTR)
     (JSR VM_REFCOUNT_INCR_RT__CELL_PTR)
     (JSR VM_REFCOUNT_DECR_RT__CELL_PTR)))

  (define vm-refcount-mmcr-rt--cell-ptr-state2
    (run-code-in-test vm-refcount-mmcr-rt--cell-ptr-code2))

  (check-equal? (vm-regt->string vm-refcount-mmcr-rt--cell-ptr-state2)
                "cell-ptr $cc02")
  (check-equal? (vm-refcount-cell-ptr vm-refcount-mmcr-rt--cell-ptr-state2 #xcc02)
                1))

;; free nonatomic (is cell-ptr, cell-pair-ptr, m1-slot-ptr, slot8-ptr)
;; parameter: zp_rt
(define VM_FREE_PTR_IN_RT
  (list
   (label VM_FREE_PTR_IN_RT)
          (LDA ZP_RT)
          (TAY)
          (LSR)
          (BCC FREE_CELL__VM_FREE_PTR_IN_RT)
          (LSR)
          (BCC FREE_CELL_PAIR__VM_FREE_PTR_IN_RT)
          ;; check other types of cells
          (CPY !TAG_BYTE_CELL_ARRAY)
          (BEQ FREE_CELL_ARRAY__VM_FREE_PTR_IN_RT)
          (CPY !TAG_BYTE_NATIVE_ARRAY)
          (BEQ FREE_NATIVE_ARRAY__VM_FREE_PTR_IN_RT)

          ;; unknown pointer type in zp_ptr
          (BRK)

   (label FREE_CELL__VM_FREE_PTR_IN_RT)
          (JMP VM_FREE_CELL_PTR_IN_RT)

   (label FREE_CELL_PAIR__VM_FREE_PTR_IN_RT)
          (JMP VM_FREE_CELL_PAIR_PTR_IN_RT)

   (label FREE_CELL_ARRAY__VM_FREE_PTR_IN_RT)
   (label FREE_NATIVE_ARRAY__VM_FREE_PTR_IN_RT)
          ;; VM_FREE_M1_SLOT_IN_RT
          (BRK)))

(module+ test #| vm-free-ptr-in-rt |#
  (define vm-free-ptr-in-rt-code
    (list
            (LDA !$00)
            (STA $ff)
            (JMP TEST_START__VM_FREE_PTR_IN_RT)

     (label VM_FREE_CELL_PTR_IN_RT)
            (INC $ff)
            (RTS)

     (label TEST_START__VM_FREE_PTR_IN_RT )
            (JSR VM_ALLOC_CELL_PTR_TO_RT)
            (JSR VM_FREE_PTR_IN_RT)))

  (define vm-free-ptr-in-rt-state
    (run-code-in-test vm-free-ptr-in-rt-code #:mock (list (label VM_FREE_CELL_PTR_IN_RT))))

  (check-equal? (memory-list vm-free-ptr-in-rt-state #xff #xff)
                (list #x01)
                "dispatches call to free-cell-ptr routine")

  (define vm-free-ptr-in-rt-2-code
    (list
            (LDA !$00)
            (STA $ff)
            (JMP TEST_START__VM_FREE_PTR_IN_RT)

     (label VM_FREE_CELL_PAIR_PTR_IN_RT)
            (INC $ff)
            (RTS)

     (label TEST_START__VM_FREE_PTR_IN_RT )
            (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)
            (JSR VM_FREE_PTR_IN_RT)))

  (define vm-free-ptr-in-rt-2-state
    (run-code-in-test vm-free-ptr-in-rt-2-code #:mock (list (label VM_FREE_CELL_PAIR_PTR_IN_RT))))

  (check-equal? (memory-list vm-free-ptr-in-rt-2-state #xff #xff)
                (list #x01)
                "dispatches call to free-cell-pair-ptr routine"))

;; allocate a cell on the page in A (allocating a new one if page is full)
;; this does not check for any free cells on the free list!
;; input:  A - page to use (if full allocate new page)
;; output: A - actual page used
;;         # cells used on page is incremented
;;         adjust page local list of free cell on this page
(define VM_ALLOC_CELL_ON_PAGE
  (list
   (label ALLOC_NEW_PAGE_PREFIX__VM_ALLOC_CELL_ON_PAGE)
          (JSR VM_ALLOC_PAGE_FOR_CELLS)

   ;; ----------------------------------------
   (label VM_ALLOC_CELL_ON_PAGE) ;; <-- real entry point of this function
          (STA ZP_RT+1) ;; safe as highbyte of ptr
          (TAX)
          (LDA VM_FIRST_FREE_SLOT_ON_PAGE,x)
          (BEQ ALLOC_NEW_PAGE_PREFIX__VM_ALLOC_CELL_ON_PAGE) ;; allocate new page first

   (label CELL_ON_THIS_PAGE__VM_ALLOC_CELL_ON_PAGE)
          (STA ZP_RT)
          (LDY !$00)
          (LDA (ZP_RT),y) ;; next free cell
          (STA VM_FIRST_FREE_SLOT_ON_PAGE,x)

          ;; increase the slot number on this page
          (STX INC_CMD__VM_ALLOC_CELL_ON_PAGE+2) ;; overwrite $c0
   (label INC_CMD__VM_ALLOC_CELL_ON_PAGE)
          (INC $c000)
          (TXA)
          (RTS)))

(module+ test #| vm_alloc_cell_on_page (allocating new page) |#
  (define test-alloc-cell-on-papge-a-code
    (list
     (LDX !$cd)
     (LDA !$00) ;; no free slot on this page, allocate new page
     (STA VM_FIRST_FREE_SLOT_ON_PAGE,x)
     (TXA)
     (JSR VM_ALLOC_CELL_ON_PAGE)
     ))

  (define test-alloc-cell-on-papge-a-state-after
    (run-code-in-test test-alloc-cell-on-papge-a-code))

  (check-equal? (memory-list test-alloc-cell-on-papge-a-state-after ZP_RT (add1 ZP_RT))
                (list #x02 #xcc))
  (check-equal? (memory-list test-alloc-cell-on-papge-a-state-after #xcfcc #xcfcc)
                (list #x08)))

(module+ test #| vm_alloc_cell_on_page (using previously allocated page) |#
  (define test-alloc-cell-on-papge-b-code
    (list
     (JSR VM_ALLOC_PAGE_FOR_CELLS) ;; allocates cc00
     (JSR VM_ALLOC_PAGE_FOR_CELLS) ;; allocates cb00
     (JSR VM_ALLOC_CELL_ON_PAGE)
     ))

  (define test-alloc-cell-on-papge-b-state-after
    (run-code-in-test test-alloc-cell-on-papge-b-code))

  (check-equal? (memory-list test-alloc-cell-on-papge-b-state-after ZP_RT (add1 ZP_RT))
                (list #x02 #xcb))
  (check-equal? (memory-list test-alloc-cell-on-papge-b-state-after #xcfcc #xcfcc)
                (list #x02)
                "cc00 has all cells left => first free is 02")
  (check-equal? (memory-list test-alloc-cell-on-papge-b-state-after #xcfcb #xcfcb)
                (list #x08)
                "cb00 has one cell allocated => first free is 08"))

;; allocate a cell, allocating a new page if necessary, reusing cells from the free list first
;; input:  none
;; output: zp_ptr = pointer to free cell
(define VM_ALLOC_CELL_PTR_TO_RT
  (list
   (label VM_ALLOC_CELL_PTR_TO_RT)
          (LDA VM_LIST_OF_FREE_CELLS+1)
          (BNE REUSE__VM_ALLOC_CELL_PTR_TO_RT)

          ;; get a cell on the given page (or allocate a new page)
          (LDA VM_FREE_CELL_PAGE)        ;; get the page that has cell available (can be 0)
          (BEQ ALLOCATE_NEW_PAGE__VM_ALLOC_CELL)
          (JMP VM_ALLOC_CELL_ON_PAGE)                        ;; allocate a new cell on that page
   (label ALLOCATE_NEW_PAGE__VM_ALLOC_CELL)
          (JMP ALLOC_NEW_PAGE_PREFIX__VM_ALLOC_CELL_ON_PAGE) ;; allocate new page and then a new cell on that page

   (label REUSE__VM_ALLOC_CELL_PTR_TO_RT)
          ;; reuse old cell (and write the head into zp_ptr)
          (STA ZP_RT+1)
          (LDA VM_LIST_OF_FREE_CELLS)
          (STA ZP_RT)

          ;; read output this old cell and store its content as new head of the free list
          (LDY !$00)
          (LDA (ZP_RT),y)
          (STA VM_LIST_OF_FREE_CELLS)
          (INY)
          (LDA (ZP_RT),y)
          (STA VM_LIST_OF_FREE_CELLS)

          (RTS)))

(module+ test #| vm_alloc_cell_ptr_to_rt (once on a new page) |#
  (define test-alloc-cell-to-zp-ptr-code
    (list
     (JSR VM_ALLOC_CELL_PTR_TO_RT)))

  (define test-alloc-cell-to-zp-ptr-state-after
    (run-code-in-test test-alloc-cell-to-zp-ptr-code))

  (check-equal? (memory-list test-alloc-cell-to-zp-ptr-state-after VM_LIST_OF_FREE_CELLS VM_LIST_OF_FREE_CELLS)
                (list #x00)
                "list of free cells is empty")

  (check-equal? (memory-list test-alloc-cell-to-zp-ptr-state-after ZP_RT (add1 ZP_RT))
                (list #x02 #xcc))

  (check-equal? (vm-page->strings test-alloc-cell-to-zp-ptr-state-after #xcc)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $08")
                "page has 1 slot in use"))

(module+ test #| vm_alloc_cell_ptr_to_rt (twice on a new page) |#
  (define test-alloc-cell-to-zp-ptr-twice-code
    (list
     (JSR VM_ALLOC_CELL_PTR_TO_RT)
     (JSR VM_ALLOC_CELL_PTR_TO_RT)))

  (define test-alloc-cell-to-zp-ptr-twice-state-after
    (run-code-in-test test-alloc-cell-to-zp-ptr-twice-code))

  (check-equal? (memory-list test-alloc-cell-to-zp-ptr-twice-state-after VM_LIST_OF_FREE_CELLS VM_LIST_OF_FREE_CELLS)
                (list #x00)
                "list of free cells is empty")

  (check-equal? (memory-list test-alloc-cell-to-zp-ptr-twice-state-after ZP_RT (add1 ZP_RT))
                (list #x08 #xcc))

  (check-equal? (vm-page->strings test-alloc-cell-to-zp-ptr-twice-state-after #xcc)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $0a")
                "page has 2 slots in use"))

;; (module+ test #| vm_alloc_cell_to_zp_ptr (twice, then free first on a new page) |#
;;   (define test-alloc-cell-to-zp-ptr-twicenfree-code
;;     (list
;;      (JSR VM_ALLOC_CELL_TO_ZP_PTR)
;;      (JSR VM_COPY_PTR_TO_PTR2)

;;      (JSR VM_ALLOC_CELL_TO_ZP_PTR)
;;      (JSR VM_COPY_PTR2_TO_PTR)
;;      (JSR VM_FREE_CELL_IN_ZP_PTR)))

;;   (define test-alloc-cell-to-zp-ptr-twicenfree-state-after
;;     (run-code-in-test test-alloc-cell-to-zp-ptr-twicenfree-code))

;;   (check-equal? (memory-list test-alloc-cell-to-zp-ptr-twicenfree-state-after VM_LIST_OF_FREE_CELLS (add1 VM_LIST_OF_FREE_CELLS))
;;                 (list #x02 #xcc)
;;                 "free cell list has cc02 now as head of the list")

;;   (check-equal? (vm-page->strings test-alloc-cell-to-zp-ptr-twicenfree-state-after #xcc)
;;                 (list "page-type:      cell page"
;;                       "previous page:  $00"
;;                       "slots used:     2"
;;                       "next free slot: $0a")
;;                 "page has still 2 slots in use (even though $cc02 was freed)")

;;   (check-equal? (memory-list test-alloc-cell-to-zp-ptr-twicenfree-state-after #xcc02 #xcc03 )
;;                 (list #x00 #x00)
;;                 "since cc02 is now part of the free cell list, it points to the next free cell which is $0000 (none)"))

;; (module+ test #| vm_alloc_cell_to_zp_ptr (twice, then free first on a new page, then allocate again) |#
;;   (define test-alloc-cell-to-zp-ptr-twicenfreenalloc-code
;;     (list
;;      (JSR VM_ALLOC_CELL_TO_ZP_PTR)
;;      (JSR VM_COPY_PTR_TO_PTR2)

;;      (JSR VM_ALLOC_CELL_TO_ZP_PTR)
;;      (JSR VM_COPY_PTR2_TO_PTR)
;;      (JSR VM_FREE_CELL_IN_ZP_PTR)

;;      (JSR VM_ALLOC_CELL_TO_ZP_PTR)))

;;   (define test-alloc-cell-to-zp-ptr-twicenfreenalloc-state-after
;;     (run-code-in-test test-alloc-cell-to-zp-ptr-twicenfreenalloc-code))

;;   (check-equal? (vm-page->strings test-alloc-cell-to-zp-ptr-twicenfreenalloc-state-after #xcc)
;;                 (list "page-type:      cell page"
;;                       "previous page:  $00"
;;                       "slots used:     2"
;;                       "next free slot: $0a"))

;;   (check-equal? (memory-list test-alloc-cell-to-zp-ptr-twicenfreenalloc-state-after VM_LIST_OF_FREE_CELLS (add1 VM_LIST_OF_FREE_CELLS))
;;                 (list #x00) ;; lowbyte is zero => it is initial (high byte is not heeded in that case)
;;                 "free cell list is initial again"))

;; input:  none
;; output: zp_rt = free cell-pair
;;
;; try to reuse root of free tree: use root but make sure to deallocate cell1 of the root (since this might still point to some data)
;; if no free tree available, find page with free cells (VM_FREE_CELL_PAIR_PAGE)
;; if no free cell page is available, allocate a new page and used the first free slot there
;; NOTE: the cell-pair is not initialized (cell0 and/or cell1 may contain old data that needs to be overwritten!)
(define VM_ALLOC_CELL_PAIR_PTR_TO_RT
  (list
   (label VM_ALLOC_CELL_PAIR_PTR_TO_RT)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1) ;; get highbyte (page) from ptr to cell-pair
          (BNE REUSE_CELL_PAIR__VM_ALLOC_CELL_PAIR_PTR_TO_RT)   ;; if != 0, cell-pair can be reused
          ;; no cell-pair to reuse available => need to allocate a new one

          ;; get a cell-pair on the given page (or allocate a new page)
          (LDA VM_FREE_CELL_PAIR_PAGE)        ;; get the page that has cell-pairs available (can be 0)
          (BEQ ALLOCATE_NEW_PAGE__VM_ALLOC_CELL_PAIR_PTR_TO_RT)
          (JMP VM_ALLOC_CELL_PAIR_ON_PAGE_A_INTO_RT)                        ;; allocate a new cell on that page
   (label ALLOCATE_NEW_PAGE__VM_ALLOC_CELL_PAIR_PTR_TO_RT)
          (JMP ALLOC_NEW_PAGE_PREFIX__VM_ALLOC_CELL_PAIR_ON_PAGE_A_INTO_RT) ;; allocate new page and then a new cell on that page

   (label REUSE_CELL_PAIR__VM_ALLOC_CELL_PAIR_PTR_TO_RT)
          ;; put root of free tree into zp_rt (and copy in TEMP_PTR of this function)
          (STA ZP_RT+1)
          (STA TEMP_PTR__VM_ALLOC_CELL_PAIR_PTR_TO_RT+1)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
          (STA ZP_RT)
          (STA TEMP_PTR__VM_ALLOC_CELL_PAIR_PTR_TO_RT)

          ;; set new tree root for free tree to original cell0
          (LDY !$00)
          (LDA (ZP_RT),y)
          (BEQ CELL0_IS_NO_PTR__VM_ALLOC_CELL_PAIR_PTR_TO_RT) ;; is zero => completely empty
          (AND !$03)
          (CMP !$03)
          (BEQ CELL0_IS_NO_PTR__VM_ALLOC_CELL_PAIR_PTR_TO_RT) ;; is no ptr

          ;; ;; cell0 is a cell-ptr or cell-pair-ptr
          ;; (LSR)
          ;; (BCC CELL0_IS_CELL_PTR__VM_ALLOC_CELL_PAIR_PTR_TO_RT) ;; <- this cannot happen! cell0 is freed before entering it into the tree

          ;; cell0 is a cell-pair-ptr => make new root of free queue 
          (LDA (ZP_RT),y)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
          (INY)
          (LDA (ZP_RT),y)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          (BNE CHECK_CELL1__VM_ALLOC_CELL_PAIR_PTR_TO_RT) ;; since must be !=0, it cannot be on page 0 always branch!

   ;; (label CELL0_IS_CELL_PTR__VM_ALLOC_CELL_PAIR_PTR_TO_RT)
   ;;        ;; cell0 is a cell-ptr => decrement cell0
   ;;        (JSR VM_WRITE_RT_CELL0_TO_RT)
   ;;        (JSR VM_REFCOUNT_DECR_RT)
   ;;        ;; restore original cell-pair-ptr
   ;;        (LDA TEMP_PTR__VM_ALLOC_CELL_PAIR_PTR_TO_RT+1)
   ;;        (STA ZP_RT+1)
   ;;        (LDA TEMP_PTR__VM_ALLOC_CELL_PAIR_PTR_TO_RT)
   ;;        (STA ZP_RT)
   ;;        ;; continue as if cell0 was no ptr, since cell-ptr was handled already

   (label CELL0_IS_NO_PTR__VM_ALLOC_CELL_PAIR_PTR_TO_RT)
          (LDA !$00)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)

   (label CHECK_CELL1__VM_ALLOC_CELL_PAIR_PTR_TO_RT)
          ;; check whether cell1 is non-ptr or ptr
          (LDY !$02)
          (LDA (ZP_RT),y) ;; get low byte
          (BEQ CELL1_IS_NO_PTR__VM_ALLOC_CELL_PAIR_PTR_TO_RT) ;; = 0 means totally empty => no ptr
          (AND !$03)       ;; mask out all but low 2 bits
          (CMP !$03)
          (BEQ CELL1_IS_NO_PTR__VM_ALLOC_CELL_PAIR_PTR_TO_RT) ;; no need to do further deallocation

          ;; write cell1 into zp_ptr and decrement
          (JSR VM_WRITE_RT_CELL1_TO_RT)
          (JSR VM_REFCOUNT_DECR_RT)
          ;; continue as if cell1 is atomic, since it was already handled

          ;; restore zp_ptr to the cell-pair to be reused
          (LDA TEMP_PTR__VM_ALLOC_CELL_PAIR_PTR_TO_RT+1)
          (STA ZP_RT+1)
          (LDA TEMP_PTR__VM_ALLOC_CELL_PAIR_PTR_TO_RT)
          (STA ZP_RT)

   (label CELL1_IS_NO_PTR__VM_ALLOC_CELL_PAIR_PTR_TO_RT)
          (RTS)

   (label TEMP_PTR__VM_ALLOC_CELL_PAIR_PTR_TO_RT)
          (word $0000)))

(module+ test #| vm-allocate-cell-pair-ptr-to-rt |#
  ;; test case 1: allocate new page, allocate first cell-pair on new page, no existing (reusable) pair available
  (define vm-allocate-cell-pair-ptr-to-rt-1-code
    (list
     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)))

  (define vm-allocate-cell-pair-ptr-to-rt-1-state
    (run-code-in-test vm-allocate-cell-pair-ptr-to-rt-1-code))

  (check-equal? (vm-regt->string vm-allocate-cell-pair-ptr-to-rt-1-state)
                "cell-pair-ptr $cc05")

  (check-equal? (memory-list vm-allocate-cell-pair-ptr-to-rt-1-state VM_FREE_CELL_PAIR_PAGE VM_FREE_CELL_PAIR_PAGE)
                (list #xcc))

  (check-equal? (vm-page->strings vm-allocate-cell-pair-ptr-to-rt-1-state #xcc)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $09"))

  ;; test case 2: allocate another cell-pair on existing page with already allocated slots
  (define vm-allocate-cell-pair-ptr-to-rt-2-code
    (list
     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)
     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)))

  (define vm-allocate-cell-pair-ptr-to-rt-2-state
    (run-code-in-test vm-allocate-cell-pair-ptr-to-rt-2-code))

  (check-equal? (vm-regt->string vm-allocate-cell-pair-ptr-to-rt-2-state)
                "cell-pair-ptr $cc09")

  (check-equal? (memory-list vm-allocate-cell-pair-ptr-to-rt-2-state VM_FREE_CELL_PAIR_PAGE VM_FREE_CELL_PAIR_PAGE)
                (list #xcc))

  (check-equal? (vm-page->strings vm-allocate-cell-pair-ptr-to-rt-2-state #xcc)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $41"))

  ;; test case 3: allocate last cell-pair on existing page
  (define vm-allocate-cell-pair-ptr-to-rt-3-code
    (list
            (LDX !49)
     (label TEST_LOOP__VM_ALLOC_CELL_PAIR_PTR_TO_RT_3_CODE)
            (TXA)
            (PHA)
            (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)
            (PLA)
            (TAX)
            (DEX)
            (BNE TEST_LOOP__VM_ALLOC_CELL_PAIR_PTR_TO_RT_3_CODE)))

  (define vm-allocate-cell-pair-ptr-to-rt-3-state
    (run-code-in-test vm-allocate-cell-pair-ptr-to-rt-3-code))

  (check-equal? (vm-regt->string vm-allocate-cell-pair-ptr-to-rt-3-state)
                "cell-pair-ptr $ccf9")

  (check-equal? (memory-list vm-allocate-cell-pair-ptr-to-rt-3-state VM_FREE_CELL_PAIR_PAGE VM_FREE_CELL_PAIR_PAGE)
                (list #xcc))

  (check-equal? (vm-page->strings vm-allocate-cell-pair-ptr-to-rt-3-state #xcc)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     49"
                      "next free slot: $00"))

  ;; test case 3a: allocate one past last cell-pair on existing page
  (define vm-allocate-cell-pair-ptr-to-rt-3a-code
    (list
            (LDX !50)
     (label TEST_LOOP__VM_ALLOC_CELL_PAIR_PTR_TO_RT_3A_CODE)
            (TXA)
            (PHA)
            (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)
            (PLA)
            (TAX)
            (DEX)
            (BNE TEST_LOOP__VM_ALLOC_CELL_PAIR_PTR_TO_RT_3A_CODE)))

  (define vm-allocate-cell-pair-ptr-to-rt-3a-state
    (run-code-in-test vm-allocate-cell-pair-ptr-to-rt-3a-code))

  (check-equal? (vm-regt->string vm-allocate-cell-pair-ptr-to-rt-3a-state)
                "cell-pair-ptr $cb05")

  (check-equal? (memory-list vm-allocate-cell-pair-ptr-to-rt-3a-state VM_FREE_CELL_PAIR_PAGE VM_FREE_CELL_PAIR_PAGE)
                (list #xcb))

  (check-equal? (vm-page->strings vm-allocate-cell-pair-ptr-to-rt-3a-state #xcc)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     49"
                      "next free slot: $00"))
  (check-equal? (vm-page->strings vm-allocate-cell-pair-ptr-to-rt-3a-state #xcb)
                (list "page-type:      cell-pair page"
                      "previous page:  $cc"
                      "slots used:     1"
                      "next free slot: $09"))

  ;; test case 4: allocate first of pairs stored in the free tree, which is filled with just non ptr atomic cells => no gc
  (define vm-allocate-cell-pair-ptr-to-rt-4-code
    (list
     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)
     ;; copy allocated cell-pair ptr to the tree
     (LDA ZP_RT)
     (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
     (LDA ZP_RT+1)
     (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)

     ;; fill cell0 with zeros, cell1 within int1
     (LDA !$00)
     (STA ZP_RA)
     (STA ZP_RA+1)
     (JSR VM_WRITE_RA_TO_CELL0_RT)
     (JSR VM_WRITE_INT1_TO_RA)
     (JSR VM_WRITE_RA_TO_CELL1_RT)

     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)))

  (define vm-allocate-cell-pair-ptr-to-rt-4-state
    (run-code-in-test vm-allocate-cell-pair-ptr-to-rt-4-code))

  (check-equal? (vm-regt->string vm-allocate-cell-pair-ptr-to-rt-4-state)
                "cell-pair-ptr $cc05"
                "cell pair at cc05 is reused (was head of tree)")
  (check-equal? (vm-cell-pair-free-tree->string vm-allocate-cell-pair-ptr-to-rt-4-state)
                "root is initial")

  ;; test case 4: allocate first of pairs stored in the free tree, which has its slot1 filled with ptr that needs to be gc'ed
  (define vm-allocate-cell-pair-ptr-to-rt-5-code
    (list
     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)
     (JSR VM_WRITE_INT1_TO_RA)
     (JSR VM_WRITE_RA_TO_CELL0_RT)
     (JSR VM_WRITE_RA_TO_CELL1_RT)                      ;; cc05: 03 01 03 01
     (JSR VM_REFCOUNT_INCR_RT__CELL_PAIR_PTR)           ;; cc01 = 1
     (JSR VM_CP_RT_TO_RA)                               ;; RA = cc05

     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)                 ;; RT = cc09

     ;; copy allocated cell-pair ptr to the tree
     (LDA ZP_RT)
     (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
     (LDA ZP_RT+1)
     (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)

     ;; fill cell0 with zeros, cell1 cell-pair-ptr (with refcount = 1)
     (JSR VM_WRITE_RA_TO_CELL1_RT)                     ;; cc09: 05 cc 00 00

     (LDA !$00)
     (STA ZP_RA)
     (STA ZP_RA+1)
     (JSR VM_WRITE_RA_TO_CELL0_RT)

     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)))

  (define vm-allocate-cell-pair-ptr-to-rt-5-state
    (run-code-in-test vm-allocate-cell-pair-ptr-to-rt-5-code))

  (check-equal? (vm-regt->string vm-allocate-cell-pair-ptr-to-rt-5-state)
                "cell-pair-ptr $cc09"
                "cellp-pair at cc09 is reused (was at head of tree)")
  (check-equal? (memory-list vm-allocate-cell-pair-ptr-to-rt-5-state #xcc01 #xcc01)
                (list #x00)
                "refcount of cc05 (is at cc01) is zero again!")
  (check-equal? (vm-cell-pair-free-tree->string vm-allocate-cell-pair-ptr-to-rt-5-state)
                "cell-pair $cc05 -> [ empty . cell-int $0001 ]")
  (check-equal? (memory-list vm-allocate-cell-pair-ptr-to-rt-5-state #xcc05 #xcc08)
                (list #x00 #x00 #x03 #x01)
                "cell-pair at cc05 holds 00 in cell0 (no further elements in queue) and int 1 in cell1")
  (check-equal? (vm-page->strings vm-allocate-cell-pair-ptr-to-rt-5-state #xcc)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $41")
                "still both are used on the page, one was allocated (reused) from tree, and the other is now head of the tree")
)

;; input cell ptr is in ZP_RT
;; ---
;; put this cell into the free-list (as head)
;; the cell will then hold the previous head of the free-list
;; allocating cells will first reuse this free-list
;; option: keep count (length) of this list to decide when to really free a cell
;;         and add it to the free list on its respective page!
(define VM_FREE_CELL_PTR_IN_RT
  (list
   (label VM_FREE_CELL_PTR_IN_RT)
          ;; check if previous content was a pointer
          (LDY !$00)
          (STY ZP_TEMP+1)

          (LDA (ZP_RT),y)
          (BEQ CELL_IS_NO_PTR__VM_FREE_CELL_PTR_IN_RT)
          (AND !$03)
          (CMP !$03)
          (BEQ CELL_IS_NO_PTR__VM_FREE_CELL_PTR_IN_RT)

          ;; cell is a pointer => save for tail call in temp
          (LDA (ZP_RT),y)
          (STA ZP_TEMP)
          (INY)
          (LDA (ZP_RT),y)
          (STA ZP_TEMP+1)
          (DEY)

          ;; copy previous head of free cells into this cell
   (label CELL_IS_NO_PTR__VM_FREE_CELL_PTR_IN_RT)
          (LDA VM_LIST_OF_FREE_CELLS)
          (STA (ZP_RT),y)
          (LDA VM_LIST_OF_FREE_CELLS+1)
          (INY)
          (STA (ZP_RT),y)

          ;; write this cell as new head into the list
          (LDA ZP_RT)
          (STA VM_LIST_OF_FREE_CELLS)
          (LDA ZP_RT+1)
          (STA VM_LIST_OF_FREE_CELLS+1)

          (LDA ZP_TEMP+1)
          (BEQ DONE__VM_FREE_CELL_PTR_IN_RT) ;; there wasn't any further pointer => done with free
          ;; fill rt for tail calling
          (STA ZP_RT+1)
          (LDA ZP_TEMP)
          (STA ZP_RT)
          (JMP VM_REFCOUNT_DECR_RT) ;; tail call if cell did hold a reference

   (label DONE__VM_FREE_CELL_PTR_IN_RT)
          (RTS)))

(module+ test #| vm-free-cell-ptr-in-rt |#
  (define vm-free-cell-ptr-in-rt-tailcall-code
    (list
     (JSR VM_ALLOC_CELL_PTR_TO_RT)
     (JSR VM_REFCOUNT_INCR_RT__CELL_PTR)
     (JSR VM_CP_RT_TO_RA)
     (JSR VM_ALLOC_CELL_PTR_TO_RT)
     (JSR VM_WRITE_RA_TO_CELL0_RT)
     (JSR VM_FREE_CELL_PTR_IN_RT)))

  (define vm-free-cell-ptr-in-rt-tailcall-state
    (run-code-in-test vm-free-cell-ptr-in-rt-tailcall-code))

  (check-equal? (memory-list vm-free-cell-ptr-in-rt-tailcall-state VM_LIST_OF_FREE_CELLS (add1 VM_LIST_OF_FREE_CELLS))
                (list #x02 #xcc)
                "cc02 is new head of the free list")
  (check-equal? (memory-list vm-free-cell-ptr-in-rt-tailcall-state #xcc02 #xcc03)
                (list #x08 #xcc)
                "cc02, which was freed, is referencing cc08 as next in the free list")
  (check-equal? (memory-list vm-free-cell-ptr-in-rt-tailcall-state #xcc08 #xcc09)
                (list #x00 #x00)
                "cc08, which was freed, is the tail of the free list")
  (check-equal? (vm-page->strings vm-free-cell-ptr-in-rt-tailcall-state #xcc)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $0a")
                "two slots still allocated on page, they are however on the free list to be reused")

  (define vm-free-cell-ptr-in-rt-code
    (list
     (JSR VM_ALLOC_CELL_PTR_TO_RT)
     (JSR VM_FREE_CELL_PTR_IN_RT)))

  (define vm-free-cell-ptr-in-rt-state
    (run-code-in-test vm-free-cell-ptr-in-rt-code))

  (check-equal? (memory-list vm-free-cell-ptr-in-rt-state VM_LIST_OF_FREE_CELLS (add1 VM_LIST_OF_FREE_CELLS))
                (list #x02 #xcc)
                "allocated cell is freed by adding it as head to the list of free cells")

  (check-equal? (memory-list vm-free-cell-ptr-in-rt-state #xcc02 #xcc03)
                (list #x00 #x00)
                "the cell is set to 00 00, marking the end of the list of free cells")

  (check-equal? (vm-page->strings vm-free-cell-ptr-in-rt-state #xcc)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $08")
                "page has still 1 slot in use (it was freed, but is no in free list, not completely unallocated)")

  (define vm-free-cell-ptr-in-rt-realloc-code
    (list
     (JSR VM_ALLOC_CELL_PTR_TO_RT)
     (JSR VM_FREE_CELL_PTR_IN_RT)
     (JSR VM_ALLOC_CELL_PTR_TO_RT)))

  (define vm-free-cell-ptr-in-rt-realloc-state
    (run-code-in-test vm-free-cell-ptr-in-rt-realloc-code))

  (check-equal? (memory-list vm-free-cell-ptr-in-rt-realloc-state VM_LIST_OF_FREE_CELLS VM_LIST_OF_FREE_CELLS)
                (list #x00)
                "list of free cells is empty again")

  (check-equal? (memory-list vm-free-cell-ptr-in-rt-realloc-state ZP_RT (add1 ZP_RT))
                (list #x02 #xcc))

  (check-equal? (vm-page->strings vm-free-cell-ptr-in-rt-realloc-state #xcc)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $08")
                "page has 1 slot in use")

  (define vm-free-cell-ptr-in-rt-2xfree-code
    (list
     (JSR VM_ALLOC_CELL_PTR_TO_RT)
     (JSR VM_CP_RT_TO_RA)
     (JSR VM_ALLOC_CELL_PTR_TO_RT)
     (JSR VM_FREE_CELL_PTR_IN_RT)       ;; free cc08
     (JSR VM_CP_RA_TO_RT)
     (JSR VM_FREE_CELL_PTR_IN_RT)))     ;; then free cc02

  (define vm-free-cell-ptr-in-rt-2xfree-state
    (run-code-in-test vm-free-cell-ptr-in-rt-2xfree-code))

  (check-equal? (memory-list vm-free-cell-ptr-in-rt-2xfree-state VM_LIST_OF_FREE_CELLS (add1 VM_LIST_OF_FREE_CELLS))
                (list #x02 #xcc)
                "last allocated cell is freed by adding it as head to the list of free cells")

  (check-equal? (memory-list vm-free-cell-ptr-in-rt-2xfree-state #xcc02 #xcc03)
                (list #x08 #xcc)
                "the cell is set to $cc08, the next element in the free list")

  (check-equal? (memory-list vm-free-cell-ptr-in-rt-2xfree-state #xcc08 #xcc08)
                (list #x00)
                "the cell is set to 00, marking the end of the list of free cells")

  (check-equal? (vm-page->strings vm-free-cell-ptr-in-rt-2xfree-state #xcc)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $0a")
                "page has still 2 slot in use (it was freed, but is no in free list, not completely unallocated)"))

;; input:  cell-pair ptr is in ZP_RT
;; uses: ZP_TEMP, ZP_TEMP+1
;; -----
;; put the cell-pair itself as new root to the free-tree
;; put the old free-tree into cell1
;; tail call free on old cell1 in this cell-pair (if not atomic, if atomic no tail call)
;; result: this cell-pair is the new root of the free-tree for cell-pairs with:
;;              cell0 = old free tree root, cell1 = non-freed (yet) original cell
(define VM_FREE_CELL_PAIR_PTR_IN_RT
  (list
   (label VM_FREE_CELL_PAIR_PTR_IN_RT)

          (LDY !$00)
          (STY ZP_TEMP+1) ;; indicator of a ptr to free is set to 0 => currently no additional ptr marked for free

          ;; check cell0
          (LDA (ZP_RT),y) ;; LOWBYTE OF FIRST cell0
          (BEQ CELL_0_NO_PTR__VM_FREE_CELL_PAIR_PTR_IN_RT)
          (AND !$03)
          (CMP !$03)
          (BEQ CELL_0_NO_PTR__VM_FREE_CELL_PAIR_PTR_IN_RT)
          ;; make sure to call free on cell0 (could be any type of cell)
          ;; remember ZP_PTR

          ;; store cell0 into ZP_TEMP (for later tail call of free)
          (LDA (ZP_RT),y)
          (STA ZP_TEMP)
          (INY)
          (LDA (ZP_RT),y)
          (STA ZP_TEMP+1)

   (label CELL_0_NO_PTR__VM_FREE_CELL_PAIR_PTR_IN_RT)
          ;; cell0 is no ptr and can thus be discarded (directly)

          ;; simply add this cell-pair as head to free tree
          ;; set cell0 to point to old root
          (LDY !$01)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          (STA (ZP_RT),y)
          (DEY)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
          (STA (ZP_RT),y)
          ;; set new root to point to cell-pair
          (LDA ZP_RT+1)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          (LDA ZP_RT)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)

          ;; write original cell0 -> zp_rt
          (LDA ZP_TEMP+1)
          (BEQ DONE__VM_FREE_CELL_PAIR_PTR_IN_RT)
          ;; otherwise zp_temp was used to store a pointer that needs to be decremented
          (STA ZP_RT+1)
          (LDA ZP_TEMP)
          (STA ZP_RT)

          (JMP VM_REFCOUNT_DECR_RT) ;; chain call

   (label DONE__VM_FREE_CELL_PAIR_PTR_IN_RT)
          (RTS)))

(module+ test #| vm-free-cell-pair-ptr-in-rt |#
  (define vm-free-cell-pair-ptr-in-rt-code
    (list
     (LDA !$00)
     (STA ZP_RT)
     (STA ZP_RT+1)
     (JSR VM_FREE_CELL_PAIR_PTR_IN_RT)))

  (define vm-free-cell-pair-ptr-in-rt-state
    (run-code-in-test vm-free-cell-pair-ptr-in-rt-code))

  (check-equal? (vm-regt->string vm-free-cell-pair-ptr-in-rt-state)
                "empty")

  (define vm-free-cell-pair-ptr-in-rt-2-code
    (list
     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)
     (JSR VM_FREE_CELL_PAIR_PTR_IN_RT)))

  (define vm-free-cell-pair-ptr-in-rt-2-state
    (run-code-in-test vm-free-cell-pair-ptr-in-rt-2-code))

  (check-equal? (vm-page->strings vm-free-cell-pair-ptr-in-rt-2-state #xcc)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $09")
                "page has still 1 slot in use (it was freed, but is now in free list, not completely unallocated)")
  (check-equal? (vm-cell-pair-free-tree->string vm-free-cell-pair-ptr-in-rt-2-state)
                "cell-pair $cc05 -> [ empty . empty ]")

  (define vm-free-cell-pair-ptr-in-rt-3-code
    (list
     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)
     (JSR VM_WRITE_INT1_TO_RA)
     (JSR VM_WRITE_RA_TO_CELL1_RT)
     (JSR VM_WRITE_INT0_TO_RA)
     (JSR VM_WRITE_RA_TO_CELL0_RT)
     (JSR VM_REFCOUNT_INCR_RT)
     (JSR VM_CP_RT_TO_RA)

     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)
     (JSR VM_WRITE_RA_TO_CELL0_RT)
     (JSR VM_WRITE_NIL_TO_RA)
     (JSR VM_WRITE_RA_TO_CELL1_RT)

     ;; rt = ( -+ . NIL )                cell-pair @ cc09
     ;;         |
     ;;         +--> ( int0 . int1 )     cell-pair @ cc05

     (JSR VM_FREE_CELL_PAIR_PTR_IN_RT)))

  (define vm-free-cell-pair-ptr-in-rt-3-state
    (run-code-in-test vm-free-cell-pair-ptr-in-rt-3-code))

  (check-equal? (vm-page->strings vm-free-cell-pair-ptr-in-rt-3-state #xcc)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $41")
                "page has still 2 slot in use (it was freed, but is now in free list, not completely unallocated)")
  (check-equal? (vm-cell-pair-free-tree->string vm-free-cell-pair-ptr-in-rt-3-state)
                "cell-pair $cc05 -> [ cell-pair-ptr $cc09 . cell-int $0001 ]")
  (check-equal? (vm-deref-cell-pair-w->string vm-free-cell-pair-ptr-in-rt-3-state #xcc09)
                "(empty . cell-pair-ptr NIL)")

  (define vm-free-cell-pair-ptr-in-rt-4-code
    (list
     (JSR VM_ALLOC_CELL_PTR_TO_RT)
     (JSR VM_REFCOUNT_INCR_RT)
     (JSR VM_WRITE_INTm1_TO_RA)
     (JSR VM_WRITE_RA_TO_CELL0_RT)
     (JSR VM_CP_RT_TO_RA)

     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)
     (JSR VM_WRITE_RA_TO_CELL0_RT)
     (JSR VM_WRITE_INT1_TO_RA)
     (JSR VM_WRITE_RA_TO_CELL1_RT)

     ;; rt = ( -+ . int1 )               cell-pair @ cb05
     ;;         |
     ;;         +--> ( int-1 )           cell      @ cc02

     (JSR VM_FREE_CELL_PAIR_PTR_IN_RT)))

  (define vm-free-cell-pair-ptr-in-rt-4-state
    (run-code-in-test vm-free-cell-pair-ptr-in-rt-4-code))

  (check-equal? (vm-page->strings vm-free-cell-pair-ptr-in-rt-4-state #xcc)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $08")
                "page has still 1 slot in use (it was freed, but is now in free list, not completely unallocated)")
  (check-equal? (vm-page->strings vm-free-cell-pair-ptr-in-rt-4-state #xcb)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $09")
                "page has still 1 slot in use (it was freed, but is now in free list, not completely unallocated)")
  (check-equal? (vm-cell-pair-free-tree->string vm-free-cell-pair-ptr-in-rt-4-state)
                "cell-pair $cb05 -> [ empty . cell-int $0001 ]")
  (check-equal? (vm-deref-cell-w->string vm-free-cell-pair-ptr-in-rt-4-state VM_LIST_OF_FREE_CELLS)
                "cell-ptr $cc02")
  (check-equal? (vm-deref-cell-w->string vm-free-cell-pair-ptr-in-rt-4-state #xcc02)
                "empty"))

;; add the given cell-pair (in zp_ptr) to the free list of cell-pairs on its page
;; input:  zp_ptr = pointer to cell-pair that is added to the free list on its page
;; output: reduces the number of used slots in byte 0
;;         next free slot of this page is the given cell-pair
(define VM_ADD_CELL_PAIR_IN_RT_TO_ON_PAGE_FREE_LIST
  (list
   (label VM_ADD_CELL_PAIR_IN_RT_TO_ON_PAGE_FREE_LIST)
          (LDX ZP_RT+1)
          (STX DEC_CMD__VM_ADD_CELL_PAIR_IN_RT_TO_ON_PAGE_FREE_LIST+2) ;; set page for dec command
          (LDA VM_FIRST_FREE_SLOT_ON_PAGE,x) ;; old first free on page
          (LDY !$00)
          (STA (ZP_RT),y) ;; set old free to next free on this very cell
          (LDA ZP_RT) ;; load idx within page
          (STA VM_FIRST_FREE_SLOT_ON_PAGE,x) ;; set this cell as new first free cell on page

          ;; clear refcount, too (should have been done already)
          (LSR)
          (LSR)
          (TAY);; y now pointer to refcount byte
          (LDA !$00)
          (STA ZP_RT) ;; modify pointer such that zp_ptr points to beginning of page
          (STA (ZP_RT),y) ;; clear refcount byte, too

   (label DEC_CMD__VM_ADD_CELL_PAIR_IN_RT_TO_ON_PAGE_FREE_LIST)
          (DEC $c000) ;; $c0 is overwritten by actual page
          (RTS)))

(module+ test #| vm-add-cell-pair-in-rt-to-on-page-free-list |#
  (define vm-add-cell-pair-in-rt-to-on-page-free-list-code
    (list
     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)
     (JSR VM_FREE_CELL_PAIR_PTR_IN_RT)
     (JSR VM_ADD_CELL_PAIR_IN_RT_TO_ON_PAGE_FREE_LIST)))

  (define vm-add-cell-pair-in-rt-to-on-page-free-list-state
    (run-code-in-test vm-add-cell-pair-in-rt-to-on-page-free-list-code))

  (check-equal? (vm-page->strings vm-add-cell-pair-in-rt-to-on-page-free-list-state #xcc)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     0"
                      "next free slot: $05")
                "again all slots are available on that page"))

(module+ test #| use case: allocate, free, reallocate small list of cell-pairs |#
  (define use-case-2-a-code
    (list
     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)                     ;; rt = freshly allocated cell (cc05)
     (JSR VM_REFCOUNT_INCR_RT__CELL_PAIR_PTR)               ;; ref(rt) ++ (=1)
     ;; set cdr to nil     
     (JSR VM_WRITE_NIL_TO_RA)
     (JSR VM_WRITE_RA_TO_CELL1_RT)                          ;; (cdr rt) := nil
     ;; set car to int 0
     (JSR VM_WRITE_INT1_TO_RA)
     (JSR VM_WRITE_RA_TO_CELL0_RT)                          ;; (car rt) := int0

     (JSR VM_CP_RT_TO_RA)                                   ;; ra := rt

     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)                     ;; rt = freshly allocated cell (cc09)
     (JSR VM_REFCOUNT_INCR_RT__CELL_PAIR_PTR)               ;; ref(rt) ++ (=1)

     ;; set cdr 
     (JSR VM_WRITE_RA_TO_CELL1_RT)                          ;; (cdr rt) := ra 
     ;; set car to int0
     (JSR VM_WRITE_INT0_TO_RA)
     (JSR VM_WRITE_RA_TO_CELL0_RT)                          ;; (car rt) := int0

     ;; now:
     ;;   rt[cc09|1] (int0 . ->[cc05|1](int0 . nil))
     ;; notation:
     ;;   [<mem-location>|<ref-count>]
     ;;   (<car-cell> . <cdr-cell>)
     ;;   intX, nil :: atomic value cells
     ;;   -> :: cell-ptr
     ))

  (define use-case-2-a-state-after
    (run-code-in-test use-case-2-a-code))

  (check-equal? (vm-deref-cell-pair-w->string use-case-2-a-state-after #xcc09)
                "(cell-int $0000 . cell-pair-ptr $cc05)")
  (check-equal? (vm-deref-cell-pair-w->string use-case-2-a-state-after #xcc05)
                "(cell-int $0001 . cell-pair-ptr NIL)")
  (check-equal? (vm-regt->string use-case-2-a-state-after)
                "cell-pair-ptr $cc09")
  (check-equal? (vm-page->strings use-case-2-a-state-after #xcc)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $41"))

  (define use-case-2-b-code
    (append use-case-2-a-code ;; zp_ptr[cc08|1] (int0 . ->[cc04|1](int0 . nil))
            (list
             (JSR VM_REFCOUNT_DECR_RT__CELL_PAIR_PTR)
             ;; now:
             ;;   free_tree -> [cc08|0] (int0 . ->[cc04|1] (int0 . nil))
             )))

  (define use-case-2-b-state-after
    (run-code-in-test use-case-2-b-code))

  (check-equal? (vm-cell-pair-free-tree->string use-case-2-b-state-after)
                "cell-pair $cc09 -> [ empty . cell-pair-ptr $cc05 ]")
  (check-equal? (vm-page->strings use-case-2-b-state-after #xcc)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $41"))

  (define use-case-2-c-code
    (append use-case-2-b-code ;; free_tree -> [cd08|0] (int0 . ->[cd04|1] (int0 . nil))
            (list (LDA !$FF) ;; marker for debug, remove when done
                  (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)
                  (JSR VM_REFCOUNT_INCR_RT__CELL_PAIR_PTR)
                  ;; now:
                  ;;   zp_rt = [cd08|1] not initialized
                  ;;   free_tree -> [cd04|0] (int0 . nil)
                  )))

  (define use-case-2-c-state-after
    (run-code-in-test use-case-2-c-code))

  (check-equal? (vm-regt->string use-case-2-c-state-after)
                "cell-pair-ptr $cc09")
  (check-equal? (vm-cell-pair-free-tree->string use-case-2-c-state-after)
                "cell-pair $cc05 -> [ empty . cell-pair-ptr NIL ]")
  (check-equal? (vm-page->strings use-case-2-c-state-after #xcc)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $41")))

;; input:  A = size (needs to include 32 bytes cell-stack + 10 byte (pc, func ptr, old params, old locals, old cell stack base ptr) + 2 * #locals
;;         ZP_CELL_STACK_BASE_PTR
;;         ZP_CELL_STACK_TOS
;;         VM_FREE_CALL_STACK_PAGE
;; output: ZP_CALL_FRAME,
;;         Y/X=low-byte, high-byte
;; info: back linking previous page is automatically done through zp_params_ptr (pointing to the previous page, if so)
(define VM_ALLOC_CALL_FRAME
  (list
   ;; ----------------------------------------
   (label VM_ALLOC_CALL_FRAME)
          (STA ZP_TEMP)
          (LDX VM_FREE_CALL_STACK_PAGE) ;; get the tos page for call stacks
          (BEQ ALLOCATE_NEW_PAGE__VM_ALLOC_CALL_FRAME)

          (LDA ZP_CELL_STACK_TOS) ;; make sure to have Y loaded if page is reused
          (CLC)
          (ADC ZP_CELL_STACK_BASE_PTR)
          (TAY) ;; y = tos + base_ptr
          (CLC)
          (ADC ZP_TEMP)
          (BCC USE_GIVEN_PAGE__VM_ALLOC_CALL_FRAME) ;; still on same page

          ;; if this call frame (size) does not fit on existing page, continue to allocate a new page

   (label ALLOCATE_NEW_PAGE__VM_ALLOC_CALL_FRAME)
          (JSR VM_ALLOC_PAGE__PAGE_UNINIT) ;; A = new page
          ;; set this new page as the TOS for call stack allocation
          (STA VM_FREE_CALL_STACK_PAGE)
          (STA SET_PAGE_TYPE__VM_ALLOC_CALL_FRAME+2)
          (LDY !$18) ;; page type call-frame
   (label SET_PAGE_TYPE__VM_ALLOC_CALL_FRAME)
          (STY $c000) ;; c0 is overwritten

          (TAX)
          (LDY !$01) ;; first free byte is 02, but y is incremented

   (label USE_GIVEN_PAGE__VM_ALLOC_CALL_FRAME)
          (STX ZP_CALL_FRAME+1) ;; x = stack page
          (INY)
          (STY ZP_CALL_FRAME)   ;; new call frame start

          (RTS)))

(module+ test #| vm_alloc_call_frame |#
  (define alloc-call-frame-code
    (list
     (LDA !$80)
     (JSR VM_ALLOC_CALL_FRAME)))

  (define alloc-call-frame-state-after
    (run-code-in-test alloc-call-frame-code))

  (check-equal? (vm-page->strings alloc-call-frame-state-after #xcd)
                '("page-type:      call-frame page"
                  "stack frame:    $cd0a"))

  (define alloc-call-frame-2times-code
    (list
     (LDA !$80)
     (JSR VM_ALLOC_CALL_FRAME)
     (LDA ZP_CALL_FRAME)
     (STA ZP_CELL_STACK_BASE_PTR)
     (LDA !$80) ;; does not fit again in this call frame
     (STA ZP_CELL_STACK_TOS) ;;
     (JSR VM_ALLOC_CALL_FRAME)))

  (define alloc-call-frame-2times-state-after
    (run-code-in-test alloc-call-frame-2times-code))

  (check-equal? (vm-page->strings alloc-call-frame-2times-state-after #xcc)
                '("page-type:      call-frame page"
                  "stack frame:    $cc02"))

  (define alloc-call-frame-2times-fitting-code
    (list
     (LDA !$20)
     (JSR VM_ALLOC_CALL_FRAME)
     (LDA ZP_CALL_FRAME)
     (STA ZP_CELL_STACK_BASE_PTR)
     (LDA !$1F) ;; stack used up $20 slots
     (STA ZP_CELL_STACK_TOS)
     (LDA !$20)
     (JSR VM_ALLOC_CALL_FRAME)))

  (define alloc-call-frame-2times-fitting-state-after
    (run-code-in-test alloc-call-frame-2times-fitting-code))

  (check-equal? (vm-page->strings alloc-call-frame-2times-fitting-state-after #xcd)
                '("page-type:      call-frame page"
                  "stack frame:    $cd2a")))

;; TODO: check vm_write_rx_to_paramy

;; input:  ZP_CELL_STACK_TOS
;;         ZP_CELL_STACK_BASE_PTR
;;         ZP_PARAMS_PTR
;; output: parameter @ Y (0) is overwritten
;; TODO: check to gc overwritten
;; (define VM_CELL_STACK_WRITE_TOS_TO_PARAMy
;;   (list
;;    (label VM_CELL_STACK_WRITE_TOS_TO_PARAM_0)
;;           (LDY !$00)

;;    ;; ----------------------------------------
;;    (label VM_CELL_STACK_WRITE_TOS_TO_PARAMy)
;;           (STY ZP_TEMP)
;;           (LDY ZP_CELL_STACK_TOS)
;;           (LDA (ZP_CELL_STACK_BASE_PTR),y)
;;           (TAX)
;;           (DEY)
;;           (LDA (ZP_CELL_STACK_BASE_PTR),y)
;;           (LDY ZP_TEMP)
;;           (STA (ZP_PARAMS_PTR),y)
;;           (TXA)
;;           (INY)
;;           (STA (ZP_PARAMS_PTR),y)
;;           (RTS)))

;; input:  ZP_CELL_STACK_TOS
;;         ZP_CELL_STACK_BASE_PTR
;;         ZP_LOCALS_PTR
;; output: local @ Y (0) is overwritten
;; TODO: check to gc overwritten
;; (define VM_CELL_STACK_WRITE_TOS_TO_LOCALy
;;   (list
;;    (label VM_CELL_STACK_WRITE_TOS_TO_LOCAL_0)
;;           (LDY !$00)

;;    ;; ----------------------------------------
;;    (label VM_CELL_STACK_WRITE_TOS_TO_LOCALy)
;;           (STY ZP_TEMP)
;;           (LDY ZP_CELL_STACK_TOS)
;;           (LDA (ZP_CELL_STACK_BASE_PTR),y)
;;           (TAX)
;;           (DEY)
;;           (LDA (ZP_CELL_STACK_BASE_PTR),y)
;;           (LDY ZP_TEMP)
;;           (STA (ZP_LOCALS_PTR),y)
;;           (TXA)
;;           (INY)
;;           (STA (ZP_LOCALS_PTR),y)
;;           (RTS)))


;; input:   A = number of parameters on the stack to be used in this call frame
;;          x = number of locals to allocate on call frame
;;          carry set = NO RT PUSH before save (in effect discard tos = rt, useful if function id/number is in rt for the call)
;;          zp_cell_frame = freshly allocated frame that has min size x*2 + 6 + 32
;;          zp_vm_pc          -> saved into cell frame (for pop)
;;          zp_vm_params_ptr  -> saved into cell frame (for pop)
;;          zp_vm_locals_ptr  -> saved into cell frame (for pop)
;; oputput: zp_vm_params_ptr = pointer into the previous cell-stack
;;          xp_vm_locals_ptr = pointer into the current call frame locals
;;          zp_cell_stack_base_ptr = point into the current call frame (right after locals)
;;          zp_cell_stack_tos = 0
;; NO CHECK IS DONE, zp_vm_pc is not overwritten
(define VM_SAVE_EXEC_STATE_TO_CALL_FRAME
  (list
   (label VM_SAVE_EXEC_STATE_TO_CALL_FRAME)
          (STA ZP_TEMP)
          (BCS NO_RT_PUSH__VM_SAVE_EXEC_STATE_TO_CALL_FRAME)
          (JSR VM_CELL_STACK_PUSH_RT_IF_NONEMPTY)
   (label NO_RT_PUSH__VM_SAVE_EXEC_STATE_TO_CALL_FRAME)
          (ASL ZP_TEMP) ;; # params * 2 (number of bytes)
          (LDA ZP_CELL_STACK_TOS)
          (SEC)
          (SBC ZP_TEMP)
          (STA ZP_TEMP) ;; keep offset to cell-stack-base-ptr for new parameter-ptr

          ;; copy 10 bytes
          (LDY !$09)
   (label LOOP__VM_SAVE_EXEC_STATE_TO_CALL_FRAME)
          (LDA ZP_VM_PC,y)
          (STA (ZP_CALL_FRAME),y)
          (DEY)
          (BPL LOOP__VM_SAVE_EXEC_STATE_TO_CALL_FRAME)

          ;; set new parameter pointer
          (LDA ZP_CELL_STACK_BASE_PTR+1)
          (STA ZP_PARAMS_PTR+1)
          (LDA ZP_CELL_STACK_BASE_PTR)
          (SEC)
          (ADC ZP_TEMP)
          (STA ZP_PARAMS_PTR)

          ;; set new ZP_LOCALS_PTR and ZP_CELL_STACK_BASE_PTR
          (LDA ZP_CALL_FRAME+1)
          (STA ZP_LOCALS_PTR+1)
          (STA ZP_CELL_STACK_BASE_PTR+1)
          (LDA ZP_CALL_FRAME)
          (CLC)
          (ADC !$0a)
          (STA ZP_LOCALS_PTR)

          (TXA)
          (ASL A) ;; # of locals *2 = bytes
          (ADC ZP_LOCALS_PTR)
          (STA ZP_CELL_STACK_BASE_PTR)

          (LDA !$ff)
          (STA ZP_CELL_STACK_TOS) ;; set this one to 0
          (LDA !$00)
          (STA ZP_RT) ;; mark RT as empty

          (RTS)))

(module+ test #| vm_save_exec_state_to_call_frame |#

  ;; before
  ;; cd04 <- ZP_PARAMS_PTR
  ;; cd08 <- ZP_CALL_FRAME
  ;; cd10 <- ZP_LOCALS_PTR
  ;; cd16 <- ZP_CELL_STACK_BASE_PTR
  ;; cd1b <- ZP_CELL_STACK_TOS (base ptr + 05, 3 elements on stack)
  ;; cc05 <- ZP_VM_FUNC_PTR
  ;; cc06 <- ZP_VM_PC

  ;; after
  ;; cd18 <- ZP_PARAMS_PTR (2 params, cd18..cd19, cd1a..cd1b)
  ;; cd1c <- ZP_CALL_FRAME
  ;; cd24 <- ZP_LOCALS_PTR
  ;; cd2a <- ZP_CELL_STACK_BASE_PTR
  ;; cd29 <- ZP_CELL_STACK_TOS ($ff)
  ;; cc05 <- ZP_VM_FUNC_PTR (unchanged)
  ;; cc06 <- ZP_VM_PC (unchanged)

  (define test-save-exec-state-code
    (list
     ;; fill pointers that will be saved
     (LDX !$04)
     (STX ZP_PARAMS_PTR) ;; $04
     (LDX !$08)
     (STX ZP_CALL_FRAME)
     (LDX !$10)
     (STX ZP_LOCALS_PTR) ;; $10
     (LDX !$16)
     (STX ZP_CELL_STACK_BASE_PTR)
     (LDX !$05)
     (STX ZP_CELL_STACK_TOS)
     (STX ZP_VM_FUNC_PTR)
     (INX)
     (STX ZP_VM_PC)     ;; $06

     (LDX ZP_CALL_FRAME+1) ;; $cd
     (STX ZP_PARAMS_PTR+1)
     (STX ZP_LOCALS_PTR+1)
     (STX ZP_CELL_STACK_BASE_PTR+1)     
     (DEX)                      ;; $cc
     (STX ZP_VM_PC+1)
     (STX ZP_VM_FUNC_PTR+1)

     ;; allocate new call frame space that can hold $20 bytes
     (LDA !$20)
     (JSR VM_ALLOC_CALL_FRAME)

     (LDA !$02) ;; two params
     (LDX !$03) ;; three locals
     (CLC)
     (JSR VM_SAVE_EXEC_STATE_TO_CALL_FRAME)
     ))

  (define test-save-exec-state-state-after
    (run-code-in-test test-save-exec-state-code))

  (check-equal? (vm-page->strings test-save-exec-state-state-after #xcd)
                '("page-type:      call-frame page"
                  "stack frame:    $cd1c"))
  (check-equal? (memory-list test-save-exec-state-state-after ZP_VM_FUNC_PTR (add1 ZP_VM_FUNC_PTR))
                (list #x05 #xcc))  ;; not changed (yet)
  (check-equal? (memory-list test-save-exec-state-state-after ZP_PARAMS_PTR (add1 ZP_PARAMS_PTR))
                (list #x18 #xcd))
  (check-equal? (memory-list test-save-exec-state-state-after ZP_LOCALS_PTR (add1 ZP_LOCALS_PTR))
                (list #x26 #xcd))
  (check-equal? (memory-list test-save-exec-state-state-after ZP_CELL_STACK_BASE_PTR (add1 ZP_CELL_STACK_BASE_PTR))
                (list #x2c #xcd))
  (check-equal? (memory-list test-save-exec-state-state-after ZP_CELL_STACK_TOS ZP_CELL_STACK_TOS)
                (list #xff))
  (check-equal? (memory-list test-save-exec-state-state-after ZP_VM_PC (add1 ZP_VM_PC))
                (list #x06 #xcc)))

;; zp_params_ptr := zp_cell_stack_base_ptr + zp_cell_stack_tos
;; if zp_params_ptr is on a different page than zp_call_frame,
;;    free page of zp_call_frame
;;      vm_free_call_stack_page = page of zp_params_ptr
;;      vm_free_slot_for_page,x = ??
;; restore zp_params_ptr
;; restore zp_locals_ptr
;; restore zp_cell_stack_base_ptr
;; restore zp_vm_pc
;; zp_call_frame = zp_locals_ptr - 6
;;
;; input:  zp_cell_stack_tos
;;         zp_call_frame
;;         zp_params_ptr
;;         zp_locals_ptr
;;         zp_cell_stack_base_ptr
;; output: zp_cell_stack_tos + 2 -= # params*2
;;         zp_call_frame
;;         zp_params_ptr
;;         zp_locals_ptr
;;         zp_cell_stack_base_ptr
;;         zp_vm_func_ptr
;;         zp_vm_pc
(define VM_POP_CALL_FRAME
  (list
   (label VM_POP_CALL_FRAME)
          ;; result is in rt (no stack manipulation necessary)

          (LDA ZP_CALL_FRAME+1)         ;; get current page
          (CMP ZP_PARAMS_PTR+1)         ;; compare with old page
          (BEQ SAME_PAGE__VM_POP_CALL_FRAME)

          ;; deallocate current page first
          (JSR VM_FREE_PAGE)
          (LDA ZP_PARAMS_PTR+1)         ;; get old page
          (STA VM_FREE_CALL_STACK_PAGE) ;; store old page as current (free) page

   (label SAME_PAGE__VM_POP_CALL_FRAME)
          ;; start to calculate tos of cell-stack
          (LDA ZP_PARAMS_PTR)
          (STA ZP_CELL_STACK_TOS) ;; needs to subtract base ptr (which is available after the following copy loop

          (LDY !$09) ;; 09..00 offset (10 bytes are copied)

   (label LOOP__VM_POP_CALL_FRAME)
          (LDA (ZP_CALL_FRAME),y)
          (STA ZP_VM_PC,y)
          (DEY)
          (BPL LOOP__VM_POP_CALL_FRAME)

          ;; finish calc of new stack
          (LDA ZP_CELL_STACK_TOS)
          (SEC)
          (SBC ZP_CELL_STACK_BASE_PTR)
          (STA ZP_CELL_STACK_TOS)
          (DEC ZP_CELL_STACK_TOS) ;; point to the tagged lowbyte

          ;; set ZP_CALL_FRAME
          (LDA ZP_LOCALS_PTR+1)
          (STA ZP_CALL_FRAME+1)
          (LDA ZP_LOCALS_PTR)
          (SEC)
          (SBC !$0a)
          (STA ZP_CALL_FRAME)

          (RTS)))

(module+ test #| vm_pop_call_frame |#
  ;; before
  ;; cd18 <- ZP_PARAMS_PTR (2 params, cd18..cd19, cd1a..cd1b)
  ;; cd1c <- ZP_CALL_FRAME
  ;; cd24 <- ZP_LOCALS_PTR
  ;; cd2a <- ZP_CELL_STACK_BASE_PTR
  ;; cd29 <- ZP_CELL_STACK_TOS ($ff)
  ;; cc04 <- ZP_VM_PC

  ;; after
  ;; cd04 <- ZP_PARAMS_PTR
  ;; cd08 <- ZP_CALL_FRAME
  ;; cd10 <- ZP_LOCALS_PTR
  ;; cd16 <- ZP_CELL_STACK_BASE_PTR
  ;; cd19 <- ZP_CELL_STACK_TOS (base ptr + 03, 2 elements on stack, 1 remaining + 1 result)
  ;; cc06 <- ZP_VM_PC

  (define test-pop-call-frame-code
    (list
     ;; fill pointers that will be saved
     (LDX !$04)
     (STX ZP_PARAMS_PTR) ;; $04
     (LDX !$08)
     (STX ZP_CALL_FRAME)
     (LDX !$10)
     (STX ZP_LOCALS_PTR) ;; $10
     (LDX !$16)
     (STX ZP_CELL_STACK_BASE_PTR)
     (LDX !$ff)  ;; (2 items on the stack + 1 in RT)
     (STX ZP_CELL_STACK_TOS)
     (LDX !$04)
     (STX ZP_VM_PC)     ;; $04
     (DEX)
     (STX ZP_VM_FUNC_PTR)

     (LDX ZP_CALL_FRAME+1) ;; $cd
     (STX ZP_PARAMS_PTR+1)
     (STX ZP_LOCALS_PTR+1)
     (STX ZP_CELL_STACK_BASE_PTR+1)
     (STX ZP_VM_FUNC_PTR+1)
     (DEX)                      ;; $cc
     (STX ZP_VM_PC+1)

     (JSR VM_WRITE_INT1_TO_RT)         ;; 1
     (JSR VM_CELL_STACK_PUSH_INT_0_R)  ;; 0 <- parameter 0
     (JSR VM_CELL_STACK_PUSH_INT_m1_R) ;; -1 <- parameter 1

     ;; allocate new call frame space that can hold $20 bytes
     (LDA !$20)
     (JSR VM_ALLOC_CALL_FRAME)

     (LDA !$02) ;; pass two params, (push rt, then two topmost)
     (LDX !$03) ;; three locals
     (CLC)      ;; push RT before saving
     (JSR VM_SAVE_EXEC_STATE_TO_CALL_FRAME)

     (JSR VM_WRITE_INT1_TO_RT)
     (INC ZP_VM_PC)
     (INC ZP_VM_FUNC_PTR)

     (JSR VM_POP_CALL_FRAME) ;; should restore to 1 item on the stack, RT = returned value (int1)
     ))

  (define test-pop-call-frame-state-after
    (run-code-in-test test-pop-call-frame-code))

  (check-equal? (memory-list test-pop-call-frame-state-after ZP_PARAMS_PTR (add1 ZP_PARAMS_PTR))
                (list #x04 #xcd))
  (check-equal? (memory-list test-pop-call-frame-state-after ZP_CALL_FRAME (add1 ZP_CALL_FRAME))
                (list #x06 #xcd))
  (check-equal? (memory-list test-pop-call-frame-state-after ZP_LOCALS_PTR (add1 ZP_LOCALS_PTR))
                (list #x10 #xcd))
  (check-equal? (memory-list test-pop-call-frame-state-after ZP_CELL_STACK_BASE_PTR (add1 ZP_CELL_STACK_BASE_PTR))
                (list #x16 #xcd))
  (check-equal? (memory-list test-pop-call-frame-state-after ZP_VM_FUNC_PTR (add1 ZP_VM_FUNC_PTR))
                (list #x03 #xcd))
  (check-equal? (memory-list test-pop-call-frame-state-after ZP_VM_PC (add1 ZP_VM_PC))
                (list #x04 #xcc))
  (check-equal? (take (vm-stack->strings test-pop-call-frame-state-after) 2)
                '("stack holds 2 items" ;; 3 before, 2 were parameters => 1 old + 1 result in RT
                  "cell-int $0001  (rt)"
                  ;; don't show the uninitialized second value on the stack (see take 2)
                  )))

;; TODO: active m1 pages code and tests

;; ----------------------------------------
;; page type slot w/ different sizes (refcount @ ptr-1) x cells
;; math: first entry @FIRST_REF_COUNT_OFFSET__VM_ALLOC_PAGE_FOR_M1_SLOTS + 1, refcount @ -1, next slot += INC_TO_NEXT_SLOT__VM_ALLOC_PAGE_FOR_M1_SLOTS, slot-size = INC_TO_NEXT_SLOT__VM_ALLOC_PAGE_FOR_M1_SLOTS -1
;; input : X = profile offset (0, 2, 4 ...)
;; uses  : ZP_PTR2
;; output:
;; (define VM_ALLOC_PAGE_FOR_M1_SLOTS
;;   (list
;;    (label VM_ALLOC_PAGE_FOR_M1_SLOTS)
;;           (STX SEL_PROFILE__VM_ALLOC_PAGE_FOR_M1_SLOTS)      ;; save profile index in local var

;;           (JSR VM_ALLOC_PAGE__PAGE_UNINIT)
;;           (STA ZP_PTR2+1)
;;           (LDA !$00)
;;           (STA ZP_PTR2)

;;           (LDY !$00)
;;           (LDX SEL_PROFILE__VM_ALLOC_PAGE_FOR_M1_SLOTS) ;; profile = 0..3
;;           (TXA)
;;           (ORA !$10)
;;           (STA (ZP_PTR2),y) ;; set page type in byte 0 to b0001 <profile>

;;           (LDA VM_FREE_M1_PAGE_P0,x) ;; current free page
;;           (INY)
;;           (STA (ZP_PTR2),y)          ;; store previous page

;;           (LDA ZP_PTR2+1)
;;           (STA VM_FREE_M1_PAGE_P0,x) ;; set page with free slots to this allocated page

;;           (LDA !$00)
;;           (INY)
;;           (STA (ZP_PTR2),y)          ;; store number of slots used

;;           (LDY FIRST_REF_COUNT_OFFSET__VM_ALLOC_PAGE_FOR_M1_SLOTS,x) ;; y = refcount field for first slot
;;           (INY)
;;           (TYA)
;;           (LDX ZP_PTR2+1)
;;           (STA VM_FIRST_FREE_SLOT_ON_PAGE,x)                    ;; set first free slot, here x = page
;;           (DEY)
;;           (LDX SEL_PROFILE__VM_ALLOC_PAGE_FOR_M1_SLOTS) ;; profile = 0..3
;;           (LDA !$00)

;;           ;; loop to initialize refcounts of each slot to 0-
;;           (label REF_COUNT_LOOP__VM_ALLOC_PAGE_FOR_M1_SLOTS)
;;           (STA (ZP_PTR2),y) ;; refcount = 0
;;           (TYA)
;;           (CLC)
;;           (ADC INC_TO_NEXT_SLOT__VM_ALLOC_PAGE_FOR_M1_SLOTS,x) ;; calc next refcount field offset
;;           (BCS END_REF_COUNT_LOOP__VM_ALLOC_PAGE_FOR_M1_SLOTS)
;;           (TAY)
;;           (ADC !$01)
;;           (LDA !$00)
;;           (BCC REF_COUNT_LOOP__VM_ALLOC_PAGE_FOR_M1_SLOTS) ;; still on this page?

;;    (label END_REF_COUNT_LOOP__VM_ALLOC_PAGE_FOR_M1_SLOTS)
;;           ;; loop to write free slot list
;;           (LDY FIRST_REF_COUNT_OFFSET__VM_ALLOC_PAGE_FOR_M1_SLOTS,x)
;;           (INY)  ;; first slot  (refcount field offset + 1)
;;           (TYA)
;;    (label WRITE_FREE_LIST__VM_ALLOC_PAGE_FOR_M1_SLOTS)
;;           (CLC)
;;           (ADC INC_TO_NEXT_SLOT__VM_ALLOC_PAGE_FOR_M1_SLOTS,x)
;;           (BCS ALMOST_DONE__VM_ALLOC_PAGE_FOR_M1_SLOTS) ;; no longer on the same page => almost done
;;           (STA (ZP_PTR2),y) ;; offset of next free cell == y for next write
;;           (TAY)
;;           (BCC WRITE_FREE_LIST__VM_ALLOC_PAGE_FOR_M1_SLOTS) ;; carry must be clear => always jump

;;    (label ALMOST_DONE__VM_ALLOC_PAGE_FOR_M1_SLOTS)
;;           (LDA !$00)
;;           (STA (ZP_PTR2),y) ;; last offset to next free slot is 00 = no next free slot!

;;           (RTS)

;;    (label SEL_PROFILE__VM_ALLOC_PAGE_FOR_M1_SLOTS)
;;           (byte $00) ;; local var

;;    (label FIRST_REF_COUNT_OFFSET__VM_ALLOC_PAGE_FOR_M1_SLOTS)
;;           (byte $03) ;; first ref count is 03, add 12 to get to next slot, slot size $11 (17), contains 14 slots
;;           (byte $0f) ;; first ref count is 0f, add 1e to get to next slot, slot size $1d (29), contains 8 slots
;;           (byte $05) ;; first ref count is 05, add 32 to get to next slot, slot-size $31 (49), contains 5 slots
;;           (byte $03) ;; first ref count is 03, add 54 to get to next slot, slot-size $53 (83), contains 3 slots

;;    (label INC_TO_NEXT_SLOT__VM_ALLOC_PAGE_FOR_M1_SLOTS)
;;           (byte $12) ;; add 12 to get to next slot, slot size $11 (17), contains 14 slots
;;           (byte $1e) ;; add 1e to get to next slot, slot size $1d (29), contains 8 slots
;;           (byte $32) ;; add 32 to get to next slot, slot-size $31 (49), contains 5 slots
;;           (byte $54) ;; add 54 to get to next slot, slot-size $53 (83), contains 3 slots

;;           ))

;; (module+ test #| vm_alloc_m1_page |#
;;   (define test-alloc-m1-01-code
;;     (list
;;      ;; fill page with $ff
;;             (LDA !$FF)
;;             (LDX !$00)
;;      (label LOOP__TEST_ALLOC_M1_01_CODE)
;;             (DEX)
;;             (STA $cc00,x)
;;             (BNE LOOP__TEST_ALLOC_M1_01_CODE)

;;             ;; now allocate the page
;;             (LDX !$00) ;; do it explicitly
;;             (JSR VM_ALLOC_PAGE_FOR_M1_SLOTS)))

;;   (define test-alloc-m1-01-state-after
;;     (run-code-in-test test-alloc-m1-01-code))

;;   (check-equal? (vm-page->strings test-alloc-m1-01-state-after #xcc)
;;                 '("page-type:      m1 page p0"
;;                   "previous page:  $00"
;;                   "slots used:     0"
;;                   "next free slot: $04"))
;;   (check-equal? (memory-list test-alloc-m1-01-state-after #xcc03 #xcc04)
;;                 (list #x00 #x16)
;;                 "slot0: refcount 0, next free slot at offset $16")
;;   (check-equal? (memory-list test-alloc-m1-01-state-after #xcc15 #xcc16)
;;                 (list #x00 #x28)
;;                 "slot1: refcount 0, next free slot at offset $28")
;;   (check-equal? (memory-list test-alloc-m1-01-state-after #xcc27 #xcc28)
;;                 (list #x00 #x3a)
;;                 "slot2: refcount 0, next free slot at offset $28")
;;   (check-equal? (memory-list test-alloc-m1-01-state-after #xcced #xccee)
;;                 (list #x00 #x00)
;;                 "slot13: refcount 0, next free slot at offset $00 = no next")

;;   (define test-alloc-m1-02-code
;;     (list
;;      ;; fill page with $ff
;;             (LDA !$FF)
;;             (LDX !$00)
;;      (label LOOP__TEST_ALLOC_M1_02_CODE)
;;             (DEX)
;;             (STA $cc00,x)
;;             (BNE LOOP__TEST_ALLOC_M1_02_CODE)

;;             ;; now allocate the page
;;             (LDX !$01) ;; do it explicitly
;;             (JSR VM_ALLOC_PAGE_FOR_M1_SLOTS)))

;;   (define test-alloc-m1-02-state-after
;;     (run-code-in-test test-alloc-m1-02-code))

;;   (check-equal? (vm-page->strings test-alloc-m1-02-state-after #xcc)
;;                 '("page-type:      m1 page p1"
;;                   "previous page:  $00"
;;                   "slots used:     0"
;;                   "next free slot: $10"))
;;   (check-equal? (memory-list test-alloc-m1-02-state-after #xcc0f #xcc10)
;;                 (list #x00 #x2e)
;;                 "slot0: refcount 0, next free slot at offset $2c")
;;   (check-equal? (memory-list test-alloc-m1-02-state-after #xcc2d #xcc2e)
;;                 (list #x00 #x4c)
;;                 "slot1: refcount 0, next free slot at offset $4a")
;;   (check-equal? (memory-list test-alloc-m1-02-state-after #xcc4b #xcc4c)
;;                 (list #x00 #x6a)
;;                 "slot2: refcount 0, next free slot at offset $68")
;;   (check-equal? (memory-list test-alloc-m1-02-state-after #xcce1 #xcce2)
;;                 (list #x00 #x00)
;;                 "slot7: refcount 0, next free slot at offset $00 = no next")

;;   (define test-alloc-m1-03-code
;;     (list
;;      ;; fill page with $ff
;;             (LDA !$FF)
;;             (LDX !$00)
;;      (label LOOP__TEST_ALLOC_M1_03_CODE)
;;             (DEX)
;;             (STA $cc00,x)
;;             (BNE LOOP__TEST_ALLOC_M1_03_CODE)

;;             ;; now allocate the page
;;             (LDX !$02) ;; do it explicitly
;;             (JSR VM_ALLOC_PAGE_FOR_M1_SLOTS)))

;;   (define test-alloc-m1-03-state-after
;;     (run-code-in-test test-alloc-m1-03-code))

;;   (check-equal? (vm-page->strings test-alloc-m1-03-state-after #xcc)
;;                 '("page-type:      m1 page p2"
;;                   "previous page:  $00"
;;                   "slots used:     0"
;;                   "next free slot: $06"))
;;   (check-equal? (memory-list test-alloc-m1-03-state-after #xcc05 #xcc06)
;;                 (list #x00 #x38)
;;                 "slot0: refcount 0, next free slot at offset $38")
;;   (check-equal? (memory-list test-alloc-m1-03-state-after #xcc37 #xcc38)
;;                 (list #x00 #x6a)
;;                 "slot1: refcount 0, next free slot at offset $6a")
;;   (check-equal? (memory-list test-alloc-m1-03-state-after #xcc69 #xcc6a)
;;                 (list #x00 #x9c)
;;                 "slot2: refcount 0, next free slot at offset $9c")
;;   (check-equal? (memory-list test-alloc-m1-03-state-after #xcccd #xccce)
;;                 (list #x00 #x00)
;;                 "slot4: refcount 0, next free slot at offset $00 = no next")

;;   (define test-alloc-m1-04-code
;;     (list
;;      ;; fill page with $ff
;;             (LDA !$FF)
;;             (LDX !$00)
;;      (label LOOP__TEST_ALLOC_M1_04_CODE)
;;             (DEX)
;;             (STA $cc00,x)
;;             (BNE LOOP__TEST_ALLOC_M1_04_CODE)

;;             ;; now allocate the page
;;             (LDX !$03) ;; do it explicitly
;;             (JSR VM_ALLOC_PAGE_FOR_M1_SLOTS)))

;;   (define test-alloc-m1-04-state-after
;;     (run-code-in-test test-alloc-m1-04-code))

;;   (check-equal? (memory-list test-alloc-m1-04-state-after #xcc00 #xcc02)
;;                 (list #x13 #x00 #x00)
;;                 "page type $13, previous page = $00, slot number used = $00")
;;   (check-equal? (memory-list test-alloc-m1-04-state-after #xcc03 #xcc04)
;;                 (list #x00 #x58)
;;                 "slot0: refcount 0, next free slot at offset $56")
;;   (check-equal? (memory-list test-alloc-m1-04-state-after #xcc57 #xcc58)
;;                 (list #x00 #xac)
;;                 "slot1: refcount 0, next free slot at offset $aa")
;;   (check-equal? (memory-list test-alloc-m1-04-state-after #xccab #xccac)
;;                 (list #x00 #x00)
;;                 "slot2: refcount 0, next free slot at offset $00 = no next")
;;   (check-equal? (vm-page->strings test-alloc-m1-04-state-after #xcc)
;;                 '("page-type:      m1 page p3"
;;                   "previous page:  $00"
;;                   "slots used:     0"
;;                   "next free slot: $04")))

;; allocate a slot of min A size, allocating a new page if necessary
;; input:  A = size
;; output: ZP_PTR2 = available slot of the given size (or a bit more)
;;         Y = actual size
;; (define VM_ALLOC_M1_SLOT_TO_ZP_PTR2
;;   (list
;;    (label VM_ALLOC_M1_SLOT_TO_ZP_PTR2)
;;           (LDX !$00)
;;           (CMP INC_TO_NEXT_SLOT__VM_ALLOC_PAGE_FOR_M1_SLOTS+0)
;;           (BPL J17PLUS__VM_ALLOC_SLOT_IN_BUCKET)

;;    (label VM_ALLOC_SLOT__TYPE_X_STORE)
;;           (STX PAGE_TYPE_IDX__VM_ALLOC_SLOT_IN_BUCKET)
;;           (JSR VM_REMOVE_FULL_PAGE_FOR_TYPE_X_SLOTS)

;;    (label VM_ALLOC_SLOT_TYPE_X)
;;           (LDA VM_FREE_M1_PAGE_P0,x) ;;
;;           (BEQ NEW_PAGE__VM_ALLOC_SLOT_TYPE_X)     ;; if the current free page is $00 (there is no page marked as having free slots) => allocate new page

;;           ;; ensure zp_ptr2 points into the page
;;           (STA ZP_PTR2+1)
;;           (STA INC_CMD__VM_ALLOC_SLOT_TYPE_X+2)
;;           (TAX)
;;           (LDY VM_FIRST_FREE_SLOT_ON_PAGE,x)           ;; first free slot offset
;;           (BEQ NEW_PAGE__VM_ALLOC_SLOT_TYPE_X)    ;; if =0 allocate new page (no more free slots on this page)
;;           ;; ensure zp_ptr2 points to the slot!

;;    (label CONTINUE__VM_ALLOC_SLOT_TYPE_X)
;;           (STY ZP_PTR2)

;;           ;; now get the next free slot (from linked list in this page)
;;           (LDY !$00)
;;           (LDA (ZP_PTR2),y) ;; content of free slot points to the next free one (or 00)
;;           (STA VM_FIRST_FREE_SLOT_ON_PAGE,x)           ;; set next free slot for this page (x is still page)

;;           ;; ensure y holds the actual available slot size
;;           (LDX PAGE_TYPE_IDX__VM_ALLOC_SLOT_IN_BUCKET)
;;           (LDY INC_TO_NEXT_SLOT__VM_ALLOC_PAGE_FOR_M1_SLOTS,x)
;;           (DEY)

;;    (label INC_CMD__VM_ALLOC_SLOT_TYPE_X)
;;           (INC $c002) ;; $c0 is overwritten with current page (increases the number of slots actually used)

;;           (RTS)

;;    (label FIND_NEXT_FREE_PAGE__VM_ALLOC_SLOT_TYPE_X)     ;; current page is full, search first non full (or end of list)
;;           ;; A = page, X = page, Y = 0
;;           (STA NEXT_PAGE_CMD__VM_ALLOC_SLOT_TYPE_X+2)

;;    (label NEXT_PAGE_CMD__VM_ALLOC_SLOT_TYPE_X)
;;           (LDA $C001) ;; $c0 is overwritten
;;           (BEQ NEW_PAGE__VM_ALLOC_SLOT_TYPE_X) ;; next page ptr = $00 => end reached, no more pages
;;           ;; check whether this page is full
;;           (TAX)
;;           (LDY VM_FIRST_FREE_SLOT_ON_PAGE,x)
;;           (BEQ FIND_NEXT_FREE_PAGE__VM_ALLOC_SLOT_TYPE_X) ;; next free slot for page is 00 => page is full, try to find next
;;           ;; page is not full => this is the new head
;;           (LDX PAGE_TYPE_IDX__VM_ALLOC_SLOT_IN_BUCKET)
;;           (STA VM_FREE_M1_PAGE_P0,x)
;;           (STA ZP_PTR2+1)
;;           (CLC)
;;           (BCC CONTINUE__VM_ALLOC_SLOT_TYPE_X)

;;    (label NEW_PAGE__VM_ALLOC_SLOT_TYPE_X)               ;; allocate a complete new page for page type x or find a page in the list that has free slots
;;           (LDX PAGE_TYPE_IDX__VM_ALLOC_SLOT_IN_BUCKET)
;;           (JSR VM_ALLOC_PAGE_FOR_M1_SLOTS)
;;           (LDX PAGE_TYPE_IDX__VM_ALLOC_SLOT_IN_BUCKET)
;;           (CLC)
;;           (BCC VM_ALLOC_SLOT_TYPE_X)

;;    (label J17PLUS__VM_ALLOC_SLOT_IN_BUCKET)
;;           (CMP INC_TO_NEXT_SLOT__VM_ALLOC_PAGE_FOR_M1_SLOTS+1)
;;           (BPL J29PLUS__VM_ALLOC_SLOT_IN_BUCKET)
;;           (LDX !$01)
;;           (BNE VM_ALLOC_SLOT__TYPE_X_STORE)

;;    (label J29PLUS__VM_ALLOC_SLOT_IN_BUCKET)
;;           (CMP INC_TO_NEXT_SLOT__VM_ALLOC_PAGE_FOR_M1_SLOTS+2)
;;           (BPL J49PLUS__VM_ALLOC_SLOT_IN_BUCKET)
;;           (LDX !$02)
;;           (BNE VM_ALLOC_SLOT__TYPE_X_STORE)

;;    (label J49PLUS__VM_ALLOC_SLOT_IN_BUCKET)
;;           (CMP INC_TO_NEXT_SLOT__VM_ALLOC_PAGE_FOR_M1_SLOTS+3)
;;           (BPL J83PLUS__VM_ALLOC_SLOT_IN_BUCKET)
;;           (LDX !$03)
;;           (BNE VM_ALLOC_SLOT__TYPE_X_STORE)

;;    (label J83PLUS__VM_ALLOC_SLOT_IN_BUCKET)
;;           ;; error, no slot this large can be allocated
;;           (BRK)

;;    (label PAGE_TYPE_IDX__VM_ALLOC_SLOT_IN_BUCKET)
;;           (byte $00) ;; local variable holding the selected page typ (0 = slots up to 17 bytes, 2 up to 29 bytes ...)
;;           ))

;; (module+ test #| vm_alloc_bucket_slot, allocate one slot of size $0b |#
;;   (define test-alloc-bucket-slot-code
;;     (list
;;      ;; fill page with $ff
;;             (LDA !$FF)
;;             (LDX !$00)
;;      (label LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
;;             (DEX)
;;             (STA $cc00,x)
;;             (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)

;;             ;; now allocate the page
;;             (LDA !$0b) ;; want slot of size 11
;;             (JSR VM_ALLOC_M1_SLOT_TO_ZP_PTR2)

;;             (LDA VM_FREE_M1_PAGE_P0+0) ;; type 0
;;             (STA ZP_TEMP)))

;;   (define test-alloc-bucket-slot-state-after
;;     (run-code-in-test test-alloc-bucket-slot-code))

;;   (check-equal? (memory-list test-alloc-bucket-slot-state-after #xcc03 #xcc04)
;;                 (list #x00 #x16)
;;                 "slot0: refcount 0, next free slot at offset $16")
;;   (check-equal? (memory-list test-alloc-bucket-slot-state-after ZP_PTR2 (add1 ZP_PTR2))
;;                 (list #x04 #xcc)
;;                 "allocated slot is at cc04")
;;   (check-equal? (memory-list test-alloc-bucket-slot-state-after ZP_TEMP ZP_TEMP)
;;                 (list #xcc)
;;                 "free page for slot type 0 is $cc")
;;   (check-equal? (vm-page->strings test-alloc-bucket-slot-state-after #xcc)
;;                 '("page-type:      m1 page p0"
;;                   "previous page:  $00"
;;                   "slots used:     1"
;;                   "next free slot: $16")))

;; (module+ test #| vm_alloc_bucket_slot 2 times slot size $0b and $09 |#
;;   (define test-alloc-bucket-slot-2x-code
;;     (list
;;      ;; fill page with $ff
;;      (LDA !$FF)
;;      (LDX !$00)
;;      (label LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
;;      (DEX)
;;      (STA $cc00,x)
;;      (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)

;;      ;; now allocate the page
;;      (LDA !$0b) ;; want slot of size 11
;;      (JSR VM_ALLOC_M1_SLOT_TO_ZP_PTR2)
;;      (LDA !$09) ;; want slot of size 9, should be on the same page
;;      (JSR VM_ALLOC_M1_SLOT_TO_ZP_PTR2)

;;      (LDA VM_FREE_M1_PAGE_P0+0) ;; type 0
;;      (STA ZP_TEMP)))

;;   (define test-alloc-bucket-slot-2x-state-after
;;     (run-code-in-test test-alloc-bucket-slot-2x-code))

;;   (check-equal? (memory-list test-alloc-bucket-slot-2x-state-after #xcc15 #xcc16)
;;                 (list #x00 #x28)
;;                 "slot1: refcount 0, next free slot at offset $28")
;;   (check-equal? (memory-list test-alloc-bucket-slot-2x-state-after ZP_PTR2 (add1 ZP_PTR2))
;;                 (list #x16 #xcc)
;;                 "allocated slot is at cc16")
;;   (check-equal? (memory-list test-alloc-bucket-slot-2x-state-after ZP_TEMP ZP_TEMP)
;;                 (list #xcc)
;;                 "free page for slot type 0 is $cc")
;;   (check-equal? (vm-page->strings test-alloc-bucket-slot-2x-state-after #xcc)
;;                 '("page-type:      m1 page p0"
;;                   "previous page:  $00"
;;                   "slots used:     2"
;;                   "next free slot: $28")))

;; (module+ test #| vm_alloc_bucket_slot, alloc 10 x slot size $14 (actual $20)  |#
;;   (define test-alloc-bucket-slot-xx-code
;;     (list
;;      ;; fill page with $ff
;;      (LDA !$FF)
;;      (LDY !$02) ;; 2 pages
;;      (LDX !$00) ;; 256 bytes
;;      (label LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
;;      (DEX)
;;      (STA $cb00,x)
;;      (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
;;      (DEY)
;;      (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)

;;      ;; loop over ...
;;      (LDA !$0a)
;;      (STA LOOP_NUM__TEST_ALLOC_BUCKET_SLOT_XX)

;;      (label LOOP__TEST_ALLOC_BUCKET_SLOT_XX)
;;      (LDA !$14) ;; want slot of size 20
;;      (JSR VM_ALLOC_M1_SLOT_TO_ZP_PTR2) ;; ... slot allocation
;;      (DEC LOOP_NUM__TEST_ALLOC_BUCKET_SLOT_XX)
;;      (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_XX)

;;      (JMP TAIL__TEST_ALLOC_BUCKET_SLOT_XX)

;;      (label LOOP_NUM__TEST_ALLOC_BUCKET_SLOT_XX)
;;      (byte $20)


;;      (label TAIL__TEST_ALLOC_BUCKET_SLOT_XX)
;;      (LDA VM_FREE_M1_PAGE_P0+1) ;; type 1
;;      (STA ZP_TEMP)))

;;   (define test-alloc-bucket-slot-xx-state-after
;;     (run-code-in-test test-alloc-bucket-slot-xx-code))

;;   (check-equal? (memory-list test-alloc-bucket-slot-xx-state-after ZP_PTR2 (add1 ZP_PTR2))
;;                 (list #x2e #xcb)
;;                 "allocated slot is at cb2e (slot1 on page 2)")
;;   (check-equal? (memory-list test-alloc-bucket-slot-xx-state-after #xcb4b #xcb4c)
;;                 (list #x00 #x6a)
;;                 "first free slot page2: refcount 0, next free slot at offset $6a")
;;   (check-equal? (memory-list test-alloc-bucket-slot-xx-state-after ZP_TEMP ZP_TEMP)
;;                 (list #xcb)
;;                 "free page for slot type 1 is $cb")
;;   (check-equal? (vm-page->strings test-alloc-bucket-slot-xx-state-after #xcc)
;;                 '("page-type:      m1 page p1"
;;                   "previous page:  $00"
;;                   "slots used:     8"
;;                   "next free slot: $00"))
;;   (check-equal? (vm-page->strings test-alloc-bucket-slot-xx-state-after #xcb)
;;                 '("page-type:      m1 page p1"
;;                   "previous page:  $00"
;;                   "slots used:     2"
;;                   "next free slot: $4c")))
  ;; free-page for slot type 0 = cc


;; inc ref count bucket slot
;; dec ref count bucket slot

;; TODO: adjust to RT/RA usage!

;; remove full pages in the free list of pages of the same type as are currently in ZP_PTR2
(define VM_REMOVE_FULL_PAGES_FOR_PTR2_SLOTS
  (list
   (label VM_REMOVE_FULL_PAGES_FOR_PTR2_SLOTS)
          (LDA ZP_PTR2+1)
          (STA READ_ENC_PAGE_TYPE__VM_REMOVE_FULL_PAGES_FOR_PTR2_SLOTS+2)
   (label READ_ENC_PAGE_TYPE__VM_REMOVE_FULL_PAGES_FOR_PTR2_SLOTS)
          (LDA $c000)
          (AND !$03)
          (TAX) ;; now x = page type

   ;; input: x (unchanged)
   (label VM_REMOVE_FULL_PAGE_FOR_TYPE_X_SLOTS)
          (LDA VM_FREE_M1_PAGE_P0,x)

   (label LOOP_REMOVE_FULL_PAGES__VM_REMOVE_FULL_PAGES_FOR_PTR2_SLOTS)
          (TAY) ;; y = page now
          (LDA VM_FIRST_FREE_SLOT_ON_PAGE,y)
          (BNE DONE__VM_REMOVE_FULL_PAGES_FOR_PTR2_SLOTS)

          ;; remove this page (in y) from list
          (STY LOAD_PREV_PAGE_CMD__VM_REMOVE_FULL_PAGES_FOR_PTR2_SLOTS+2)
          (STY STORE_PREV_PAGE_CMD__VM_REMOVE_FULL_PAGES_FOR_PTR2_SLOTS+2)
          (LDY !$00)
   (label LOAD_PREV_PAGE_CMD__VM_REMOVE_FULL_PAGES_FOR_PTR2_SLOTS)
          (LDA $c001) ;; $c0 is overwritten with page
          (STA VM_FREE_M1_PAGE_P0,x) ;; optional optimization: needs only be done once! (is here done in a loop)
   (label STORE_PREV_PAGE_CMD__VM_REMOVE_FULL_PAGES_FOR_PTR2_SLOTS)
          (STY $c001) ;; $c0 is overwritten
          (BNE LOOP_REMOVE_FULL_PAGES__VM_REMOVE_FULL_PAGES_FOR_PTR2_SLOTS)

   (label DONE__VM_REMOVE_FULL_PAGES_FOR_PTR2_SLOTS)
          (RTS)))

;; put this page as head of the page free list for slots of type as in ZP_PTR2
(define VM_ENQUEUE_PAGE_AS_HEAD_FOR_PTR2_SLOTS
  (list
   (label VM_ENQUEUE_PAGE_AS_HEAD_FOR_PTR2_SLOTS)
          (LDA ZP_PTR2)
          (STA ZP_TEMP) ;; keep for later

          (LDA !$00)    ;; set to zero
          (STA ZP_PTR2)

          (LDY !$01)
          (LDA (ZP_PTR2),y) ;; get previous
          (BNE CONTINUE_WITH_RESTORE__VM_ENQUEUE_PAGE_AS_HEAD_FOR_PTR2_SLOTS)     ;; is != 0 => is still part of the list, don't change the list
          ;; is no longer part of the free list of pages, add this page at the head of the page

          (DEY) ;; now 0
          (LDA (ZP_PTR2),y) ;; get encoded page type
          (AND !$03)
          (TAX) ;; now x = page type

          (LDA VM_FREE_M1_PAGE_P0,x)

          (INY) ;; now 1
          (STA (ZP_PTR2),y) ;; set previous

          ;; x = page type, a = page
          (LDA ZP_PTR2+1)
          (STA VM_FREE_M1_PAGE_P0,x)
          (TAX)  ;; x = page

   (label CONTINUE_WITH_RESTORE__VM_ENQUEUE_PAGE_AS_HEAD_FOR_PTR2_SLOTS)
          (LDA ZP_TEMP)
          (STA ZP_PTR2) ;; restore
          (LDA VM_FIRST_FREE_SLOT_ON_PAGE,x)           ;; first free slot offset

          (RTS)
))

;; input:  ZP_PTR2
;; output: ZP_PTR2 is invalid
;; currently once allocated pages are not garbage collected. this is bad and needs to be changed
;; (e.g. keep count of used slots)? used slots = 0 => free page
;; INFO: NO GC! (this must be done, freeing specific types (e.g. an array) <- knows the number of slots etc.
;;       REF COUNT IS SET TO ZERO
(define VM_FREE_M1_SLOT_IN_ZP_PTR2
  (list
   (label VM_FREE_M1_SLOT_IN_ZP_PTR2)   
   ;;        ;; make sure to remove fulls from free page list first !!
   ;;        (JSR VM_REMOVE_FULL_PAGES_FOR_PTR2_SLOTS)

   ;;        ;; now free the slot
   ;; (label REGULAR_FREE__VM_FREE_M1_SLOT_IN_ZP_PTR2)
   ;;        (LDX ZP_PTR2+1)
   ;;        (STX DEC_CMD__VM_FREE_M1_SLOT_IN_ZP_PTR2+2)    ;; write page for later dec execution
   ;;        (LDA VM_FIRST_FREE_SLOT_ON_PAGE,x)           ;; first free slot offset
   ;;        (BNE CONTINUE__VM_FREE_M1_SLOT_IN_ZP_PTR2)     ;; regular free

   ;;        ;; this page was full (since next free slot was 0) => register with the list of pages with free slots
   ;;        (JSR VM_ENQUEUE_PAGE_AS_HEAD_FOR_PTR2_SLOTS)
   ;;        (LDX DEC_CMD__VM_FREE_M1_SLOT_IN_ZP_PTR2+2)    ;; restore x
   ;;        (LDA !$00)                              ;; next free slot offset (=0)

   ;; (label CONTINUE__VM_FREE_M1_SLOT_IN_ZP_PTR2)
   ;;        (LDY !$00)
   ;;        (STA (ZP_PTR2),y)                       ;; set (zp_ptr) = previous free
   ;;        (LDA ZP_PTR2)                           ;; low byte of pointer = new free slot
   ;;        (STA VM_FIRST_FREE_SLOT_ON_PAGE,x)           ;; set new first free slot offset

   ;;        (DEC ZP_PTR2)                           ;; now points to ref count
   ;;        (TYA)                                   ;; y is still 0 => a := 0
   ;;        (STA (ZP_PTR2),y)                       ;; set refcount := 0

   ;; (label DEC_CMD__VM_FREE_M1_SLOT_IN_ZP_PTR2)
   ;;        (DEC $c002)                             ;; $c0 is overwritten

          (RTS)
          ))

;; (module+ test #| vm_free_bucket_slot  allocate two slots, free first slot |#
;;   (define test-free-bucket-slot-code
;;     (list
;;      ;; fill page with $ff
;;             (LDA !$FF)
;;             (LDX !$00)
;;      (label LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
;;             (DEX)
;;             (STA $cc00,x)
;;             (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)

;;             ;; now allocate the page
;;             (LDA !$0b) ;; want slot of size 11
;;             (JSR VM_ALLOC_M1_SLOT_TO_ZP_PTR2)
;;             (JSR VM_COPY_PTR2_TO_PTR)

;;             (LDA !$09) ;; want slot of size 9, should be on the same page
;;             (JSR VM_ALLOC_M1_SLOT_TO_ZP_PTR2)

;;             (JSR VM_COPY_PTR_TO_PTR2)
;;             (JSR VM_FREE_M1_SLOT_IN_ZP_PTR2)))

;;   (define test-free-bucket-slot-state-after
;;     (run-code-in-test test-free-bucket-slot-code))

;;   (check-equal? (memory-list test-free-bucket-slot-state-after #xcc03 #xcc04)
;;                 (list #x00 #x28)
;;                 "slot0 (now free): refcount 0, next free slot at offset $28")
;;   (check-equal? (vm-page->strings test-free-bucket-slot-state-after #xcc)
;;                 '("page-type:      m1 page p0"
;;                   "previous page:  $00"
;;                   "slots used:     1"
;;                   "next free slot: $04")))

;; (module+ test #| vm_free_bucket_slot  allocate 16 slots, free first slot |#
;;   (define test-free-bucket-a20-slot-code
;;     (list
;;      ;; fill page with $ff
;;             (LDY !$03) ;; fill two pages
;;             (LDA !$FF) ;; with $ff
;;             (LDX !$00) ;; each 256 bytes long
;;      (label LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
;;             (DEX)
;;             (STA $cb00,x) ;; starting at $cb00
;;             (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
;;             (DEY)
;;             (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)

;;             ;; now allocate the page
;;             (LDA !$17)
;;             (STA LOOP_VAR__TEST_FREE_BUCKET_A20_SLOT_CODE)
;;      (label LOOP__TEST_FREE_BUCKET_A20_SLOT_CODE)
;;             (LDA !$14) ;; want slot of size 14 (max size $1e)
;;             (JSR VM_ALLOC_M1_SLOT_TO_ZP_PTR2)
;;             (DEC LOOP_VAR__TEST_FREE_BUCKET_A20_SLOT_CODE)
;;             (BPL LOOP__TEST_FREE_BUCKET_A20_SLOT_CODE)
;;             (JMP CONT__TEST_FREE_BUCKET_A20_SLOT_CODE )
;;      (label LOOP_VAR__TEST_FREE_BUCKET_A20_SLOT_CODE)
;;             (byte $00)

;;      (label CONT__TEST_FREE_BUCKET_A20_SLOT_CODE)
;;             ;; select first pointer
;;             (LDA !$cb)
;;             (STA ZP_PTR2+1)
;;             (LDA !$10)
;;             (STA ZP_PTR2)
;;             (JSR VM_FREE_M1_SLOT_IN_ZP_PTR2)

;;             (LDA !$cc)
;;             (STA ZP_PTR2+1)
;;             (LDA !$10)
;;             (STA ZP_PTR2)
;;             (JSR VM_FREE_M1_SLOT_IN_ZP_PTR2)
;;             ))

;;   (define test-free-bucket-a20-slot-state-after
;;     (run-code-in-test test-free-bucket-a20-slot-code))

;;   (check-equal? (memory-list test-free-bucket-a20-slot-state-after #xcec7 #xceca)
;;                 (list #x00 #xcc #x00 #x00)
;;                 "first free page of profiles 0, 1, 2, 3 is $cc for page profile 1")
;;   (check-equal? (vm-page->strings test-free-bucket-a20-slot-state-after #xca)
;;                 '("page-type:      m1 page p1"
;;                   "previous page:  $00" ;; is removed, since full
;;                   "slots used:     8"
;;                   "next free slot: $00"))
;;   (check-equal? (vm-page->strings test-free-bucket-a20-slot-state-after #xcb)
;;                 '("page-type:      m1 page p1"
;;                   "previous page:  $00" ;; is the last in list
;;                   "slots used:     7"
;;                   "next free slot: $10"))
;;   (check-equal? (vm-page->strings test-free-bucket-a20-slot-state-after #xcc)
;;                 '("page-type:      m1 page p1"
;;                   "previous page:  $cb" ;; next free
;;                   "slots used:     7"
;;                   "next free slot: $10")))

;; (define VM_REFCOUNT_INCR_ZP_PTR__M1_SLOT
;;   (list
;;    (label VM_REFCOUNT_INCR_ZP_PTR__M1_SLOT)
;;           (DEC ZP_PTR)
;;           (LDY !$00)
;;           (LDA (ZP_PTR),y)
;;           (CLC)
;;           (ADC !$01)
;;           (STA (ZP_PTR),y)
;;           (INC ZP_PTR)
;;           (RTS)))

;; (module+ test #| vm_inc_ref_bucket_slot |#
;;   (define test-inc-ref-bucket-slot-1-code
;;     (list
;;      (LDA !$a0)
;;      (STA $a003)
;;      (STA ZP_PTR+1)
;;      (LDA !$04)
;;      (STA ZP_PTR)

;;      (JSR VM_REFCOUNT_INCR_ZP_PTR__M1_SLOT)))

;;   (define test-inc-ref-bucket-slot-1-state-after
;;     (run-code-in-test test-inc-ref-bucket-slot-1-code))

;;   (check-equal? (memory-list test-inc-ref-bucket-slot-1-state-after #xa003 #xa003)
;;                 (list #xa1))
;;   (check-equal? (memory-list test-inc-ref-bucket-slot-1-state-after ZP_PTR (add1 ZP_PTR))
;;                 (list #x04 #xa0)))

;; input: ZP_PTR  pointer to bucket slot (which can be anything, but most likely a cell-array or a native-array)
;; (define VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT
;;   (list
;;    (label VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT)
;;           (DEC ZP_PTR)
;;           (LDY !$00)
;;           (LDA (ZP_PTR),y)
;;           (SEC)
;;           (SBC !$01)            ;;  pointers are organized such that there is no page boundary crossed (=> no adjustment of highbyte necessary)
;;           (STA (ZP_PTR),y)
;;           (BNE NO_GC__VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT)

;;           ;; DO GC THIS SLOT and then FREE!!
;;           ;; what kind of object is this (read header cell)
;;           ;; then dispatch an header cell type
;;           (INC ZP_PTR) ;; now pointing at the first (lowbyte) of the cell header
;;           (LDA (ZP_PTR),y) ;; y still 0
;;           (CMP !TAG_BYTE_CELL_ARRAY)       ;;
;;           (BNE NEXT0__VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT)

;;           ;; its a regular array slot, (gc each slot, beware recursion!!!!)
;;           (JSR VM_GC_ARRAY_SLOT_PTR)
;;           (JMP VM_FREE_M1_SLOT_IN_ZP_PTR2)

;;    (label NEXT0__VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT)
;;           (CMP !TAG_BYTE_NATIVE_ARRAY)
;;           (BNE NEXT1__VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT)

;;           ;; it's a native array slot (no gc necessary)
;;           (JMP VM_FREE_M1_SLOT_IN_ZP_PTR2)

;;    (label NEXT1__VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT)
;;           (BRK) ;; error, unknown complex slot type

;;    (label NO_GC__VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT)
;;           (INC ZP_PTR)
;;           (RTS)))


;; (module+ test #| vm_dec_ref_bucket_slot (no gc) |#
;;   (define test-dec-ref-bucket-slot-1-code
;;     (list
;;      (LDA !$a0)
;;      (STA $a003) ;; $a004 - 1 = location for ref counting (now set to $a0)
;;      (STA ZP_PTR+1)
;;      (LDA !$04)
;;      (STA ZP_PTR) ;; ZP_PTR is set to $a004

;;      (JSR VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT)))

;;   (define test-dec-ref-bucket-slot-1-state-after
;;     (run-code-in-test test-dec-ref-bucket-slot-1-code))

;;   (check-equal? (memory-list test-dec-ref-bucket-slot-1-state-after #xa003 #xa003)
;;                 (list #x9f)
;;                 "a0 - 1 = 9f")
;;   (check-equal? (memory-list test-dec-ref-bucket-slot-1-state-after ZP_PTR (add1 ZP_PTR))
;;                 (list #x04 #xa0)
;;                 "points to $a004"))

;; (module+ test #| vm_dec_ref_bucket_slot (gc native array) |#

;;   (define test-dec-ref-bucket-slot-2-code
;;     (list
;;             (LDA !$00)
;;             (STA $a000) ;; counter for how often VM_GC_ARRAY_SLOT_PTR was called
;;             (JMP TEST_DEC_REF_BUCKET_SLOT_2_CODE)

;;      (label VM_GC_ARRAY_SLOT_PTR)
;;             (INC $a000)
;;             (RTS)

;;      (label TEST_DEC_REF_BUCKET_SLOT_2_CODE)
;;             (LDA !$10)
;;             (JSR VM_ALLOC_NATIVE_ARRAY_TO_ZP_PTR2)
;;             (JSR VM_COPY_PTR2_TO_PTR) ;; allocation is in zp_ptr2
;;             (JSR VM_REFCOUNT_INCR_ZP_PTR__M1_SLOT) ;; inc ref uses zp_ptr

;;             (JSR VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT) ;; dec ref uses zp_ptr
;;      ))

;;   (define test-dec-ref-bucket-slot-2-state-after
;;     (run-code-in-test test-dec-ref-bucket-slot-2-code
;;                       #:mock (list (label VM_GC_ARRAY_SLOT_PTR))))

;;   (check-equal? (vm-page->strings test-dec-ref-bucket-slot-2-state-after #xcc)
;;                 (list "page-type:      m1 page p1"
;;                       "previous page:  $00"
;;                       "slots used:     0"
;;                       "next free slot: $10"))
;;   (check-equal? (memory-list test-dec-ref-bucket-slot-2-state-after #xa000 #xa000)
;;                 (list #x00)
;;                 "VM_GC_ARRAY_SLOT_PTR should NOT have been called (native arrays do not need to free any cells."))

;; (module+ test #| vm_dec_ref_bucket_slot (gc cell array) |#
;;   ;; check that all cells ref counts (if present) in the bucket slot (cell-array) are decremented
;;   (define test-dec-ref-bucket-slot-3-code
;;     (list
;;             (LDA !$00)
;;             (STA $a000) ;; counter for how often VM_GC_ARRAY_SLOT_PTR was called
;;             (JMP TEST_DEC_REF_BUCKET_SLOT_3_CODE)
;;      (label VM_GC_ARRAY_SLOT_PTR)
;;             (INC $a000)
;;             (RTS)

;;      (label TEST_DEC_REF_BUCKET_SLOT_3_CODE)
;;             (LDA !$04)
;;             (JSR VM_ALLOC_CELL_ARRAY_TO_ZP_PTR2)
;;             (JSR VM_COPY_PTR2_TO_PTR) ;; allocation is in zp_ptr2
;;             (JSR VM_REFCOUNT_INCR_ZP_PTR__M1_SLOT) ;; inc ref uses zp_ptr

;;             (JSR VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT) ;; dec ref uses zp_ptr
;;             ))

;;   (define test-dec-ref-bucket-slot-3-state-after
;;     (run-code-in-test test-dec-ref-bucket-slot-3-code
;;                       #:mock (list (label VM_GC_ARRAY_SLOT_PTR))))

;;   (check-equal? (vm-page->strings test-dec-ref-bucket-slot-3-state-after #xcc)
;;                 (list "page-type:      m1 page p0"
;;                       "previous page:  $00"
;;                       "slots used:     0"
;;                       "next free slot: $04"))
;;   (check-equal? (memory-list test-dec-ref-bucket-slot-3-state-after #xa000 #xa000)
;;                 (list #x01)
;;                 "VM_GC_ARRAY_SLOT_PTR should have been called exactly once"))

;; TODO: check necessity for this function, adjust to rt/ra usage

;; dereference pointer in zp_ptr2, writing dereferenced value into zp_ptr
;; input:  ZP_PTR2 (pointer another pointer)
;; output: ZP_PTR  (dereferenced ZP_PTR2)
;; use case:
;;    e.g. ZP_PTR2 points to a car cell (of a cell-pair), which in turn points to a cell-array
;;    => ZP_PTR points to the cell-array
;; (define VM_DEREF_PTR2_INTO_PTR
;;   (list
;;    (label VM_DEREF_PTR2_INTO_PTR)
;;           (LDY !$00)
;;           (LDA (ZP_PTR2),y)
;;           (STA ZP_PTR_TAGGED)
;;           (AND !TAG_PTR_MASK)
;;           (STA ZP_PTR)
;;           (INY)
;;           (LDA (ZP_PTR2),y)
;;           (STA ZP_PTR+1)
;;           (RTS)))

;; (module+ test #| vm_deref_ptr2_into_ptr |#
;;   (define test-dref-ptr2-into-ptr-code
;;     (list
;;      (LDA !$a0)
;;      (STA ZP_PTR2+1)
;;      (LDA !$04)
;;      (STA ZP_PTR2)   ;; ZP_PTR2 = $a004

;;      (LDA !$b0)
;;      (STA $a005)
;;      (LDA !$0a)
;;      (STA $a004)        ;; $a004 b00a which is (untagged) b008 (bit 1 masked out)

;;      (JSR VM_DEREF_PTR2_INTO_PTR)))

;;   (define test-dref-ptr2-into-ptr-state-after
;;     (run-code-in-test test-dref-ptr2-into-ptr-code))

;;   (check-equal? (memory-list test-dref-ptr2-into-ptr-state-after ZP_PTR (add1 ZP_PTR))
;;                 (list #x08 #xb0)
;;                 "pointer found at location zp_ptr2 points to, masking out the tag bits (lower two)")
;;   (check-equal? (memory-list test-dref-ptr2-into-ptr-state-after ZP_PTR_TAGGED ZP_PTR_TAGGED)
;;                 (list #x0a)
;;                 "lowbyte of zp_ptr but with tag bits still set")
;;   (check-equal? (memory-list test-dref-ptr2-into-ptr-state-after ZP_PTR2 (add1 ZP_PTR2))
;;                 (list #x04 #xa0)
;;                 "zp_ptr2 is not modified"))

;; TODO: reactivate when encountered necessary

;; execute garbage collection on a cell array (decr-ref all array elements and collect if 0)
;; input:  ZP_PTR(2) = pointer to array (slot)
;; used:   ZP_PTR    = dreferenced array element (if array element is a ptr)
;;         ZP_PTR2   = pointer to last element of array
;; ouput: -
;; (define VM_GC_ARRAY_SLOT_PTR
;;   (list
;;    (label VM_GC_ARRAY_SLOT_PTR)
;;           (JSR VM_COPY_PTR_TO_PTR2)

;;    (label VM_GC_ARRAY_SLOT_PTR2)
;;           ;; loop over slots and decrement their slots
;;           (LDY !$01)
;;           (LDA (ZP_PTR2),y)  ;; a = number of array elements
;;           (STA LOOP_COUNT__VM_GC_ARRAY_SLOT_PTR) ;;

;;           (LDY !$00)

;;    (label LOOP__VM_GC_ARRAY_SLOT_PTR)
;;           (INC ZP_PTR2)
;;           (INC ZP_PTR2)
;;           ;; deref zp_ptr into zp_ptr2?
;;           (LDA (ZP_PTR2),y) ;; load tagged low byte
;;           (AND !$03)
;;           (BEQ NEXT__VM_GC_ARRAY_SLOT_PTR)
;;           (JSR VM_DEREF_PTR2_INTO_PTR)
;;           (JSR VM_REFCOUNT_DECR_ZP_PTR)
;;           (LDY !$00)
;;     (label NEXT__VM_GC_ARRAY_SLOT_PTR)
;;           (DEC LOOP_COUNT__VM_GC_ARRAY_SLOT_PTR)
;;           (BNE LOOP__VM_GC_ARRAY_SLOT_PTR)

;;           (RTS)

;;    (label LOOP_COUNT__VM_GC_ARRAY_SLOT_PTR)
;;           (byte $00)
;;    ))

;; (module+ test #| vm_gc_array_slot_ptr |#
;;   (define test-gc-array-slot-ptr-code
;;     (list
;;      (LDA !$04)
;;      (JSR VM_ALLOC_CELL_ARRAY_TO_ZP_PTR2)                       ;; ZP_PTR2 = pointer to the allocated array (with 4 cells)

;;      (JSR VM_ALLOC_CELL_PAIR_TO_ZP_PTR)                           ;; ZP_PTR = allocated cell-pair
;;      (JSR VM_REFCOUNT_INCR_RT__CELL_PAIR_PTR)
;;      (JSR VM_CELL_STACK_PUSH_ZP_PTR)                    ;;cell-pair -> stack

;;      ;; wrote a new cell-pair @2
;;      (LDA !$02)
;;      (JSR VM_CELL_STACK_WRITE_TOS_TO_ARRAY_ATa_PTR2)    ;; tos (cell-pair) -> @2

;;      (JSR VM_CELL_STACK_PUSH_INT_m1)                    ;; int -1 -> stack
;;      (LDA !$01)
;;      (JSR VM_CELL_STACK_WRITE_TOS_TO_ARRAY_ATa_PTR2)    ;; tos (int -1) -> 1

;;      (JSR VM_GC_ARRAY_SLOT_PTR2)
;;      ))                      ;; gc array

;;   (define test-gc-array-slot-ptr-state-after
;;     (run-code-in-test test-gc-array-slot-ptr-code))

;;   (check-equal? (vm-stack->strings test-gc-array-slot-ptr-state-after)
;;                 (list "stack holds 2 items"
;;                       "cell-int $1fff"
;;                       "cell-pair-ptr $cb04"))
;;   (check-equal? (vm-page->strings test-gc-array-slot-ptr-state-after #xcb)
;;                 (list "page-type:      cell-pair page"
;;                       "previous page:  $00"
;;                       "slots used:     1"
;;                       "next free slot: $08"))
;;   (check-equal? (memory-list test-gc-array-slot-ptr-state-after #xcb01 #xcb01)
;;                 (list #x00)
;;                 "refcount for cell-pair at cb04..cb07 is at cb01 = 0 (was freed)")
;;   (check-equal? (vm-cell-pair-free-tree->string test-gc-array-slot-ptr-state-after)
;;                 "cell-pair $cb04 -> [ cell-int $0000 . cell-int $0000 ]"
;;                 "...and added as free tree root (for reuse)"))

;; allocate an array of bytes (native) (also useful for strings)
;; input:  A = number of bytes (1..)
;; output: ZP_PTR2 -> points to an allocated array
;; (define VM_ALLOC_NATIVE_ARRAY_TO_ZP_PTR2
;; (list
;;    (label VM_ALLOC_NATIVE_ARRAY_TO_ZP_PTR2)
;;           (PHA)
;;           (CLC)
;;           (ADC !$02) ;; add to total slot size

;;           (JSR VM_ALLOC_M1_SLOT_TO_ZP_PTR2)

;;           ;; write header cell
;;           (LDY !$00)
;;           (LDA !TAG_BYTE_NATIVE_ARRAY)
;;           (STA (ZP_PTR2),y) ;; store tag byte

;;           (INY)
;;           (PLA)
;;           (STA (ZP_PTR2),y) ;; store number of array elements

;;           (TAX) ;; use number of array elements as loop counter

;;           ;; initialize slots/array with 0
;;           (LDA !$00)
;;    (label LOOP_INIT__VM_ALLOC_NATIVE_ARRAY_TO_ZP_PTR2)
;;           (INY)
;;           (STA (ZP_PTR2),y)
;;           (DEX)
;;           (BNE LOOP_INIT__VM_ALLOC_NATIVE_ARRAY_TO_ZP_PTR2)

;;           (RTS)))

;; (module+ test #| vm_allocate_native_array |#
;;   (define test-alloc-native-array-code
;;     (list
;;      (LDA !$10)
;;      (JSR VM_ALLOC_NATIVE_ARRAY_TO_ZP_PTR2)))

;;   (define test-alloc-native-array-state-after
;;     (run-code-in-test test-alloc-native-array-code))

;;   (check-equal? (vm-page->strings test-alloc-native-array-state-after #xcc)
;;                 (list
;;                  "page-type:      m1 page p1"
;;                  "previous page:  $00"
;;                  "slots used:     1"
;;                  "next free slot: $2e"))
;;   (check-equal? (memory-list test-alloc-native-array-state-after ZP_PTR2 (add1 ZP_PTR2))
;;                 (list #x10 #xcc))
;;   (check-equal? (memory-list test-alloc-native-array-state-after #xcc10 #xcc21)
;;                 (list TAG_BYTE_NATIVE_ARRAY #x10
;;                       #x00 #x00
;;                       #x00 #x00
;;                       #x00 #x00
;;                       #x00 #x00
;;                       #x00 #x00
;;                       #x00 #x00
;;                       #x00 #x00
;;                       #x00 #x00)))

;; ;; allocate an array of cells (also useful for structures)
;; ;; input:  A = number of cells (1..)
;; ;; output: ZP_PTR2 -> points to an allocated array
;; (define VM_ALLOC_CELL_ARRAY_TO_ZP_PTR2
;;   (list
;;    (label VM_ALLOC_CELL_ARRAY_TO_ZP_PTR2)
;;           ;; optional: optimization for arrays with 3 cells => s8 page!
;;           (PHA)
;;           (ASL A)
;;           (CLC)
;;           (ADC !$02) ;; add to total slot size

;;           (JSR VM_ALLOC_M1_SLOT_TO_ZP_PTR2)

;;           ;; write header cell
;;           (LDY !$00)
;;           (LDA !TAG_BYTE_CELL_ARRAY)
;;           (STA (ZP_PTR2),y) ;; store tag byte

;;           (INY)
;;           (PLA)
;;           (STA (ZP_PTR2),y) ;; store number of array elements

;;           (TAX) ;; use number of array elements as loop counter

;;           ;; initialize slots/array with nil
;;    (label LOOP_INIT__VM_ALLOC_CELL_ARRAY_TO_ZP_PTR2)
;;           (INY)
;;           (LDA !<TAGGED_NIL)
;;           (STA (ZP_PTR2),y)
;;           (INY)
;;           (LDA !>TAGGED_NIL)
;;           (STA (ZP_PTR2),y)
;;           (DEX)
;;           (BNE LOOP_INIT__VM_ALLOC_CELL_ARRAY_TO_ZP_PTR2)

;;           (RTS)))

;; (module+ test #| vm_allocate_cell_array |#
;;   (define test-alloc-cell-array-code
;;     (list
;;      (LDA !$04)
;;      (JSR VM_ALLOC_CELL_ARRAY_TO_ZP_PTR2)))

;;   (define test-alloc-cell-array-state-after
;;     (run-code-in-test test-alloc-cell-array-code))

;;   (check-equal? (vm-page->strings test-alloc-cell-array-state-after #xcc)
;;                 (list
;;                  "page-type:      m1 page p0"
;;                  "previous page:  $00"
;;                  "slots used:     1"
;;                  "next free slot: $16"))
;;   (check-equal? (memory-list test-alloc-cell-array-state-after ZP_PTR2 (add1 ZP_PTR2))
;;                 (list #x04 #xcc))
;;   (check-equal? (memory-list test-alloc-cell-array-state-after #xcc04 #xcc0d)
;;                 (list TAG_BYTE_CELL_ARRAY #x04
;;                       #x02 #x00
;;                       #x02 #x00
;;                       #x02 #x00
;;                       #x02 #x00))
;; )

;; TODO: rewrite to RT/RA usage

;; ;; write the tos into array element a (0 indexed), array pointed to by zp_ptr2
;; ;; input:  a = index (0 indexed)
;; ;;         ZP_PTR2 = pointer to array
;; ;; NO CHECKING (NO BOUNDS, NO TYPE ...)
;; ;; DECREMENT ref of pointer if array element was a pointer
;; (define VM_CELL_STACK_WRITE_TOS_TO_ARRAY_ATa_PTR2
;;   (list
;;    (label VM_CELL_STACK_WRITE_TOS_TO_ARRAY_ATa_PTR2)
;;           (ASL A)
;;           (CLC)
;;           (ADC !$02) ;; point to low byte
;;           (STA ZP_TEMP) ;; keep for later

;;           ;; copy low byte
;;           (LDY ZP_CELL_STACK_TOS)          ;; points to tagged low byte of stack
;;           (LDA (ZP_CELL_STACK_BASE_PTR),y) ;;
;;           (LDY ZP_TEMP)
;;           (TAX)
;;           (LDA (ZP_PTR2),y) ;; previous low byte in that slot
;;           (AND !$03)
;;           (BEQ NO_PTR_IN_SLOT__VM_CELL_STACK_WRITE_TOS_TO_ARRAY_ATa_PTR2)

;;           ;; GC the slot before actually writing to it <- can maybe optimized be ensuring that this cannot happen
;;           (INY)
;;           (LDA (ZP_PTR2),y) ;; if high byte is 0, it is nil, no gc there
;;           (BEQ IS_NIL____VM_CELL_STACK_WRITE_TOS_TO_ARRAY_ATa_PTR2)

;;           (DEY)

;;           (LDA (ZP_PTR2),y) ;; previous low byte in that slot (load again)
;;           (STA ZP_PTR_TAGGED)
;;           (AND !$fc)
;;           (STA ZP_PTR)
;;           (INY)
;;           (LDA (ZP_PTR2),y)
;;           (STA ZP_PTR+1)
;;           (JSR VM_REFCOUNT_DECR_ZP_PTR)

;;           ;; ensure x = lowbyte (or a and jump even further)

;;    (label IS_NIL____VM_CELL_STACK_WRITE_TOS_TO_ARRAY_ATa_PTR2)
;;           (LDY ZP_CELL_STACK_TOS)          ;; points to tagged low byte of stack
;;           (LDA (ZP_CELL_STACK_BASE_PTR),y) ;;
;;           (LDY ZP_TEMP)
;;           (TAX)
;;    (label NO_PTR_IN_SLOT__VM_CELL_STACK_WRITE_TOS_TO_ARRAY_ATa_PTR2)
;;           (TXA)
;;           (STA (ZP_PTR2),y)

;;           ;; copy high byte
;;           (LDY ZP_CELL_STACK_TOS)
;;           (DEY)
;;           (LDA (ZP_CELL_STACK_BASE_PTR),y) ;; high byte in stack
;;           (LDY ZP_TEMP)
;;           (INY)
;;           (STA (ZP_PTR2),y) ;; write high byte into array

;;           (RTS)))

;; (module+ test #| vm_cell_stack_write_tos_to_array_ata_ptr |#
;;   (define vm_cell_stack_write_tos_to_array_ata_ptr-code
;;     (list
;;      (LDA !$04)
;;      (JSR VM_ALLOC_CELL_ARRAY_TO_ZP_PTR2)

;;      (LDA !$ff)
;;      (LDX !$01)
;;      (JSR VM_CELL_STACK_PUSH_INT)

;;      (LDA !$02)
;;      (JSR VM_CELL_STACK_WRITE_TOS_TO_ARRAY_ATa_PTR2)))

;;   (define vm_cell_stack_write_tos_to_array_ata_ptr-state-after
;;     (run-code-in-test vm_cell_stack_write_tos_to_array_ata_ptr-code))

;;   (check-equal? (vm-page->strings vm_cell_stack_write_tos_to_array_ata_ptr-state-after #xcc)
;;                 (list
;;                  "page-type:      m1 page p0"
;;                  "previous page:  $00"
;;                  "slots used:     1"
;;                  "next free slot: $16"))
;;   (check-equal? (memory-list vm_cell_stack_write_tos_to_array_ata_ptr-state-after ZP_PTR2 (add1 ZP_PTR2))
;;                 (list #x04 #xcc))
;;   (check-equal? (memory-list vm_cell_stack_write_tos_to_array_ata_ptr-state-after #xcc04 #xcc0d)
;;                 (list TAG_BYTE_CELL_ARRAY #x04
;;                       #x02 #x00
;;                       #x02 #x00
;;                       #x04 #xff
;;                       #x02 #x00)))

;; (module+ test #| vm_cell_stack_push_array_ata_ptr |#
;;   (define test-cell-stack-push-array-ata-ptr-code
;;     (list
;;      (LDA !$04)
;;      (JSR VM_ALLOC_CELL_ARRAY_TO_ZP_PTR2)

;;      (LDA !$02)
;;      (JSR VM_CELL_STACK_PUSH_ARRAY_ATa_PTR2) ;; @2 = nil -> stack

;;      (LDA !$ff)
;;      (LDX !$01)
;;      (JSR VM_CELL_STACK_PUSH_INT)            ;; int $1ff -> stack

;;      (LDA !$02)
;;      (JSR VM_CELL_STACK_WRITE_TOS_TO_ARRAY_ATa_PTR2) ;; tos (int $1ff) -> @2 (overwriting nil)

;;      (LDA !$02)
;;      (JSR VM_CELL_STACK_PUSH_ARRAY_ATa_PTR2)  ;; @2 (now int $1ff) -> stack
;;      ))

;;   (define test-cell-stack-push-array-ata-ptr-state-after
;;     (run-code-in-test test-cell-stack-push-array-ata-ptr-code))

;;   (check-equal? (cpu-state-clock-cycles test-cell-stack-push-array-ata-ptr-state-after)
;;                 1371)
;;   (check-equal? (vm-stack->strings test-cell-stack-push-array-ata-ptr-state-after)
;;                 (list "stack holds 3 items"
;;                       "cell-int $01ff"
;;                       "cell-int $01ff"
;;                       "cell-pair-ptr NIL")))

;; idea: have a list of code pages (adding new page as head if allocated)
;;       TODO: how does relocation work here?
;; idea: function id = ptr to bytecode
;; idea: relocatable function ids = function id = ptr into reloc table (identified by lowest bit = 1)
;;       reloc table is a page with entries:
;;             function id -> [lowbyte] [highbyte] <- actual location of function
;;             moving a function can be done by rewriting function id entry in that table
;;             calling that function needs to do one more indirection
;;             functions currently in the call stack cannot (easily) be relocated!
;; module descriptor
;;   module name string
;;   child modules (list of references to child modules)
;;   modules code pages head (ptr to the head of the list of code pages of this module)

;; code page
;;   00           : page type
;;   01..02       : pointer to module descriptor
;;   03           : len of bytecode 0..127 (blen), highest bit set means exported function <-- start of first function descriptor
;;   04           : #locals
;;   05           : #params
;;   06..06+blen  : bytecode    <- function id points on first byte of byte code? [must start word aligned!]
;;   07+blen      : len of function name str (slen) [only if exported function]
;;   08+blen..    : function name                   [only if exported function]
;;   09+blen+slen :                        <-- start of next function descriptor
;;

;; could a constants pool be put into these pages, too? (relocation would be difficult, though)

;; load from disk: https://c64os.com/post/c64kernalrom#file_load


;; input:  x,y  id
;; output: x,y  ptr to function bytecode
(define VM_LOCATE_FUNCTION_BY_ID
  (list
   (label VM_LOCATE_FUNCTION_BY_ID)
   (RTS)))

;; input:  zp_ptr ptr to function name string
;; output: x,y    ptr to function bytecode
(define VM_LOCATE_FUNCTION_BY_NAME
  (list
   (label VM_LOCATE_FUNCTION_BY_NAME)
          (BRK))) ;; not implemented yet

;; input:  x/y  ptr to the bytecode descriptor
;;         descriptor:
;;           00..01  ptr to corresponding module descriptor
;;           02     byte code len
;;           03     function name string len
;;           04     #locals (max)
;;           05     #params
;;           06..   function byte code
;;           ...    function name string
;; output: x/y  function id
;; make sure to have a page allocated that can hold this function
;;
(define VM_REGISTER_FUNCTION
  (list
   (label VM_REGISTER_FUNCTION)
          ;; check for space on a code page of the given module (else allocate)
          ;; copy data into the code page
          ;; optional: create function id mapping on function id mapping page
          (RTS))) ;; not implemented yet

(define vm-memory-manager
  (append VM_MEMORY_MANAGEMENT_CONSTANTS
          VM_INITIALIZE_MEMORY_MANAGER

          ;; ---------------------------------------- alloc/free pages
          VM_FREE_PAGE                                       ;; free a page (the type specific stuff, of any, must have finished)
          VM_ALLOC_PAGE__PAGE_UNINIT                         ;; allocate new page (not initialized)

          VM_ALLOC_PAGE_FOR_CELLS                            ;; allocate page and initialize for ref counted cells
          VM_ALLOC_PAGE_FOR_CELL_PAIRS                       ;; allocate page and initialize for ref counted cell-pairs
          ;; VM_ALLOC_PAGE_FOR_M1_SLOTS                         ;; allocate page and initialize for ref counted m1 slots of a specific profile (and thus size)
          ;; VM_ALLOC_PAGE_FOR_S8_SLOTS                         ;; allocate page and initialize to hold ref counted 8 byte slots <- really, maybe s8 slots can be removed alltogether

          ;; VM_ALLOC_PAGE_FOR_MODULE_CODE                      ;; allocate page and initialize to hold immutable byte code (not ref counted)

          ;; ---------------------------------------- alloc/free cells, pairs, slots
          VM_ALLOC_CELL_ON_PAGE                              ;; allocate a cell on the page in A (allocating a new one if page is full)
          VM_ALLOC_CELL_PAIR_ON_PAGE_A_INTO_RT               ;; allocate a cell-pair from this page (if page has no free cell-pairs, a new page is allocated and is used to get a free cell-pair!)

          VM_ALLOC_CELL_PAIR_PTR_TO_RT                       ;; allocate a cell-pair from the current page (or from a new page if full)
          VM_FREE_CELL_PAIR_PTR_IN_RT                        ;; free this cell-pair (adding it to the free tree)

          VM_ALLOC_CELL_PTR_TO_RT                            ;; allocate a cell, allocating a new page if necessary, reusing cells from the free list first
          VM_FREE_CELL_PTR_IN_RT                             ;; free this cell pointed to by RT (adding it to the free list)

          ;; VM_ALLOC_NATIVE_ARRAY_TO_ZP_PTR2                   ;; allocate an array of bytes (native) (also useful for strings)
          ;; VM_ALLOC_CELL_ARRAY_TO_ZP_PTR2                     ;; allocate an array of cells (also useful for structures)

          ;; VM_ALLOC_M1_SLOT_TO_ZP_PTR2                        ;; allocate a slot of min A size, allocating a new page if necessary
          ;; VM_FREE_M1_SLOT_IN_ZP_PTR2                         ;; free a slot (adding it to the free list)

          ;; VM_ALLOC_MODULE_CODE_SLOT_TO_ZP_PTR                ;; allocate a slot for module code
          ;; VM_FREE_MODULE
          ;; VM_RELOCATE_MODULE_X_TO_                           ;; relocate module identified by page x to ??

          ;; ---------------------------------------- refcount
          VM_REFCOUNT_DECR_RT                                ;; generic decrement of refcount (dispatches depending on type)
          VM_REFCOUNT_INCR_RT                                ;; generic increment of refcount (dispatches depending on type)

          VM_REFCOUNT_DECR_RT__CELL_PAIR_PTR                 ;; decrement refcount, calling vm_free_cell_pair_in_zp_ptr if dropping to 0
          ;; VM_REFCOUNT_DECR_RT__M1_SLOT_PTR                   ;; decrement refcount, calling vm_free_m1_slot_in_zp_ptr if dropping to 0
          VM_REFCOUNT_DECR_RT__CELL_PTR                      ;; decrement refcount, calling vm_free_cell_in_zp_ptr if dropping to 0

          VM_REFCOUNT_INCR_RT__CELL_PAIR_PTR                 ;; increment refcount of cell-pair
          ;; VM_REFCOUNT_INCR_RT__M1_SLOT_PTR                   ;; increment refcount of m1-slot
          VM_REFCOUNT_INCR_RT__CELL_PTR                      ;; increment refcount of the cell, rt is pointing to

          ;; ---------------------------------------- call frame
          VM_ALLOC_CALL_FRAME                                ;; allocate a new call frame of minimum size
          VM_SAVE_EXEC_STATE_TO_CALL_FRAME                   ;; write current execution state into the (allocated) call frame
          VM_POP_CALL_FRAME                                  ;; pop the topmost call frame, restoring execution state of calling function


          ;; ---------------------------------------- misc

          ;; VM_REMOVE_FULL_PAGES_FOR_PTR2_SLOTS                ;; remove full pages in the free list of pages of the same type as are currently in ZP_PTR2
          ;; VM_ENQUEUE_PAGE_AS_HEAD_FOR_PTR2_SLOTS             ;; put this page as head of the page free list for slots of type as in ZP_PTR2

          ;; VM_GC_ARRAY_SLOT_PTR                               ;; execute garbage collection on a cell array (decr-ref all array elements and collect if 0)

          ;; VM_DEREF_PTR2_INTO_PTR                             ;; dereference pointer in zp_ptr2, writing dereferenced value into zp_ptr

          VM_FREE_PTR_IN_RT                                 ;; free pointer (is cell-ptr, cell-pair-ptr, m1-slot-ptr, slot8-ptr)

          VM_ADD_CELL_PAIR_IN_RT_TO_ON_PAGE_FREE_LIST       ;; add the given cell-pair (in zp_rt) to the free list of cell-pairs on its page

          ;; ---------------------------------------- CELL_STACK / RT / RA
          VM_CELL_STACK_POP_R                                ;; pop cell-stack into RT (discarding RT)

          VM_CELL_STACK_PUSH_R                               ;; push value into RT, pushing RT onto the call frame cell stack if not empty
          ;; vm_cell_stack_push_rt_if_nonempty
          VM_CELL_STACK_JUST_PUSH_RT                         ;; push RT onto call frame cell stack

          ;; VM_WRITE_INTm1_TO_RA                             ;; write cell-int -1 into RA
          ;; VM_WRITE_INTm1_TO_RT                             
          ;; VM_WRITE_INTm1_TO_Rx                             ;; x=0 -> RT, x=2 -> RA

          ;; VM_WRITE_INT1_TO_RA                              ;; write cell-int +1 into RA
          ;; VM_WRITE_INT1_TO_RT
          ;; VM_WRITE_INT1_TO_Rx                              ;; x=0 -> RT, x=2 -> RA

          ;; VM_WRITE_INT0_TO_RA                              ;; write cell-int 0 into RA
          ;; VM_WRITE_INT0_TO_RT
          ;; VM_WRITE_INT0_TO_Rx                              ;; x=0 -> RT, x=2 -> RA

          ;; VM_WRITE_INT_A_TO_RA                             ;; write cell-int (only byte sized) A into RA
          ;; VM_WRITE_INT_A_TO_RT
          ;; VM_WRITE_INT_A_TO_Rx                             ;; x=0 -> RT, x=2 -> RA

          ;; VM_WRITE_ENC_INT_AY_TO_Rx                        ;; encoded int in  A(lowbyte of int)/Y(encoded high byte), x=0 -> RT, x=2 -> RA

          ;; VM_WRITE_INT_AY_TO_RA                            ;; int in A(lowbyte)/Y(highbyte) into RA
          ;; VM_WRITE_INT_AY_TO_RT
          VM_WRITE_INT_AY_TO_Rx                              ;; int in A(lowbyte)/Y(highbyte), x=0 -> RT, x=2 -> RA

          ;; VM_WRITE_NIL_TO_RA
          ;; VM_WRITE_NIL_TO_RT
          VM_WRITE_NIL_TO_Rx

          ;; VM_WRITE_RT_CELL1_TO_RT
          ;; VM_WRITE_RT_CELL0_TO_RT
          VM_WRITE_RT_CELLy_TO_RT                            ;; write CELLy (y=0 cell0, y=2 cell1) pointed to by RT into RT

          VM_WRITE_RA_TO_CELLy_RT                            ;; write RA cell into CELLy (y=0 cell0, y=2 cell1) pointer to by RT

          ;; VM_WRITE_RT_CELL1_TO_RA
          ;; VM_WRITE_RT_CELL0_TO_RA
          VM_WRITE_RT_CELLy_TO_RA                            ;; write CELLy (y=0 cell0, y=2 cell1) pointed to by RT into RA

          VM_WRITE_RT_TO_CELLy_RA                            ;; write RT cell into CELLy (y=0 cell0, y=2 cell1) pointer to by RA

          VM_CP_RT_TO_RA                                     ;; copy RT -> RA
          VM_CP_RA_TO_RT                                     ;; copy RA -> RT

          VM_POP_FSTOS_TO_CELLy_RT                           ;; POP the cell-stack top into CELLy (y=0 cell0, y=2 cell1) pointed to by RT, reducing the stack size by 1, keeping rt as tos

          (list (label END__MEMORY_MANAGER))
          ;; ---------------------------------------- registers and maps
          (list (org #xcec0))
          VM_INITIAL_MM_REGS
          (list (org #xcf00))
          VM_PAGE_SLOT_DATA))
