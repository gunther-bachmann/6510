#lang racket/base

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

;; naming: atomic cell
;;         cell                     :: 16 bit value (finest granular memory managed block)
;;         atomic cell              :: a cell that has no followup value and is complete in itself
;;         cell-ptr                 :: an atomic cell, lowest bit of low byte is set, points to another cell
;;                                    lowbyte mask: #bxxxx xxx1, (lowbyte payload = xxxx xxx0 => address at 2 bytes offsets)
;;                                    highbyte = page
;;         cell-pair-ptr            :: an atomic cell, second lowest bit is set, lowest bit is unset, points to a cell-pair
;;                                    lowbyte mask: #bxxxx xx10 (lowbyte payload = xxxx xx00 => address at 4 bytes offsets)
;;                                    highbyte = page
;;         int-cell                 :: an atomic cell having 13 bit as payload
;;                                    lowbyte mask: #b0xxx xx00, xxxxx = high bits of int
;;                                    highbyte = lowbyte of int
;;         byte-cell (char|bool)    :: an atomic cell having one byte as payload
;;                                    lowbyte mask: #b1111 1100
;;                                    highbyte = payload
;;         complex cell             :: a cell that functions as header for followup values (follows directly in memory)
;;                                    complex cells cannot be pushed on the stack, they can only be pointed to by cell-ptr
;;         (cell-structure-header   :: a complex cell that defines a structure)
;;         cell-array-header        :: a complex cell that defines an array, defining the number of cells in the second byte
;;                                    a structure is an array of cells
;;                                    lowbyte mask: #b1000 0000
;;                                    highbyte: #of cells in this array
;;                                    n*2 bytes with cells <- each cell needs to be gc'ed separately
;;         cell-native-array-header :: a complex cell that defines an array of bytes
;;                                    a string is an native array of bytes
;;                                    lowbyte mask: #b1000 0100
;;                                    highbyte: #of bytes in this array
;;                                    n bytes with byte payloads <- no gc of this necessary
;;         (cell-float-header       :: a complex cell that defines a float)
;;         page                     :: 256 byte memory managed unit, holding slots
;;         slot                     :: a fixed size portion of memory on a page (sizes are 2, 4, 8 ...), only one size per page is allowed

;; naming: m1 page px       :: page for slots with ref count at -1 position, with profile x (0..3) <- defines size and payload start offset
;;         call-frame page  :: page for call-frames (stack organized, no ref counting etc.)
;;         cell-pairs page  :: page for cell-pairs, (lowbyte) lsr x 2 to get ref count position
;;         cell page        :: page for cells, (lowbyte) lsr x 1 to get ref count position (last cell unusable)
;;         [s8 page          :: page for slots of size <=8, (lowbyte) lsr x 3 to get ref count position] optional
;;         fid->loc page    :: page that maps a function id to a location of first byte code
;;         code page        :: page holding byte code (and function meta data, module meta data?)
;;         constants page   :: page holding constants (not ref counted)
;;         page block       :: a number of consecutive pages allocated/freed as a block, allowing for larger memory objects (having less wasted bytes (e.g. for call-frames)?)

;; idea: keep allocated #slots to detect empty pages (# drops to zero)
;; idea: page 00 = page mod byte
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

;; ---------------------------------------- cell-pairs page
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
;; ..3F   ref-count for cell-pair at fc (cell-pair 50)
;; 40     cell-pair 3
;; 44     cell-pair 4
;; ..fc   cell-pair 50
;; fd..fe unused
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
;; 07     ref-count for cell at 08 (cell 4)
;; 08..09 cell 1
;; ...
;; 0e..0f cell 4
;; 10    ref-count for cell at 20 (cell 5)
;; ...
;; 1f    ref-count for cell at 20 (cell 20)
;; 20..21 cell 5
;; ...
;; 3e..3f cell 20
;; 40..7e ref-count for cell at 80..fc (cell 21..83)
;; 7f    unused
;; 80..fd cell 21..83
;; fe    unused
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

(provide vm-memory-manager vm-stack->strings ast-const-get vm-page->strings vm-regt->string vm-rega->string vm-deref-cell-pair-w->string vm-deref-cell-pair->string
         ZP_VM_PC
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
    [(> stack-tos-idx #xf0) (list "stack is empty")]
    [else
     (define stack-ptr (peek-word-at-address state ZP_CELL_STACK_BASE_PTR))
     (define stack (memory-list state stack-ptr (+ 1 stack-tos-idx stack-ptr)))
     (define stack-values stack)
     (define stack-item-no (/ (add1 stack-tos-idx) 2))
     (cons (format "stack holds ~a ~a" stack-item-no (if (= 1 stack-item-no) "item" "items"))
           (reverse (map (lambda (pair) (vm-cell->string (cdr pair) (car pair))) (pairing stack-values))))]))

;; make a list of adjacent pairs
(define (pairing list (paired-list '()))
  (if (< (length list) 2)
      (reverse paired-list)
      (pairing (drop list 2) (cons `(,(car list) . ,(cadr list)) paired-list))))

(module+ test #| pairing |#
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

;; write the car, cdr cell of the cell-pair at low/high in memory
(define (vm-deref-cell-pair->string state low high)  
  (vm-deref-cell-pair-w->string state (bytes->int low high)))

;; write decoded cell described by word
(define (vm-cell-w->string word)
  (vm-cell->string (low-byte word) (high-byte word)))

;; write decoded cell described by low high
;; the low 2 bits are used for pointer tagging
(define (vm-cell->string low high)
  (cond
    [(= 1 (bitwise-and #x01 low)) (format "cell-ptr $~a~a"
                                          (format-hex-byte high)
                                          (format-hex-byte (bitwise-and #xfe low)))]
    [(and (= 2 (bitwise-and #x02 low)) (= high 0)) "cell-pair-ptr NIL"]
    [(= 2 (bitwise-and #x02 low)) (format "cell-pair-ptr $~a~a"
                                          (format-hex-byte high)
                                          (format-hex-byte (bitwise-and #xfc low)))]
    [(= 0 (bitwise-and #x83 low)) (format "cell-int $~a~a"
                                          (format-hex-byte (arithmetic-shift low -2))
                                          (format-hex-byte high))]
    [(= TAG_BYTE_BYTE_CELL (bitwise-and #xfc low)) (format "cell-byte $~a" (format-hex-byte high))]
    ;; TODO: a structure has a special value + follow bytes
    ;; (= ? (bitwise-and #xfc low)) e.g. #x04 = structure, high byte = number of fields
    ;; the following number of fields * cells cannot be structure cells, but only atomic or pointer cells
    [else "?"]))

(define (vm-regt->string state)
  (vm-cell->string
   (peek state ZP_RT_TAGGED_LB)
   (peek state (add1 ZP_RT))))

(define (vm-rega->string state)
  (vm-cell->string
   (peek state ZP_RA_TAGGED_LB)
   (peek state (add1 ZP_RA))))

(module+ test #| vm-cell->string |#
  (check-equal? (vm-cell->string #xc5 #xc0)
                "cell-ptr $c0c4")
  (check-equal? (vm-cell->string #xc2 #xc0)
                "cell-pair-ptr $c0c0")
  (check-equal? (vm-cell->string #x78 #x15)
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
  (check-equal? (vm-cells->strings '(#x02 #x00 #x00 #x01))
                '("cell-pair-ptr NIL" "cell-int $0001")))

(module+ test #| vm-stack->string |#
  (define test-vm_stack_to_string-a-code
    (list (JSR VM_CELL_STACK_PUSH_NIL)
          (JSR VM_CELL_STACK_PUSH_NIL)
          (LDA !$01)
          (LDX !$03)
          (JSR VM_CELL_STACK_PUSH_INT)))

  (define test-vm_stack_to_string-a-state-after
    (run-code-in-test test-vm_stack_to_string-a-code))

  (check-equal? (vm-stack->strings test-vm_stack_to_string-a-state-after)
                '("stack holds 3 items"
                  "cell-int $0301"
                  "cell-pair-ptr NIL"
                  "cell-pair-ptr NIL")))

;; test one roundtrip:

;; Method index + description
;; DATA
;;  VM_MEMORY_MANAGEMENT_CONSTANTS              :: constants that are used by the assembler code
;;  VM_ALLOC_PAGE_JUMP_TABLE                    :: jump table  page-type->allocation method
;;  VM_INITIAL_MM_REGS                          :: (initial data for) the memory management registers
;;
;; CODE (FULL PAGE)
;;  VM_INITIALIZE_MEMORY_MANAGER                :: initialize memory management (paging, cell stack)
;;  VM_ALLOC_PAGE                               :: INCOMPLETE! allocate page (of any kind)
;;  VM_ALLOC_PAGE_FOR_CELL_PAIRS              :: allocate a complete new page and initialize it to hold reference counted cell-pairs
;;  VM_FREE_PAGE                                :: free the given page (may then be allocated again via VM_ALLOC_PAGE*
;;  VM_ALLOC_PAGE__PAGE_UNINIT                  :: allocate page (without initialization for specific type)
;;  VM_ALLOC_PAGE__CALL_STACK                   :: INCOMPLETE! allocate page for call stack usage
;;
;; CODE
;;  VM_ALLOC_CELL_PAIR_ON_PAGE                  :: allocate a cell-pair on given page, auto allocate new page if full
;;  VM_REFCOUNT_DECR                            :: INCOMPLETE: dispatch to type specific decrement of ref count
;;  VM_REFCOUNT_DECR_CELL_PAIR                  :: decrement ref count for cell-pair, mark as free if ref count drops to 0 (calls VM_FREE_CELL_PAIR_IN_ZP_PTR)
;;  VM_REFCOUNT_INCR_ZP_PTR__CELL_PAIR                  :: increments ref count for cell-pair
;;  VM_FREE_NON_ATOMIC                          :: INCOMPLETE: free a non atomic cell (e.g. cell-ptr, cell-pair, float, array/struct)
;;  VM_ALLOC_CELL_PAIR_TO_ZP_PTR                          :: allocate a cell-pair (reuse marked free, allocate new if no reuse possible)
;;  VM_FREE_CELL_PAIR_IN_ZP_PTR                           :: mark cell-pair as free, tail call free on cell1 (which is used for free tree)
;;  VM_ADD_CELL_PAIR_TO_ON_PAGE_FREE_LIST       :: add the given cell-pair to its free list on its page (cell1 and cell2 must not point to anything), refcount is set to 0

;; STACK Functions
;;   VM_CELL_STACK_PUSH_NIL                     :: NIL->Stack++
;;
;;   VM_CELL_STACK_PUSH_INT                     :: INT->Stack++
;;   VM_CELL_STACK_PUSH_INT_0                   :: INT->Stack++
;;   VM_CELL_STACK_PUSH_INT_1                   :: INT->Stack++
;;   VM_CELL_STACK_PUSH_INT_2                   :: INT->Stack++
;;   VM_CELL_STACK_PUSH_INT_m1                  :: INT->Stack++
;;
;; STACK ZP_PTR Functions
;;   VM_CELL_STACK_PUSH_CELLy_OF_ZP_PTR         :: ZP_PTR (CELLy) -> Stack++
;;   VM_CELL_STACK_PUSH_CELL0_OF_ZP_PTR         :: ZP_PTR (CELL0) -> Stack++
;;   VM_CELL_STACK_PUSH_CELL1_OF_ZP_PTR         :: ZP_PTR (CELL1) -> Stack++
;;
;;   VM_CELL_STACK_PUSH_ZP_PTRy                 :: ZP_PTRy -> Stack++
;;   VM_CELL_STACK_PUSH_ZP_PTR                  :: ZP_PTR  -> Stack++
;;   VM_CELL_STACK_PUSH_ZP_PTR2                 :: ZP_PTR2 -> Stack++

;;   VM_CELL_STACK_WRITE_TOS_TO_CELLx_OF_ZP_PTR :: Stack -> ZP_PTR (CELLx)
;;   VM_CELL_STACK_WRITE_TOS_TO_CELL0_OF_ZP_PTR :: Stack -> ZP_PTR (CELL0)
;;   VM_CELL_STACK_WRITE_TOS_TO_CELL1_OF_ZP_PTR :: Stack -> ZP_PTR (CELL1)
;;
;;   VM_CELL_STACK_WRITE_CELLy_OF_ZP_PTR_TO_TOS :: ZP_PTR (CELLy) -> Stack
;;   VM_CELL_STACK_WRITE_CELL0_OF_ZP_PTR_TO_TOS :: ZP_PTR (CELL0) -> Stack
;;   VM_CELL_STACK_WRITE_CELL1_OF_ZP_PTR_TO_TOS :: ZP_PTR (CELL1) -> Stack
;;
;;   VM_CELL_STACK_WRITE_TOS_TO_ZP_PTRx         :: Stack -> ZP_PTRx
;;   VM_CELL_STACK_WRITE_TOS_TO_ZP_PTR          :: Stack -> ZP_PTR
;;   VM_CELL_STACK_WRITE_TOS_TO_ZP_PTR2         :: Stack -> ZP_PTR2
;;
;;   VM_CELL_STACK_WRITE_ZP_PTRy_TO_TOS         :: ZP_PTRy -> Stack
;;   VM_CELL_STACK_WRITE_ZP_PTR_TO_TOS          :: ZP_PTR  -> Stack
;;   VM_CELL_STACK_WRITE_ZP_PTR2_TO_TOS         :: ZP_PTR2 -> Stack
;;
;;   VM_CELL_STACK_POP                          :: Stack -> Stack--
;;
;; ZP_PTR Functions
;;   VM_COPY_PTR2_TO_PTR                        :: ZP_PTR2->ZP_PTR
;;   VM_COPY_PTR_TO_PTR2                        :: ZP_PTR->ZP_PTR2

;; constants that are used by the assembler code
(define VM_MEMORY_MANAGEMENT_CONSTANTS
  (list
   ;; page type list-pair-cells
   (byte-const PT_LIST_PAIR_CELLS        $00)   ;; page type: list pair cells
   (byte-const PT_CALL_STACK             $01)   ;; page type: call stack
   (byte-const PT_CODE                   $02)   ;; page type: code

   ;; highest bit 0 and the lowest 2 bits are reserved for int, cell-ptr and cell-pair-ptr
   ;; => 32 values still available
   (byte-const TAG_PTR_MASK              $fc)   ;; use bitwise-and this value to remove any pointer specific bits

   (byte-const TAG_BYTE_BYTE_CELL        $fc)
   (byte-const TAG_BYTE_CELL_ARRAY       $80)
   (byte-const TAG_BYTE_NATIVE_ARRAY     $84)

   (byte-const NEXT_FREE_PAGE_PAGE       $cf)   ;; cf00..cfff is a byte array, mapping each page idx to the next free page idx, 00 = no next free page for the given page
   (word-const VM_FIRST_FREE_SLOT_ON_PAGE     $cf00) ;; location: table of first free slot for each page

   (word-const TAGGED_INT_0              $0000)
   (word-const TAGGED_BYTE0              $00fc)
   (word-const TAGGED_NIL                $0002) ;; tag indicates cell-pair-ptr

   (byte-const ZP_CELL_STACK_TOS         $dc) ;; byte (fe = empty stack, 0 = first element, 2 = second element, 4 = third element ...)

   (byte-const ZP_PTR_TAGGED             $dd) ;; dd = low byte with tag bits , tagged low bytes are 2 appart, to use same offset as ZP_PTR <-> ZP_PTR2
   (byte-const ZP_PTR2_TAGGED            $df) ;; df = low byte with tag bits

   (byte-const ZP_TEMP                   $de) ;; may not be used after sub calls (just within a routine without jsr)

   ;; the following eight bytes need to be continuous, since they are saved into the call frame!
   (byte-const ZP_VM_PC                  $e0)
   (byte-const ZP_PARAMS_PTR             $e2) ;; pointer to first parameter in call-frame
   (byte-const ZP_LOCALS_PTR             $e4) ;; pointer to first local in call-frame
   (byte-const ZP_CELL_STACK_BASE_PTR    $e6) ;; e6..e7 (pointer to the base of the eval stack of the currently running function (+ZP_CELL_STACK_TOS => pointer to tos of the call-frame, in register mode, actual TOS is ZP_RT!)

   (byte-const ZP_CALL_FRAME             $f1) ;; f1..f2 <- may be not needed (zp_locals_ptr always = zp_call_frame + 6)

   ;; zp_ptr holds either a cell-ptr or a cell-pair (ptr) with out the tag bits!
   (byte-const ZP_PTR                    $fb)   ;; fb = low byte (without tag bits), fc = high byte,   tagged low byte is held in ZP_PTR_TAGGED
   (byte-const ZP_PTR2                   $fd)   ;; fd = low byte (without tag bits), fe = high byte

   ;; implementation using registers
   ;; register T = top of stack, used as main register for operations, could be a pointer to a cell or an atomic cell. if it is a pointer to a cell, the low byte here is without tag bits => (zp_rt) points to the cell
   (byte-const ZP_RT                     $fb) ;; fb = low byte (without tag bits), fc = high byte,   tagged low byte is held in ZP_RT_TAGGED_LB
   (byte-const ZP_RT_TAGGED_LB           $dd) ;; dd = low byte with tag bits , tagged low bytes are 2 apart, to use same offset as ZP_RT <-> ZP_RA
   ;; register A
   (byte-const ZP_RA                     $fd) ;; fd = low byte (without tag bits), fe = high byte,   tagged low byte is held in ZP_RA_TAGGED_LB
   (byte-const ZP_RA_TAGGED_LB           $df) ;; df = low byte with tag bits
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
(define ZP_RT_TAGGED_LB         (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_RT_TAGGED_LB"))
(define ZP_RA                   (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_RA"))
(define ZP_RA_TAGGED_LB         (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_RA_TAGGED_LB"))
(define ZP_PTR                  (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_PTR"))
(define ZP_PTR_TAGGED           (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_PTR_TAGGED"))
(define ZP_PTR2                 (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_PTR2"))
(define ZP_PTR2_TAGGED          (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_PTR2_TAGGED"))
(define ZP_CALL_FRAME           (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CALL_FRAME"))
(define ZP_CELL_STACK_BASE_PTR  (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CELL_STACK_BASE_PTR"))
(define ZP_CELL_STACK_TOS       (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CELL_STACK_TOS"))
(define ZP_TEMP                 (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_TEMP"))
(define ZP_VM_PC                (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_VM_PC"))
(define ZP_LOCALS_PTR           (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_LOCALS_PTR"))
(define ZP_PARAMS_PTR           (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_PARAMS_PTR"))
(define TAG_BYTE_BYTE_CELL      (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAG_BYTE_BYTE_CELL"))
(define TAG_BYTE_CELL_ARRAY     (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAG_BYTE_CELL_ARRAY"))
(define TAG_BYTE_NATIVE_ARRAY   (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAG_BYTE_NATIVE_ARRAY"))

;; naming
;;   cell-stack         == stack of cells (could be any atomic or ptr cell)
;;   zp_ptr             == cell ptr (either cell-ptr or cell-pair-ptr)
;;   cell-ptr           == pointer to any type of cell (except to cell-pairs)
;;   cell-pair-ptr      == pointer to cell-pair (only)
;;   untagged low byte  == low byte without ptr tags
;;   tagged low byte    == low byte with tags (ptr or other)
;;   page bitmap        == set of bits each indicating whether a page is free/used (total #x20 bytes long)
;;   ref count          == byte counting how many pointers to this value exist, there can be pointer to pointers

;; rules (up to debate)
;;   ? zp_ptr is regarded as volatile!
;;   ? pointers pushed on the stack increase the ref count
;;   ? pointers popped from the stack decrease the ref count
;;   ? copying to zp_ptr does NOT change the ref count
;;   ? writing a cell-ptr into a cell increases ref count (regardless where this cell-ptr comes from)
;;   ? erasing cell-ptr from a cell decreases ref count


;; (define VM_WRITE_BYTE_A_TO_RT)
;; (define VM_WRITE_RT_CELLy_TO_RT)

;; input: A = lowbyte (untagged)
;;        X = highbyte 
;; output: RT = cell-ptr -> (A/X)
(define VM_WRITE_CELL_PTR_AX_TO_RT
  (list
   (label VM_WRITE_CELL_PTR_AX_TO_RT)
          (STA ZP_RT)
          (STX ZP_RT+1)
          (ORA !$01)
          (STA ZP_RT_TAGGED_LB)
          (RTS)))

;; input: A = lowbyte  (untagged)
;;        X = highbyte
;; output: RT = cell-pair-ptr -> (A/X)
(define VM_WRITE_CELL_PAIR_PTR_AX_TO_RT
  (list
   (label VM_WRITE_CELL_PAIR_PTR_AX_TO_RT)
          (STA ZP_RT)
          (STX ZP_RT+1)
          (ORA !$02)
          (STA ZP_RT_TAGGED_LB)
          (RTS)))

;; input: A = lowbyte of int (0..255), written into high byte of cell register RT
;;        Y = highbyte (0.31), written into lowbyte and tagged lowbyte of cell register
;;        X = (0 = RT, 2 = RA)
;; output: Rx = cell-int
(define VM_WRITE_INT_AY_TO_Rx
  (list
   (label VM_WRITE_INTm1_TO_RA)
          (LDX !$02)
          (BNE VM_WRITE_INTm1_TO_Rx)
   (label VM_WRITE_INTm1_TO_RT)
          (LDX !$00)
   (label VM_WRITE_INTm1_TO_Rx)
          (LDA !$ff) 
          (LDY !$7c) ;; 1f << 2
          (BNE VM_WRITE_ENC_INT_AY_TO_Rx)


   (label VM_WRITE_INT1_TO_RA)
          (LDX !$02)
          (BNE VM_WRITE_INT1_TO_Rx)
   (label VM_WRITE_INT1_TO_RT)
          (LDX !$00)
   (label VM_WRITE_INT1_TO_Rx)
          (LDA !$01)
          (BNE VM_WRITE_INT_A_TO_Rx)

   (label VM_WRITE_INT0_TO_RA)
          (LDX !$02)
          (BNE VM_WRITE_INT0_TO_Rx)
   (label VM_WRITE_INT0_TO_RT)
          (LDX !$00)
   (label VM_WRITE_INT0_TO_Rx)
          (LDA !$00)
          (BEQ VM_WRITE_INT_A_TO_Rx)

   (label VM_WRITE_INT_A_TO_RA)
          (LDX !$02)
          (BNE VM_WRITE_INT_A_TO_Rx)
   (label VM_WRITE_INT_A_TO_RT)
          (LDX !$00)
   (label VM_WRITE_INT_A_TO_Rx)
          (LDY !$00)
   (label VM_WRITE_ENC_INT_AY_TO_Rx)
          (STY ZP_RT_TAGGED_LB,x)
          (STY ZP_RT,x)
          (STA ZP_RT+1,x)
          (RTS)

   (label VM_WRITE_INT_AY_TO_RA)
          (LDX !$02)
          (BNE VM_WRITE_INT_AY_TO_Rx)
   (label VM_WRITE_INT_AY_TO_RT)
          (LDX !$00)
   (label VM_WRITE_INT_AY_TO_Rx)
          (STA ZP_RT+1,x)
          (TYA)
          (ASL A)
          (ASL A)
          (AND !$7c)           ;; mask out top and two low bits!
          (STA ZP_RT_TAGGED_LB,x)
          (STA ZP_RT,x)
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
          (LDY !$02)
          (BNE VM_WRITE_RT_TO_CELLy_RA)

   (label VM_WRITE_RT_TO_CELL0_RA)
          (LDY !$00)

   ;; ----------------------------------------
   (label VM_WRITE_RT_TO_CELLy_RA)
          (LDA ZP_RT_TAGGED_LB)
          (STA (ZP_RA),y)
          (INY)
          (LDA ZP_RT+1)
          (STA (ZP_RA),y)
          (RTS)))

(module+ test #| vm-write-rt-to-celly-ra |#
  (define vm_write_rt_to_celly_ra_code
    (list
     (JSR VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
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
                "cell-pair-ptr $cc04")

  (check-equal? (vm-deref-cell-pair-w->string vm_write_rt_to_celly_ra_state #xcc04)
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
          (LDA ZP_RA_TAGGED_LB)
          (STA (ZP_RT),y)
          (INY)
          (LDA ZP_RA+1)
          (STA (ZP_RT),y)
          (RTS)))

(module+ test #| vm-write-ra-to-celly-rt |#
  (define vm_write_ra_to_celly_rt_code
    (list
     (JSR VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
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
                "cell-pair-ptr $cc04")

  (check-equal? (vm-deref-cell-pair-w->string vm_write_ra_to_celly_rt_state #xcc04)
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
     (JSR VM_ALLOC_CELL_PAIR_TO_RT)
     (LDY !$00)
     (JSR VM_POP_FSTOS_TO_CELLy_RT)
     (LDY !$02)
     (JSR VM_POP_FSTOS_TO_CELLy_RT)
     ))

  (define vm-pop-fstos-to-celly-rt-state
    (run-code-in-test vm-pop-fstos-to-celly-rt-code))

  (check-equal? (vm-stack->strings vm-pop-fstos-to-celly-rt-state)
                (list "stack is empty"))
  (check-equal? (vm-regt->string vm-pop-fstos-to-celly-rt-state)
                "cell-pair-ptr $cc04")
  (check-equal? (vm-deref-cell-pair-w->string vm-pop-fstos-to-celly-rt-state #xcc04)
                "(cell-int $1fff . cell-int $0001)"))

;; input:  RA
;; output: RT (copy of RA)
(define VM_CP_RA_TO_RT
  (list
   (label VM_CP_RA_TO_RT)
          (LDA ZP_RA_TAGGED_LB)
          (STA ZP_RT_TAGGED_LB)
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
          (LDA ZP_RT_TAGGED_LB)
          (STA ZP_RA_TAGGED_LB)
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
(define VM_CELL_STACK_WRITE_RT_CELLy_TO_RT
  (list
   (label VM_CELL_STACK_WRITE_RT_CELL1_TO_RT)
          (LDY !$02)
          (BNE VM_CELL_STACK_WRITE_RT_CELLy_TO_RT)
   (label VM_CELL_STACK_WRITE_RT_CELL0_TO_RT)
          (LDY !$00)
   (label VM_CELL_STACK_WRITE_RT_CELLy_TO_RT)
          (LDA (ZP_RT),y)
          (TAX)
          (INY)
          (LDA (ZP_RT),y)
          (STA ZP_RT+1)

          (TXA)
          (STA ZP_RT_TAGGED_LB)

          (LSR)
          (BCC NO_CELL_PTR__VM_CELL_STACK_WRITE_RT_CELLy_TO_RT)
          ;; it is a cell-ptr
          (TAX)
          (AND !$fe)
          (STA ZP_PTR)
          (RTS)

   (label NO_CELL_PTR__VM_CELL_STACK_WRITE_RT_CELLy_TO_RT)
          (LSR)
          (BCC NO_CELL_PAIR_PTR__VM_CELL_STACK_WRITE_RT_CELLy_TO_RT)
          ;; it is a cell-pair-ptr
          (TAX)
          (AND !$fc)
          (TAX)
          
   (label NO_CELL_PAIR_PTR__VM_CELL_STACK_WRITE_RT_CELLy_TO_RT)
          (STX ZP_PTR)
          (RTS)))

(module+ test #| vm-cell-stack-write-rt-celly-to-rt |#
  (define vm-cell-stack-write-rt-celly-to-rt-code
    (list
     (JSR VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
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
     (JSR VM_CELL_STACK_WRITE_RT_CELL0_TO_RT)))

  (define vm-cell-stack-write-rt-celly-to-rt-state
    (run-code-in-test vm-cell-stack-write-rt-celly-to-rt-code))

  (check-equal? (vm-regt->string vm-cell-stack-write-rt-celly-to-rt-state)
                "cell-int $1001")

  (check-equal? (vm-deref-cell-pair-w->string vm-cell-stack-write-rt-celly-to-rt-state #xcc04)
                "(cell-int $1001 . cell-int $0110)"))

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
          (ASL A)
          (ASL A)
          (AND !$7c)           ;; mask out top and two low bits!
          (TAX)
          (TYA)
          (CLC)
          (BCC VM_CELL_STACK_PUSH_R)

   (label VM_CELL_STACK_PUSH_INT_m1_R)
          (LDA !$ff) ;; 1f << 2
          (LDX !$7c)
          (BNE VM_CELL_STACK_PUSH_R)

   (label VM_CELL_STACK_PUSH_INT_2_R)
          (LDA !$02)
          (LDX !$00)
          (BEQ VM_CELL_STACK_PUSH_R)

   (label VM_CELL_STACK_PUSH_INT_1_R)
          (LDA !$01)
          (LDX !$00)
          (BEQ VM_CELL_STACK_PUSH_R)

   (label VM_CELL_STACK_PUSH_INT_0_R)
          (LDA !$00)
          (TAX)
          (BEQ VM_CELL_STACK_PUSH_R)

   ;; push NIL (cell-pair-ptr)           ;; idea: can be optimized since it is known that this is cell-pair-ptr
   (label VM_CELL_STACK_PUSH_NIL_R)
          (LDX !<TAGGED_NIL)
          (LDA !>TAGGED_NIL)

   ;; push a cell
   ;; A = high byte
   ;; X = tagged low byte
   (label VM_CELL_STACK_PUSH_R)
          (LDY !$01)
          (CPY ZP_RT_TAGGED_LB)     ;; if tagged-byte = 01 (empty marker) then push simply writes only into RT
          (BEQ VM_WRITE_AX_TO_RT)

   (label PUSH_RT__VM_CELL_STACK_PUSH_R)
          (LDY ZP_CELL_STACK_TOS)

          ;; check that stack pointer does not run out of bound (over the page)
          (CPY !$fd) ;; marks the end of page, for pushes at least!
          [BNE NO_ERROR__VM_CELL_STACK_PUSH_R]

          (BRK)

   (label NO_ERROR__VM_CELL_STACK_PUSH_R)
          (PHA) ;; save A, X is not used => needs not to be saved

          (LDA ZP_RT+1)
          (INY)
          (STA (ZP_CELL_STACK_BASE_PTR),y)       ;; write high byte! (untagged)

          (LDA ZP_RT_TAGGED_LB)
          (INY)
          (STA (ZP_CELL_STACK_BASE_PTR),y)      ;; write low byte (tagged)

          (STY ZP_CELL_STACK_TOS)      ;; set new tos

          (PLA) ;; restore A from call

   (label VM_WRITE_AX_TO_RT)
          (STX ZP_RT_TAGGED_LB)          
          (STA ZP_RT+1)
          (TXA)

          (LSR) ;; C = cell-ptr?
          (BCC NO_CELL_PTR__VM_CELL_STACK_PUSH_R)

          ;; it is a cell-ptr
          (TXA)
          (AND !$fe)  ;; mask out tag bit 0 (since this is a cell-ptr)
          (STA ZP_RT)
          (RTS)

   (label NO_CELL_PTR__VM_CELL_STACK_PUSH_R)
          (LSR) ;; C = cell-pair-ptr?
          (BCC NO_CELL_PAIR_PTR__VM_CELL_STACK_PUSH_R)  

          ;; it is a cell-pair ptr
          (TXA)
          (AND !$fc)  ;; mask out tag bit 0 and 1 (since this is a cell-pair-ptr)
          (STA ZP_RT)
          (RTS)

   (label NO_CELL_PAIR_PTR__VM_CELL_STACK_PUSH_R)
          (TXA)
          (STA ZP_RT)
          (RTS)))

(module+ test #| vm_cell_stack_push_r (basically on write into rt, since stack is completely empty) |#

  (define vm_cell_stack_push_r_int0_code
    (list
     (JSR VM_CELL_STACK_PUSH_INT_0_R)))

  (define vm_cell_stack_push_r_int0_state
    (run-code-in-test vm_cell_stack_push_r_int0_code))

  (check-equal? (memory-list vm_cell_stack_push_r_int0_state ZP_RT (add1 ZP_RT))
                (list #x00 #x00))

  (define vm_cell_stack_push_r_int1_code
    (list
     (JSR VM_CELL_STACK_PUSH_INT_1_R)))

  (define vm_cell_stack_push_r_int1_state
    (run-code-in-test vm_cell_stack_push_r_int1_code))

  (check-equal? (memory-list vm_cell_stack_push_r_int1_state ZP_RT (add1 ZP_RT))
                (list #x00 #x01))

  (define vm_cell_stack_push_r_intm1_code
    (list
     (JSR VM_CELL_STACK_PUSH_INT_m1_R)))

  (define vm_cell_stack_push_r_intm1_state
    (run-code-in-test vm_cell_stack_push_r_intm1_code))

  (check-equal? (memory-list vm_cell_stack_push_r_intm1_state ZP_RT (add1 ZP_RT))
                (list #x7c #xff))

  (define vm_cell_stack_push_r_nil_code
    (list
     (JSR VM_CELL_STACK_PUSH_NIL_R)))

  (define vm_cell_stack_push_r_nil_state
    (run-code-in-test vm_cell_stack_push_r_nil_code))

  (check-equal? (memory-list vm_cell_stack_push_r_nil_state ZP_RT (add1 ZP_RT))
                (list #x00 #x00))
  (check-equal? (memory-list vm_cell_stack_push_r_nil_state ZP_RT_TAGGED_LB ZP_RT_TAGGED_LB)
                (list #x02))

  (define vm_cell_stack_push_r_cell_ptr_code
    (list
     (LDX !$03)
     (LDA !$ce)
     (JSR VM_CELL_STACK_PUSH_R)))

  (define vm_cell_stack_push_r_cell_ptr_state
    (run-code-in-test vm_cell_stack_push_r_cell_ptr_code))

  (check-equal? (memory-list vm_cell_stack_push_r_cell_ptr_state ZP_RT (add1 ZP_RT))
                (list #x02 #xce))
  (check-equal? (memory-list vm_cell_stack_push_r_cell_ptr_state ZP_RT_TAGGED_LB ZP_RT_TAGGED_LB)
                (list #x03))

  (define vm_cell_stack_push_r_cell_pair_ptr_code
    (list
     (LDX !$06)
     (LDA !$ce)
     (JSR VM_CELL_STACK_PUSH_R)))

  (define vm_cell_stack_push_r_cell_pair_ptr_state
    (run-code-in-test vm_cell_stack_push_r_cell_pair_ptr_code))

  (check-equal? (memory-list vm_cell_stack_push_r_cell_pair_ptr_state ZP_RT (add1 ZP_RT))
                (list #x04 #xce))
  (check-equal? (memory-list vm_cell_stack_push_r_cell_pair_ptr_state ZP_RT_TAGGED_LB ZP_RT_TAGGED_LB)
                (list #x06)))

(module+ test #| vm_cell_stack_push_r (push rt, and write rt) |#
  (define vm_cell_stack_push_r_push1_code
    (list
     (JSR VM_CELL_STACK_PUSH_INT_m1_R)
     (JSR VM_CELL_STACK_PUSH_INT_1_R)
     ))

  (define vm_cell_stack_push_r_push1_state
    (run-code-in-test vm_cell_stack_push_r_push1_code))

  (check-equal? (vm-stack->strings vm_cell_stack_push_r_push1_state)
                (list "stack holds 1 item"
                      "cell-int $1fff"))

  (check-equal? (memory-list vm_cell_stack_push_r_push1_state ZP_RT (add1 ZP_RT))
                (list #x00 #x01))

  (define vm_cell_stack_push_r_push2_code
    (list
     (JSR VM_CELL_STACK_PUSH_INT_m1_R)
     (JSR VM_CELL_STACK_PUSH_INT_1_R)
     (JSR VM_CELL_STACK_PUSH_NIL_R)
     ))

  (define vm_cell_stack_push_r_push2_state
    (run-code-in-test vm_cell_stack_push_r_push2_code))

  (check-equal? (vm-stack->strings vm_cell_stack_push_r_push2_state)
                (list "stack holds 2 items"
                      "cell-int $0001"
                      "cell-int $1fff"))

  (check-equal? (memory-list vm_cell_stack_push_r_push2_state ZP_RT (add1 ZP_RT))
                (list #x00 #x00)))

;; pop cell from stack (that is, discard RegT, move tos of call-frame stack into RegT (if available))
;; input: call-frame stack, RT
;; output: call-frame stack, RT
;; NO GC CHECKS!
(define VM_CELL_STACK_POP_R
  (list
   (label VM_CELL_STACK_POP_R)
          ;; optional: stack marked empty? => error: cannot pop from empty stack!
          ;; (LDA !$03)
          ;; (CMP ZP_RT_TAGGED_LB)
          ;; (BEQ ERROR_NO_VALUE_ON_STACK)

          ;; is call-frame stack empty? => mark stack as empty and return | alternatively simply write NIL into RT
          (LDY ZP_CELL_STACK_TOS)
          (BMI WRITE_01_TO_RT) ;; which effectively clears the RT
          ;; pop value from call-frame stack into RT!
          (LDA (ZP_CELL_STACK_BASE_PTR),y) ;; tagged low byte
          (STA ZP_RT_TAGGED_LB)
          (TAX) ;; keep for later


          ;; (optional) quick check for atomic cells [speeds up popping atomic cells, slows popping cell-ptr, slight slows popping cell-pair-ptr
          ;; (AND !$03)
          ;; (BEQ WRITE_TOS_TO_RT__VM_CELL_STACK_POP_R)
          ;; (TXA)

          ;; do pointer checks now
          (LSR) ;; C = cell-ptr?
          (BCC NO_CELL_PTR__VM_CELL_STACK_POP_R)          

          ;; it is a cell-ptr
          (TXA)
          (AND !$fe)  ;; mask out tag bit 0 (since this is a cell-ptr)
          (BCS WRITE_TOS_TO_RT_ASET__VM_CELL_STACK_POP_R) ;; C is still set, since TXA and AND do not change Carry!

   (label NO_CELL_PTR__VM_CELL_STACK_POP_R)
          (LSR) ;; C = cell-pair-ptr?                   ;; if pointer quick check is done, this is not necessary
          (BCC WRITE_TOS_TO_RT__VM_CELL_STACK_POP_R)    ;; if pointer quick check is done, this is not necessary

          ;; it is a cell-pair ptr
          (TXA)
          (AND !$fc)  ;; mask out tag bit 0 and 1 (since this is a cell-pair-ptr)
          (BCS WRITE_TOS_TO_RT_ASET__VM_CELL_STACK_POP_R) ;; C is still set, since TXA and AND do not change Carry!

   (label WRITE_TOS_TO_RT__VM_CELL_STACK_POP_R)
          (TXA) ;; tagged low byte
   (label WRITE_TOS_TO_RT_ASET__VM_CELL_STACK_POP_R)
          (STA ZP_RT)
          (DEY)
          (LDA (ZP_CELL_STACK_BASE_PTR),y) ;; high byte
          (STA ZP_RT+1) 
          (DEY)
          (STY ZP_CELL_STACK_TOS)
          (RTS)

   (label WRITE_01_TO_RT)
          ;; simple write NIL into RT
          (LDA !$01) ;; 01 is an invalid value, since it cannot be cell-ptr pointing to $xx00!
          (STA ZP_RT_TAGGED_LB)
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
     (JSR VM_CELL_STACK_POP_R)))

  (define vm_cell_stack_pop3_r_state
    (run-code-in-test vm_cell_stack_pop3_r_code))

  (check-equal? (vm-stack->strings vm_cell_stack_pop3_r_state)
                (list "stack holds 1 item"
                      "cell-int $0001"))

  (check-equal? (memory-list vm_cell_stack_pop3_r_state ZP_RT (add1 ZP_RT))
                (list #x7c #xff))

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
                (list "stack is empty"))

  (check-equal? (memory-list vm_cell_stack_pop2_r_state ZP_RT (add1 ZP_RT))
                (list #x00 #x01))

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
                (list #x00 #x00))

  (check-equal? (memory-list vm_cell_stack_pop1_r_state ZP_RT_TAGGED_LB ZP_RT_TAGGED_LB)
                (list #x01)))

;; pop cell from stack, decrease ref count if it is a reference
;; input: stack
;; output: stack--
;;         A  ?
;;         X  TOS (on untagged low byte)
;;         Y  unchanged if cell is no ptr
;;            ? if cell is a ptr
;;    zp_ptr  is overwritte if cell is a ptr
;; check stack empty!
(define VM_CELL_STACK_POP
  (list
   (label VM_CELL_STACK_POP)
          (LDY ZP_CELL_STACK_TOS)
          (BMI STACK_EMPTY_ERROR__VM_CELL_STACK_POP)

          ;; now check whether deallocation needs to take place (that is the cell being popped is a ptr)
          (LDA (ZP_CELL_STACK_BASE_PTR),y) ;; tagged low byte
          (AND !$03)
          (BEQ DO_POP_2DEY__VM_CELL_STACK_POP) ;; is no pointer => jump

          ;; is a pointer?
          (LSR)
          (BCC IS_CELL_PAIR_PTR__VM_CELL_STACK_POP)

          ;; is a cell-ptr
          ;; not implemented yet: should decrement cell-ptr and check whether it can be garbage collected!

   (label STACK_EMPTY_ERROR__VM_CELL_STACK_POP)
          (BRK) ;; not implemented yet

   (label IS_CELL_PAIR_PTR__VM_CELL_STACK_POP)
          ;; move cell-pair-ptr (tos) -> zp_ptr
          (DEY)
          (LDA (ZP_CELL_STACK_BASE_PTR),y)  ;; high byte
          (BEQ DO_POP__VM_CELL_STACK_POP) ;; hight byte = 0 => is nil
          (STA ZP_PTR+1)      ;; to zp_ptr+1
          (INY)
          (LDA (ZP_CELL_STACK_BASE_PTR),y)  ;; tagged low byte
          (AND !TAG_PTR_MASK)           ;; remove low 2 bits
          (STA ZP_PTR)        ;; to zp_ptr
          ;; no need for copying tagged low byte, since I already know it is a cell-pair-ptr
          (JSR VM_REFCOUNT_DECR_ZP_PTR__CELL_PAIR) ;; decrement and gc if necessary

   (label VM_CELL_STACK_POP__NO_GC) ;; entry for just popping!
          (LDY ZP_CELL_STACK_TOS)   ;; restore tos

   (label DO_POP_2DEY__VM_CELL_STACK_POP)
          (DEY)               ;; dex (such that in total x-=3)

   (label DO_POP__VM_CELL_STACK_POP)
          (DEY) ;; y was already decremented => dec 2x
          (STY ZP_CELL_STACK_TOS) ;; store new tos
          (RTS)))

(module+ test #| vm_cell_stack_pop |#

  (define test-vm_cell_stack_pop-a-state-after
    (run-code-in-test
     (list (JSR VM_CELL_STACK_PUSH_NIL)
           (JSR VM_CELL_STACK_POP)
           (JSR VM_CELL_STACK_POP) ;; stops at brk in this routine
           (LDA !$00)              ;; is never run
           (STA ZP_CELL_STACK_TOS))))

  (check-equal? (memory-list test-vm_cell_stack_pop-a-state-after ZP_CELL_STACK_TOS ZP_CELL_STACK_TOS)
                '(#xff)
                "tos = ff (empty)"))

;; push a cell in A/X (high/low) onto the cell-stack
;; input: stack
;;        X = low byte,  (tagged)
;;        A = high byte,
;; output: stack++[nil]
;; registers: A  ?
;;            Y  TOS+2
;; check stack full!
(define VM_CELL_STACK_PUSH
  (list
   ;; push array@a pointed to by zp_ptr onto the cell-stack
   ;; input:  A       index into array (0 indexed)
   ;;         ZP_PTR  pointer to array slot
   ;; output: cell-stack with array element pushed pushed (=> ZP_CELL_TOS+=2)
   ;;         zp_ptr and pointed to cells unchanged
   (label VM_CELL_STACK_PUSH_ARRAY_ATa_PTR2)
          (ASL A)
          (CLC)
          (ADC !$02) ;; point to low byte of array@a
          (TAY)

          (LDA (ZP_PTR2),y)
          (TAX) ;; lowbyte -> x
          (INY)
          (LDA (ZP_PTR2),y) ;; highbyte -> a
          (CLC)
          (BCC VM_CELL_STACK_PUSH)

   (label VM_CELL_STACK_PUSH_CELL1_OF_ZP_PTR)
          (LDY !$02)
          (BNE VM_CELL_STACK_PUSH_CELLy_OF_ZP_PTR)

   (label VM_CELL_STACK_PUSH_CELL0_OF_ZP_PTR)
          (LDY !$00)

   ;; push celly of cell-pair pointer to by zp_ptr (y=0 car-cell, y=2 cdr-cell) onto cell-stack
   ;; input:  cell-stack
   ;;         y register (0 = car-cell, 2 = cdr-cell)
   ;;         zp_ptr pointing to the cells to read from (psuh to cell-stack)
   ;; output: cell-stack with celly pushed (=> ZP_CELL_TOS+=2)
   ;;         zp_ptr and pointed to cells unchanged
   ;;         flags: result of lda high byte of celly
   ;; check stack full!
   (label VM_CELL_STACK_PUSH_CELLy_OF_ZP_PTR)
          (LDA (ZP_PTR),y)
          (TAX)
          (INY)
          (LDA (ZP_PTR),y)
          (CLC)
          (BCC VM_CELL_STACK_PUSH)

   (label VM_CELL_STACK_PUSH_ZP_PTR2)
          (LDY !$02)
          (BNE VM_CELL_STACK_PUSH_ZP_PTRy)
   (label VM_CELL_STACK_PUSH_ZP_PTR)
          (LDY !$00)

   ;; push zp_ptry (y=0 zp_ptr, y=2 zp_ptr1) onto cell-stack
   ;; input:  stack
   ;;         zp_ptry
   ;; output: stack++[zp_ptry] (uses zp_ptr_tagged, too)
   ;;         A  high byte of zp_ptr
   ;;         X  TOS
   ;;         Y  0|2 (orig Y * 2)
   ;; check stack full!
   (label VM_CELL_STACK_PUSH_ZP_PTRy)
          (LDX ZP_PTR_TAGGED,y)
          (LDA ZP_PTR+1,y)
          (CLC)
          (BCC VM_CELL_STACK_PUSH)

   ;; ints are saved high byte first, then low byte !!!!
   ;; input:  stack
   ;;         X = high byte of int (max 31 = $1f) (stored in low byte (tagged) position)
   ;;         A = low byte of int (0..255) (stored in high byte (untagged) position)
   ;; output: stack++[INT]
   ;; check stack full!
   (label VM_CELL_STACK_PUSH_INT)
          (TAY)
          (TXA)
          (ASL A)
          (ASL A)
          (AND !$7c)           ;; mask out top and two low bits!
          (TAX)
          (TYA)
          (CLC)
          (BCC VM_CELL_STACK_PUSH)

   (label VM_CELL_STACK_PUSH_INT_m1)
          (LDA !$ff) ;; 1f << 2
          (LDX !$7c)
          (BNE VM_CELL_STACK_PUSH)

   (label VM_CELL_STACK_PUSH_INT_2)
          (LDA !$02)
          (LDX !$00)
          (BEQ VM_CELL_STACK_PUSH)

   (label VM_CELL_STACK_PUSH_INT_1)
          (LDA !$01)
          (LDX !$00)
          (BEQ VM_CELL_STACK_PUSH)

   (label VM_CELL_STACK_PUSH_INT_0)
          (LDA !$00)
          (TAX)
          (BEQ VM_CELL_STACK_PUSH)

   (label VM_CELL_STACK_PUSH_NIL)
          (LDX !<TAGGED_NIL)
          (LDA !>TAGGED_NIL)

   ;; ---------------------------------------- (a=high byte, x=tagged low)
   (label VM_CELL_STACK_PUSH)
          (LDY ZP_CELL_STACK_TOS)

          ;; check that stack pointer does not run out of bound (over the page)
          (CPY !$fd)
          (BNE NO_ERROR__VM_CELL_STACK_PUSH)

          (BRK)

   (label NO_ERROR__VM_CELL_STACK_PUSH)
          (INY)
          (STA (ZP_CELL_STACK_BASE_PTR),y)       ;; write high byte! (untagged)
          (INY)
          (TXA)
          (STA (ZP_CELL_STACK_BASE_PTR),y)      ;; write low byte (tagged)
          (STY ZP_CELL_STACK_TOS)      ;; set new tos
          (RTS)))

(module+ test #| vm_cell_stack_push_nil |#
  (define test-vm_cell_stack_push_nil-a-state-after
    (run-code-in-test
     (list (JSR VM_CELL_STACK_PUSH_NIL))))

  (check-equal? (vm-stack->strings test-vm_cell_stack_push_nil-a-state-after)
                '("stack holds 1 item"
                  "cell-pair-ptr NIL"))

  (define test-vm_cell_stack_push_nil-b-state-after
    (run-code-in-test
     (list (JSR VM_CELL_STACK_PUSH_NIL) ;; 1
           (JSR VM_CELL_STACK_PUSH_NIL) ;;
           (JSR VM_CELL_STACK_PUSH_NIL) ;; 3
           (JSR VM_CELL_STACK_PUSH_NIL) ;;
           (JSR VM_CELL_STACK_PUSH_NIL) ;; 5
           (JSR VM_CELL_STACK_PUSH_NIL) ;;
           (JSR VM_CELL_STACK_PUSH_NIL) ;; 7
           (JSR VM_CELL_STACK_PUSH_NIL)))) ;; 8

  (check-equal? (vm-stack->strings test-vm_cell_stack_push_nil-b-state-after)
                '("stack holds 8 items"
                  "cell-pair-ptr NIL"
                  "cell-pair-ptr NIL"
                  "cell-pair-ptr NIL"
                  "cell-pair-ptr NIL"
                  "cell-pair-ptr NIL"
                  "cell-pair-ptr NIL"
                  "cell-pair-ptr NIL"
                  "cell-pair-ptr NIL")))

;; input: A = lowbyte of int
;;        X = tagged highbyte of int
(define VM_CELL_STACK_WRITE_INT_TO_TOS
  (list
   (label VM_CELL_STACK_WRITE_INT_1_TO_TOS)
          (LDX !$01)
          (LDA !$00)
          (BEQ VM_CELL_STACK_WRITE_INT_TO_TOS)

   (label VM_CELL_STACK_WRITE_INT_0_TO_TOS)
          (LDA !$00)
          (TAX)

   ;; ----------------------------------------
   (label VM_CELL_STACK_WRITE_INT_TO_TOS)
          (LDY ZP_CELL_STACK_TOS)
          (ASL A)
          (ASL A)
          (AND !$7c)            ;; mask out top and two low bits!
          (STA (ZP_CELL_STACK_BASE_PTR),y)   ;; write tagged int high byte first
          (DEY)
          (TXA)
          (STA (ZP_CELL_STACK_BASE_PTR),y)   ;; write untagged int low byte
          (RTS)))

(module+ test #| vm_cell_push_int |#
  (define test-vm_cell_stack_push_int-a-state-after
    (run-code-in-test
     (list (JSR VM_CELL_STACK_PUSH_INT_m1)
           (LDA !$00) ;; -4096
           (LDX !$10)
           (JSR VM_CELL_STACK_PUSH_INT)
           (JSR VM_CELL_STACK_PUSH_INT_1)
           (JSR VM_CELL_STACK_PUSH_INT_0)
           (LDA !$ff) ;; 4095
           (LDX !$0f)
           (JSR VM_CELL_STACK_PUSH_INT))))

  (check-equal? (vm-stack->strings test-vm_cell_stack_push_int-a-state-after)
                '("stack holds 5 items"
                  "cell-int $0fff"
                  "cell-int $0000"
                  "cell-int $0001"
                  "cell-int $1000"
                  "cell-int $1fff")))

(module+ test #| vm_cell_stack_push_celly_of_zp_ptr |#
  (define test-vm_cell_stack_push_celly_to_zp_ptr-a-state-after
    (run-code-in-test
     (list
      (LDA !$12)
      (LDX !$02)
      (JSR VM_CELL_STACK_PUSH_INT)                      ;;  push cell-int $0212

      (JSR VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
      (JSR VM_REFCOUNT_INCR_ZP_PTR__CELL_PAIR)
      (LDX !$00)
      (JSR VM_CELL_STACK_WRITE_TOS_TO_CELLx_OF_ZP_PTR)  ;; write cell-int $0212 -> car of allocated cell-pair

      (JSR VM_CELL_STACK_POP)
      (JSR VM_CELL_STACK_PUSH_NIL)                      ;; push cell-pair-ptr nil

      (LDX !$02)
      (JSR VM_CELL_STACK_WRITE_TOS_TO_CELLx_OF_ZP_PTR)  ;; write cell-pair-ptr nil -> cdr of allocated cell-pair

      (JSR VM_CELL_STACK_POP)                           ;; empty stack

      (LDY !$00)
      (JSR VM_CELL_STACK_PUSH_CELLy_OF_ZP_PTR)          ;; push car (should be cell-int $0212)
      (LDY !$02)
      (JSR VM_CELL_STACK_PUSH_CELLy_OF_ZP_PTR))))       ;; push cdr (should be cell-pair-ptr nil)

  (check-equal? (vm-stack->strings test-vm_cell_stack_push_celly_to_zp_ptr-a-state-after)
                '("stack holds 2 items"
                  "cell-pair-ptr NIL"
                  "cell-int $0212"))
  (check-equal? (memory-list test-vm_cell_stack_push_celly_to_zp_ptr-a-state-after ZP_PTR (+ 1 ZP_PTR))
                '(#x04 #xcc)
                "zp_ptr => cd04 (first free pair cell)")
  (check-equal? (memory-list test-vm_cell_stack_push_celly_to_zp_ptr-a-state-after #xcc04 #xcc07)
                '(#x08 #x12 #x02 #x00)
                "zp_ptr => (int530 . nil)"))

;; write TOS into celly of cell-pair pointer to by zp_ptr (y=0 car-cell, y=2 cdr-cell)
;; input:  cell-stack
;;         x register (0 = car-cell, 2 = cdr-cell)
;;         zp_ptr pointing to the cells to write
;; output: cell-stack unchanged
;;         zp_ptr celly is set to tos cell
;;         a = ?
;; no stack empty check!
(define VM_CELL_STACK_WRITE_TOS_TO_CELLx_OF_ZP_PTR
  (list
   (label VM_CELL_STACK_WRITE_TOS_TO_CELL1_OF_ZP_PTR)
          (LDX !$02)
          (BNE VM_CELL_STACK_WRITE_TOS_TO_CELLx_OF_ZP_PTR)
   (label VM_CELL_STACK_WRITE_TOS_TO_CELL0_OF_ZP_PTR)
          (LDX !$00)

   ;;------------------------------------------------
   (label VM_CELL_STACK_WRITE_TOS_TO_CELLx_OF_ZP_PTR)
          ;; move just tagged low byte and high byte, since CELLy has no untagged byte!!
          (LDY ZP_CELL_STACK_TOS)

   (label VM_CELL_STACK_WRITE_TOS_TO_ZP_PTR__XY_SET)
          (STX ZP_TEMP)
          (LDA (ZP_CELL_STACK_BASE_PTR),y)
          (PHA) ;; STA ZP_TEMP2
          (DEY)
          (LDA (ZP_CELL_STACK_BASE_PTR),y)
          (LDY ZP_TEMP)
          (INY)
          (STA (ZP_PTR),y) ;; cell high byte
          (DEY)
          (PLA) ;; LDA ZP_TEMP2
          (STA (ZP_PTR),y) ;; cell low byte
          (RTS)))

(module+ test #| vm_cell_stack_write_tos_to_cellx_of_zp_ptr |#
  (define test-vm_cell_stack_write_tos_to_cellx_of_zp_ptr-a-state-after
    (run-code-in-test
     (list
      (JSR VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
      (JSR VM_REFCOUNT_INCR_ZP_PTR__CELL_PAIR)
      (LDX !$02)
      (LDA !$12)
      (JSR VM_CELL_STACK_PUSH_INT)
      (LDX !$00)
      (JSR VM_CELL_STACK_WRITE_TOS_TO_CELLx_OF_ZP_PTR)
      (JSR VM_CELL_STACK_POP)
      (JSR VM_CELL_STACK_PUSH_NIL)
      (LDX !$02)
      (JSR VM_CELL_STACK_WRITE_TOS_TO_CELLx_OF_ZP_PTR)
      (JSR VM_CELL_STACK_POP))))

  (check-equal? (vm-stack->strings test-vm_cell_stack_write_tos_to_cellx_of_zp_ptr-a-state-after)
                '("stack is empty"))
  (check-equal? (memory-list test-vm_cell_stack_write_tos_to_cellx_of_zp_ptr-a-state-after ZP_PTR (+ 1 ZP_PTR))
                '(#x04 #xcc)
                "zp_ptr => cd04 (first free pair cell)")
  (check-equal? (memory-list test-vm_cell_stack_write_tos_to_cellx_of_zp_ptr-a-state-after #xcc04 #xcc07)
                '(#x08 #x12 #x02 #x00)
                "zp_ptr => (int530 . nil)"))

;; write zp_ptry (x=0 zp_ptr, x=2 zp_ptr1) into top of cell-stack
;; input:  stack
;;         zp_ptry
;; output: stack[zp_ptry] (uses zp_ptr_tagged, too)
;;         A  high byte of zp_ptr
;;         X  TOS
;;         Y  0|2 (orig Y * 2)
;; NO CHECKS WHATSO EVER, NO GC NO REFCOUNT STUFF
(define VM_CELL_STACK_WRITE_ZP_PTRy_TO_TOS
  (list
   (label VM_CELL_STACK_WRITE_ZP_PTR2_TO_TOS)
          (LDX !$02)
          (BNE VM_CELL_STACK_WRITE_ZP_PTRy_TO_TOS)
   (label VM_CELL_STACK_WRITE_ZP_PTR_TO_TOS)
          (LDX !$00)

   ;; ----------------------------------------
   (label VM_CELL_STACK_WRITE_ZP_PTRy_TO_TOS)
          (LDY ZP_CELL_STACK_TOS)

          (LDA ZP_PTR_TAGGED,x)
          (STA (ZP_CELL_STACK_BASE_PTR),y)    ;; tagged low byte
          (DEY)
          (LDA ZP_PTR+1,x) ;; high byte of ptr
          (STA (ZP_CELL_STACK_BASE_PTR),y)

          (RTS)))

(define VM_CELL_STACK_WRITE_CELLy_OF_ZP_PTR_TO_TOS
  (list
   (label VM_CELL_STACK_WRITE_CELL1_OF_ZP_PTR_TO_TOS)
          (LDY !$02)
          (BNE VM_CELL_STACK_WRITE_CELLy_OF_ZP_PTR_TO_TOS)
   (label VM_CELL_STACK_WRITE_CELL0_OF_ZP_PTR_TO_TOS)
          (LDY !$00)

   ;; ----------------------------------------
   (label VM_CELL_STACK_WRITE_CELLy_OF_ZP_PTR_TO_TOS)
          ;; write tagged low byte and high byte
          (LDA (ZP_PTR),y) ;; cell low byte
          (STA ZP_TEMP)
          (INY)
          (LDA (ZP_PTR),y) ;; cell high byte
          (LDY ZP_CELL_STACK_TOS)
          (STA (ZP_CELL_STACK_BASE_PTR),y) ;; store high first
          (INY)
          (PLA)
          (STA (ZP_CELL_STACK_BASE_PTR),y) ;; store low there after
          (RTS)))

(module+ test #| vm_cell_stack_push_zp_ptry |#
  (define test-vm_cell_stack_push_zp_ptry-a-state-after
    (run-code-in-test
     (list
      (JSR VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
      (JSR VM_REFCOUNT_INCR_ZP_PTR__CELL_PAIR)
      (LDY !$00)
      (JSR VM_CELL_STACK_PUSH_ZP_PTRy))))

  (check-equal? (vm-stack->strings test-vm_cell_stack_push_zp_ptry-a-state-after)
                '("stack holds 1 item"
                  "cell-pair-ptr $cc04"))
  (check-equal? (memory-list test-vm_cell_stack_push_zp_ptry-a-state-after ZP_PTR (+ ZP_PTR 1))
                '(#x04 #xcc)
                "zp_ptr => cc04 (first free pair cell)")
  (check-equal? (memory-list test-vm_cell_stack_push_zp_ptry-a-state-after ZP_PTR_TAGGED ZP_PTR_TAGGED)
                '(#x06)
                "tagged lowbyte is 04 | 02 = 06")

  (define test-vm_cell_stack_push_zp_ptry-b-state-after
    (run-code-in-test
     (list
      (JSR VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
      (JSR VM_REFCOUNT_INCR_ZP_PTR__CELL_PAIR)
      (JSR VM_COPY_PTR_TO_PTR2)
      (LDA !$00)
      (STA ZP_PTR)
      (STA ZP_PTR+1)
      (LDY !$02)
      (JSR VM_CELL_STACK_PUSH_ZP_PTRy))))

  (check-equal? (vm-stack->strings test-vm_cell_stack_push_zp_ptry-b-state-after)
                '("stack holds 1 item"
                  "cell-pair-ptr $cc04"))
  (check-equal? (memory-list test-vm_cell_stack_push_zp_ptry-b-state-after ZP_PTR2 (+ 1 ZP_PTR2))
                '(#x04 #xcc)
                "zp_ptr2 => cc04 (first free pair cell)")
  (check-equal? (memory-list test-vm_cell_stack_push_zp_ptry-b-state-after ZP_PTR2_TAGGED ZP_PTR2_TAGGED)
                '(#x06)
                "tagged lowbyte is 04 | 02 = 06"))

;; write tos of cell-stack to zp_ptry (y=0 zp_ptr, y=1 zp_ptr2), (makes only sense if it is a cell-ptr or cell-pair-ptr
;; input:  stack
;; output: stack unchanged
;;         ZP_PTRy
;;         A  high byte of cell-ptr
;;         X  TOS-1
;;         Y  0|2 (orig *2)
(define VM_CELL_STACK_WRITE_TOS_TO_ZP_PTRx
  (list
   (label VM_CELL_STACK_WRITE_TOS_TO_ZP_PTR2)
          (LDX !$02)
          (BNE VM_CELL_STACK_WRITE_TOS_TO_ZP_PTRx)
   (label VM_CELL_STACK_WRITE_TOS_TO_ZP_PTR)
          (LDX !$00)

   ;;------------------------------------------------
   (label VM_CELL_STACK_WRITE_TOS_TO_ZP_PTRx)
          (LDY ZP_CELL_STACK_TOS)
          (LDA (ZP_CELL_STACK_BASE_PTR),y)    ;; low byte tagged
          (STA ZP_TEMP)
          (DEY)
          (LDA (ZP_CELL_STACK_BASE_PTR),y)    ;; high byte
          (STA ZP_PTR+1,x)
          (LDA ZP_TEMP)
          (AND !TAG_PTR_MASK)
          (STA ZP_PTR,x)

          (LDA ZP_TEMP)
          (STA ZP_PTR_TAGGED,x)
          (RTS)))

(module+ test #| vm_cell_stack_write_tos_to_zp_ptry |#
  (define test-vm_cell_stack_write_tos_to_zp_ptry-a-state-after
    (run-code-in-test
     (list
      (JSR VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
      (JSR VM_REFCOUNT_INCR_ZP_PTR__CELL_PAIR)
      (LDY !$00)
      (JSR VM_CELL_STACK_PUSH_ZP_PTRy)

      (LDA !$00)
      (STA ZP_PTR)
      (STA ZP_PTR+1)
      (STA ZP_PTR2)
      (STA ZP_PTR2+1)

      (LDX !$00)
      (JSR VM_CELL_STACK_WRITE_TOS_TO_ZP_PTRx))))

  (check-equal? (vm-stack->strings test-vm_cell_stack_write_tos_to_zp_ptry-a-state-after)
                '("stack holds 1 item"
                  "cell-pair-ptr $cc04"))
  (check-equal? (memory-list test-vm_cell_stack_write_tos_to_zp_ptry-a-state-after ZP_PTR (+ 1 ZP_PTR))
                '(#x04 #xcc)
                "zp_ptr => cc04 (first free pair cell)")
  (check-equal? (memory-list test-vm_cell_stack_write_tos_to_zp_ptry-a-state-after ZP_PTR_TAGGED ZP_PTR_TAGGED)
                '(#x06)
                "tagged lowbyte is 04 | 02 = 06")

  (define test-vm_cell_stack_write_tos_to_zp_ptry-b-state-after
    (run-code-in-test
     (list
      (JSR VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
      (JSR VM_REFCOUNT_INCR_ZP_PTR__CELL_PAIR)
      (LDY !$00)
      (JSR VM_CELL_STACK_PUSH_ZP_PTRy)

      (LDA !$00)
      (STA ZP_PTR)
      (STA ZP_PTR+1)
      (STA ZP_PTR2)
      (STA ZP_PTR2+1)

      (LDX !$02)
      (JSR VM_CELL_STACK_WRITE_TOS_TO_ZP_PTRx))))

  (check-equal? (vm-stack->strings test-vm_cell_stack_write_tos_to_zp_ptry-b-state-after)
                '("stack holds 1 item"
                  "cell-pair-ptr $cc04"))
  (check-equal? (memory-list test-vm_cell_stack_write_tos_to_zp_ptry-b-state-after ZP_PTR2 (+ 1 ZP_PTR2))
                '(#x04 #xcc)
                "zp_ptr2 => cc04 (first free pair cell)")
  (check-equal? (memory-list test-vm_cell_stack_write_tos_to_zp_ptry-b-state-after ZP_PTR2_TAGGED ZP_PTR2_TAGGED)
                '(#x06)
                "tagged lowbyte is 04 | 02 = 06"))

;; copy zp_ptr2 to zp_ptr (including tag byte)
;; input:  ZP_PTR2
;; output: ZP_PTR
;;         A  ?
;;         X  unchanged
;;         Y  unchanged
(define VM_COPY_PTR2_TO_PTR
  (list
   (label VM_COPY_PTR2_TO_PTR)
          (LDA ZP_PTR2+1)       ;; high byte
          (STA ZP_PTR+1)
          (LDA ZP_PTR2)         ;; low byte (clean of tag)
          (STA ZP_PTR)
          (LDA ZP_PTR2_TAGGED)  ;; tagged low byte
          (STA ZP_PTR_TAGGED)
          (RTS)))

;; copy zp_ptr to zp_ptr2 (including tag byte)
;; input:  ZP_PTR
;; output: ZP_PTR2
;;         A  ?
;;         X  unchanged
;;         Y  unchanged
(define VM_COPY_PTR_TO_PTR2
  (list
   (label VM_COPY_PTR_TO_PTR2)
          (LDA ZP_PTR+1)        ;; high byte
          (STA ZP_PTR2+1)
          (LDA ZP_PTR)          ;; low byte (clean of tag)
          (STA ZP_PTR2)
          (LDA ZP_PTR_TAGGED)   ;; tagged low byte
          (STA ZP_PTR2_TAGGED)
          (RTS)))

;; initial data for the memory management registers
;; put into memory @ #xced0 - len (currently 3)
(define VM_INITIAL_MM_REGS
  (list
   (label VM_INITIAL_MM_REGS)

   ;; $cec0
   (label VM_FREE_CELL_PAGE) ;; cell page with free cells
          (byte $00)
   ;; $cec1
   (label VM_FREE_CALL_STACK_PAGE) ;; call stack page with free space
          (byte $00) ;; initial -> first allocation will allocate a new page
   ;; $cec2
   (label VM_FREE_CODE_PAGE) ;; code page with free space
          (byte $00)

   ;; $cec3
   (label VM_FREE_CELL_PAIR_PAGE) ;; cell page with free cells
          (byte $00) ;; none

   ;; $cec4
   (label VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH) ;; what is the highest page to start searching for a free page
          (byte $cd) ;; safe to start with $cd is index

   ;; $cec5
   (label VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
          (word $0000) ;; if high byte is 0, the tree is empty!

   ;; $cec7
   (label VM_FREE_M1_PAGE_P0)
          (byte $00) ;; cell page with free slots for m1 page p0 pages
          (byte $00) ;; cell page with free slots for m1 page p1 pages
          (byte $00) ;; cell page with free slots for m1 page p2 pages
          (byte $00) ;; cell page with free slots for m1 page p3 pages

   ;; $cecb..cecc
   (label VM_LIST_OF_FREE_CELLS)
          (word $0000)

   ;; $cecd..$cecf (unused)
   ))

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
           (STY ZP_PARAMS_PTR)
           (STY ZP_LOCALS_PTR)
           (STY ZP_CELL_STACK_BASE_PTR)
           (STX ZP_PARAMS_PTR+1)
           (STX ZP_LOCALS_PTR+1)
           (STX ZP_CELL_STACK_BASE_PTR+1)

           (LDX !$ff)          ;; negative and iny will produce 0
           (STX ZP_CELL_STACK_TOS)
           (LDX !$01)
           (STX ZP_RT_TAGGED_LB) ;; set RT to hold no value
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
(define VM_ALLOC_PAGE_FOR_CELLS
  (list
   (label VM_ALLOC_PAGE_FOR_CELLS)
          (JSR VM_ALLOC_PAGE__PAGE_UNINIT) ;; page is in A

          (STA ZP_PTR+1)
          (TAY)
          (LDA !$02)
          (STA VM_FIRST_FREE_SLOT_ON_PAGE,y) ;; set slot @02 as the first free slot

          (LDA !$03)
          (STA BLOCK_LOOP_COUNT__VM_ALLOC_PAGE_FOR_CELLS) ;; how many blocks do we have (3)

          (LDA !$00)
          (STA ZP_PTR)

          (LDY !$01)
          (LDX !$01)
          (STX LOOP_COUNT__VM_ALLOC_PAGE_FOR_CELLS)

          ;; option: optimization: maybe clearing the whole page would be faster (and shorter) for setting all refcounts to 0?
   (label LOOP_REF_COUNT__VM_ALLOC_PAGE_FOR_CELLS)
          (STA (ZP_PTR),y) ;; refcount set to 0
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
          (STA (ZP_PTR),y)

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
          (STA (ZP_PTR),y)
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
          (STA (ZP_PTR),y)
          (TAY)
          (CLC)
          (ADC !$02)
          (DEX)
          (DEC BLOCK_LOOP_COUNT__VM_ALLOC_PAGE_FOR_CELLS)
          (BPL LOOP_NEXT_FREE__VM_ALLOC_PAGE_FOR_CELLS)

          ;; write last entry
          (LDA !$00)
          (LDY !$fc) ;; fc..fd is the last cell, fe..ff is unusable (since ff holds the previous page)
          (STA (ZP_PTR),y)

          (LDY !$ff)
          (LDA VM_FREE_CELL_PAGE) ;; store last free cell page in $ff
          (STA (ZP_PTR),y)

          ;; store page type in byte 0
          (LDY !$00)
          (LDA !$80)
          (STA (ZP_PTR),y)

          (LDA ZP_PTR+1) ;; page
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
                (make-list #x3f #x0))
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


  ;; memory layout of a cell-pairs page
  ;; offset content
  ;; --------------
  ;; 00     unused
  ;; 01     ref-count for cell-pair at 04 (cell-pair 0)
  ;; 02     ref-count for cell-pair at 08 (cell-pair 1)
  ;; 03     ref-count for cell-pair at 0C (cell-pair 2)
  ;; 04..07  cell-pair 0 (@04: initially 08)
  ;; 08..0b  cell-pair 1 (@08: initially 0c)
  ;; 0c..0f  cell-pair 2 (@0c: initially 40)
  ;; 10     ref-count for cell-pair at 40 (cell-pair 3)
  ;; 11     ref-count for cell-pair at 44 (cell-pair 4)
  ;; ..3F   ref-count for cell-pair at fc (cell-pair 50)
  ;; 40     cell-pair 3 (@40: initially 44)
  ;; 44     cell-pair 4 (...)
  ;; ..fc   cell-pair 50
  ;;
  ;; VM_FIRST_FREE_SLOT_ON_PAGE + pageidx: holds the index within the page of the first free cell-pair on that page (0 = no free cell-pair on this page)
  ;; the free cell-pair holds in byte 0 of the cell-pair the offset of the next free cell-pair (0 = no other free cell-pair)
  ;;
  ;; allocate a complete new page and initialize it to hold reference counted cell-pairs
  ;; connect all cell-pairs in a free-list
  ;; also set the first free slot of this allocated page (in VM_FIRST_FREE_SLOT_ON_PAGE + pageidx)
  (define VM_ALLOC_PAGE_FOR_CELL_PAIRS
    (list
     (label VM_ALLOC_PAGE_FOR_CELL_PAIRS)
            (JSR VM_ALLOC_PAGE__PAGE_UNINIT)
            ;; now initialize page in A

            (TAX)

            (STA ZP_PTR+1)
            (TAY) ;; page used as idx
            (LDA !$04) ;; first free slot (after initialization)
            (STA VM_FIRST_FREE_SLOT_ON_PAGE,y)

            (LDY !$00)
            (STY ZP_PTR)
            (LDA !$40) ;; page type cell-pairs w/ 0 slots allocated
            (STA (ZP_PTR),y)

            (LDY !$ff)
            (LDA VM_FREE_CELL_PAIR_PAGE)
            (STA (ZP_PTR),y) ;; previous page of this type is (@$ff = )

            (STX VM_FREE_CELL_PAIR_PAGE)

            (INC ZP_PTR)
            ;; first write all reference count fields (zero)
            (LDY !$02)
            (LDA !$00) ;; reference count initialized with 0

     (label FIRST_RC_BLOCK__VM_ALLOC_PAGE_FOR_CELL_PAIRS)
            (STA (ZP_PTR),y)
            (DEY)
            (BPL FIRST_RC_BLOCK__VM_ALLOC_PAGE_FOR_CELL_PAIRS)

            (LDY !$10)
            (STY ZP_PTR)
            (LDY !$2F)

     (label SECOND_RC_BLOCK__VM_ALLOC_PAGE_FOR_CELL_PAIRS)
            (STA (ZP_PTR),y)
            (DEY)
            (BPL SECOND_RC_BLOCK__VM_ALLOC_PAGE_FOR_CELL_PAIRS)

            (STA ZP_PTR) ;; clear lowbyte of ptr
            ;; write all cell-pairs to point to next free one
            (LDA !$04)
     (label FIRST_CELL_PAIR_BLOCK__VM_ALLOC_PAGE_FOR_CELL_PAIRS)
            (TAY)
            (CLC)
            (ADC !$04)
            (STA (ZP_PTR),y)
            (CMP !$0C)
            (BMI FIRST_CELL_PAIR_BLOCK__VM_ALLOC_PAGE_FOR_CELL_PAIRS)

            ;; write last pointer in this block to next free cell pair (in next block)
            (TAY)
            (LDA !$40)
            (STA (ZP_PTR),y)

     (label SECOND_CELL_PAIR_BLOCK__VM_ALLOC_PAGE_FOR_CELL_PAIRS)
            (TAY)
            (CLC)
            (ADC !$04)
            (STA (ZP_PTR),y)
            (CMP !$FC)
            (BNE SECOND_CELL_PAIR_BLOCK__VM_ALLOC_PAGE_FOR_CELL_PAIRS)

            (RTS)))

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
          (BEQ OUT_OF_MEMORY__VM_ALLOC_PAGE__PAGE_UNINIT)
          (CMP !$ff)
          (BNE LOOP__VM_ALLOC_PAGE__PAGE_UNINIT)

          (INY)
          (LDA !$00) ;; mark as initially full but allocated
          (STA VM_FIRST_FREE_SLOT_ON_PAGE,y)
          (TYA)
          (STA VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH)
          (RTS)

   (label OUT_OF_MEMORY__VM_ALLOC_PAGE__PAGE_UNINIT)
          (BRK)
          ))

;; allocate a cell-pair from this page (if page has no free cell-pairs, a new page is allocated and is used to get a free cell-pair!)
;; this will not check the free cell-pair tree!
;; input:  A : page to allocate cell-pair on (a new page is allocated, if this page does not have any free cell-pairs)
;; output: ZP_PTR
(define VM_ALLOC_CELL_PAIR_ON_PAGE
  (list
   (label ALLOC_NEW_PAGE_PREFIX__VM_ALLOC_CELL_PAIR_ON_PAGE)
          (JSR VM_ALLOC_PAGE_FOR_CELL_PAIRS)
          (LDA ZP_PTR+1)

   ;; ----------------------------------------
   (label VM_ALLOC_CELL_PAIR_ON_PAGE) ;; <-- real entry point of this function
          (STA ZP_PTR+1) ;; safe as highbyte of ptr
          (TAX)
          (LDA VM_FIRST_FREE_SLOT_ON_PAGE,x)
          (BEQ ALLOC_NEW_PAGE_PREFIX__VM_ALLOC_CELL_PAIR_ON_PAGE) ;; allocate new page first

   (label CELL_ON_THIS_PAGE__VM_ALLOC_CELL_PAIR_ON_PAGE)
          (STA ZP_PTR)
          (ORA !$02) ;; sets the bit indicating that this is pointer to a cell-pair
          (STA ZP_PTR_TAGGED)
          (LDY !$00)
          (LDA (ZP_PTR),y) ;; next free cell
          (STA VM_FIRST_FREE_SLOT_ON_PAGE,x)

          ;; increase the slot number on this page
          (STX INC_CMD__VM_ALLOC_CELL_PAIR_ON_PAGE+2) ;; overwrite $c0
   (label INC_CMD__VM_ALLOC_CELL_PAIR_ON_PAGE)
          (INC $c000)
          (TXA)
          (RTS)))

;; find out what kind of cell zp_ptr points to,
;; then call the right decrement refcounts function
;; input:  ZP_PTR
;; output: the right refcount is decremented
;;         (in case of m1 pages, @ZP_PTR-1)
;;         (in case of cell pages @ZP_PTR>>1)
;;         (in case of cell-pair pages @ZP_PTR>>2)
;;         (in case of 8s pages @ZP_PTR>>3)
(define VM_REFCOUNT_DECR_ZP_PTR
  (list
   (label VM_REFCOUNT_DECR_ZP_PTR)
          (LDA ZP_PTR_TAGGED)
          (TAY)
          (LSR)
          (BCS DECR_CELL_PTR__VM_REFCOUNT_DECR_ZP_PTR)
          (LSR)
          (BCS DECR_CELL_PAIR__VM_REFCOUNT_DECR_ZP_PTR)
          ;; check other types of cells
          (CPY !TAG_BYTE_CELL_ARRAY)
          (BEQ DECR_CELL_ARRAY__VM_REFCOUNT_DECR_ZP_PTR)
          (CPY !TAG_BYTE_NATIVE_ARRAY)
          (BEQ DECR_NATIVE_ARRAY__VM_REFCOUNT_DECR_ZP_PTR)

          ;; unknown object type (or atomic value that cannot be ref counted and MUST NOT END UP in ZP_PTR_TAGGED)
          (BRK)

   (label DECR_CELL_ARRAY__VM_REFCOUNT_DECR_ZP_PTR)
   (label DECR_NATIVE_ARRAY__VM_REFCOUNT_DECR_ZP_PTR)
          (JMP VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT)

   (label DECR_CELL_PAIR__VM_REFCOUNT_DECR_ZP_PTR)
          (JMP VM_REFCOUNT_DECR_ZP_PTR__CELL_PAIR)

   (label DECR_CELL_PTR__VM_REFCOUNT_DECR_ZP_PTR)
          (JMP VM_REFCOUNT_DECR_ZP_PTR__CELL)))

;; input: cell-pair ptr in ZP_PTR
;; decrement ref count, if 0 deallocate
(define VM_REFCOUNT_DECR_ZP_PTR__CELL_PAIR
  (list
   (label VM_REFCOUNT_DECR_ZP_PTR__CELL_PAIR)
          (LDA ZP_PTR+1)
          (STA PAGE__VM_REFCOUNT_DECR_ZP_PTR__CELL_PAIR+2) ;; store high byte (page) into dec-command high-byte (thus +2 on the label)
          (LDA ZP_PTR)
          (LSR)
          (LSR)
          (TAX)
   (label PAGE__VM_REFCOUNT_DECR_ZP_PTR__CELL_PAIR)
          (DEC $c000,x) ;; c0 is overwritten by page (see above)
          (BNE DONE__VM_REFCOUNT_DECR_ZP_PTR__CELL_PAIR)
          (JMP VM_FREE_CELL_PAIR_IN_ZP_PTR) ;; free delayed
   (label DONE__VM_REFCOUNT_DECR_ZP_PTR__CELL_PAIR)
          (RTS)))

(define VM_REFCOUNT_INCR_ZP_PTR__CELL_PAIR
  (list
   (label VM_REFCOUNT_INCR_ZP_PTR__CELL_PAIR)
          (LDA ZP_PTR+1)
          (STA PAGE__VM_REFCOUNT_INCR_ZP_PTR__CELL_PAIR+2) ;; store high byte (page) into inc-command high-byte (thus +2 on the label)
          (LDA ZP_PTR)
          (LSR)
          (LSR)
          (TAX)
   (label PAGE__VM_REFCOUNT_INCR_ZP_PTR__CELL_PAIR)
          (INC $c000,x) ;; c0 is overwritten by page (see above)
          (RTS)))

;; input: cell ptr in ZP_PTR
;; decrement ref count, if 0 deallocate
(define VM_REFCOUNT_DECR_ZP_PTR__CELL
  (list
   (label VM_REFCOUNT_DECR_ZP_PTR__CELL)
          (LDA ZP_PTR+1)
          (STA PAGE__VM_REFCOUNT_DECR_ZP_PTR__CELL+2) ;; store high byte (page) into dec-command high-byte (thus +2 on the label)
          (LDA ZP_PTR)
          (LSR)
          (TAX)
   (label PAGE__VM_REFCOUNT_DECR_ZP_PTR__CELL)
          (DEC $c000,x) ;; c0 is overwritten by page (see above)
          (BNE DONE__VM_REFCOUNT_DECR_ZP_PTR__CELL)
          (JMP VM_FREE_CELL_IN_ZP_PTR) ;; free delayed
   (label DONE__VM_REFCOUNT_DECR_ZP_PTR__CELL)
          (RTS)))

(define VM_REFCOUNT_INCR_ZP_PTR__CELL
  (list
   (label VM_REFCOUNT_INCR_ZP_PTR__CELL)
          (LDA ZP_PTR+1)
          (STA PAGE__VM_REFCOUNT_INCR_ZP_PTR__CELL+2) ;; store high byte (page) into inc-command high-byte (thus +2 on the label)
          (LDA ZP_PTR)
          (LSR)
          (TAX)
   (label PAGE__VM_REFCOUNT_INCR_ZP_PTR__CELL)
          (INC $c000,x) ;; c0 is overwritten by page (see above)
          (RTS)))

;; free nonatomic (is cell-ptr, cell-pair-ptr, m1-slot-ptr, slot8-ptr)
;; parameter: zp_ptr
(define VM_FREE_NON_ATOMIC
  (list
   (label VM_FREE_NON_ATOMIC)
          (LDA ZP_PTR_TAGGED)
          (TAY)
          (LSR)
          (BCS FREE_CELL__VM_FREE_NON_ATOMIC)
          (LSR)
          (BCS FREE_CELL_PAIR__VM_FREE_NON_ATOMIC)
          ;; check other types of cells
          (CPY !TAG_BYTE_CELL_ARRAY)
          (BEQ FREE_CELL_ARRAY__VM_FREE_NON_ATOMIC)
          (CPY !TAG_BYTE_NATIVE_ARRAY)
          (BEQ FREE_NATIVE_ARRAY__VM_FREE_NON_ATOMIC)

          ;; unknown pointer type in zp_ptr
          (BRK)

   (label FREE_CELL__VM_FREE_NON_ATOMIC)
          (JMP VM_FREE_CELL_IN_ZP_PTR)

   (label FREE_CELL_PAIR__VM_FREE_NON_ATOMIC)
          (JMP VM_FREE_CELL_PAIR_IN_ZP_PTR)

   (label FREE_CELL_ARRAY__VM_FREE_NON_ATOMIC)
   (label FREE_NATIVE_ARRAY__VM_FREE_NON_ATOMIC)
          (JSR VM_COPY_PTR_TO_PTR2)
          (JMP VM_FREE_M1_SLOT_IN_ZP_PTR2)))

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
          (LDA ZP_PTR+1)

   ;; ----------------------------------------
   (label VM_ALLOC_CELL_ON_PAGE) ;; <-- real entry point of this function
          (STA ZP_PTR+1) ;; safe as highbyte of ptr
          (TAX)
          (LDA VM_FIRST_FREE_SLOT_ON_PAGE,x)
          (BEQ ALLOC_NEW_PAGE_PREFIX__VM_ALLOC_CELL_ON_PAGE) ;; allocate new page first

   (label CELL_ON_THIS_PAGE__VM_ALLOC_CELL_ON_PAGE)
          (STA ZP_PTR)
          (ORA !$01) ;; sets the bit indicating that this is pointer to a cell
          (STA ZP_PTR_TAGGED)
          (LDY !$00)
          (LDA (ZP_PTR),y) ;; next free cell
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

  (check-equal? (memory-list test-alloc-cell-on-papge-a-state-after ZP_PTR (add1 ZP_PTR))
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

  (check-equal? (memory-list test-alloc-cell-on-papge-b-state-after ZP_PTR (add1 ZP_PTR))
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
(define VM_ALLOC_CELL_TO_ZP_PTR
  (list
   (label VM_ALLOC_CELL_TO_ZP_PTR)
          (LDA VM_LIST_OF_FREE_CELLS+1)
          (BNE REUSE__VM_ALLOC_CELL_TO_ZP_PTR)

          ;; get a cell on the given page (or allocate a new page)
          (LDA VM_FREE_CELL_PAGE)        ;; get the page that has cell available (can be 0)
          (BEQ ALLOCATE_NEW_PAGE__VM_ALLOC_CELL)
          (JMP VM_ALLOC_CELL_ON_PAGE)                        ;; allocate a new cell on that page
   (label ALLOCATE_NEW_PAGE__VM_ALLOC_CELL)
          (JMP ALLOC_NEW_PAGE_PREFIX__VM_ALLOC_CELL_ON_PAGE) ;; allocate new page and then a new cell on that page

   (label REUSE__VM_ALLOC_CELL_TO_ZP_PTR)
          ;; reuse old cell (and write the head into zp_ptr)
          (STA ZP_PTR+1)
          (LDA VM_LIST_OF_FREE_CELLS)
          (STA ZP_PTR)

          ;; read output this old cell and store its content as new head of the free list
          (LDY !$00)
          (LDA (ZP_PTR),y)
          (STA VM_LIST_OF_FREE_CELLS)
          (INY)
          (LDA (ZP_PTR),y)
          (STA VM_LIST_OF_FREE_CELLS)

          (RTS)))

(module+ test #| vm_alloc_cell_to_zp_ptr (once on a new page) |#
  (define test-alloc-cell-to-zp-ptr-code
    (list
     (JSR VM_ALLOC_CELL_TO_ZP_PTR)))

  (define test-alloc-cell-to-zp-ptr-state-after
    (run-code-in-test test-alloc-cell-to-zp-ptr-code))

  (check-equal? (memory-list test-alloc-cell-to-zp-ptr-state-after ZP_PTR (add1 ZP_PTR))
                (list #x02 #xcc))

  (check-equal? (vm-page->strings test-alloc-cell-to-zp-ptr-state-after #xcc)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $08")
                "page has 1 slot in use"))

(module+ test #| vm_alloc_cell_to_zp_ptr (twice on a new page) |#
  (define test-alloc-cell-to-zp-ptr-twice-code
    (list
     (JSR VM_ALLOC_CELL_TO_ZP_PTR)
     (JSR VM_ALLOC_CELL_TO_ZP_PTR)))

  (define test-alloc-cell-to-zp-ptr-twice-state-after
    (run-code-in-test test-alloc-cell-to-zp-ptr-twice-code))

  (check-equal? (memory-list test-alloc-cell-to-zp-ptr-twice-state-after ZP_PTR (add1 ZP_PTR))
                (list #x08 #xcc))

  (check-equal? (vm-page->strings test-alloc-cell-to-zp-ptr-twice-state-after #xcc)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $0a")
                "page has 2 slots in use"))

(module+ test #| vm_alloc_cell_to_zp_ptr (twice, then free first on a new page) |#
  (define test-alloc-cell-to-zp-ptr-twicenfree-code
    (list
     (JSR VM_ALLOC_CELL_TO_ZP_PTR)
     (JSR VM_COPY_PTR_TO_PTR2)

     (JSR VM_ALLOC_CELL_TO_ZP_PTR)
     (JSR VM_COPY_PTR2_TO_PTR)
     (JSR VM_FREE_CELL_IN_ZP_PTR)))

  (define test-alloc-cell-to-zp-ptr-twicenfree-state-after
    (run-code-in-test test-alloc-cell-to-zp-ptr-twicenfree-code))

  (check-equal? (memory-list test-alloc-cell-to-zp-ptr-twicenfree-state-after #xcecb #xcecc )
                (list #x02 #xcc)
                "free cell list has cc02 now as head of the list")

  (check-equal? (vm-page->strings test-alloc-cell-to-zp-ptr-twicenfree-state-after #xcc)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $0a")
                "page has still 2 slots in use (even though $cc02 was freed)")

  (check-equal? (memory-list test-alloc-cell-to-zp-ptr-twicenfree-state-after #xcc02 #xcc03 )
                (list #x00 #x00)
                "since cc02 is now part of the free cell list, it points to the next free cell which is $0000 (none)"))

(module+ test #| vm_alloc_cell_to_zp_ptr (twice, then free first on a new page, then allocate again) |#
  (define test-alloc-cell-to-zp-ptr-twicenfreenalloc-code
    (list
     (JSR VM_ALLOC_CELL_TO_ZP_PTR)
     (JSR VM_COPY_PTR_TO_PTR2)

     (JSR VM_ALLOC_CELL_TO_ZP_PTR)
     (JSR VM_COPY_PTR2_TO_PTR)
     (JSR VM_FREE_CELL_IN_ZP_PTR)

     (JSR VM_ALLOC_CELL_TO_ZP_PTR)))

  (define test-alloc-cell-to-zp-ptr-twicenfreenalloc-state-after
    (run-code-in-test test-alloc-cell-to-zp-ptr-twicenfreenalloc-code))

  (check-equal? (vm-page->strings test-alloc-cell-to-zp-ptr-twicenfreenalloc-state-after #xcc)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $0a"))

  (check-equal? (memory-list test-alloc-cell-to-zp-ptr-twicenfreenalloc-state-after #xcecb #xcecb)
                (list #x00) ;; lowbyte is zero => it is initial (high byte is not heeded in that case)
                "free cell list is initial again"))

;; input:  none
;; output: zp_ptr = free cell-pair
;;
;; try to reuse root of free tree: use root but make sure to deallocate cell2 of the root (since this might still point to some data)
;; if no free tree available, find page with free cells (VM_FREE_CELL_PAIR_PAGE)
;; if no free cell page is available, allocate a new page and used the first free slot there
;; NOTE: the cell-pair is not initialized (cell1 and/or cell2 may contain old data that needs to be overwritten!)
(define VM_ALLOC_CELL_PAIR_TO_ZP_PTR
  (list
   (label VM_ALLOC_CELL_PAIR_TO_RT) ;; ZP_RT and ZP_PTR are identical thus this alias is ok
   (label VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1) ;; get highbyte (page) from ptr to cell-pair
          (BNE REUSE_CELL_PAIR__VM_ALLOC_CELL_PAIR_TO_ZP_PTR)   ;; if != 0, cell-pair can be reused
          ;; no cell-pair to reuse available => need to allocate a new one

          ;; get a cell-pair on the given page (or allocate a new page)
          (LDA VM_FREE_CELL_PAIR_PAGE)        ;; get the page that has cell-pairs available (can be 0)
          (BEQ ALLOCATE_NEW_PAGE__VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
          (JMP VM_ALLOC_CELL_PAIR_ON_PAGE)                        ;; allocate a new cell on that page
   (label ALLOCATE_NEW_PAGE__VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
          (JMP ALLOC_NEW_PAGE_PREFIX__VM_ALLOC_CELL_PAIR_ON_PAGE) ;; allocate new page and then a new cell on that page

   (label REUSE_CELL_PAIR__VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
          ;; put root of free tree into zp_ptr (and copy in TEMP_PTR of this function)
          (STA ZP_PTR+1)
          (STA TEMP_PTR__VM_ALLOC_CELL_PAIR_TO_ZP_PTR+1)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
          (STA ZP_PTR)
          (STA TEMP_PTR__VM_ALLOC_CELL_PAIR_TO_ZP_PTR)

          ;; set new tree root for free tree to original cell0
          (LDY !$00)
          (LDA (ZP_PTR),y)
          (AND !$03)
          (BEQ CELL0_IS_ATOMIC__VM_ALLOC_CELL_PAIR_TO_ZP_PTR)

          ;; cell0 is a cell-ptr or cell-pair-ptr
          (LSR)
          (BCS CELL0_IS_CELL_PTR__VM_ALLOC_CELL_PAIR_TO_ZP_PTR)

          ;; cell0 is a cell-pair-ptr
          (LDA (ZP_PTR),y)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          (INY)
          (LDA (ZP_PTR),y)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
          (BNE CHECK_CELL1__VM_ALLOC_CELL_PAIR_TO_ZP_PTR) ;; since must be !=0, it cannot be on page 0 always branch!

   (label CELL0_IS_CELL_PTR__VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
          ;; cell0 is a cell-ptr => decrement cell0
          (JSR WRITE_CELLy_INTO_ZP_PTR_AND_REFCOUNT_DECR__VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
          (LDA !$00)
          ;; continue as if cell0 was atomic, since cell-ptr was handled already

   (label CELL0_IS_ATOMIC__VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
          ;; a is zero (otherwise would not have branched here)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)

   (label CHECK_CELL1__VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
          ;; check whether cell1 is atomic or ptr
          (LDY !$02)
          (LDA (ZP_PTR),y) ;; get low byte
          (AND !$03)       ;; mask out all but low 2 bits
          (BEQ CELL1_IS_ATOMIC__VM_ALLOC_CELL_PAIR_TO_ZP_PTR) ;; no need to do further deallocation

          ;; write cell1 into zp_ptr and decrement
          (JSR WRITE_CELLy_INTO_ZP_PTR_AND_REFCOUNT_DECR__VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
          ;; continue as if cell1 is atomic, since it was already handled

   (label CELL1_IS_ATOMIC__VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
          ;; restore zp_ptr to the cell-pair to be reused
          (LDA TEMP_PTR__VM_ALLOC_CELL_PAIR_TO_ZP_PTR+1)
          (STA ZP_PTR+1)
          (LDA TEMP_PTR__VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
          (STA ZP_PTR)

          (RTS)

   ;; subroutine that writes CELLy (00 = cell0, 02 = cell1) of zp_ptr into zp_ptr (overwriting it)
   ;; and does a generic ref count decr, freeing (mark for freeing) handling cell-ptr and cell-pair-ptr
   (label WRITE_CELLy_INTO_ZP_PTR_AND_REFCOUNT_DECR__VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
          (LDA (ZP_PTR),y)
          (PHA)
          (INY)
          (LDA (ZP_PTR),y)
          (STA ZP_PTR+1)
          (PLA)
          (STA ZP_PTR_TAGGED) ;; tag
          (AND !TAG_PTR_MASK)
          (STA ZP_PTR) ;; cleared from tag, => real pointer
          (JMP VM_REFCOUNT_DECR_ZP_PTR)

   (label TEMP_PTR__VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
          (word $0000)))

;; input cell ptr is in ZP_PTR
;; ---
;; put this cell into the free-list (as head)
;; the cell will then hold the previous head of the free-list
;; allocating cells will first reuse this free-list
;; option: keep count (lenght) of this list to decide when to really free a cell
;;         and add it to the free list on its respective page!
(define VM_FREE_CELL_IN_ZP_PTR
  (list
   (label VM_FREE_CELL_IN_ZP_PTR)
          ;; copy previous head of free cells into this cell
          (LDY !$00)
          (LDA VM_LIST_OF_FREE_CELLS)
          (STA (ZP_PTR),y)
          (LDA VM_LIST_OF_FREE_CELLS+1)
          (INY)
          (STA (ZP_PTR),y)

          ;; write this cell as new head into the list
          (LDA ZP_PTR)
          (STA VM_LIST_OF_FREE_CELLS)
          (LDA ZP_PTR+1)
          (STA VM_LIST_OF_FREE_CELLS+1)
          (RTS)))

;; input:  cell-pair ptr is in ZP_PTR
;; -----
;; put the cell-pair itself as new root to the free-tree
;; put the old free-tree into cell1
;; tail call free on old cell1 in this cell-pair (if not atomic, if atomic no tail call)
;; result: this cell-pair is the new root of the free-tree for cell-pairs with:
;;              cell1 = old free tree root, cell2 = non-freed (yet) original cell
(define VM_FREE_CELL_PAIR_IN_ZP_PTR
  (list
   (label VM_FREE_CELL_PAIR_IN_ZP_PTR)

          ;; check cell0
          (LDY !$00)
          (LDA (ZP_PTR),y) ;; LOWBYTE OF FIRST cell0
          (AND !$03)
          (BEQ CELL_0_ATOMIC__VM_FREE_CELL_PAIR_IN_ZP_PTR)
          ;; make sure to call free on cell0 (could be any type of cell)
          ;; remember ZP_PTR

          ;; store cell0 into TEMP_PTR__VM_FREE_CELL_PAIR_IN_ZP_PTR (for later tail call of free)
          (LDA (ZP_PTR),y)
          (STA TEMP_PTR__VM_FREE_CELL_PAIR_IN_ZP_PTR)
          (INY)
          (LDA (ZP_PTR),y)
          (STA TEMP_PTR__VM_FREE_CELL_PAIR_IN_ZP_PTR+1)

   (label CELL_0_ATOMIC__VM_FREE_CELL_PAIR_IN_ZP_PTR)
          ;; cell0 is atomic and can thus be discarded (directly)

          ;; simply add this cell-pair as head to free tree
          ;; set cell0 to point to old root
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

          ;; write original cell0 -> zp_ptr
          (LDA TEMP_PTR__VM_FREE_CELL_PAIR_IN_ZP_PTR+1)
          (BEQ DONE__VM_FREE_CELL_PAIR_IN_ZP_PTR)
          (STA ZP_PTR+1)
          (LDA TEMP_PTR__VM_FREE_CELL_PAIR_IN_ZP_PTR)
          (STA ZP_PTR)

          (LDA !$00)
          (STA TEMP_PTR__VM_FREE_CELL_PAIR_IN_ZP_PTR+1) ;; mark temp_ptr as clear

          (JMP VM_FREE_NON_ATOMIC) ;; chain call

   (label DONE__VM_FREE_CELL_PAIR_IN_ZP_PTR)
          (RTS)

   (label TEMP_PTR__VM_FREE_CELL_PAIR_IN_ZP_PTR)
          (word $0000)))

;; add the given cell-pair (in zp_ptr) to the free list of cell-pairs on its page
;; input:  zp_ptr = pointer to cell-pair that is added to the free list on its page
;; output: reduces the number of used slots in byte 0
;;         next free slot of this page is the given cell-pair
(define VM_ADD_CELL_PAIR_TO_ON_PAGE_FREE_LIST
  (list
   (label VM_ADD_CELL_PAIR_TO_ON_PAGE_FREE_LIST)
          (LDX ZP_PTR+1)
          (STX DEC_CMD__VM_ADD_CELL_PAIR_TO_ON_PAGE_FREE_LIST+2) ;; set page for dec command
          (LDA VM_FIRST_FREE_SLOT_ON_PAGE,x) ;; old first free on page
          (LDY !$00)
          (STA (ZP_PTR),y) ;; set old free to next free on this very cell
          (LDA ZP_PTR) ;; load idx within page
          (STA VM_FIRST_FREE_SLOT_ON_PAGE,x) ;; set this cell as new first free cell on page

          ;; clear refcount, too (should have been done already)
          (LSR)
          (LSR)
          (TAY);; y now pointer to refcount byte
          (LDA !$00)
          (STA ZP_PTR) ;; modify pointer such that zp_ptr points to beginning of page
          (STA (ZP_PTR),y) ;; clear refcount byte, too

   (label DEC_CMD__VM_ADD_CELL_PAIR_TO_ON_PAGE_FREE_LIST)
          (DEC $c000) ;; $c0 is overwritten by actual page
          (RTS)))


(module+ test #| use case 1 allocate, free, reallocate single cell-pair |#
  (define use-case-1-code
    (list
      (JSR VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
      (JSR VM_REFCOUNT_INCR_ZP_PTR__CELL_PAIR)
      ;; set cell2 to int 0
      (JSR VM_CELL_STACK_PUSH_INT_0)
      (JSR VM_CELL_STACK_WRITE_TOS_TO_CELL1_OF_ZP_PTR)
      ;; set cell1 to int 0
      (JSR VM_CELL_STACK_WRITE_TOS_TO_CELL0_OF_ZP_PTR)))

  (define use-case-1-a-state-after
    (run-code-in-test use-case-1-code))

  (check-equal? (memory-list use-case-1-a-state-after ZP_PTR (+ 1 ZP_PTR))
                '(#x04 #xcc)
                "zp_ptr -> $cd04 = first free cell-pair on page $cd after initialization")
  (check-equal? (memory-list use-case-1-a-state-after #xccff #xccff)
                '(#x00)
                "previous page of this type is 00 (none)")
  (check-equal? (memory-list use-case-1-a-state-after #xcc00 #xcc01)
                '(#x41 #x01)
                "page type b0100 0001 refcount for first cell-pair allocated = 1")
  (check-equal? (memory-list use-case-1-a-state-after #xcc04 #xcc07)
                '(#x00 #x00 #x00 #x00)
                "cell1=int0 cell2=int0")
  (check-equal? (memory-list use-case-1-a-state-after #xcfcc #xcfcc)
                '(#x08)
                "next free cell-pair on page $cd is at $08")

  (define use-case-1-b-code
    (append
     use-case-1-code ;; cell was allocated and set to hold int 0 in car and cdr
     (list
      ;; refcount will drop to zero
      (JSR VM_REFCOUNT_DECR_ZP_PTR__CELL_PAIR))))

  (define use-case-1-b-state-after
    (run-code-in-test use-case-1-b-code))

  (check-equal? (vm-cell-pair-free-tree->string use-case-1-b-state-after)
                "cell-pair $cc04 -> [ cell-int $0000 . cell-int $0000 ]")
  (check-equal? (memory-list use-case-1-b-state-after #xcc01 #xcc01)
                '(#x00)
                "refcount for cell-pair freed = 0")
  (check-equal? (memory-list use-case-1-b-state-after #xcfcc #xcfcc)
                '(#x08)
                "next free cell-pair on page $cd is still $08")

  (define use-case-1-c-code
    (append
     use-case-1-b-code ;; cell was freed and in the free tree
     (list
      ;; clear zp_ptr just to make sure
      (LDA !$00)
      (STA ZP_PTR)
      (STA ZP_PTR+1)

      ;; allocate a new cell (should reuse pair in free-tree)
      (JSR VM_ALLOC_CELL_PAIR_TO_ZP_PTR))))

  (define use-case-1-c-state-after
    (run-code-in-test use-case-1-c-code))

  (check-equal? (memory-list use-case-1-c-state-after ZP_PTR (+ 1 ZP_PTR))
                '(#x04 #xcc )
                "allocated cell-pair is reused cell-pair of free tree")
  (check-equal? (vm-cell-pair-free-tree->string use-case-1-c-state-after)
                "root is initial")
  (check-equal? (memory-list use-case-1-c-state-after #xcc01 #xcc01)
                '(#x00)
                "refcount for (reused) cell-pair = 1")
  (check-equal? (memory-list use-case-1-c-state-after #xcfcc #xcfcc)
                '(#x08)
                "next free cell-pair on page $cd is still $08"))

(module+ test #| use case: allocate, free, reallocate small list of cell-pairs |#
  (define use-case-2-a-code
    (list
     (JSR VM_ALLOC_CELL_PAIR_TO_ZP_PTR)                               ;; zp_ptr = freshly allocated cell (cd04)
     (JSR VM_REFCOUNT_INCR_ZP_PTR__CELL_PAIR)                       ;; ref(zp_ptr) ++ (=1)
     ;; set cdr to nil
     (JSR VM_CELL_STACK_PUSH_NIL)                           ;; cell-stack <- push nil
     (JSR VM_CELL_STACK_WRITE_TOS_TO_CELL1_OF_ZP_PTR)       ;; (cdr zp_ptr) := nil
     ;; set car to int 0
     (JSR VM_CELL_STACK_PUSH_INT_0)                         ;; cell-stack <- push int0
     (JSR VM_CELL_STACK_WRITE_TOS_TO_CELL0_OF_ZP_PTR)       ;; (car zp_ptr) := int0

     (JSR VM_COPY_PTR_TO_PTR2)                              ;; zp_ptr2 := zp_ptr

     (JSR VM_ALLOC_CELL_PAIR_TO_ZP_PTR)                               ;; zp_ptr = freshly allocated cell (cd08)
     (JSR VM_REFCOUNT_INCR_ZP_PTR__CELL_PAIR)                       ;; ref(zp_ptr) ++ (=1)
     ;; set cdr to zp_ptr2->
     (JSR VM_CELL_STACK_PUSH_ZP_PTR2)                       ;; cell-stack <- push zp_ptr2
     (JSR VM_CELL_STACK_WRITE_TOS_TO_CELL1_OF_ZP_PTR)       ;; (cdr zp_ptr) := tos (which is zp_ptr2)
     (JSR VM_CELL_STACK_POP__NO_GC)                         ;; just pop (no check, no gc)
     ;; set car to int0
     (JSR VM_CELL_STACK_WRITE_TOS_TO_CELL0_OF_ZP_PTR)       ;; (car zp_ptr) := tos (which is int0 again

     ;; now:
     ;;   zp_ptr[cc08|1] (int0 . ->[cc04|1](int0 . nil))
     ;; notation:
     ;;   [<mem-location>|<ref-count>]
     ;;   (<car-cell> . <cdr-cell>)
     ;;   intX, nil :: atomic value cells
     ;;   -> :: cell-ptr
     ))

  (define use-case-2-a-state-after
    (run-code-in-test use-case-2-a-code))

  (check-equal? (memory-list use-case-2-a-state-after ZP_PTR (+ 3 ZP_PTR))
                '(#x08 #xcc #x04 #xcc)
                "case 2a: zp_ptr -> $cc08, zp_ptr2 -> $cc04 = first two free cell-pairs on page $cc after initialization")
  (check-equal? (memory-list use-case-2-a-state-after #xcc01 #xcc0b)
                '(#x01 #x01 #x00      ;; refcounts
                  #x00 #x00 #x02 #x00 ;; tail cell
                  #x00 #x00 #x06 #xcc ;; head cell
                  )
                "case 2a: cell-pairs contain (int0 . -> next cell), (int 0 . nil)")

  (define use-case-2-b-code
    (append use-case-2-a-code ;; zp_ptr[cc08|1] (int0 . ->[cc04|1](int0 . nil))
            (list
             (JSR VM_REFCOUNT_DECR_ZP_PTR__CELL_PAIR)
             ;; now:
             ;;   free_tree -> [cc08|0] (int0 . ->[cc04|1] (int0 . nil))
             )))

  (define use-case-2-b-state-after
    (run-code-in-test use-case-2-b-code))

  (check-equal? (memory-list use-case-2-b-state-after #xcc01 #xcc0b)
                '(#x01 #x00 #x00      ;; refcounts
                  #x00 #x00 #x02 #x00 ;; cc04 (old tail of list)
                  #x00 #x00 #x06 #xcc ;; cc08 (old head of list)
                  )
                "case 2b: refcount cd08 = 0 (head), cc04 unchanged (tail)")
  (check-equal? (vm-cell-pair-free-tree->string use-case-2-b-state-after)
                "cell-pair $cc08 -> [ cell-int $0000 . cell-pair-ptr $cc04 ]")

  (define use-case-2-c-code
    (append use-case-2-b-code ;; free_tree -> [cd08|0] (int0 . ->[cd04|1] (int0 . nil))
            (list (LDA !$FF) ;; marker for debug, remove when done
                  (JSR VM_ALLOC_CELL_PAIR_TO_ZP_PTR)
                  (JSR VM_REFCOUNT_INCR_ZP_PTR__CELL_PAIR)
                  ;; now:
                  ;;   zp_ptr = [cd08|1] not initialized
                  ;;   free_tree -> [cd04|0] (int0 . nil)
                  )))

  (define use-case-2-c-state-after
    (run-code-in-test use-case-2-c-code))

  (check-equal? (memory-list use-case-2-c-state-after ZP_PTR (+ 1 ZP_PTR))
                '(#x08 #xcc)
                "case 2c: zp_ptr -> $cc08, reallocated")
  (check-equal? (memory-list use-case-2-c-state-after #xcc01 #xcc0b)
                '(#x00 #x01 #x00      ;; refcounts
                  #x00 #x00 #x02 #x00 ;; tail cell
                  #x00 #x00 #x06 #xcc ;; head cell
                  )
                "case 2c: refcount cc08 = 1 reallocated, refcount cc04 = 0 (original tail, now in the free tree)")
  (check-equal? (vm-cell-pair-free-tree->string use-case-2-c-state-after)
                "cell-pair $cc04 -> [ cell-int $0000 . cell-pair-ptr NIL ]"))

;; input:  A = size (needs to include 32 bytes cell-stack + 8 byte (pc, old params, old locals, old cell stack base ptr) + 2 * #locals
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
                  "stack frame:    $cd02"))

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
                  "stack frame:    $cd22")))

;; input:  ZP_CELL_STACK_TOS
;;         ZP_CELL_STACK_BASE_PTR
;;         ZP_PARAMS_PTR
;; output: parameter @ Y (0) is overwritten
;; TODO: check to gc overwritten
(define VM_CELL_STACK_WRITE_TOS_TO_PARAMy
  (list
   (label VM_CELL_STACK_WRITE_TOS_TO_PARAM_0)
          (LDY !$00)

   ;; ----------------------------------------
   (label VM_CELL_STACK_WRITE_TOS_TO_PARAMy)
          (STY ZP_TEMP)
          (LDY ZP_CELL_STACK_TOS)
          (LDA (ZP_CELL_STACK_BASE_PTR),y)
          (TAX)
          (DEY)
          (LDA (ZP_CELL_STACK_BASE_PTR),y)
          (LDY ZP_TEMP)
          (STA (ZP_PARAMS_PTR),y)
          (TXA)
          (INY)
          (STA (ZP_PARAMS_PTR),y)
          (RTS)))

;; input:  ZP_CELL_STACK_TOS
;;         ZP_CELL_STACK_BASE_PTR
;;         ZP_LOCALS_PTR
;; output: local @ Y (0) is overwritten
;; TODO: check to gc overwritten
(define VM_CELL_STACK_WRITE_TOS_TO_LOCALy
  (list
   (label VM_CELL_STACK_WRITE_TOS_TO_LOCAL_0)
          (LDY !$00)

   ;; ----------------------------------------
   (label VM_CELL_STACK_WRITE_TOS_TO_LOCALy)
          (STY ZP_TEMP)
          (LDY ZP_CELL_STACK_TOS)
          (LDA (ZP_CELL_STACK_BASE_PTR),y)
          (TAX)
          (DEY)
          (LDA (ZP_CELL_STACK_BASE_PTR),y)
          (LDY ZP_TEMP)
          (STA (ZP_LOCALS_PTR),y)
          (TXA)
          (INY)
          (STA (ZP_LOCALS_PTR),y)
          (RTS)))


;; input:   A = number of parameters on the stack to be used in this call frame
;;          x = number of locals to allocate on call frame
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
          (ASL A) ;; # params * 2 (number of bytes)
          (STA ZP_TEMP)
          (LDA ZP_CELL_STACK_TOS)
          (SEC)
          (SBC ZP_TEMP)
          (STA ZP_TEMP) ;; keep offset to cell-stack-base-ptr for new parameter-ptr

          ;; copy 8 bytes
          (LDY !$07)
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
          (ADC !$08)
          (STA ZP_LOCALS_PTR)

          (TXA)
          (ASL A) ;; # of locals *2 = bytes
          (ADC ZP_LOCALS_PTR)
          (STA ZP_CELL_STACK_BASE_PTR)

          (LDA !$ff)
          (STA ZP_CELL_STACK_TOS) ;; set this one to 0

          (RTS)))

(module+ test #| vm_save_exec_state_to_call_frame |#

  ;; before
  ;; cd04 <- ZP_PARAMS_PTR
  ;; cd08 <- ZP_CALL_FRAME
  ;; cd10 <- ZP_LOCALS_PTR
  ;; cd16 <- ZP_CELL_STACK_BASE_PTR
  ;; cd1b <- ZP_CELL_STACK_TOS (base ptr + 05, 3 elements on stack)
  ;; cc06 <- ZP_VM_PC

  ;; after
  ;; cd18 <- ZP_PARAMS_PTR (2 params, cd18..cd19, cd1a..cd1b)
  ;; cd1c <- ZP_CALL_FRAME
  ;; cd24 <- ZP_LOCALS_PTR
  ;; cd2a <- ZP_CELL_STACK_BASE_PTR
  ;; cd29 <- ZP_CELL_STACK_TOS ($ff)
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
     (INX)
     (STX ZP_VM_PC)     ;; $17

     (LDX ZP_CALL_FRAME+1) ;; $cd
     (STX ZP_PARAMS_PTR+1)
     (STX ZP_LOCALS_PTR+1)
     (STX ZP_CELL_STACK_BASE_PTR+1)
     (DEX)                      ;; $cc
     (STX ZP_VM_PC+1)

     ;; allocate new call frame space that can hold $20 bytes
     (LDA !$20)
     (JSR VM_ALLOC_CALL_FRAME)

     (LDA !$02) ;; two params
     (LDX !$03) ;; three locals
     (JSR VM_SAVE_EXEC_STATE_TO_CALL_FRAME)
     ))

  (define test-save-exec-state-state-after
    (run-code-in-test test-save-exec-state-code))

  (check-equal? (vm-page->strings test-save-exec-state-state-after #xcd)
                '("page-type:      call-frame page"
                  "stack frame:    $cd1c"))
  (check-equal? (memory-list test-save-exec-state-state-after ZP_PARAMS_PTR (add1 ZP_PARAMS_PTR))
                (list #x18 #xcd))
  (check-equal? (memory-list test-save-exec-state-state-after ZP_LOCALS_PTR (add1 ZP_LOCALS_PTR))
                (list #x24 #xcd))
  (check-equal? (memory-list test-save-exec-state-state-after ZP_CELL_STACK_BASE_PTR (add1 ZP_CELL_STACK_BASE_PTR))
                (list #x2a #xcd))
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
;;         zp_vm_pc
(define VM_POP_CALL_FRAME
  (list
   (label VM_POP_CALL_FRAME)
          (JSR VM_CELL_STACK_WRITE_TOS_TO_PARAM_0)

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

          (LDY !$07) ;; 07..00 offset (8 bytes are copied)

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
          (INC ZP_CELL_STACK_TOS) ;; point to the tagged lowbyte

          ;; set ZP_CALL_FRAME
          (LDA ZP_LOCALS_PTR+1)
          (STA ZP_CALL_FRAME+1)
          (LDA ZP_LOCALS_PTR)
          (SEC)
          (SBC !$08)
          (STA ZP_CALL_FRAME)

          (RTS)))

(module+ test #| vm_pop_call_frame |#
  ;; before
  ;; cd18 <- ZP_PARAMS_PTR (2 params, cd18..cd19, cd1a..cd1b)
  ;; cd1c <- ZP_CALL_FRAME
  ;; cd24 <- ZP_LOCALS_PTR
  ;; cd2a <- ZP_CELL_STACK_BASE_PTR
  ;; cd29 <- ZP_CELL_STACK_TOS ($ff)
  ;; cc06 <- ZP_VM_PC (unchanged)

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
     (LDX !$05)
     (STX ZP_CELL_STACK_TOS)
     (INX)
     (STX ZP_VM_PC)     ;; $17

     (LDX ZP_CALL_FRAME+1) ;; $cd
     (STX ZP_PARAMS_PTR+1)
     (STX ZP_LOCALS_PTR+1)
     (STX ZP_CELL_STACK_BASE_PTR+1)
     (DEX)                      ;; $cc
     (STX ZP_VM_PC+1)

     ;; allocate new call frame space that can hold $20 bytes
     (LDA !$20)
     (JSR VM_ALLOC_CALL_FRAME)

     (LDA !$02) ;; two params
     (LDX !$03) ;; three locals
     (JSR VM_SAVE_EXEC_STATE_TO_CALL_FRAME)

     (JSR VM_CELL_STACK_PUSH_INT_1)

     (JSR VM_POP_CALL_FRAME)
     ))

  (define test-pop-call-frame-state-after
    (run-code-in-test test-pop-call-frame-code))

  (check-equal? (memory-list test-pop-call-frame-state-after ZP_PARAMS_PTR (add1 ZP_PARAMS_PTR))
                (list #x04 #xcd))
  (check-equal? (memory-list test-pop-call-frame-state-after ZP_CALL_FRAME (add1 ZP_CALL_FRAME))
                (list #x08 #xcd))
  (check-equal? (memory-list test-pop-call-frame-state-after ZP_LOCALS_PTR (add1 ZP_LOCALS_PTR))
                (list #x10 #xcd))
  (check-equal? (memory-list test-pop-call-frame-state-after ZP_CELL_STACK_BASE_PTR (add1 ZP_CELL_STACK_BASE_PTR))
                (list #x16 #xcd))
  (check-equal? (memory-list test-pop-call-frame-state-after ZP_CELL_STACK_TOS ZP_CELL_STACK_TOS)
                (list #x03))
  (check-equal? (memory-list test-pop-call-frame-state-after ZP_VM_PC (add1 ZP_VM_PC))
                (list #x06 #xcc))
  (check-equal? (take (vm-stack->strings test-pop-call-frame-state-after) 2)
                '("stack holds 2 items" ;; 3 before, 2 were parameters => 1 old + 1 result (old tos)
                  "cell-int $0001")))


;; ----------------------------------------
;; page type slot w/ different sizes (refcount @ ptr-1) x cells
;; math: first entry @FIRST_REF_COUNT_OFFSET__VM_ALLOC_PAGE_FOR_M1_SLOTS + 1, refcount @ -1, next slot += INC_TO_NEXT_SLOT__VM_ALLOC_PAGE_FOR_M1_SLOTS, slot-size = INC_TO_NEXT_SLOT__VM_ALLOC_PAGE_FOR_M1_SLOTS -1
;; input : X = profile offset (0, 2, 4 ...)
;; uses  : ZP_PTR2
;; output:
(define VM_ALLOC_PAGE_FOR_M1_SLOTS
  (list
   (label VM_ALLOC_PAGE_FOR_M1_SLOTS)
          (STX SEL_PROFILE__VM_ALLOC_PAGE_FOR_M1_SLOTS)      ;; save profile index in local var

          (JSR VM_ALLOC_PAGE__PAGE_UNINIT)
          (STA ZP_PTR2+1)
          (LDA !$00)
          (STA ZP_PTR2)

          (LDY !$00)
          (LDX SEL_PROFILE__VM_ALLOC_PAGE_FOR_M1_SLOTS) ;; profile = 0..3
          (TXA)
          (ORA !$10)
          (STA (ZP_PTR2),y) ;; set page type in byte 0 to b0001 <profile>

          (LDA VM_FREE_M1_PAGE_P0,x) ;; current free page
          (INY)
          (STA (ZP_PTR2),y)          ;; store previous page

          (LDA ZP_PTR2+1)
          (STA VM_FREE_M1_PAGE_P0,x) ;; set page with free slots to this allocated page

          (LDA !$00)
          (INY)
          (STA (ZP_PTR2),y)          ;; store number of slots used

          (LDY FIRST_REF_COUNT_OFFSET__VM_ALLOC_PAGE_FOR_M1_SLOTS,x) ;; y = refcount field for first slot
          (INY)
          (TYA)
          (LDX ZP_PTR2+1)
          (STA VM_FIRST_FREE_SLOT_ON_PAGE,x)                    ;; set first free slot, here x = page
          (DEY)
          (LDX SEL_PROFILE__VM_ALLOC_PAGE_FOR_M1_SLOTS) ;; profile = 0..3
          (LDA !$00)

          ;; loop to initialize refcounts of each slot to 0-
          (label REF_COUNT_LOOP__VM_ALLOC_PAGE_FOR_M1_SLOTS)
          (STA (ZP_PTR2),y) ;; refcount = 0
          (TYA)
          (CLC)
          (ADC INC_TO_NEXT_SLOT__VM_ALLOC_PAGE_FOR_M1_SLOTS,x) ;; calc next refcount field offset
          (BCS END_REF_COUNT_LOOP__VM_ALLOC_PAGE_FOR_M1_SLOTS)
          (TAY)
          (ADC !$01)
          (LDA !$00)
          (BCC REF_COUNT_LOOP__VM_ALLOC_PAGE_FOR_M1_SLOTS) ;; still on this page?

   (label END_REF_COUNT_LOOP__VM_ALLOC_PAGE_FOR_M1_SLOTS)
          ;; loop to write free slot list
          (LDY FIRST_REF_COUNT_OFFSET__VM_ALLOC_PAGE_FOR_M1_SLOTS,x)
          (INY)  ;; first slot  (refcount field offset + 1)
          (TYA)
   (label WRITE_FREE_LIST__VM_ALLOC_PAGE_FOR_M1_SLOTS)
          (CLC)
          (ADC INC_TO_NEXT_SLOT__VM_ALLOC_PAGE_FOR_M1_SLOTS,x)
          (BCS ALMOST_DONE__VM_ALLOC_PAGE_FOR_M1_SLOTS) ;; no longer on the same page => almost done
          (STA (ZP_PTR2),y) ;; offset of next free cell == y for next write
          (TAY)
          (BCC WRITE_FREE_LIST__VM_ALLOC_PAGE_FOR_M1_SLOTS) ;; carry must be clear => always jump

   (label ALMOST_DONE__VM_ALLOC_PAGE_FOR_M1_SLOTS)
          (LDA !$00)
          (STA (ZP_PTR2),y) ;; last offset to next free slot is 00 = no next free slot!

          (RTS)

   (label SEL_PROFILE__VM_ALLOC_PAGE_FOR_M1_SLOTS)
          (byte $00) ;; local var

   (label FIRST_REF_COUNT_OFFSET__VM_ALLOC_PAGE_FOR_M1_SLOTS)
          (byte $03) ;; first ref count is 03, add 12 to get to next slot, slot size $11 (17), contains 14 slots
          (byte $0f) ;; first ref count is 0f, add 1e to get to next slot, slot size $1d (29), contains 8 slots
          (byte $05) ;; first ref count is 05, add 32 to get to next slot, slot-size $31 (49), contains 5 slots
          (byte $03) ;; first ref count is 03, add 54 to get to next slot, slot-size $53 (83), contains 3 slots

   (label INC_TO_NEXT_SLOT__VM_ALLOC_PAGE_FOR_M1_SLOTS)
          (byte $12) ;; add 12 to get to next slot, slot size $11 (17), contains 14 slots
          (byte $1e) ;; add 1e to get to next slot, slot size $1d (29), contains 8 slots
          (byte $32) ;; add 32 to get to next slot, slot-size $31 (49), contains 5 slots
          (byte $54) ;; add 54 to get to next slot, slot-size $53 (83), contains 3 slots

          ))

(module+ test #| vm_alloc_m1_page |#
  (define test-alloc-m1-01-code
    (list
     ;; fill page with $ff
            (LDA !$FF)
            (LDX !$00)
     (label LOOP__TEST_ALLOC_M1_01_CODE)
            (DEX)
            (STA $cc00,x)
            (BNE LOOP__TEST_ALLOC_M1_01_CODE)

            ;; now allocate the page
            (LDX !$00) ;; do it explicitly
            (JSR VM_ALLOC_PAGE_FOR_M1_SLOTS)))

  (define test-alloc-m1-01-state-after
    (run-code-in-test test-alloc-m1-01-code))

  (check-equal? (vm-page->strings test-alloc-m1-01-state-after #xcc)
                '("page-type:      m1 page p0"
                  "previous page:  $00"
                  "slots used:     0"
                  "next free slot: $04"))
  (check-equal? (memory-list test-alloc-m1-01-state-after #xcc03 #xcc04)
                (list #x00 #x16)
                "slot0: refcount 0, next free slot at offset $16")
  (check-equal? (memory-list test-alloc-m1-01-state-after #xcc15 #xcc16)
                (list #x00 #x28)
                "slot1: refcount 0, next free slot at offset $28")
  (check-equal? (memory-list test-alloc-m1-01-state-after #xcc27 #xcc28)
                (list #x00 #x3a)
                "slot2: refcount 0, next free slot at offset $28")
  (check-equal? (memory-list test-alloc-m1-01-state-after #xcced #xccee)
                (list #x00 #x00)
                "slot13: refcount 0, next free slot at offset $00 = no next")

  (define test-alloc-m1-02-code
    (list
     ;; fill page with $ff
            (LDA !$FF)
            (LDX !$00)
     (label LOOP__TEST_ALLOC_M1_02_CODE)
            (DEX)
            (STA $cc00,x)
            (BNE LOOP__TEST_ALLOC_M1_02_CODE)

            ;; now allocate the page
            (LDX !$01) ;; do it explicitly
            (JSR VM_ALLOC_PAGE_FOR_M1_SLOTS)))

  (define test-alloc-m1-02-state-after
    (run-code-in-test test-alloc-m1-02-code))

  (check-equal? (vm-page->strings test-alloc-m1-02-state-after #xcc)
                '("page-type:      m1 page p1"
                  "previous page:  $00"
                  "slots used:     0"
                  "next free slot: $10"))
  (check-equal? (memory-list test-alloc-m1-02-state-after #xcc0f #xcc10)
                (list #x00 #x2e)
                "slot0: refcount 0, next free slot at offset $2c")
  (check-equal? (memory-list test-alloc-m1-02-state-after #xcc2d #xcc2e)
                (list #x00 #x4c)
                "slot1: refcount 0, next free slot at offset $4a")
  (check-equal? (memory-list test-alloc-m1-02-state-after #xcc4b #xcc4c)
                (list #x00 #x6a)
                "slot2: refcount 0, next free slot at offset $68")
  (check-equal? (memory-list test-alloc-m1-02-state-after #xcce1 #xcce2)
                (list #x00 #x00)
                "slot7: refcount 0, next free slot at offset $00 = no next")

  (define test-alloc-m1-03-code
    (list
     ;; fill page with $ff
            (LDA !$FF)
            (LDX !$00)
     (label LOOP__TEST_ALLOC_M1_03_CODE)
            (DEX)
            (STA $cc00,x)
            (BNE LOOP__TEST_ALLOC_M1_03_CODE)

            ;; now allocate the page
            (LDX !$02) ;; do it explicitly
            (JSR VM_ALLOC_PAGE_FOR_M1_SLOTS)))

  (define test-alloc-m1-03-state-after
    (run-code-in-test test-alloc-m1-03-code))

  (check-equal? (vm-page->strings test-alloc-m1-03-state-after #xcc)
                '("page-type:      m1 page p2"
                  "previous page:  $00"
                  "slots used:     0"
                  "next free slot: $06"))
  (check-equal? (memory-list test-alloc-m1-03-state-after #xcc05 #xcc06)
                (list #x00 #x38)
                "slot0: refcount 0, next free slot at offset $38")
  (check-equal? (memory-list test-alloc-m1-03-state-after #xcc37 #xcc38)
                (list #x00 #x6a)
                "slot1: refcount 0, next free slot at offset $6a")
  (check-equal? (memory-list test-alloc-m1-03-state-after #xcc69 #xcc6a)
                (list #x00 #x9c)
                "slot2: refcount 0, next free slot at offset $9c")
  (check-equal? (memory-list test-alloc-m1-03-state-after #xcccd #xccce)
                (list #x00 #x00)
                "slot4: refcount 0, next free slot at offset $00 = no next")

  (define test-alloc-m1-04-code
    (list
     ;; fill page with $ff
            (LDA !$FF)
            (LDX !$00)
     (label LOOP__TEST_ALLOC_M1_04_CODE)
            (DEX)
            (STA $cc00,x)
            (BNE LOOP__TEST_ALLOC_M1_04_CODE)

            ;; now allocate the page
            (LDX !$03) ;; do it explicitly
            (JSR VM_ALLOC_PAGE_FOR_M1_SLOTS)))

  (define test-alloc-m1-04-state-after
    (run-code-in-test test-alloc-m1-04-code))

  (check-equal? (memory-list test-alloc-m1-04-state-after #xcc00 #xcc02)
                (list #x13 #x00 #x00)
                "page type $13, previous page = $00, slot number used = $00")
  (check-equal? (memory-list test-alloc-m1-04-state-after #xcc03 #xcc04)
                (list #x00 #x58)
                "slot0: refcount 0, next free slot at offset $56")
  (check-equal? (memory-list test-alloc-m1-04-state-after #xcc57 #xcc58)
                (list #x00 #xac)
                "slot1: refcount 0, next free slot at offset $aa")
  (check-equal? (memory-list test-alloc-m1-04-state-after #xccab #xccac)
                (list #x00 #x00)
                "slot2: refcount 0, next free slot at offset $00 = no next")
  (check-equal? (vm-page->strings test-alloc-m1-04-state-after #xcc)
                '("page-type:      m1 page p3"
                  "previous page:  $00"
                  "slots used:     0"
                  "next free slot: $04")))

;; allocate a slot of min A size, allocating a new page if necessary
;; input:  A = size
;; output: ZP_PTR2 = available slot of the given size (or a bit more)
;;         Y = actual size
(define VM_ALLOC_M1_SLOT_TO_ZP_PTR2
  (list
   (label VM_ALLOC_M1_SLOT_TO_ZP_PTR2)
          (LDX !$00)
          (CMP INC_TO_NEXT_SLOT__VM_ALLOC_PAGE_FOR_M1_SLOTS+0)
          (BPL J17PLUS__VM_ALLOC_SLOT_IN_BUCKET)

   (label VM_ALLOC_SLOT__TYPE_X_STORE)
          (STX PAGE_TYPE_IDX__VM_ALLOC_SLOT_IN_BUCKET)
          (JSR VM_REMOVE_FULL_PAGE_FOR_TYPE_X_SLOTS)

   (label VM_ALLOC_SLOT_TYPE_X)
          (LDA VM_FREE_M1_PAGE_P0,x) ;;
          (BEQ NEW_PAGE__VM_ALLOC_SLOT_TYPE_X)     ;; if the current free page is $00 (there is no page marked as having free slots) => allocate new page

          ;; ensure zp_ptr2 points into the page
          (STA ZP_PTR2+1)
          (STA INC_CMD__VM_ALLOC_SLOT_TYPE_X+2)
          (TAX)
          (LDY VM_FIRST_FREE_SLOT_ON_PAGE,x)           ;; first free slot offset
          (BEQ NEW_PAGE__VM_ALLOC_SLOT_TYPE_X)    ;; if =0 allocate new page (no more free slots on this page)
          ;; ensure zp_ptr2 points to the slot!

   (label CONTINUE__VM_ALLOC_SLOT_TYPE_X)
          (STY ZP_PTR2)

          ;; now get the next free slot (from linked list in this page)
          (LDY !$00)
          (LDA (ZP_PTR2),y) ;; content of free slot points to the next free one (or 00)
          (STA VM_FIRST_FREE_SLOT_ON_PAGE,x)           ;; set next free slot for this page (x is still page)

          ;; ensure y holds the actual available slot size
          (LDX PAGE_TYPE_IDX__VM_ALLOC_SLOT_IN_BUCKET)
          (LDY INC_TO_NEXT_SLOT__VM_ALLOC_PAGE_FOR_M1_SLOTS,x)
          (DEY)

   (label INC_CMD__VM_ALLOC_SLOT_TYPE_X)
          (INC $c002) ;; $c0 is overwritten with current page (increases the number of slots actually used)

          (RTS)

   (label FIND_NEXT_FREE_PAGE__VM_ALLOC_SLOT_TYPE_X)     ;; current page is full, search first non full (or end of list)
          ;; A = page, X = page, Y = 0
          (STA NEXT_PAGE_CMD__VM_ALLOC_SLOT_TYPE_X+2)

   (label NEXT_PAGE_CMD__VM_ALLOC_SLOT_TYPE_X)
          (LDA $C001) ;; $c0 is overwritten
          (BEQ NEW_PAGE__VM_ALLOC_SLOT_TYPE_X) ;; next page ptr = $00 => end reached, no more pages
          ;; check whether this page is full
          (TAX)
          (LDY VM_FIRST_FREE_SLOT_ON_PAGE,x)
          (BEQ FIND_NEXT_FREE_PAGE__VM_ALLOC_SLOT_TYPE_X) ;; next free slot for page is 00 => page is full, try to find next
          ;; page is not full => this is the new head
          (LDX PAGE_TYPE_IDX__VM_ALLOC_SLOT_IN_BUCKET)
          (STA VM_FREE_M1_PAGE_P0,x)
          (STA ZP_PTR2+1)
          (CLC)
          (BCC CONTINUE__VM_ALLOC_SLOT_TYPE_X)

   (label NEW_PAGE__VM_ALLOC_SLOT_TYPE_X)               ;; allocate a complete new page for page type x or find a page in the list that has free slots
          (LDX PAGE_TYPE_IDX__VM_ALLOC_SLOT_IN_BUCKET)
          (JSR VM_ALLOC_PAGE_FOR_M1_SLOTS)
          (LDX PAGE_TYPE_IDX__VM_ALLOC_SLOT_IN_BUCKET)
          (CLC)
          (BCC VM_ALLOC_SLOT_TYPE_X)

   (label J17PLUS__VM_ALLOC_SLOT_IN_BUCKET)
          (CMP INC_TO_NEXT_SLOT__VM_ALLOC_PAGE_FOR_M1_SLOTS+1)
          (BPL J29PLUS__VM_ALLOC_SLOT_IN_BUCKET)
          (LDX !$01)
          (BNE VM_ALLOC_SLOT__TYPE_X_STORE)

   (label J29PLUS__VM_ALLOC_SLOT_IN_BUCKET)
          (CMP INC_TO_NEXT_SLOT__VM_ALLOC_PAGE_FOR_M1_SLOTS+2)
          (BPL J49PLUS__VM_ALLOC_SLOT_IN_BUCKET)
          (LDX !$02)
          (BNE VM_ALLOC_SLOT__TYPE_X_STORE)

   (label J49PLUS__VM_ALLOC_SLOT_IN_BUCKET)
          (CMP INC_TO_NEXT_SLOT__VM_ALLOC_PAGE_FOR_M1_SLOTS+3)
          (BPL J83PLUS__VM_ALLOC_SLOT_IN_BUCKET)
          (LDX !$03)
          (BNE VM_ALLOC_SLOT__TYPE_X_STORE)

   (label J83PLUS__VM_ALLOC_SLOT_IN_BUCKET)
          ;; error, no slot this large can be allocated
          (BRK)

   (label PAGE_TYPE_IDX__VM_ALLOC_SLOT_IN_BUCKET)
          (byte $00) ;; local variable holding the selected page typ (0 = slots up to 17 bytes, 2 up to 29 bytes ...)
          ))

(module+ test #| vm_alloc_bucket_slot, allocate one slot of size $0b |#
  (define test-alloc-bucket-slot-code
    (list
     ;; fill page with $ff
            (LDA !$FF)
            (LDX !$00)
     (label LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
            (DEX)
            (STA $cc00,x)
            (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)

            ;; now allocate the page
            (LDA !$0b) ;; want slot of size 11
            (JSR VM_ALLOC_M1_SLOT_TO_ZP_PTR2)

            (LDA VM_FREE_M1_PAGE_P0+0) ;; type 0
            (STA ZP_TEMP)))

  (define test-alloc-bucket-slot-state-after
    (run-code-in-test test-alloc-bucket-slot-code))

  (check-equal? (memory-list test-alloc-bucket-slot-state-after #xcc03 #xcc04)
                (list #x00 #x16)
                "slot0: refcount 0, next free slot at offset $16")
  (check-equal? (memory-list test-alloc-bucket-slot-state-after ZP_PTR2 (add1 ZP_PTR2))
                (list #x04 #xcc)
                "allocated slot is at cc04")
  (check-equal? (memory-list test-alloc-bucket-slot-state-after ZP_TEMP ZP_TEMP)
                (list #xcc)
                "free page for slot type 0 is $cc")
  (check-equal? (vm-page->strings test-alloc-bucket-slot-state-after #xcc)
                '("page-type:      m1 page p0"
                  "previous page:  $00"
                  "slots used:     1"
                  "next free slot: $16")))

(module+ test #| vm_alloc_bucket_slot 2 times slot size $0b and $09 |#
  (define test-alloc-bucket-slot-2x-code
    (list
     ;; fill page with $ff
     (LDA !$FF)
     (LDX !$00)
     (label LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
     (DEX)
     (STA $cc00,x)
     (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)

     ;; now allocate the page
     (LDA !$0b) ;; want slot of size 11
     (JSR VM_ALLOC_M1_SLOT_TO_ZP_PTR2)
     (LDA !$09) ;; want slot of size 9, should be on the same page
     (JSR VM_ALLOC_M1_SLOT_TO_ZP_PTR2)

     (LDA VM_FREE_M1_PAGE_P0+0) ;; type 0
     (STA ZP_TEMP)))

  (define test-alloc-bucket-slot-2x-state-after
    (run-code-in-test test-alloc-bucket-slot-2x-code))

  (check-equal? (memory-list test-alloc-bucket-slot-2x-state-after #xcc15 #xcc16)
                (list #x00 #x28)
                "slot1: refcount 0, next free slot at offset $28")
  (check-equal? (memory-list test-alloc-bucket-slot-2x-state-after ZP_PTR2 (add1 ZP_PTR2))
                (list #x16 #xcc)
                "allocated slot is at cc16")
  (check-equal? (memory-list test-alloc-bucket-slot-2x-state-after ZP_TEMP ZP_TEMP)
                (list #xcc)
                "free page for slot type 0 is $cc")
  (check-equal? (vm-page->strings test-alloc-bucket-slot-2x-state-after #xcc)
                '("page-type:      m1 page p0"
                  "previous page:  $00"
                  "slots used:     2"
                  "next free slot: $28")))

(module+ test #| vm_alloc_bucket_slot, alloc 10 x slot size $14 (actual $20)  |#
  (define test-alloc-bucket-slot-xx-code
    (list
     ;; fill page with $ff
     (LDA !$FF)
     (LDY !$02) ;; 2 pages
     (LDX !$00) ;; 256 bytes
     (label LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
     (DEX)
     (STA $cb00,x)
     (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
     (DEY)
     (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)

     ;; loop over ...
     (LDA !$0a)
     (STA LOOP_NUM__TEST_ALLOC_BUCKET_SLOT_XX)

     (label LOOP__TEST_ALLOC_BUCKET_SLOT_XX)
     (LDA !$14) ;; want slot of size 20
     (JSR VM_ALLOC_M1_SLOT_TO_ZP_PTR2) ;; ... slot allocation
     (DEC LOOP_NUM__TEST_ALLOC_BUCKET_SLOT_XX)
     (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_XX)

     (JMP TAIL__TEST_ALLOC_BUCKET_SLOT_XX)

     (label LOOP_NUM__TEST_ALLOC_BUCKET_SLOT_XX)
     (byte $20)


     (label TAIL__TEST_ALLOC_BUCKET_SLOT_XX)
     (LDA VM_FREE_M1_PAGE_P0+1) ;; type 1
     (STA ZP_TEMP)))

  (define test-alloc-bucket-slot-xx-state-after
    (run-code-in-test test-alloc-bucket-slot-xx-code))

  (check-equal? (memory-list test-alloc-bucket-slot-xx-state-after ZP_PTR2 (add1 ZP_PTR2))
                (list #x2e #xcb)
                "allocated slot is at cb2e (slot1 on page 2)")
  (check-equal? (memory-list test-alloc-bucket-slot-xx-state-after #xcb4b #xcb4c)
                (list #x00 #x6a)
                "first free slot page2: refcount 0, next free slot at offset $6a")
  (check-equal? (memory-list test-alloc-bucket-slot-xx-state-after ZP_TEMP ZP_TEMP)
                (list #xcb)
                "free page for slot type 1 is $cb")
  (check-equal? (vm-page->strings test-alloc-bucket-slot-xx-state-after #xcc)
                '("page-type:      m1 page p1"
                  "previous page:  $00"
                  "slots used:     8"
                  "next free slot: $00"))
  (check-equal? (vm-page->strings test-alloc-bucket-slot-xx-state-after #xcb)
                '("page-type:      m1 page p1"
                  "previous page:  $00"
                  "slots used:     2"
                  "next free slot: $4c")))
  ;; free-page for slot type 0 = cc


;; inc ref count bucket slot
;; dec ref count bucket slot

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
          ;; make sure to remove fulls from free page list first !!
          (JSR VM_REMOVE_FULL_PAGES_FOR_PTR2_SLOTS)

          ;; now free the slot
   (label REGULAR_FREE__VM_FREE_M1_SLOT_IN_ZP_PTR2)
          (LDX ZP_PTR2+1)
          (STX DEC_CMD__VM_FREE_M1_SLOT_IN_ZP_PTR2+2)    ;; write page for later dec execution
          (LDA VM_FIRST_FREE_SLOT_ON_PAGE,x)           ;; first free slot offset
          (BNE CONTINUE__VM_FREE_M1_SLOT_IN_ZP_PTR2)     ;; regular free

          ;; this page was full (since next free slot was 0) => register with the list of pages with free slots
          (JSR VM_ENQUEUE_PAGE_AS_HEAD_FOR_PTR2_SLOTS)
          (LDX DEC_CMD__VM_FREE_M1_SLOT_IN_ZP_PTR2+2)    ;; restore x
          (LDA !$00)                              ;; next free slot offset (=0)

   (label CONTINUE__VM_FREE_M1_SLOT_IN_ZP_PTR2)
          (LDY !$00)
          (STA (ZP_PTR2),y)                       ;; set (zp_ptr) = previous free
          (LDA ZP_PTR2)                           ;; low byte of pointer = new free slot
          (STA VM_FIRST_FREE_SLOT_ON_PAGE,x)           ;; set new first free slot offset

          (DEC ZP_PTR2)                           ;; now points to ref count
          (TYA)                                   ;; y is still 0 => a := 0
          (STA (ZP_PTR2),y)                       ;; set refcount := 0

   (label DEC_CMD__VM_FREE_M1_SLOT_IN_ZP_PTR2)
          (DEC $c002)                             ;; $c0 is overwritten

          (RTS)))

(module+ test #| vm_free_bucket_slot  allocate two slots, free first slot |#
  (define test-free-bucket-slot-code
    (list
     ;; fill page with $ff
            (LDA !$FF)
            (LDX !$00)
     (label LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
            (DEX)
            (STA $cc00,x)
            (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)

            ;; now allocate the page
            (LDA !$0b) ;; want slot of size 11
            (JSR VM_ALLOC_M1_SLOT_TO_ZP_PTR2)
            (JSR VM_COPY_PTR2_TO_PTR)

            (LDA !$09) ;; want slot of size 9, should be on the same page
            (JSR VM_ALLOC_M1_SLOT_TO_ZP_PTR2)

            (JSR VM_COPY_PTR_TO_PTR2)
            (JSR VM_FREE_M1_SLOT_IN_ZP_PTR2)))

  (define test-free-bucket-slot-state-after
    (run-code-in-test test-free-bucket-slot-code))

  (check-equal? (memory-list test-free-bucket-slot-state-after #xcc03 #xcc04)
                (list #x00 #x28)
                "slot0 (now free): refcount 0, next free slot at offset $28")
  (check-equal? (vm-page->strings test-free-bucket-slot-state-after #xcc)
                '("page-type:      m1 page p0"
                  "previous page:  $00"
                  "slots used:     1"
                  "next free slot: $04")))

(module+ test #| vm_free_bucket_slot  allocate 16 slots, free first slot |#
  (define test-free-bucket-a20-slot-code
    (list
     ;; fill page with $ff
            (LDY !$03) ;; fill two pages
            (LDA !$FF) ;; with $ff
            (LDX !$00) ;; each 256 bytes long
     (label LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
            (DEX)
            (STA $cb00,x) ;; starting at $cb00
            (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
            (DEY)
            (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)

            ;; now allocate the page
            (LDA !$17)
            (STA LOOP_VAR__TEST_FREE_BUCKET_A20_SLOT_CODE)
     (label LOOP__TEST_FREE_BUCKET_A20_SLOT_CODE)
            (LDA !$14) ;; want slot of size 14 (max size $1e)
            (JSR VM_ALLOC_M1_SLOT_TO_ZP_PTR2)
            (DEC LOOP_VAR__TEST_FREE_BUCKET_A20_SLOT_CODE)
            (BPL LOOP__TEST_FREE_BUCKET_A20_SLOT_CODE)
            (JMP CONT__TEST_FREE_BUCKET_A20_SLOT_CODE )
     (label LOOP_VAR__TEST_FREE_BUCKET_A20_SLOT_CODE)
            (byte $00)

     (label CONT__TEST_FREE_BUCKET_A20_SLOT_CODE)
            ;; select first pointer
            (LDA !$cb)
            (STA ZP_PTR2+1)
            (LDA !$10)
            (STA ZP_PTR2)
            (JSR VM_FREE_M1_SLOT_IN_ZP_PTR2)

            (LDA !$cc)
            (STA ZP_PTR2+1)
            (LDA !$10)
            (STA ZP_PTR2)
            (JSR VM_FREE_M1_SLOT_IN_ZP_PTR2)
            ))

  (define test-free-bucket-a20-slot-state-after
    (run-code-in-test test-free-bucket-a20-slot-code))

  (check-equal? (memory-list test-free-bucket-a20-slot-state-after #xcec7 #xceca)
                (list #x00 #xcc #x00 #x00)
                "first free page of profiles 0, 1, 2, 3 is $cc for page profile 1")
  (check-equal? (vm-page->strings test-free-bucket-a20-slot-state-after #xca)
                '("page-type:      m1 page p1"
                  "previous page:  $00" ;; is removed, since full
                  "slots used:     8"
                  "next free slot: $00"))
  (check-equal? (vm-page->strings test-free-bucket-a20-slot-state-after #xcb)
                '("page-type:      m1 page p1"
                  "previous page:  $00" ;; is the last in list
                  "slots used:     7"
                  "next free slot: $10"))
  (check-equal? (vm-page->strings test-free-bucket-a20-slot-state-after #xcc)
                '("page-type:      m1 page p1"
                  "previous page:  $cb" ;; next free
                  "slots used:     7"
                  "next free slot: $10")))

(define VM_REFCOUNT_INCR_ZP_PTR__M1_SLOT
  (list
   (label VM_REFCOUNT_INCR_ZP_PTR__M1_SLOT)
          (DEC ZP_PTR)
          (LDY !$00)
          (LDA (ZP_PTR),y)
          (CLC)
          (ADC !$01)
          (STA (ZP_PTR),y)
          (INC ZP_PTR)
          (RTS)))

(module+ test #| vm_inc_ref_bucket_slot |#
  (define test-inc-ref-bucket-slot-1-code
    (list
     (LDA !$a0)
     (STA $a003)
     (STA ZP_PTR+1)
     (LDA !$04)
     (STA ZP_PTR)

     (JSR VM_REFCOUNT_INCR_ZP_PTR__M1_SLOT)))

  (define test-inc-ref-bucket-slot-1-state-after
    (run-code-in-test test-inc-ref-bucket-slot-1-code))

  (check-equal? (memory-list test-inc-ref-bucket-slot-1-state-after #xa003 #xa003)
                (list #xa1))
  (check-equal? (memory-list test-inc-ref-bucket-slot-1-state-after ZP_PTR (add1 ZP_PTR))
                (list #x04 #xa0)))

;; input: ZP_PTR  pointer to bucket slot (which can be anything, but most likely a cell-array or a native-array)
(define VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT
  (list
   (label VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT)
          (DEC ZP_PTR)
          (LDY !$00)
          (LDA (ZP_PTR),y)
          (SEC)
          (SBC !$01)            ;;  pointers are organized such that there is no page boundary crossed (=> no adjustment of highbyte necessary)
          (STA (ZP_PTR),y)
          (BNE NO_GC__VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT)

          ;; DO GC THIS SLOT and then FREE!!
          ;; what kind of object is this (read header cell)
          ;; then dispatch an header cell type
          (INC ZP_PTR) ;; now pointing at the first (lowbyte) of the cell header
          (LDA (ZP_PTR),y) ;; y still 0
          (CMP !TAG_BYTE_CELL_ARRAY)       ;;
          (BNE NEXT0__VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT)

          ;; its a regular array slot, (gc each slot, beware recursion!!!!)
          (JSR VM_GC_ARRAY_SLOT_PTR)
          (JMP VM_FREE_M1_SLOT_IN_ZP_PTR2)

   (label NEXT0__VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT)
          (CMP !TAG_BYTE_NATIVE_ARRAY)
          (BNE NEXT1__VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT)

          ;; it's a native array slot (no gc necessary)
          (JMP VM_FREE_M1_SLOT_IN_ZP_PTR2)

   (label NEXT1__VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT)
          (BRK) ;; error, unknown complex slot type

   (label NO_GC__VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT)
          (INC ZP_PTR)
          (RTS)))


(module+ test #| vm_dec_ref_bucket_slot (no gc) |#
  (define test-dec-ref-bucket-slot-1-code
    (list
     (LDA !$a0)
     (STA $a003) ;; $a004 - 1 = location for ref counting (now set to $a0)
     (STA ZP_PTR+1)
     (LDA !$04)
     (STA ZP_PTR) ;; ZP_PTR is set to $a004

     (JSR VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT)))

  (define test-dec-ref-bucket-slot-1-state-after
    (run-code-in-test test-dec-ref-bucket-slot-1-code))

  (check-equal? (memory-list test-dec-ref-bucket-slot-1-state-after #xa003 #xa003)
                (list #x9f)
                "a0 - 1 = 9f")
  (check-equal? (memory-list test-dec-ref-bucket-slot-1-state-after ZP_PTR (add1 ZP_PTR))
                (list #x04 #xa0)
                "points to $a004"))

(module+ test #| vm_dec_ref_bucket_slot (gc native array) |#

  (define test-dec-ref-bucket-slot-2-code
    (list
            (LDA !$00)
            (STA $a000) ;; counter for how often VM_GC_ARRAY_SLOT_PTR was called
            (JMP TEST_DEC_REF_BUCKET_SLOT_2_CODE)

     (label VM_GC_ARRAY_SLOT_PTR)
            (INC $a000)
            (RTS)

     (label TEST_DEC_REF_BUCKET_SLOT_2_CODE)
            (LDA !$10)
            (JSR VM_ALLOC_NATIVE_ARRAY_TO_ZP_PTR2)
            (JSR VM_COPY_PTR2_TO_PTR) ;; allocation is in zp_ptr2
            (JSR VM_REFCOUNT_INCR_ZP_PTR__M1_SLOT) ;; inc ref uses zp_ptr

            (JSR VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT) ;; dec ref uses zp_ptr
     ))

  (define test-dec-ref-bucket-slot-2-state-after
    (run-code-in-test test-dec-ref-bucket-slot-2-code
                      #:mock (list (label VM_GC_ARRAY_SLOT_PTR))))

  (check-equal? (vm-page->strings test-dec-ref-bucket-slot-2-state-after #xcc)
                (list "page-type:      m1 page p1"
                      "previous page:  $00"
                      "slots used:     0"
                      "next free slot: $10"))
  (check-equal? (memory-list test-dec-ref-bucket-slot-2-state-after #xa000 #xa000)
                (list #x00)
                "VM_GC_ARRAY_SLOT_PTR should NOT have been called (native arrays do not need to free any cells."))

(module+ test #| vm_dec_ref_bucket_slot (gc cell array) |#
  ;; check that all cells ref counts (if present) in the bucket slot (cell-array) are decremented
  (define test-dec-ref-bucket-slot-3-code
    (list
            (LDA !$00)
            (STA $a000) ;; counter for how often VM_GC_ARRAY_SLOT_PTR was called
            (JMP TEST_DEC_REF_BUCKET_SLOT_3_CODE)
     (label VM_GC_ARRAY_SLOT_PTR)
            (INC $a000)
            (RTS)

     (label TEST_DEC_REF_BUCKET_SLOT_3_CODE)
            (LDA !$04)
            (JSR VM_ALLOC_CELL_ARRAY_TO_ZP_PTR2)
            (JSR VM_COPY_PTR2_TO_PTR) ;; allocation is in zp_ptr2
            (JSR VM_REFCOUNT_INCR_ZP_PTR__M1_SLOT) ;; inc ref uses zp_ptr

            (JSR VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT) ;; dec ref uses zp_ptr
            ))

  (define test-dec-ref-bucket-slot-3-state-after
    (run-code-in-test test-dec-ref-bucket-slot-3-code
                      #:mock (list (label VM_GC_ARRAY_SLOT_PTR))))

  (check-equal? (vm-page->strings test-dec-ref-bucket-slot-3-state-after #xcc)
                (list "page-type:      m1 page p0"
                      "previous page:  $00"
                      "slots used:     0"
                      "next free slot: $04"))
  (check-equal? (memory-list test-dec-ref-bucket-slot-3-state-after #xa000 #xa000)
                (list #x01)
                "VM_GC_ARRAY_SLOT_PTR should have been called exactly once"))


;; dereference pointer in zp_ptr2, writing dereferenced value into zp_ptr
;; input:  ZP_PTR2 (pointer another pointer)
;; output: ZP_PTR  (dereferenced ZP_PTR2)
;; use case:
;;    e.g. ZP_PTR2 points to a car cell (of a cell-pair), which in turn points to a cell-array
;;    => ZP_PTR points to the cell-array
(define VM_DEREF_PTR2_INTO_PTR
  (list
   (label VM_DEREF_PTR2_INTO_PTR)
          (LDY !$00)
          (LDA (ZP_PTR2),y)
          (STA ZP_PTR_TAGGED)
          (AND !TAG_PTR_MASK)
          (STA ZP_PTR)
          (INY)
          (LDA (ZP_PTR2),y)
          (STA ZP_PTR+1)
          (RTS)))

(module+ test #| vm_deref_ptr2_into_ptr |#
  (define test-dref-ptr2-into-ptr-code
    (list
     (LDA !$a0)
     (STA ZP_PTR2+1)
     (LDA !$04)
     (STA ZP_PTR2)   ;; ZP_PTR2 = $a004

     (LDA !$b0)
     (STA $a005)
     (LDA !$0a)
     (STA $a004)        ;; $a004 b00a which is (untagged) b008 (bit 1 masked out)

     (JSR VM_DEREF_PTR2_INTO_PTR)))

  (define test-dref-ptr2-into-ptr-state-after
    (run-code-in-test test-dref-ptr2-into-ptr-code))

  (check-equal? (memory-list test-dref-ptr2-into-ptr-state-after ZP_PTR (add1 ZP_PTR))
                (list #x08 #xb0)
                "pointer found at location zp_ptr2 points to, masking out the tag bits (lower two)")
  (check-equal? (memory-list test-dref-ptr2-into-ptr-state-after ZP_PTR_TAGGED ZP_PTR_TAGGED)
                (list #x0a)
                "lowbyte of zp_ptr but with tag bits still set")
  (check-equal? (memory-list test-dref-ptr2-into-ptr-state-after ZP_PTR2 (add1 ZP_PTR2))
                (list #x04 #xa0)
                "zp_ptr2 is not modified"))

;; execute garbage collection on a cell array (decr-ref all array elements and collect if 0)
;; input:  ZP_PTR(2) = pointer to array (slot)
;; used:   ZP_PTR    = dreferenced array element (if array element is a ptr)
;;         ZP_PTR2   = pointer to last element of array
;; ouput: -
(define VM_GC_ARRAY_SLOT_PTR
  (list
   (label VM_GC_ARRAY_SLOT_PTR)
          (JSR VM_COPY_PTR_TO_PTR2)

   (label VM_GC_ARRAY_SLOT_PTR2)
          ;; loop over slots and decrement their slots
          (LDY !$01)
          (LDA (ZP_PTR2),y)  ;; a = number of array elements
          (STA LOOP_COUNT__VM_GC_ARRAY_SLOT_PTR) ;;

          (LDY !$00)

   (label LOOP__VM_GC_ARRAY_SLOT_PTR)
          (INC ZP_PTR2)
          (INC ZP_PTR2)
          ;; deref zp_ptr into zp_ptr2?
          (LDA (ZP_PTR2),y) ;; load tagged low byte
          (AND !$03)
          (BEQ NEXT__VM_GC_ARRAY_SLOT_PTR)
          (JSR VM_DEREF_PTR2_INTO_PTR)
          (JSR VM_REFCOUNT_DECR_ZP_PTR)
          (LDY !$00)
    (label NEXT__VM_GC_ARRAY_SLOT_PTR)
          (DEC LOOP_COUNT__VM_GC_ARRAY_SLOT_PTR)
          (BNE LOOP__VM_GC_ARRAY_SLOT_PTR)

          (RTS)

   (label LOOP_COUNT__VM_GC_ARRAY_SLOT_PTR)
          (byte $00)
   ))

(module+ test #| vm_gc_array_slot_ptr |#
  (define test-gc-array-slot-ptr-code
    (list
     (LDA !$04)
     (JSR VM_ALLOC_CELL_ARRAY_TO_ZP_PTR2)                       ;; ZP_PTR2 = pointer to the allocated array (with 4 cells)

     (JSR VM_ALLOC_CELL_PAIR_TO_ZP_PTR)                           ;; ZP_PTR = allocated cell-pair
     (JSR VM_REFCOUNT_INCR_ZP_PTR__CELL_PAIR)
     (JSR VM_CELL_STACK_PUSH_ZP_PTR)                    ;;cell-pair -> stack

     ;; wrote a new cell-pair @2
     (LDA !$02)
     (JSR VM_CELL_STACK_WRITE_TOS_TO_ARRAY_ATa_PTR2)    ;; tos (cell-pair) -> @2

     (JSR VM_CELL_STACK_PUSH_INT_m1)                    ;; int -1 -> stack
     (LDA !$01)
     (JSR VM_CELL_STACK_WRITE_TOS_TO_ARRAY_ATa_PTR2)    ;; tos (int -1) -> 1

     (JSR VM_GC_ARRAY_SLOT_PTR2)
     ))                      ;; gc array

  (define test-gc-array-slot-ptr-state-after
    (run-code-in-test test-gc-array-slot-ptr-code))

  (check-equal? (vm-stack->strings test-gc-array-slot-ptr-state-after)
                (list "stack holds 2 items"
                      "cell-int $1fff"
                      "cell-pair-ptr $cb04"))
  (check-equal? (vm-page->strings test-gc-array-slot-ptr-state-after #xcb)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $08"))
  (check-equal? (memory-list test-gc-array-slot-ptr-state-after #xcb01 #xcb01)
                (list #x00)
                "refcount for cell-pair at cb04..cb07 is at cb01 = 0 (was freed)")
  (check-equal? (vm-cell-pair-free-tree->string test-gc-array-slot-ptr-state-after)
                "cell-pair $cb04 -> [ cell-int $0000 . cell-int $0000 ]"
                "...and added as free tree root (for reuse)"))

;; allocate an array of bytes (native) (also useful for strings)
;; input:  A = number of bytes (1..)
;; output: ZP_PTR2 -> points to an allocated array
(define VM_ALLOC_NATIVE_ARRAY_TO_ZP_PTR2
(list
   (label VM_ALLOC_NATIVE_ARRAY_TO_ZP_PTR2)
          (PHA)
          (CLC)
          (ADC !$02) ;; add to total slot size

          (JSR VM_ALLOC_M1_SLOT_TO_ZP_PTR2)

          ;; write header cell
          (LDY !$00)
          (LDA !TAG_BYTE_NATIVE_ARRAY)
          (STA (ZP_PTR2),y) ;; store tag byte

          (INY)
          (PLA)
          (STA (ZP_PTR2),y) ;; store number of array elements

          (TAX) ;; use number of array elements as loop counter

          ;; initialize slots/array with 0
          (LDA !$00)
   (label LOOP_INIT__VM_ALLOC_NATIVE_ARRAY_TO_ZP_PTR2)
          (INY)
          (STA (ZP_PTR2),y)
          (DEX)
          (BNE LOOP_INIT__VM_ALLOC_NATIVE_ARRAY_TO_ZP_PTR2)

          (RTS)))

(module+ test #| vm_allocate_native_array |#
  (define test-alloc-native-array-code
    (list
     (LDA !$10)
     (JSR VM_ALLOC_NATIVE_ARRAY_TO_ZP_PTR2)))

  (define test-alloc-native-array-state-after
    (run-code-in-test test-alloc-native-array-code))

  (check-equal? (vm-page->strings test-alloc-native-array-state-after #xcc)
                (list
                 "page-type:      m1 page p1"
                 "previous page:  $00"
                 "slots used:     1"
                 "next free slot: $2e"))
  (check-equal? (memory-list test-alloc-native-array-state-after ZP_PTR2 (add1 ZP_PTR2))
                (list #x10 #xcc))
  (check-equal? (memory-list test-alloc-native-array-state-after #xcc10 #xcc21)
                (list TAG_BYTE_NATIVE_ARRAY #x10
                      #x00 #x00
                      #x00 #x00
                      #x00 #x00
                      #x00 #x00
                      #x00 #x00
                      #x00 #x00
                      #x00 #x00
                      #x00 #x00)))

;; allocate an array of cells (also useful for structures)
;; input:  A = number of cells (1..)
;; output: ZP_PTR2 -> points to an allocated array
(define VM_ALLOC_CELL_ARRAY_TO_ZP_PTR2
  (list
   (label VM_ALLOC_CELL_ARRAY_TO_ZP_PTR2)
          ;; optional: optimization for arrays with 3 cells => s8 page!
          (PHA)
          (ASL A)
          (CLC)
          (ADC !$02) ;; add to total slot size

          (JSR VM_ALLOC_M1_SLOT_TO_ZP_PTR2)

          ;; write header cell
          (LDY !$00)
          (LDA !TAG_BYTE_CELL_ARRAY)
          (STA (ZP_PTR2),y) ;; store tag byte

          (INY)
          (PLA)
          (STA (ZP_PTR2),y) ;; store number of array elements

          (TAX) ;; use number of array elements as loop counter

          ;; initialize slots/array with nil
   (label LOOP_INIT__VM_ALLOC_CELL_ARRAY_TO_ZP_PTR2)
          (INY)
          (LDA !<TAGGED_NIL)
          (STA (ZP_PTR2),y)
          (INY)
          (LDA !>TAGGED_NIL)
          (STA (ZP_PTR2),y)
          (DEX)
          (BNE LOOP_INIT__VM_ALLOC_CELL_ARRAY_TO_ZP_PTR2)

          (RTS)))

(module+ test #| vm_allocate_cell_array |#
  (define test-alloc-cell-array-code
    (list
     (LDA !$04)
     (JSR VM_ALLOC_CELL_ARRAY_TO_ZP_PTR2)))

  (define test-alloc-cell-array-state-after
    (run-code-in-test test-alloc-cell-array-code))

  (check-equal? (vm-page->strings test-alloc-cell-array-state-after #xcc)
                (list
                 "page-type:      m1 page p0"
                 "previous page:  $00"
                 "slots used:     1"
                 "next free slot: $16"))
  (check-equal? (memory-list test-alloc-cell-array-state-after ZP_PTR2 (add1 ZP_PTR2))
                (list #x04 #xcc))
  (check-equal? (memory-list test-alloc-cell-array-state-after #xcc04 #xcc0d)
                (list TAG_BYTE_CELL_ARRAY #x04
                      #x02 #x00
                      #x02 #x00
                      #x02 #x00
                      #x02 #x00))
)

;; write the tos into array element a (0 indexed), array pointed to by zp_ptr2
;; input:  a = index (0 indexed)
;;         ZP_PTR2 = pointer to array
;; NO CHECKING (NO BOUNDS, NO TYPE ...)
;; DECREMENT ref of pointer if array element was a pointer
(define VM_CELL_STACK_WRITE_TOS_TO_ARRAY_ATa_PTR2
  (list
   (label VM_CELL_STACK_WRITE_TOS_TO_ARRAY_ATa_PTR2)
          (ASL A)
          (CLC)
          (ADC !$02) ;; point to low byte
          (STA ZP_TEMP) ;; keep for later

          ;; copy low byte
          (LDY ZP_CELL_STACK_TOS)          ;; points to tagged low byte of stack
          (LDA (ZP_CELL_STACK_BASE_PTR),y) ;;
          (LDY ZP_TEMP)
          (TAX)
          (LDA (ZP_PTR2),y) ;; previous low byte in that slot
          (AND !$03)
          (BEQ NO_PTR_IN_SLOT__VM_CELL_STACK_WRITE_TOS_TO_ARRAY_ATa_PTR2)

          ;; GC the slot before actually writing to it <- can maybe optimized be ensuring that this cannot happen
          (INY)
          (LDA (ZP_PTR2),y) ;; if high byte is 0, it is nil, no gc there
          (BEQ IS_NIL____VM_CELL_STACK_WRITE_TOS_TO_ARRAY_ATa_PTR2)

          (DEY)

          (LDA (ZP_PTR2),y) ;; previous low byte in that slot (load again)
          (STA ZP_PTR_TAGGED)
          (AND !$fc)
          (STA ZP_PTR)
          (INY)
          (LDA (ZP_PTR2),y)
          (STA ZP_PTR+1)
          (JSR VM_REFCOUNT_DECR_ZP_PTR)

          ;; ensure x = lowbyte (or a and jump even further)

   (label IS_NIL____VM_CELL_STACK_WRITE_TOS_TO_ARRAY_ATa_PTR2)
          (LDY ZP_CELL_STACK_TOS)          ;; points to tagged low byte of stack
          (LDA (ZP_CELL_STACK_BASE_PTR),y) ;;
          (LDY ZP_TEMP)
          (TAX)
   (label NO_PTR_IN_SLOT__VM_CELL_STACK_WRITE_TOS_TO_ARRAY_ATa_PTR2)
          (TXA)
          (STA (ZP_PTR2),y)

          ;; copy high byte
          (LDY ZP_CELL_STACK_TOS)
          (DEY)
          (LDA (ZP_CELL_STACK_BASE_PTR),y) ;; high byte in stack
          (LDY ZP_TEMP)
          (INY)
          (STA (ZP_PTR2),y) ;; write high byte into array

          (RTS)))

(module+ test #| vm_cell_stack_write_tos_to_array_ata_ptr |#
  (define vm_cell_stack_write_tos_to_array_ata_ptr-code
    (list
     (LDA !$04)
     (JSR VM_ALLOC_CELL_ARRAY_TO_ZP_PTR2)

     (LDA !$ff)
     (LDX !$01)
     (JSR VM_CELL_STACK_PUSH_INT)

     (LDA !$02)
     (JSR VM_CELL_STACK_WRITE_TOS_TO_ARRAY_ATa_PTR2)))

  (define vm_cell_stack_write_tos_to_array_ata_ptr-state-after
    (run-code-in-test vm_cell_stack_write_tos_to_array_ata_ptr-code))

  (check-equal? (vm-page->strings vm_cell_stack_write_tos_to_array_ata_ptr-state-after #xcc)
                (list
                 "page-type:      m1 page p0"
                 "previous page:  $00"
                 "slots used:     1"
                 "next free slot: $16"))
  (check-equal? (memory-list vm_cell_stack_write_tos_to_array_ata_ptr-state-after ZP_PTR2 (add1 ZP_PTR2))
                (list #x04 #xcc))
  (check-equal? (memory-list vm_cell_stack_write_tos_to_array_ata_ptr-state-after #xcc04 #xcc0d)
                (list TAG_BYTE_CELL_ARRAY #x04
                      #x02 #x00
                      #x02 #x00
                      #x04 #xff
                      #x02 #x00)))

(module+ test #| vm_cell_stack_push_array_ata_ptr |#
  (define test-cell-stack-push-array-ata-ptr-code
    (list
     (LDA !$04)
     (JSR VM_ALLOC_CELL_ARRAY_TO_ZP_PTR2)

     (LDA !$02)
     (JSR VM_CELL_STACK_PUSH_ARRAY_ATa_PTR2) ;; @2 = nil -> stack

     (LDA !$ff)
     (LDX !$01)
     (JSR VM_CELL_STACK_PUSH_INT)            ;; int $1ff -> stack

     (LDA !$02)
     (JSR VM_CELL_STACK_WRITE_TOS_TO_ARRAY_ATa_PTR2) ;; tos (int $1ff) -> @2 (overwriting nil)

     (LDA !$02)
     (JSR VM_CELL_STACK_PUSH_ARRAY_ATa_PTR2)  ;; @2 (now int $1ff) -> stack
     ))

  (define test-cell-stack-push-array-ata-ptr-state-after
    (run-code-in-test test-cell-stack-push-array-ata-ptr-code))

  (check-equal? (cpu-state-clock-cycles test-cell-stack-push-array-ata-ptr-state-after)
                1371)
  (check-equal? (vm-stack->strings test-cell-stack-push-array-ata-ptr-state-after)
                (list "stack holds 3 items"
                      "cell-int $01ff"
                      "cell-int $01ff"
                      "cell-pair-ptr NIL")))

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
          VM_ALLOC_PAGE_FOR_M1_SLOTS                         ;; allocate page and initialize for ref counted m1 slots of a specific profile (and thus size)
          ;; VM_ALLOC_PAGE_FOR_S8_SLOTS                         ;; allocate page and initialize to hold ref counted 8 byte slots <- really, maybe s8 slots can be removed alltogether

          ;; VM_ALLOC_PAGE_FOR_MODULE_CODE                      ;; allocate page and initialize to hold immutable byte code (not ref counted)

          ;; ---------------------------------------- alloc/free cells, pairs, slots
          VM_ALLOC_CELL_ON_PAGE                              ;; allocate a cell on the page in A (allocating a new one if page is full)
          VM_ALLOC_CELL_PAIR_ON_PAGE                         ;; allocate a cell-pair from this page (if page has no free cell-pairs, a new page is allocated and is used to get a free cell-pair!)

          VM_ALLOC_CELL_PAIR_TO_ZP_PTR                       ;; allocate a cell-pair, allocating a new page if necessary, reusing cell-pairs from the free tree first
          VM_FREE_CELL_PAIR_IN_ZP_PTR                        ;; free this cell-pair (adding it to the free tree)

          VM_ALLOC_CELL_TO_ZP_PTR                            ;; allocate a cell, allocating a new page if necessary, reusing cells from the free list first
          VM_FREE_CELL_IN_ZP_PTR                             ;; free this cell (adding it to the free list)

          VM_ALLOC_NATIVE_ARRAY_TO_ZP_PTR2                   ;; allocate an array of bytes (native) (also useful for strings)
          VM_ALLOC_CELL_ARRAY_TO_ZP_PTR2                     ;; allocate an array of cells (also useful for structures)

          VM_ALLOC_M1_SLOT_TO_ZP_PTR2                        ;; allocate a slot of min A size, allocating a new page if necessary
          VM_FREE_M1_SLOT_IN_ZP_PTR2                         ;; free a slot (adding it to the free list)

          ;; VM_ALLOC_MODULE_CODE_SLOT_TO_ZP_PTR                ;; allocate a slot for module code
          ;; VM_FREE_MODULE
          ;; VM_RELOCATE_MODULE_X_TO_                           ;; relocate module identified by page x to ??

          ;; ---------------------------------------- refcount
          VM_REFCOUNT_DECR_ZP_PTR                            ;; generic decrement of refcount (dispatches depending on type)

          VM_REFCOUNT_DECR_ZP_PTR__CELL_PAIR                 ;; decrement refcount, calling vm_free_cell_pair_in_zp_ptr if dropping to 0
          VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT                   ;; decrement refcount, calling vm_free_m1_slot_in_zp_ptr if dropping to 0
          VM_REFCOUNT_DECR_ZP_PTR__CELL                      ;; decrement refcount, calling vm_free_cell_in_zp_ptr if dropping to 0

          VM_REFCOUNT_INCR_ZP_PTR__CELL_PAIR                 ;; increment refcount of cell-pair
          VM_REFCOUNT_INCR_ZP_PTR__M1_SLOT                   ;; increment refcount of m1-slot
          VM_REFCOUNT_INCR_ZP_PTR__CELL                      ;; increment refcount of cell

          ;; ---------------------------------------- call frame
          VM_ALLOC_CALL_FRAME                                ;; allocate a new call frame of minimum size
          VM_SAVE_EXEC_STATE_TO_CALL_FRAME                   ;; write current execution state into the (allocated) call frame
          VM_POP_CALL_FRAME                                  ;; pop the topmost call frame, restoring execution state of calling function


          ;; ---------------------------------------- misc

          VM_REMOVE_FULL_PAGES_FOR_PTR2_SLOTS                ;; remove full pages in the free list of pages of the same type as are currently in ZP_PTR2
          VM_ENQUEUE_PAGE_AS_HEAD_FOR_PTR2_SLOTS             ;; put this page as head of the page free list for slots of type as in ZP_PTR2

          VM_GC_ARRAY_SLOT_PTR                               ;; execute garbage collection on a cell array (decr-ref all array elements and collect if 0)

          VM_DEREF_PTR2_INTO_PTR                             ;; dereference pointer in zp_ptr2, writing dereferenced value into zp_ptr

          VM_FREE_NON_ATOMIC                                 ;; free nonatomic (is cell-ptr, cell-pair-ptr, m1-slot-ptr, slot8-ptr)

          VM_ADD_CELL_PAIR_TO_ON_PAGE_FREE_LIST              ;; add the given cell-pair (in zp_ptr) to the free list of cell-pairs on its page

          VM_COPY_PTR2_TO_PTR                                ;; copy zp_ptr2 to zp_ptr (including tag byte)
          VM_COPY_PTR_TO_PTR2                                ;; copy zp_ptr to zp_ptr2 (including tag byte)

          ;; ---------------------------------------- CELL_STACK

          ;; vm_cell_stack_write_int_1_to_tos
          ;; vm_cell_stack_write_int_0_to_tos
          VM_CELL_STACK_WRITE_INT_TO_TOS                     ;; write the given integer into the topmost element of the cell-stack

          ;; vm_cell_stack_write_tos_to_param_0
          VM_CELL_STACK_WRITE_TOS_TO_PARAMy                  ;; write the topmost cell into function parameter y
          ;; vm_cell_stack_write_tos_to_local_0
          VM_CELL_STACK_WRITE_TOS_TO_LOCALy                  ;; write the topmost cell into function local y

          ;; vm_cell_stack_write_tos_to_cell1_of_zp_ptr
          ;; vm_cell_stack_write_tos_to_cell0_of_zp_ptr
          VM_CELL_STACK_WRITE_TOS_TO_CELLx_OF_ZP_PTR         ;; write topmost cell into x-position of the cell-pair referenced by zp_ptr

          ;; vm_cell_stack_write_cell1_of_zp_ptr_to_tos
          ;; vm_cell_stack_write_cell0_of_zp_ptr_to_tos
          VM_CELL_STACK_WRITE_CELLy_OF_ZP_PTR_TO_TOS         ;; write the cell in y-position of the cell-pair referenced by zp_ptr into the topmost cell

          ;; vm_cell_stack_write_tos_to_zp_ptr2
          ;; vm_cell_stack_write_tos_to_zp_ptr
          VM_CELL_STACK_WRITE_TOS_TO_ZP_PTRx                 ;; write the topmost cell into either zp_ptr or zp_ptr2 (must be a pointer)

          ;; vm_cell_stack_write_zp_ptr2_to_tos
          ;; vm_cell_stack_write_zp_ptr_to_tos
          VM_CELL_STACK_WRITE_ZP_PTRy_TO_TOS                 ;; write the zp_ptr or zp_ptr2 into the topmost cell

          VM_CELL_STACK_WRITE_TOS_TO_ARRAY_ATa_PTR2          ;; write the tos into an array at a position that is referenced through zp_ptr2

          VM_CELL_STACK_POP                                  ;; pop the topmost element of the stack
          VM_CELL_STACK_POP_R

          ;; vm_cell_stack_push_array_ata_ptr2                  push cell from an array (referenced through zp_ptr2) at a position

          ;; vm_cell_stack_push_zp_ptr2                         push cell referenced by zp_ptr2
          ;; vm_cell_stack_push_zp_ptr                          push cell referenced by zp_ptr
          ;; vm_cell_stack_push_zp_ptry

          ;; vm_cell_stack_push_cell1_of_zp_ptr                 push cell1 of a cell-pair referenced by zp_ptr
          ;; vm_cell_stack_push_cell0_of_zp_ptr                 push cell0 of a cell-pair referenced by zp_ptr
          ;; vm_cell_stack_push_celly_of_zp_ptr

          ;; vm_cell_stack_push_int_0                           push int value 0
          ;; vm_cell_stack_push_int_1                           push int value 1
          ;; vm_cell_stack_push_int_2                           push int value 2
          ;; vm_cell_stack_push_int_m1                          push int value -1
          ;; vm_cell_stack_push_int

          ;; vm_cell_stack_push_nil                             push cell-pair constant NIL
          VM_CELL_STACK_PUSH                                 ;; push a cell onto the stack
          VM_CELL_STACK_PUSH_R

          ;; vm_cell_stack_write_rt_cell0_to_rt
          ;; vm_cell_stack_write_rt_cell1_to_rt
          VM_CELL_STACK_WRITE_RT_CELLy_TO_RT
          VM_WRITE_INT_AY_TO_Rx
          VM_CP_RT_TO_RA
          VM_CP_RA_TO_RT
          VM_POP_FSTOS_TO_CELLy_RT
          VM_WRITE_RA_TO_CELLy_RT
          VM_WRITE_RT_TO_CELLy_RA          

          ;; ---------------------------------------- registers and maps
          (list (org #xcec0))
          VM_INITIAL_MM_REGS
          (list (org #xcf00))
          VM_PAGE_SLOT_DATA))
