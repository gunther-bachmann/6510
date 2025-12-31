#lang racket/base

#|

 define virtual machine variables/constants used through out the vm

|#

(require (only-in racket/list empty?)
         (only-in "../../6510.rkt"
                  byte-const
                  word-const)
         (only-in "../../ast/6510-command.rkt"
                  ast-const-byte-cmd?
                  ast-const-byte-cmd-label
                  ast-const-byte-cmd-byte
                  ast-const-word-cmd?
                  ast-const-word-cmd-label
                  ast-const-word-cmd-word))

(provide ast-const-get                   ;; extract constant definition in assembler into racket constant
         VM_MEMORY_MANAGEMENT_CONSTANTS  ;; contains all zp variable locations and constant definitions to be included into a asm program

         ZP_RT                           ;; (word) register top of stack held on zp (whereas the rest of the evlstk is held in main memory)
         ZP_RP                           ;; (word) register used temporary for 2+ operand operations (to reduce stack copy/move ops during exec)
         ZP_RA                           ;; (word) primary register for array access
         ZP_RB                           ;; (word) secondary register for array access
         ZP_RC                           ;; (word) tertiary register for array access
         ZP_RAI                          ;; (byte) index for primary array register
         ZP_RBI                          ;; (byte) index for secondary array register
         ZP_RCI                          ;; (byte) index for tertiary array register
         ZP_RZ                           ;; (word) register reserved for garbage collection operations

         ZP_INC_COLLECTIBLE_LIST         ;; (word) ptr to the head of the incrementally collectible cell-array list
         ZP_CALL_FRAME                   ;; (word) pointer to current call frame
         ZP_CALL_FRAME_TOP_MARK          ;; (byte) top mark of call frame stack
         ZP_CELL_STACK_LB_PTR            ;; (word) pointer to low byte of current eval stack
         ZP_CELL_STACK_HB_PTR            ;; (word) pointer to high byte of current eval stack
         ZP_CELL_STACK_TOS               ;; (byte) top mark of evlstk
         ZP_PAGE_REG                     ;; (word) page register (zp_page_reg+1 is always zero! to allow indirect access)
         ZP_PAGE_FREE_LIST               ;; (byte) pointer to first free page of list
         ZP_PAGE_FREE_SLOTS_LIST         ;; (byte) x profiles, bytes pointing to pages with free slots of this profile
         ZP_PROFILE_PAGE_FREE_LIST       ;; (byte) x profiles, bytes pointing to pages which are completely free initialized with this profile
         ZP_TEMP                         ;; (byte) temp location, can be used in combination with TEMP2 to be used as word
         ZP_TEMP2                        ;; (byte) temp location
         ZP_TEMP3                        ;; (byte) temp location, can be used in combination with TEMP4 to be used as word
         ZP_TEMP4                        ;; (byte) temp location
         ZP_VM_FUNC_PTR                  ;; (word) pointer to the currently executing function
         ZP_LOCALS_LB_PTR                ;; (word) pointer to the low byte of the current locals of this function
         ZP_LOCALS_HB_PTR                ;; (word) pointer to the high byte of the current locals of this function
         ZP_LOCALS_TOP_MARK              ;; (byte) top mark of locals cell stack
         TAG_BYTE_BYTE_CELL              ;; (byte) constant identifying a byte cell (low-byte = this constant)
         TAG_BYTE_CELL_ARRAY             ;; (byte) constant identifying a cell-array (first byte in a m1 slot)
         TAG_BYTE_NATIVE_ARRAY           ;; (byte) constant identifying a native-array (first byte in a m1 slot)
         TAGGED_INT_0                    ;; (word) constant identifying an int cell with value 0
         TAGGED_INT_0_HB                 ;; (byte) high byte of const int 0
         TAGGED_INT_0_LB                 ;; (byte) low byte of const int 0
         TAGGED_NIL)                     ;; (word) constant identifying a nil cell (low + high byte)

(define VM_MEMORY_MANAGEMENT_CONSTANTS
  (list
   ;; highest bit 0 and the lowest 2 bits are reserved for int, cell-ptr and cell-pair-ptr
   ;; => 32 values still available
   ;; @DC-C: TAG_BYTE_BYTE_CELL, group: cell
   (byte-const TAG_BYTE_BYTE_CELL         $01) ;; low byte in a cell that indicates the cell to be a byte-cell
   (byte-const TAG_BYTE_CELL_ARRAY        $83) ;; LSR -> 41, LSR -> 20
   (byte-const TAG_BYTE_CELL_ARRAY_LSR2   $30)
   (byte-const TAG_BYTE_NATIVE_ARRAY      $87) ;; LSR -> 43, LSR -> 21
   (byte-const TAG_BYTE_NATIVE_ARRAY_LSR2 $21)

   (byte-const NEXT_FREE_PAGE_PAGE       $cf)   ;; cf00..cfff is a byte array, mapping each page idx to the next free page idx, 00 = no next free page for the given page
   ;; (word-const VM_FIRST_FREE_SLOT_ON_PAGE     $cf00) ;; location: table of first free slot for each page

   (word-const TAGGED_INT_0              $0003)
   (byte-const TAGGED_INT_0_HB           $00)
   (byte-const TAGGED_INT_0_LB           $03)
   (word-const TAGGED_BYTE0              $0100)
   (word-const TAGGED_NIL                $0000) ;; tag indicates cell-ptr

                                              ;; @DC-ZP: ZP_TEMP3, group: temp

   (byte-const ZP_INC_COLLECTIBLE_LIST   $ae) ;; ae..af,  ptr to the head of incremental collectible cell-arrays list
   (byte-const ZP_PROFILE_PAGE_FREE_LIST $b0) ;; b0..b5,  b0 (profile 0) .. b5 (profile 5)
   (byte-const ZP_PAGE_FREE_SLOTS_LIST   $b6) ;; b6..bb,  b6 (profile 0) .. bb (profile 5)
   (byte-const ZP_PAGE_FREE_LIST         $bc) ;; bc..bd = ptr to first free page
   (byte-const ZP_PAGE_REG               $be) ;; be..bf = page reg

   (byte-const ZP_RB                     $c0) ;; c0..c1 array register b
   (byte-const ZP_RC                     $c2) ;; c2..c3 array register c
   (byte-const ZP_RBI                    $c4) ;; byte index into array b
   (byte-const ZP_RCI                    $c5) ;; byte index into array c

   (byte-const ZP_RP                     $c6) ;; c6..c7 register for cell pairs

                                              ;; @DC-ZP: ZP_VM_PC, group: call_frame
   ;; the following twelve bytes need to be continuous, since they are saved into the call frame!
   ;; zero page location 3 for temp usage
   (byte-const ZP_TEMP3                  $d9) ;; may be used as pointer (in combination with ZP_TEMP4 => must be in adjacent memory locations)
                                              ;; @DC-ZP: ZP_TEMP4, group: temp
   ;; zero page location 4 for temp usage
   (byte-const ZP_TEMP4                  $da)
                                              ;; @DC-ZP: ZP_CELL_STACK_TOS, group: evlstk
   (byte-const ZP_CELL_STACK_TOS         $db) ;; byte (fe = empty stack, 0 = first element, 2 = second element, 4 = third element ...)

   ;; ZP_TEMP may be used as pointer (in combination with ZP_TEMP2 => must be in adjacent memory locations)
   (byte-const ZP_TEMP                   $dc) ;; may not be used after sub calls (just within a routine without jsr)
   (byte-const ZP_TEMP2                  $dd) ;; may not be used after sub calls (just within a routine without jsr)

                                              ;; @DC-ZP: ZP_VM_FUNC_PTR, group: call_frame
   (byte-const ZP_VM_FUNC_PTR            $e0) ;; e0..e1 pointer to the currently running function
                                              ;; @DC-ZP: ZP_LOCALS_LB_PTR, group: locals
   (byte-const ZP_LOCALS_LB_PTR          $e2) ;; e2..e3 pointer to low byte of first local in call-frame
                                              ;; @DC-ZP: ZP_LOCALS_HB_PTR, group: locals
   (byte-const ZP_LOCALS_HB_PTR          $e4) ;; e4..e5 pointer to high byte of first local in call-frame
                                              ;; @DC-ZP: ZP_CELL_STACK_LB_PTR, group: evlstk
   (byte-const ZP_CELL_STACK_LB_PTR      $e6) ;; e6..e7 (pointer to low byte of the eval stack of the currently running function (+ZP_CELL_STACK_TOS => pointer to tos of the call-frame, in register mode, actual TOS is ZP_RT!)
                                              ;; @DC-ZP: ZP_CELL_STACK_HB_PTR, group: evlstk
   (byte-const ZP_CELL_STACK_HB_PTR      $e8) ;; e8..e9 (pointer to high byte of the eval stack of the currently running function (+ZP_CELL_STACK_TOS => pointer to tos of the call-frame, in register mode, actual TOS is ZP_RT!)
                                              ;; @DC-ZP: ZP_CALL_FRAME_TOP_MARK, group: call_frame
   (byte-const ZP_CALL_FRAME_TOP_MARK    $ea) ;; ea byte pointing to current top of call-frame (is swapped in/out of call-frame page $02)
                                              ;; @DC-ZP: ZP_LOCALS_TOP_MARK, group: locals
   (byte-const ZP_LOCALS_TOP_MARK        $eb) ;; eb byte pointing to the byte past the last local on the locals stack
                                              ;; @DC-ZP: ZP_CALL_FRAME, group: call_frame
   (byte-const ZP_CALL_FRAME             $f1) ;; f1..f2

   (byte-const ZP_RZ                     $f3) ;; f3..f4   for garbage collection (and temp use outside of gc) only

   ;; implementation using registers
   ;; register T = top of stack, used as main register for operations, could be a pointer to a cell or an atomic cell. if it is a pointer to a cell, the low byte here is without tag bits => (zp_rt) points to the cell
   (byte-const ZP_RT                     $fb) ;; fb = low byte, fc = high byte,
   ;; register A
   (byte-const ZP_RA                     $fd) ;; fd = low byte, fe = high byte,
   (byte-const ZP_RAI                    $ff) ;; ff = byte index into byte array a
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

;; make constants available in racket (to allow for usage e.g. in test code)
(define ZP_INC_COLLECTIBLE_LIST   (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_INC_COLLECTIBLE_LIST"))
(define ZP_RT                     (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_RT"))
(define ZP_RP                     (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_RP"))
(define ZP_RA                     (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_RA"))
(define ZP_RB                     (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_RB"))
(define ZP_RC                     (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_RC"))
(define ZP_RAI                    (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_RAI"))
(define ZP_RBI                    (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_RBI"))
(define ZP_RCI                    (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_RCI"))
(define ZP_RZ                     (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_RZ"))
(define ZP_CALL_FRAME             (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CALL_FRAME"))
(define ZP_CALL_FRAME_TOP_MARK    (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CALL_FRAME_TOP_MARK"))
(define ZP_CELL_STACK_LB_PTR      (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CELL_STACK_LB_PTR"))
(define ZP_CELL_STACK_HB_PTR      (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CELL_STACK_HB_PTR"))
(define ZP_CELL_STACK_TOS         (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CELL_STACK_TOS"))
(define ZP_PAGE_REG               (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_PAGE_REG"))
(define ZP_PAGE_FREE_LIST         (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_PAGE_FREE_LIST"))
(define ZP_PAGE_FREE_SLOTS_LIST   (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_PAGE_FREE_SLOTS_LIST"))
(define ZP_PROFILE_PAGE_FREE_LIST (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_PROFILE_PAGE_FREE_LIST"))
(define ZP_TEMP                   (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_TEMP"))
(define ZP_TEMP2                  (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_TEMP2"))
(define ZP_TEMP3                  (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_TEMP3"))
(define ZP_TEMP4                  (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_TEMP4"))
(define ZP_LOCALS_LB_PTR          (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_LOCALS_LB_PTR"))
(define ZP_LOCALS_HB_PTR          (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_LOCALS_HB_PTR"))
(define ZP_LOCALS_TOP_MARK        (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_LOCALS_TOP_MARK"))
(define ZP_VM_FUNC_PTR            (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_VM_FUNC_PTR"))

(define TAG_BYTE_BYTE_CELL        (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAG_BYTE_BYTE_CELL"))
(define TAG_BYTE_CELL_ARRAY       (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAG_BYTE_CELL_ARRAY"))
(define TAG_BYTE_NATIVE_ARRAY     (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAG_BYTE_NATIVE_ARRAY"))
(define TAGGED_INT_0              (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAGGED_INT_0"))
(define TAGGED_INT_0_HB           (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAGGED_INT_0_HB"))
(define TAGGED_INT_0_LB           (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAGGED_INT_0_LB"))
(define TAGGED_NIL                (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAGGED_NIL"))
