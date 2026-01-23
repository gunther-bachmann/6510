#lang racket/base

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
         ZP_CALL_FRAME_LB                ;; (word) pointer to current call frame (low byte) (offset always 0)
         ZP_CALL_FRAME_HB                ;; (word) pointer to current call frame (high byte) (offset always 0)
         ZP_CALL_FRAME_TOP_MARK          ;; (byte) top mark of call frame stack
         ZP_EVAL_STACK_TAIL_LB_PTR       ;; (word) pointer to low byte of current eval stack tail
         ZP_EVAL_STACK_TAIL_HB_PTR       ;; (word) pointer to high byte of current eval stack tail
         ZP_EVAL_STACK_TAIL_TOP          ;; (byte) top mark of evlstk tail
         ZP_PAGE_REG                     ;; (word) page register (offset is always zero! to allow indirect access)
         ZP_PAGE_FREE_LIST               ;; (byte) first free page (in free list)
         ZP_PAGE_FREE_SLOTS_LIST         ;; (bytes x profiles), bytes pointing to pages with free slots of this profile
         ZP_PROFILE_PAGE_FREE_LIST       ;; (bytes x profiles), bytes pointing to pages which are completely free initialized with this profile
         ZP_TEMP                         ;; (byte) temp location, can be used in combination with TEMP2 to be used as word
         ZP_TEMP2                        ;; (byte) temp location
         ZP_TEMP3                        ;; (byte) temp location, can be used in combination with TEMP4 to be used as word
         ZP_TEMP4                        ;; (byte) temp location
         ZP_FUNC_PTR                     ;; (word) pointer to the currently executing function [pointing to 1 before actual code]
         ZP_LOCALS_LB_PTR                ;; (word) pointer to the low byte of the current locals of this function
         ZP_LOCALS_HB_PTR                ;; (word) pointer to the high byte of the current locals of this function

         TAG_BYTE_BYTE_CELL              ;; (byte) constant identifying a byte cell (low-byte = this constant)
         TAG_BYTE_CELL_ARRAY             ;; (byte) constant identifying a cell-array (first byte in a m1 slot)
         TAG_BYTE_NATIVE_ARRAY           ;; (byte) constant identifying a native-array (first byte in a m1 slot)
         TAGGED_INT_0                    ;; (word) constant identifying an int cell with value 0
         TAGGED_INT_0_HB                 ;; (byte) high byte of const int 0
         TAGGED_INT_0_LB                 ;; (byte) low byte of const int 0
         TAGGED_NIL)                     ;; (word) constant identifying a nil cell (low + high byte)

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


(define VM_MEMORY_MANAGEMENT_CONSTANTS
  (list
   ;; highest bit 0 and the lowest 2 bits are reserved for int, cell-ptr and cell-pair-ptr
   ;; => 32 values still available
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

   (byte-const ZP_INC_COLLECTIBLE_LIST   $c9) ;; c9..ca,  ptr to the head of incremental collectible cell-arrays list
   (byte-const ZP_PROFILE_PAGE_FREE_LIST $74) ;; 74..79,  74 (profile 0) .. 79 (profile 5)
   (byte-const ZP_PAGE_FREE_SLOTS_LIST   $7a) ;; 7a..7f,  7a (profile 0) .. 7f (profile 5)
   (byte-const ZP_PAGE_FREE_LIST         $dc) ;; dc..dd = ptr to first free page
   (byte-const ZP_PAGE_REG               $de) ;; de..df = page reg

   (byte-const ZP_RB                     $d1) ;; d1..d2 array register b
   (byte-const ZP_RC                     $ce) ;; ce..cf array register c
   (byte-const ZP_RBI                    $d3) ;; byte index into array b
   (byte-const ZP_RCI                    $d6) ;; byte index into array c

   (byte-const ZP_RP                     $d9) ;; d9..da register for cell pairs

   ;; the following twelve bytes need to be continuous, since they are saved into the call frame!
   ;; zero page location 3 for temp usage
   (byte-const ZP_TEMP3                  $b4) ;; may be used as pointer (in combination with ZP_TEMP4 => must be in adjacent memory locations)
   ;; zero page location 4 for temp usage
   (byte-const ZP_TEMP4                  $b5)
   (byte-const ZP_EVAL_STACK_TAIL_TOP    $db) ;; byte (fe = empty stack, 0 = first element, 2 = second element, 4 = third element ...)

   ;; ZP_TEMP may be used as pointer (in combination with ZP_TEMP2 => must be in adjacent memory locations)
   (byte-const ZP_TEMP                   $be) ;; may not be used after sub calls (just within a routine without jsr)
   (byte-const ZP_TEMP2                  $bf) ;; may not be used after sub calls (just within a routine without jsr)

   (byte-const ZP_FUNC_PTR               $e0) ;; e0..e1 pointer to the currently running function
   (byte-const ZP_LOCALS_LB_PTR          $e2) ;; e2..e3 pointer to low byte of first local in call-frame
   (byte-const ZP_LOCALS_HB_PTR          $e4) ;; e4..e5 pointer to high byte of first local in call-frame
   (byte-const ZP_EVAL_STACK_TAIL_LB_PTR $e6) ;; e6..e7 (pointer to low byte of the eval stack of the currently running function (+ZP_EVAL_STACK_TAIL_TOP => pointer to tos of the call-frame, in register mode, actual TOS is ZP_RT!)
   (byte-const ZP_EVAL_STACK_TAIL_HB_PTR $e8) ;; e8..e9 (pointer to high byte of the eval stack of the currently running function (+ZP_EVAL_STACK_TAIL_TOP => pointer to tos of the call-frame, in register mode, actual TOS is ZP_RT!)
   (byte-const ZP_CALL_FRAME_TOP_MARK    $73) ;; 73 byte pointing to current top of call-frame
   (byte-const ZP_RZ                     $ec) ;; ec..ed   for garbage collection (and temp use outside of gc) only

   (byte-const ZP_CALL_FRAME_LB          $ee) ;; ee..ef
   (byte-const ZP_CALL_FRAME_HB          $f0) ;; f0..f1


   ;; implementation using registers
   ;; register T = top of stack, used as main register for operations, could be a pointer to a cell or an atomic cell. if it is a pointer to a cell, the low byte here is without tag bits => (zp_rt) points to the cell
   (byte-const ZP_RT                     $fb) ;; fb = low byte, fc = high byte,
   ;; register A
   (byte-const ZP_RA                     $fd) ;; fd = low byte, fe = high byte,
   (byte-const ZP_RAI                    $f2) ;; ff = byte index into byte array a
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
(define ZP_CALL_FRAME_LB          (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CALL_FRAME_LB"))
(define ZP_CALL_FRAME_HB          (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CALL_FRAME_HB"))
(define ZP_CALL_FRAME_TOP_MARK    (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CALL_FRAME_TOP_MARK"))
(define ZP_EVAL_STACK_TAIL_LB_PTR (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_EVAL_STACK_TAIL_LB_PTR"))
(define ZP_EVAL_STACK_TAIL_HB_PTR (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_EVAL_STACK_TAIL_HB_PTR"))
(define ZP_EVAL_STACK_TAIL_TOP    (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_EVAL_STACK_TAIL_TOP"))
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
(define ZP_FUNC_PTR               (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_FUNC_PTR"))

(define TAG_BYTE_BYTE_CELL        (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAG_BYTE_BYTE_CELL"))
(define TAG_BYTE_CELL_ARRAY       (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAG_BYTE_CELL_ARRAY"))
(define TAG_BYTE_NATIVE_ARRAY     (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAG_BYTE_NATIVE_ARRAY"))
(define TAGGED_INT_0              (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAGGED_INT_0"))
(define TAGGED_INT_0_HB           (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAGGED_INT_0_HB"))
(define TAGGED_INT_0_LB           (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAGGED_INT_0_LB"))
(define TAGGED_NIL                (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAGGED_NIL"))

#|

 | offset | definition vm                       | definition c64 (https://www.pagetable.com/c64ref/c64mem/) |
 |--------+-------------------------------------+-----------------------------------------------------------|
 | ff     |                                     | float conversion                                          |
 | fe     | ZP_RA (hb)                          | -                                                         |
 | fd     | ZP_RA (lb)                          | -                                                         |
 | fc     | ZP_RT (hb)                          | -                                                         |
 | fb     | ZP_RT (lb)                          | -                                                         |
 | fa     |                                     | ptr: rs 232 output buffer                                 |
 | f9     |                                     | ptr: rs 232 output buffer                                 |
 | f8     |                                     | ptr: rs 232 input buffer                                  |
 | f7     |                                     | ptr: rs 232 input buffer                                  |
 | f6     |                                     | vector: keyboard decode table                             |
 | f5     |                                     | vector: keyboard decode table                             |
 | f4     |                                     | ptr: screen color ram location                            |
 | f3     |                                     | ptr: screen color ram location                            |
 | f2     | ZP_RAI                              | end of screen line link table/editor storage              |
 | f1     | ZP_CALL_FRAME_HB (hb)               | :                                                         |
 | f0     | ZP_CALL_FRAME_HB (lb)               | :                                                         |
 | ef     | ZP_CALL_FRAME_LB (hb)               | :                                                         |
 | ee     | ZP_CALL_FRAME_LB (lb)               | :                                                         |
 | ed     | ZP_RZ (hb)                          | :                                                         |
 | ec     | ZP_RZ (lb)                          | :                                                         |
 | eb     |                                     | :                                                         |
 | ea     |                                     | :                                                         |
 | e9     | ZP_EVAL_STACK_TAIL_HB_PTR (hb)      | :                                                         |
 | e8     | ZP_EVAL_STACK_TAIL_HB_PTR (lb)      | :                                                         |
 | e7     | ZP_EVAL_STACK_TAIL_LB_PTR (hb)      | :                                                         |
 | e6     | ZP_EVAL_STACK_TAIL_LB_PTR (lb)      | :                                                         |
 | e5     | ZP_LOCALS_HB_PTR (hb)               | :                                                         |
 | e4     | ZP_LOCALS_HB_PTR (lb)               | :                                                         |
 | e3     | ZP_LOCALS_LB_PTR (hb)               | :                                                         |
 | e2     | ZP_LOCALS_LB_PTR (lb)               | :                                                         |
 | e1     | ZP_FUNC_PTR (hb)                    | :                                                         |
 | e0     | ZP_FUNC_PTR (lb)                    | :                                                         |
 | df     | ZP_PAGE_REG (hb)                    | :                                                         |
 | de     | ZP_PAGE_REG (lb) = 0                | :                                                         |
 | dd     | ZP_PAGE_FREE_LIST (hb)              | :                                                         |
 | dc     | ZP_PAGE_FREE_LIST (lb)              | :                                                         |
 | db     | ZP_EVAL_STACK_TAIL_TOP              | :                                                         |
 | da     | ZP_RP (hb)                          | :                                                         |
 | d9     | ZP_RP (lb)                          | start of screen line link table/editor storage            |
 | d8     |                                     | flag: insert mode                                         |
 | d7     |                                     | temp: ascii last char printed                             |
 | d6     | ZP_RCI                              | cursor physical line number                               |
 | d5     |                                     | max len of physical screen                                |
 | d4     |                                     | flag: editor in quote mode                                |
 | d3     | ZP_RBI                              | cursor column number (0-79)                               |
 | d2     | ZP_RB (hb)                          | ptr: address of current screen line (hb)                  |
 | d1     | ZP_RB (lb)                          | ptr: address of current screen line (lb)                  |
 | d0     |                                     | flag: input key/screen                                    |
 | cf     | ZP_RC (lb)                          | flag: last cursor blink on/off                            |
 | ce     | ZP_RC (hb)                          | char under cursor                                         |
 | cd     |                                     | timer: countdown to blink cursor                          |
 | cc     |                                     | cursor blink enable                                       |
 | cb     |                                     | key pressed scan code                                     |
 | ca     | ZP_INC_COLLECTIBLE_LIST (hb)        | ptr to row/col position of last char (hb)                 |
 | c9     | ZP_INC_COLLECTIBLE_LIST (lb)        | ptr to row/col position of last char (lb)                 |
 | c8     |                                     | offset end of logical line (0-79)                         |
 | c7     |                                     | flag: print reverse                                       |
 | c6     |                                     | number of chars in keyboard buffer                        |
 | c5     |                                     | scan code of last key pressed                             |
 | c4     |                                     | ptr program start behind tape header (hb)                 |
 | c3     |                                     | ptr program start behind tape header (lb)                 |
 | c2     |                                     | i/o start address (hb)                                    |
 | c1     |                                     | i/o start address (lb)                                    |
 | c0     |                                     | tape motor interlock                                      |
 | bf     | ZP_TEMP2                            | tape input byte buffer                                    |
 | be     | ZP_TEMP1                            | tape read/write block count                               |
 | bd     |                                     | rs-232 output parity/tape buffer                          |
 | bc     |                                     | ptr: current file name (hb)                               |
 | bb     |                                     | ptr: current file name (lb)                               |
 | ba     |                                     | current device number                                     |
 | b9     |                                     | current secondary address                                 |
 | b8     |                                     | current logical file number                               |
 | b7     |                                     | length of current file name                               |
 | b6     |                                     | rs-232 output buffer                                      |
 | b5     | ZP_TEMP4                            | rs-232 next bit to send / end of tape flag                |
 | b4     | ZP_TEMP3                            | rs-232 output bot count / tape temp storage               |
 | b3     |                                     | ptr: start of tape buffer (hb)                            |
 | b2     |                                     | ptr: start of tape buffer (lb)                            |
 | b1     |                                     | tape timer (hb)                                           |
 | b0     |                                     | tape timer (lb)                                           |
 | af     |                                     | ptr: end address of load (hb)                             |
 | ae     |                                     | ptr: end address of load (lb)                             |
 | ad     |                                     | ptr: start addess of load (hb)                            |
 | ac     |                                     | ptr: start addess of load (lb)                            |
 | ab     |                                     | rs-232 parity / tape counter                              |
 | aa     |                                     | rs-232 input byte buffer / tape temp storage              |
 | a9     |                                     | rs-232 flag: check for start bit                          |
 | a8     |                                     | rs-232 input bit count / tape temp storage                |
 | a7     |                                     | rs-232 input bits / tape temp storate                     |
 | a6     |                                     | count of chars in tape i/o buffer                         |
 | a5     |                                     | tape sync count down                                      |
 | a4     |                                     | temp i/o buffer (floppy, tape, printer)                   |
 | a3     |                                     | temp i/o buffer (floppy, tape, printer)                   |
 | a2     |                                     | internal clock                                            |
 | a1     |                                     | internal clock                                            |
 | a0     |                                     | internal clock                                            |
 | 9f     |                                     | tape error correction log                                 |
 | 9e     |                                     | tape error log                                            |
 | 9d     |                                     | flag: kernal message control                              |
 | 9c     |                                     | flag: tape byte received                                  |
 | 9b     |                                     | tape char parity                                          |
 | 9a     |                                     | default output device                                     |
 | 99     |                                     | default input device                                      |
 | 98     |                                     | number of open i/o files                                  |
 | 97     |                                     | temp buffer for x-reg save                                |
 | 96     |                                     | tape block sync number                                    |
 | 95     |                                     | buffer for char to serial bus                             |
 | 94     |                                     | flag: floppy/printer output                               |
 | 93     |                                     | flag: load or verify                                      |
 | 92     |                                     | timing constant for tape read                             |
 | 91     |                                     | flag: stop key pressed?                                   |
 | 90     |                                     | kernal I/O status word (ST)                               |
 | 8f     | end of interpreter loop             | end RND seed value                                        |
 | 8e     | :                                   | :                                                         |
 | 8d     | :                                   | :                                                         |
 | 8c     | :                                   | :                                                         |
 | 8b     | :                                   | start RND seed value                                      |
 | 8a     | :                                   | end subroutine get next basic char (CHRGET)               |
 | 89     | :                                   | :   (copied on startup from ROM $E3A2)                    |
 | 88     | :                                   | :                                                         |
 | 87     | :                                   | :                                                         |
 | 86     | ZP_VM_PC (hb)                       | :                                                         |
 | 85     | ZP_VM_PC (lb)                       | :                                                         |
 | 84     | :                                   | :                                                         |
 | 83     | :                                   | :                                                         |
 | 82     | :                                   | :                                                         |
 | 81     | :                                   | :                                                         |
 | 80     | start of interpreter-loop           | :                                                         |
 | 7f     | ZP_PAGE_FREE_SLOTS_LIST Profile 5   | :                                                         |
 | 7e     | ZP_PAGE_FREE_SLOTS_LIST Profile 4   | :                                                         |
 | 7d     | ZP_PAGE_FREE_SLOTS_LIST Profile 3   | :                                                         |
 | 7c     | ZP_PAGE_FREE_SLOTS_LIST Profile 2   | :                                                         |
 | 7b     | ZP_PAGE_FREE_SLOTS_LIST Profile 1   | :                                                         |
 | 7a     | ZP_PAGE_FREE_SLOTS_LIST Profile 0   | :                                                         |
 | 79     | ZP_PROFILE_PAGE_FREE_LIST Profile 5 | :                                                         |
 | 78     | ZP_PROFILE_PAGE_FREE_LIST Profile 4 | :                                                         |
 | 77     | ZP_PROFILE_PAGE_FREE_LIST Profile 3 | :                                                         |
 | 76     | ZP_PROFILE_PAGE_FREE_LIST Profile 2 | :                                                         |
 | 75     | ZP_PROFILE_PAGE_FREE_LIST Profile 1 | :                                                         |
 | 74     | ZP_PROFILE_PAGE_FREE_LIST Profile 0 | :                                                         |
 | 73     | ZP_CALL_FRAME_TOP_MARK              | start subroutine get next basic char (CHRGET)             |

 |#
