#lang typed/racket/base

#|review: ignore|#
#|  review does show several false positives |#

#|

  VM definition, interpreter and disassembler for the stack based virtual machine

 |#

(require (only-in racket/fixnum fx+ fx= fx< fx- fx>= fx>))
(require (only-in racket/list drop takef take empty?))
(require (only-in racket/match/match match-define))
(require (only-in racket/vector vector-append vector-set/copy))
(require/typed racket/vector [vector-set/copy (All (a) (-> (Immutable-Vectorof a) Nonnegative-Integer a (Vectorof a)))])

(provide disassemble-byte-code
         make-vm
         CONS
         CAR
         CDR
         GOTO
         RET
         BYTE+
         BRA
         CALL
         BRK
         NIL?
         TAIL_CALL

         PUSH_INT
         PUSH_ARRAY_FIELD
         PUSH_BYTE
         PUSH_NIL
         PUSH_LOCAL
         PUSH_GLOBAL
         PUSH_STRUCT_FIELD
         PUSH_PARAM

         POP_TO_PARAM
         POP_TO_LOCAL
         POP_TO_GLOBAL

         TRUE
         FALSE

         NIL_CELL

         is-push-param-short

         sPUSH_PARAMc
         sPUSH_PARAMn
         sPOP_TO_PARAMc
         sPOP_TO_PARAMn
         sNIL?-RET-PARAMc

         (struct-out cell-byte-)
         cell-byte-

         make-function-def
         make-vm
         vm--value-stack

         run-until-break
         list->cell-list-ptr
         )

(module+ test #| require test utils |#
  (require typed/rackunit))

;; data is organized in cells (prefixed with cell-
;; atomic cells hold the value they represent
;; - cell-byte-, cell-init-
;; atomic cells point to more complex structures (which are then non atomic cells)
;; - cell-ptr-, cell-list-ptr-,
;; non atomic cells are arrays, cell-pairs (as used by lists)

(struct cell- () #:transparent)

;; arbitrary number of cells
(struct celln- cell- ()
  #:transparent)

;; 16 bit element
;; exactly one cell
(struct atomic-cell- cell- ()
  #:transparent)

;; pointer to a (possibly complex, nonatomic) cell
(struct cell-ptr- atomic-cell-
  ([ref : cell-])
  #:transparent)

;; pointer to a pair, which is 2 cells memory adjacent, car (any cell) and cdr cell (usually a cell-list-ptr)
(struct cell-list-ptr- atomic-cell-
  ([car : atomic-cell-]
   [cdr : atomic-cell-])
  #:transparent)

(struct cell-nil- cell-list-ptr- ()
  #:transparent)

;; the one and only nil cell (is compared to with eq?
(define NIL_CELL (cell-nil- (atomic-cell-) (atomic-cell-)))


;; flat value 0..255 or -128..127 (used for byte, char and boolean)
(struct cell-byte- atomic-cell-
  ([value : Byte])
  #:transparent)

;; flat value 0..8191 or -4096...4095
(struct cell-int- atomic-cell-
  ([value : Integer])
  #:transparent)

;; convenience function to get the value out of a fixed number
(define (cell--value [a-cell : cell-] ) :  (U Integer Byte)
  (cond
    [(cell-byte-? a-cell) (cell-byte--value a-cell)]
    [(cell-int-? a-cell)  (cell-int--value a-cell)]
    [else                 (raise-user-error (format "not a value cell ~a" a-cell))]))

(struct cell-array- celln-
  ([size : Nonnegative-Integer]
   ;; [array : (Immutable-Vectorof atomic-cell-)]
   [id : Nonnegative-Integer]) ;; index into global array of arrays <- special for this vm, won't be implemented on c64
  #:transparent) ;; vector of atomic cells!

(struct vm-frame-
  ([fun-idx        : Nonnegative-Integer]        ;; index into vm--functions
   [bc-idx         : Nonnegative-Integer]        ;; index into byte array of the running function
   [parameter-tail : (Listof atomic-cell-)]             ;; is a pointer to the last parameter on the value stack, at the beginning of a call this is identical to value-stack!
   [locals         : (Immutable-Vectorof atomic-cell-)] ;; a list of locals identified by index
   )
  #:transparent)

(define (make-frame #:fun-idx        (fun-idx : Nonnegative-Integer 0)
                    #:bc-idx         (bc-idx : Nonnegative-Integer 0)
                    #:parameter-tail (parameter-tail : (Listof atomic-cell-) '())
                    #:locals         (locals : (Immutable-Vectorof atomic-cell-) (vector-immutable)))
        : vm-frame-
  (vm-frame- fun-idx bc-idx parameter-tail locals))

(struct vm-struct-def-
  ([name     : String]
   [field-no : Nonnegative-Integer])
  #:transparent)

(struct vm-function-def-
  ([stack-size-used : Byte] ;; how much (in addition to the parameters) is the stack used (max)
   [parameter-count : Byte] ;; how many parameters are expected (fix)
   (locals-count    : Byte) ;; how many locals are used in this function
   [name            : String]
   [byte-code       : (Immutable-Vectorof Byte)])
  #:transparent)

(define (make-function-def #:stack-size-used (stack-size-used : Byte 0)
                           #:parameter-count (parameter-count : Byte 0)
                           #:locals-count (locals-count : Byte 0)
                           #:name (name : String "some-func")
                           #:byte-code (byte-code : (Immutable-Vectorof Byte) (vector-immutable 0 1 2 3 4)))
        : vm-function-def-
  (vm-function-def- stack-size-used parameter-count locals-count name byte-code))

;; TODO: in order to allow constants to be generated for racket-stack-vm and c64-stack-vm,
;;       the representation need to be byte-encoded and compact
;;       => globals are renamed into memory, being an array of bytes. pushing constants/globals will
;;          decode the bytes into atomic cells, since no other cells may be pushed
;;          popping cells into globals is only valid for atomic cells?
;;       => the heap shares its memory with globals
;;          cell-ptr- point into the heap (globals etc. likewise)
;;          the global idx is just a pointer (since ints are 13 bit, globals may only reside in lower memory)
;;          globals and dynamically allocated cells may reference each other (through pointers) that all
;;          point into the heap array of byte values
;;          the heap need not be allocated continuously (e.g. having pages for arrays, for cell-pairs etc.)
;; TODO: structs need to be converted to bytes and vice versa?
;; TODO: arrays are directly stored in the heap-structure

;; How about racket vm decoding data bytes -> cells at startup, from then on working with cells only?
;; of course the byte encoding need to be interpretable for the c64
;; [constants, byte encoded (relocatable)]
;; [functions, byte encoded (relocatable)] ;; functions reference (through idx) constants of this or other modules), functions reference functions (through idx) of this module or others


;; idea: each index reference (locals, globals, functions ...) is translated through a module specific translation table
;;   advantage:    relocatable, no need to do any adjustments on load
;;   disadvantage: additional indirection, additional memory overhead <-- how about making that module optional (e.g. modules may use this and stay relocatable, or modules don't use it and are pinned) => modules need some data
;; other idea: generated byte code is relocatable, but writing it to memory will resolve indices to actual memory addresses
;; what about relocating again (usage: move code/data to other memory page, e.g. module wise to gain benefits from extended memory) <- is that really a requirement?
;;  (could be done if reloading and no current state is needed)

(struct vm-
  ([frame-stack : (Listof vm-frame-)]
   [value-stack : (Listof atomic-cell-)]
   [functions   : (Immutable-Vectorof vm-function-def-)]
   ;; [heap        : (Mutable-Vectorof Byte)]
   [globals     : (Immutable-Vectorof atomic-cell-)]
   [structs     : (Immutable-Vectorof vm-struct-def-)]
   [options     : (Listof Symbol)]
   [arrays      : (Immutable-Vectorof (Vectorof atomic-cell-))])
  #:transparent)

;; musing about heap and such
;; vm execution in racket is faster and less complex if decoded info in structures is available
;; dynamically created values on the heap may reference arrays
;; arrays may reference elements on the heap -> idx must be handled appropriately
;; e.g. in heap CELL_PTR -> heap CELL_ARRAY
;;      in heap CELL_PTR -> vm array CELL_ARRAY
;;      in heap CELL_ARRAY -> CELL_PTR -> heap ...
;;      in heap CELL_ARRAY -> CELL_PTR -> vm array CELL_ARRAY ...
;; ... etc

;;                           <-(encode and write):   (generated by compiler)
;; idea: byte-encoded module ->(load) relocatable-module- ->(link and/or pin) resolved-module- <- is exectuable
;;       be-...
;;                             rl-...
;;                                                          vm-...
;; this should be a process that can be implemented by a c64 (6510) loader and linker, too

;; other idea: encode vm interpreter exactly as on 6510 => cell = 2 bytes (no structure), push_... => copy 2 bytes heap->heap ...
;;   advantage: implementation on c64 is simple, implementation on c64 is always possible
;;   disadvantage: implementation is more complex (memory management, encoding/decoding, access etc.), c64 restriction may apply etc.
;;   => vm interpreter may be different but should keep restrictions in mind (and keep close to c64 impl. wherever feasable)
;;   => no direct correspondence racket-vm-interpreter, c64-vm-interpreter (but of course transitive correspondence)
;;   => bytecode-correspondence, bytecode-format-correspondence
;;   => racket vm may translate bytecode representation in some runtime representation more suitable to vm execution
;;   => compiler should generate bytcode suitable for c64-vm and racket-vm (since the same) alike
;;   => bytecode encoding (be-) is uniform over vm implementations
;;   => loader can be racket specific
;;   => vm bytecode interpretation can be racket specific (using vm- structures which may differ [slightly] from c64 implementations)


(struct rl-module-
  ((globals   : (Immutable-Vectorof cell-))
   (functions : (Immutable-Vectorof rl-function-def-))))

(struct rl-reloc-entry-
  ((offset    : Nonnegative-Fixnum)     ;; into the bytecode
   (byte-size : Byte)                   ;; width of reference (usually 2 = 2 byte index)
   (index     : Nonnegative-Integer)))  ;; current index (if known), could also be encoded in the bytecode already

(struct rl-function-def-
  ([stack-size-used : Byte] ;; how much (in addition to the parameters) is the stack used (max)
   [parameter-count : Byte] ;; how many parameters are expected (fix)
   (locals-count    : Byte) ;; how many locals are used in this function
   [name            : String]
   [reloc-bytecode  : rl-bytecode-])
  #:transparent)

(struct rl-bytecode-
  ((byte-code       : (Immutable-Vectorof Byte))
   (global-relocs   : (Listof rl-reloc-entry-))    ;; list of reloc entries of references to global variables
   (function-relocs : (Listof rl-reloc-entry-))))  ;; list of reloc entries of references to (other) functions

(define (make-vm
         #:frame-stack (frame-stack : (Listof vm-frame-) (list (make-frame)))
         #:value-stack (value-stack : (Listof atomic-cell-) '())
         #:functions (functions : (Immutable-Vectorof vm-function-def-) (vector-immutable))
         ;; #:heap (heap : (Mutable-Vectorof Byte) (make-vector 65536 (cast #b0 Byte)))
         #| deprecated (should be held in heap) |# #:globals (globals : (Immutable-Vectorof atomic-cell-) (vector-immutable))
         #:structs (structs : (Immutable-Vectorof vm-struct-def-) (vector-immutable))
         #:options (options : (Listof Symbol) '())
         #| deprecated (should be held in heap) |# #:arrays (arrays : (Immutable-Vectorof (Vectorof atomic-cell-)) (vector-immutable))) ;; kept to allow modifications (not part of c64 implementation)
        : vm-
  (vm- frame-stack value-stack functions #| heap |# globals structs options arrays))

;; utility functions for arrays/structures
(define (set-array-value [vm : vm-] [array : cell-array-] [idx : Nonnegative-Integer] [value : atomic-cell-]) : vm-
  (define value-array (vector-ref (vm--arrays vm) (cell-array--id array)))
  (define new-value-array (vector-set/copy (vector->immutable-vector value-array) idx value))
  (define new-arrays (vector->immutable-vector (vector-set/copy (vm--arrays vm) (cell-array--id array) new-value-array)))
  (struct-copy vm- vm
               [arrays new-arrays]))

(define (get-array-value [vm : vm-] [array : cell-array-] [idx : Integer]) : atomic-cell-
  (define value-array (vector-ref (vm--arrays vm) (cell-array--id array)))
  (vector-ref value-array idx))

(module+ test #| set-array-value |#
  (check-equal? (get-array-value
                 (make-vm #:arrays (vector->immutable-vector (vector (vector (ann (cell-byte- 0) atomic-cell-)
                                                                            (ann (cell-byte- 1) atomic-cell-)))))
                 (cell-array- 2 0)
                 1)
                (cell-byte- 1))
  (check-equal? (get-array-value
                 (set-array-value (make-vm #:arrays (vector->immutable-vector (vector (vector (ann (cell-byte- 0) atomic-cell-)
                                                                                             (ann (cell-byte- 1) atomic-cell-)))))
                                  (cell-array- 2 0)
                                  1
                                  (cell-int- 10))
                 (cell-array- 2 0)
                 1)
                (cell-int- 10)))


;; naming convention
;; {(s)hort | (p)refix} COMMAND {(m)ask | (n)umber-mask | (m)ost(s)ignificant(b)it position}
;; (s) short commands reduce the number of bytes by reducing the domain of the operands
;; (m) mask bytes are used to mask the relevant bits for the command to be selected
;; (n) number mask bytes are uesd to mask the relevant bits for the the operand
;; (msb) describe the first relevant bit position for the operand

;; basic concept: 1 byte = command, following are operands (usually 1 byte each)
;; commands operate mostly on tos
;; (p) prefix commands: modify the behaviour of the following command or change the operand sizes for the command operands

(define BRK                  0) ;; stop
(define pLONG                1) ;; double operand size
(define NOP                  2) ;; just increase pc (no operation)

(define PUSH_BYTE            5) ;; op = byte value, stack [] -> [cell-byte]
(define PUSH_INT             6) ;; op1=low byte op2=high byte, stack [] -> [cell-int]
;; also used for struct-index or function-index

(define PUSH_NIL             9) ;; stack: [] -> [NIL]
(define PUSH_PARAM          10) ;; op = param-idx from tail, stack [] -> [cell-]
(define PUSH_GLOBAL         11) ;; op1=low byte index op2=high byte index stack [] -> [cell-]
(define PUSH_LOCAL          12) ;; op = local-idx, stack [] -> [cell-]
(define PUSH_STRUCT_FIELD   13) ;; op = field-idx, stack [struct-ref] -> [cell-]
(define PUSH_ARRAY_FIELD    14) ;; op = field-idx, stack [array-ref] -> [cell-]

(define POP_TO_PARAM        15) ;; op= param-idx from tail, stack [cell-] -> []
(define POP_TO_GLOBAL       16) ;; op1=low byte index op2=high byte index, stack [cell-] -> []
(define POP_TO_LOCAL        17) ;; op = local-idx, stack [cell-] -> []
(define POP_TO_STRUCT_FIELD 18) ;; op = field-idx, stack [cell- struct-ptr-] -> []
(define POP_TO_ARRAY_FIELD  19) ;; op = array-idx, stack [cell- array-ptr-] -> []

(define NIL?                20) ;; stack [cell-list-ptr] -> [cell-boolean]

(define BRA                 31) ;; op = relative offset
(define GOTO                32) ;; op = relative offset
(define RET                 33) ;; stack [cell paramN, ... cell param1, cell param0] -> []
(define CALL                34) ;; stack [int-cell: function index, cell paramN, ... cell param1, cell param0] -> [cell paramN, ... cell param1, cell param0]
(define TAIL_CALL           35) ;; stack [new-paramN .. new-param0, ..., original-paramN ... original-param0] -> [new-paramN .. new-param0]
(define NIL?-RET-PARAM      36) ;; op = param, stack [ ... paramN .. param0 ] -> [ paramOP ] if tos is nil, else no change!
(define NIL?-RET-LOCAL      37) ;; op = param, stack [ ... paramN .. param0 ] -> [ localOP ] if tos is nil, else no change!

(define CAR                 40) ;; stack [cell-list-ptr] -> [cell- car of list pointed at]
(define CDR                 41) ;; stack [cell-list-ptr] -> [cell-list-ptr cdr of list pointed at]
(define CONS                42) ;; stack [cell- car, cell-list-ptr cdr] -> stack [cell-list-ptr new-list]

(define BYTE+               60) ;; stack [cell-byte a, cell-byte b] -> [sum]

;; BYTE-INC, BYTE-DEC, BYTE-, BYTE*, BYTE_DIV, BYTE_REM
;; INT-INC, INT-DEC, INT+, INT-, INT*, INT_DIV, INT_REM
;; BOOL_AND, BOOL_OR, BOOL_NOT
;; BIT_AND, BIT_OR, BIT_NOT, BIT_XOR
;; BIT_SHIFT_RIGHT, BIT_SHIFT_LEFT

;; BYTE>, BYTE<, BYTE=, BYTE>=, BYTE<=, BYTE!=
;; INT>, INT<, INT=, INT>=, INT<=, INT!=

;; BRA_NOT

;; STRUCT_LEN, ARRAY_LEN
;; STRUCT_COPY, ARRAY_COPY

;; THROW_EXCEPTION

;; LIST->ARRAY ;; since string and structs are arrays, there is no need to provide others
;; ARRAY->LIST ;;

;; native functions (foreign interface)
;; FI_POKE_BYTE, FI_PEEK_BYTE
;; FI_CALL
;; FI_BYTE_ARRAY- <- allocate to fixed position in memory
;;  - read/write/copy

(define ALLOCATE_STRUCT     70) ;; op = struct-def-idx (int),  stack [] -> [struct-ref-]
(define FREE_STRUCT         71) ;; stack [struct-ref-] -> []
(define CREATE_STRUCT       72) ;; op = struct-def-idx (int), stack [fieldN .. field0] -> [struct-ref-]

(define CREATE_LIST         73) ;; op = N+1 (byte), stack [elN .. el0] -> [cell-list-ptr-]

(define ALLOCATE_ARRAY      75) ;; op = array len, stack [] -> [array-ref-]
(define FREE_ARRAY          76) ;; stack [array-ref-] -> []

;; example of short (one byte instruction)
;; using 128..131
(define sPUSH_PARAM         #b10000000) ;; short push param, lower 2 bits
(define sPUSH_PARAMm        #b11111100)
(define sPUSH_PARAMn        (bitwise-xor #xff sPUSH_PARAMm))
;; using 132..135
;; could also be used for sPUSH__FIELD pushing array/struct fields
(define sPUSH_GLOBAL        #b10000100) ;; short push global, lower 2 bits + next byte
(define sPUSH_GLOBALm       #b11111100)
(define sPUSH_GLOBALn       (bitwise-xor #xff sPUSH_GLOBALm))
;; using 136..139
(define sPUSH_LOCAL         #b10001000) ;; short push local, lower 2 bits
(define sPUSH_LOCALm        #b11111100)
(define sPUSH_LOCALn        (bitwise-xor #xff sPUSH_LOCALm))
;; using 140..143
(define sPUSH_BYTE          #b10001100) ;; short push byte, lower 2 bits  #b00 = #x00, #b01 = #x01, #b10 = #x02|#xfe (-2), #b11 = #xff (-1)
(define sPUSH_BYTEm         #b11111100)
(define sPUSH_BYTEn         (bitwise-xor #xff sPUSH_BYTEm))

;; using 144..147
(define sPOP_TO_PARAM       #b10010000) ;; short pop to param, lower 2 bits
(define sPOP_TO_PARAMm      #b11111100)
(define sPOP_TO_PARAMn      (bitwise-xor #xff sPOP_TO_PARAMm))
;; using 148..151
;; could also be used for POP_TO_FIELD poping into array/struct fields
(define sPOP_TO_GLOBAL      #b10010100) ;; short pop to global, lower 2 bits + next byte
(define sPOP_TO_GLOBALm     #b11111100)
(define sPOP_TO_GLOBALn     (bitwise-xor #xff sPOP_TO_GLOBALm))
;; using 152..155
(define sPOP_TO_LOCAL       #b10011000) ;; short pop to local, lower 2 bits
(define sPOP_TO_LOCALm      #b11111100)
(define sPOP_TO_LOCALn      (bitwise-xor #xff sPOP_TO_LOCALm))

;; using 156..159
(define sNIL?-RET-PARAM     #b10011100)
(define sNIL?-RET-PARAMm    #b11111100)
(define sNIL?-RET-PARAMn    (bitwise-xor #xff sNIL?-RET-PARAMm))
;; using 160..163
(define sNIL?-RET-LOCAL     #b10100000)
(define sNIL?-RET-LOCALm    #b11111100)
(define sNIL?-RET-LOCALn    (bitwise-xor #xff sNIL?-RET-LOCALm))

;; this is using lots of slots, maybe goto and bra are not used often enough to allow this
;; maybe push/pop of structure fields/arrays is more valuable here
;; using 192..223
(define sBRA                #b11000000)
(define sBRAm               #b11100000)
(define sBRAn               #b00011111)
(define sBRAmsb             5)
;; using 224..255
(define sGOTO               #b11100000)
(define sGOTOm              #b11100000)
(define sGOTOn              #b00011111)
(define sGOTOmsb            5)

(define (integer->two-complement [value : Integer]) : Nonnegative-Integer
  (cond
    [(fx< value 0)
     (define new-val (fx+ #x10000 value))
     (if (fx< new-val #x8000)
         (raise-user-error (format "integer out of range ~a" value))
         new-val)]
    [(fx< value #x8000) value]
    [else (raise-user-error (format "integer out of range ~a" value))]))

(module+ test #| integer->two-complement |#
  (check-equal? (integer->two-complement -1)
                #xFFFF)
  (check-equal? (integer->two-complement 1)
                #x0001)
  (check-equal? (integer->two-complement 0)
                #x0000)
  (check-equal? (integer->two-complement -32768)
                #x8000)
  (check-equal? (integer->two-complement 32767)
                #x7FFF)
  (check-exn exn:fail? (lambda () (integer->two-complement -32769)))
  (check-exn exn:fail? (lambda () (integer->two-complement 32768))))

(define (two-complement->signed-byte [a : Byte] [start-bit : Byte 8]) : Integer
  (define max-positive-p1 (arithmetic-shift #x80 (fx- start-bit 8)))
  (define mask (arithmetic-shift #xff (fx- start-bit 8)))
  (define masked-a (bitwise-and a))
  (cond
    [(< masked-a max-positive-p1) masked-a]
    [else (fx- masked-a (arithmetic-shift max-positive-p1 1))]))

(module+ test #| two-complement->signed-byte |#
  (check-equal? (two-complement->signed-byte #x7f)
                127)
  (check-equal? (two-complement->signed-byte 0)
                0)
  (check-equal? (two-complement->signed-byte #x80)
                -128)
  (check-equal? (two-complement->signed-byte #xff)
                -1)

  (check-equal? (two-complement->signed-byte #x7f 8)
                127)
  (check-equal? (two-complement->signed-byte 0 8)
                0)
  (check-equal? (two-complement->signed-byte #x80 8)
                -128)
  (check-equal? (two-complement->signed-byte #xff 8)
                -1)

  (check-equal? (two-complement->signed-byte #x07 4)
                7)
  (check-equal? (two-complement->signed-byte 0 4)
                0)
  (check-equal? (two-complement->signed-byte #x08 4)
                -8)
  (check-equal? (two-complement->signed-byte #x0f 4)
                -1)

  (check-equal? (two-complement->signed-byte #x1f 6)
                31)
  (check-equal? (two-complement->signed-byte 0 6)
                0)
  (check-equal? (two-complement->signed-byte #x40 6)
                0)
  (check-equal? (two-complement->signed-byte #x20 6)
                -32)
  (check-equal? (two-complement->signed-byte #x3f 6)
                -1))

(define (running-function (vm : vm-)) : vm-function-def-
  (vector-ref (vm--functions vm) (vm-frame--fun-idx (car (vm--frame-stack vm)))))

(define (peek-pc-byte [vm : vm-] [ delta : Nonnegative-Integer 0]) : Byte
  (define active-frame (car (vm--frame-stack vm)))
  (define function (running-function vm))
  (vector-ref (vm-function-def--byte-code function) (fx+ delta (vm-frame--bc-idx active-frame))))

(define (peek-pc-int [vm : vm-] [ delta : Nonnegative-Integer 0]) : Nonnegative-Integer
  (define active-frame (car (vm--frame-stack vm)))
  (define function (running-function vm))
  (define bc-index (fx+ delta (vm-frame--bc-idx active-frame)))
  (define low-byte (vector-ref (vm-function-def--byte-code function) bc-index))
  (define high-byte (vector-ref (vm-function-def--byte-code function) (fx+ 1 bc-index)))
  (fx+ (arithmetic-shift high-byte 8) low-byte))

(module+ test #| peek-pc-byte |#
  (check-equal? (peek-pc-byte
                 (make-vm #:frame-stack (list (make-frame #:bc-idx 2))
                          #:functions (vector-immutable
                                       (make-function-def #:byte-code (vector-immutable 7 8 9 10 11)))))
                9)

  (check-equal? (peek-pc-byte
                 (make-vm #:frame-stack (list (make-frame #:bc-idx 2))
                          #:functions (vector-immutable
                                       (make-function-def #:byte-code (vector-immutable 7 8 9 10 11))))
                 1)
                10))

(define (increment-pc [vm : vm-] [delta : Integer 1]) : vm-
  (define active-frame (car (vm--frame-stack vm)))
  (define new-bc-idx (fx+ delta (vm-frame--bc-idx active-frame)))
  (define new-frame (struct-copy vm-frame- active-frame
                                 [bc-idx (if (fx< new-bc-idx 0)
                                             (raise-user-error (format "pc reduced to negative offset ~a" new-bc-idx))
                                             new-bc-idx)]))
  (struct-copy vm- vm
               [frame-stack (cons new-frame (cdr (vm--frame-stack vm)))]))

(module+ test #| increment-pc |#
  (check-equal? (peek-pc-byte
                 (increment-pc
                  (make-vm #:frame-stack (list (make-frame #:bc-idx 2))
                           #:functions (vector-immutable
                                        (make-function-def #:byte-code (vector-immutable 7 8 9 10 11))))))
                10))

(define (tos-value [vm : vm-] [offset : Byte 0]) : atomic-cell-
  (car (drop (vm--value-stack vm) offset)))

(define (pop-value [vm : vm-] [count : Byte 1]) : vm-
  (struct-copy vm- vm [value-stack (drop (vm--value-stack vm) count)]))

;; inner function for tail call recursion required, because recursive typed call w/ optional field does not typecheck
(define (pop-and-get-values [vm : vm-] [count : Nonnegative-Integer]) : (List (Listof atomic-cell-) vm-)
  (define (pop-values-[vm : vm-] [count : Nonnegative-Integer] [acc : (Listof atomic-cell-)]) : (List (Listof atomic-cell-) vm-)
    (cond
      [(fx< 0 count)
       (pop-values- (ann (pop-value vm) vm-) (ann (sub1 count) Nonnegative-Integer) (ann (cons (tos-value vm) acc) (Listof atomic-cell-)))]
      [else (list acc vm)]))
  (pop-values- vm count '()))

(define (push-value [vm : vm-] [value : atomic-cell-]) : vm-
  (struct-copy vm- vm [value-stack (cons value (vm--value-stack vm))]))

;; ensure overflow is caught to enable type checking result to be Byte
(define (byte+ [a : Byte] [b : Byte]) : Byte
  (define result (fx+ a b))
  (if (byte? result)
      result
      (raise-user-error (format "byte+ overflow ~a + ~a" a b))))

(module+ test #| byte+ |#
  (check-equal? (byte+ 1 2)
                3)
  (check-equal? (byte+ 254 1)
                255)
  (check-equal? (byte+ 255 0)
                255)
  (check-exn exn:fail? (lambda () (byte+ 128 128)))
  (check-exn exn:fail? (lambda () (byte+ 255 1))))

;; bytecode: op, len: 1b
;; stack: [ func-idx:cell-int- pN ... p1 p0 ] -> [ pN ... p1 p0 ], growth: -1c
(define (interpret-call [vm : vm-]) : vm-
  (define fun-idx (integer->two-complement (cell--value (tos-value vm))))
  (define locals-count (vm-function-def--locals-count (vector-ref (vm--functions vm) fun-idx)))
  (define new-frame (make-frame #:fun-idx fun-idx
                                #:bc-idx  0
                                #:parameter-tail (cdr (vm--value-stack vm))
                                #:locals (vector->immutable-vector (make-vector locals-count (atomic-cell-)))))
  (struct-copy vm- vm
               [frame-stack (cons new-frame (vm--frame-stack (increment-pc vm)))]
               [value-stack (cdr (vm--value-stack vm))]))

(define (active-function-param-no (vm : vm-)) : Byte
  (vm-function-def--parameter-count
   (vector-ref (vm--functions vm)
               (vm-frame--fun-idx (car (vm--frame-stack vm))))))

;; bytecode: op, len: 1b
;; stack: [ res ... pN .. p1 p0 ...] -> [ res ... ], growth: -N-1 - any other stuff on the stack (for n+1 parameter + any other stuff)
(define (interpret-ret [vm : vm-]) : vm-
  ;; drop the parameters
  (define value-stack (vm--value-stack vm))
  (define param-tail (vm-frame--parameter-tail (car (vm--frame-stack vm))))
  (define new-value-stack (cons (car value-stack)
                                (drop param-tail (active-function-param-no vm))))
  (struct-copy vm- vm [frame-stack (cdr (vm--frame-stack vm))]
               [value-stack new-value-stack]))

(module+ test
  (define interpret-ret--vm
    (interpret-ret (make-vm #:value-stack (list (cell-byte- 10) (atomic-cell-) (atomic-cell-))
                            #:frame-stack (list (make-frame #:fun-idx 0 #:bc-idx 0 #:parameter-tail (list (atomic-cell-) (atomic-cell-))))
                            #:functions (vector-immutable (make-function-def #:parameter-count 2)))))

  (check-equal? (vm--value-stack interpret-ret--vm)
                (list (cell-byte- 10))
                "dropping 2 parameters from value stack should keep tos as value returned")
  (check-equal? (vm--frame-stack interpret-ret--vm)
                (list)
                "returning from the only function on the stack should leave an empty frame stack")

  (define interpret-ret--vm2
    (interpret-ret (make-vm #:value-stack (list (cell-byte- 10) (atomic-cell-) (atomic-cell-) (cell-byte- 20))
                            #:frame-stack (list (make-frame #:fun-idx 0 #:bc-idx 0 #:parameter-tail (list (atomic-cell-) (atomic-cell-) (cell-byte- 20)))
                                                (make-frame #:fun-idx 1 #:bc-idx 10))
                            #:functions (vector-immutable (make-function-def #:parameter-count 2)))))

  (check-equal? (vm--value-stack interpret-ret--vm2)
                (list (cell-byte- 10) (cell-byte- 20))
                "dropping two parameters should leave tos (10) and the value before the parameters (20)")
  (check-equal? (vm--frame-stack interpret-ret--vm2)
                (list (make-frame #:fun-idx 1 #:bc-idx 10))
                "returning should pop the top frame, leaving the next"))

;; bytecode: op, len: 1b
;; stack: [ a:cell-byte b:cell-byte ... ] -> [ a+b:cell-byte- ... ], growth: -1c
(define (interpret-byte+ [vm : vm-]) : vm-
  ;; replace tos and tos-1 with sum (of the two bytes)
  (match-define (list (list a b) new-vm) (pop-and-get-values vm 2))
  (if (and (cell-byte-? a)
         (cell-byte-? b))
      (increment-pc
       (push-value new-vm (cell-byte- (byte+ (cell-byte--value a)
                                             (cell-byte--value b)))))
      (raise-user-error (format "byte+ encountered non byte in ~a or ~a" a b))))

(module+ test #| interpret-byte+ |#
  (check-equal? (vm--value-stack (interpret-byte+ (make-vm #:value-stack (list (cell-byte- 1) (cell-byte- 2))
                                                           #:frame-stack (list (make-frame)))))
                (list (cell-byte- 3))))

;; bytecode: op idx:byte, len: 2b
;; stack: [ ... pN .. p1 p0] -> [ pIDX ... pN .. p1 p0], growth: 1c
(define (interpret-push-param [vm : vm-]) : vm-
  (define param-idx (peek-pc-byte vm 1))
  (interpret-push-param- vm param-idx 2))

(define (interpret-push-param- [vm : vm-] [idx : Nonnegative-Integer] [pc-inc : Byte]) : vm-
  (define param-value (car (drop (vm-frame--parameter-tail (car (vm--frame-stack vm))) idx)))
  (increment-pc (push-value vm param-value) pc-inc))

(module+ test #| interpret-push-param |#
  (define test_interpret-push-param
    (interpret-push-param
     (make-vm #:functions (vector-immutable (make-function-def #:byte-code (vector-immutable 0 2 0 0)))
              #:frame-stack (list (make-frame #:parameter-tail (list (atomic-cell-) (atomic-cell-) (cell-int- 10) (atomic-cell-)))))))

  (check-equal? (tos-value test_interpret-push-param)
                (cell-int- 10)
                "the pushed value is ten (which was the 2nd parameter in the parameter-tail list")

  (check-equal? (vm-frame--bc-idx (car (vm--frame-stack test_interpret-push-param)))
                2
                "the program counter got incremented by 2 (push + operand)"))

;; bytecode: op val:byte, len: 2b
;; stack: [ ... pN .. p1 p0] -> [ val:cell-byte- ... pN .. p1 p0], growth: 1c
(define (interpret-push-byte [vm : vm-]) : vm-
  (define byte-value (peek-pc-byte vm 1))
  (interpret-push-byte- vm byte-value 2))

(define (interpret-push-byte- [vm : vm-] [value : Byte] [pc-inc : Byte]) : vm-
  (increment-pc (push-value vm (cell-byte- value)) pc-inc))

;; bytecode: op val-low:byte val-high:byte, len: 3b
;; stack: [ ... ] -> [ val:cell-int- ... ], growth: 1c
(define (interpret-push-int [vm : vm-]) : vm-
  (define int-value (peek-pc-int vm 1))
  (increment-pc (push-value vm (cell-int- int-value)) 3))

;; bytecode: op idx-low:byte idx-high:byte, len: 3b
;; stack: [ ... ] -> [ global@IDX:cell- ... ], growth: 1c
(define (interpret-push-global [vm : vm-]) : vm-
  (define global-idx (peek-pc-int vm 1))
  (interpret-push-global- vm global-idx 3))

(define (interpret-push-global- [vm : vm-] [idx : Nonnegative-Integer] [ pc-inc : Byte]) : vm-
  (define global-value (vector-ref (vm--globals vm) idx))
  (increment-pc (push-value vm global-value) pc-inc))

;; bytecode: op idx:byte, len: 2b
;; stack: [ val ... pN .. p1 p0] -> [ ... pN .. pIDX=val .. p1 p0], growth: -1c
(define (interpret-pop-to-param [vm : vm-]) : vm-
  (define parameter-offset-from-tail (peek-pc-byte vm 1))
  (interpret-pop-to-param- vm parameter-offset-from-tail 2))

(define (interpret-pop-to-param- [vm : vm-] [tail-offset : Byte] [pc-inc : Byte]) : vm-
  (define value-stack (vm--value-stack vm))
  (define active-frame (car (vm--frame-stack vm)))
  (define parameter-tail (vm-frame--parameter-tail active-frame))
  (define values-without-parameters (takef value-stack (lambda (elt) (not (eq? elt (car parameter-tail))))))
  (define new-parameter-tail (append (take parameter-tail tail-offset)
                                     (list (car value-stack))
                                     (drop parameter-tail (add1 tail-offset))))
  (define new-frame (struct-copy vm-frame- active-frame
                                 [parameter-tail new-parameter-tail]))
  (increment-pc
   (struct-copy vm- vm
                [value-stack (append (cdr values-without-parameters) new-parameter-tail)]
                [frame-stack (cons new-frame (cdr (vm--frame-stack vm)))])
   pc-inc))

(module+ test #| interpret pop to param |#
  (define test_param_2-values-interpret-pop-to-param
    (list (cell-byte- 10) (atomic-cell-) #| params tail is here |# (atomic-cell-) (atomic-cell-) (cell-byte- 9) (atomic-cell-)))
  (define test_param_2-interpret-pop-to-param
    (interpret-pop-to-param
     (make-vm #:functions (vector-immutable (make-function-def #:byte-code (vector-immutable 0 2 0 0)))
              #:frame-stack (list (make-frame #:parameter-tail (cddr test_param_2-values-interpret-pop-to-param)))
              #:value-stack test_param_2-values-interpret-pop-to-param)))

  (check-equal? (vm-frame--parameter-tail (car (vm--frame-stack test_param_2-interpret-pop-to-param)))
                (list (atomic-cell-) (atomic-cell-) (cell-byte- 10) (atomic-cell-))
                "replace exactly the 2nd parameter (from tail)")

  (check-equal? (vm--value-stack test_param_2-interpret-pop-to-param)
                (list (atomic-cell-) #| params tail is here |# (atomic-cell-) (atomic-cell-) (cell-byte- 10) (atomic-cell-))
                "the value stack should contain the parameter tail (and have the 2nd parameter replaced)")

  (define test_param_0-values-interpret-pop-to-param
    (list (cell-byte- 10) #| params tail is here |# (cell-byte- 9) (atomic-cell-) (atomic-cell-) (atomic-cell-)))
  (define test_param_0-interpret-pop-to-param
    (interpret-pop-to-param
     (make-vm #:functions (vector-immutable (make-function-def #:byte-code (vector-immutable 0 0 0 0)))
              #:frame-stack (list (make-frame #:parameter-tail (cdr test_param_0-values-interpret-pop-to-param)))
              #:value-stack test_param_0-values-interpret-pop-to-param)))

  (check-equal? (vm-frame--parameter-tail (car (vm--frame-stack test_param_0-interpret-pop-to-param)))
                (list (cell-byte- 10) (atomic-cell-) (atomic-cell-) (atomic-cell-))
                "replace the 1st parameter (from tail)")

  (check-equal? (vm--value-stack test_param_0-interpret-pop-to-param)
                (list #| params tail is here |# (cell-byte- 10) (atomic-cell-) (atomic-cell-) (atomic-cell-))
                "replace 1st parameter being the tos after this pop!")

  (define test_param_4-values-interpret-pop-to-param
    (list (cell-byte- 10) (atomic-cell-) #| params tail is here |# (atomic-cell-) (atomic-cell-) (atomic-cell-) (atomic-cell-) (cell-byte- 9)))
  (define test_param_4-interpret-pop-to-param
    (interpret-pop-to-param
     (make-vm #:functions (vector-immutable (make-function-def #:byte-code (vector-immutable 0 4 0 0)))
              #:frame-stack (list (make-frame #:parameter-tail (cddr test_param_4-values-interpret-pop-to-param)))
              #:value-stack test_param_4-values-interpret-pop-to-param)))

  (check-equal? (vm-frame--parameter-tail (car (vm--frame-stack test_param_4-interpret-pop-to-param)))
                (list (atomic-cell-) (atomic-cell-) (atomic-cell-) (atomic-cell-) (cell-byte- 10))
                "replace the last parameter (from tail = first param passed)")

  (check-equal? (vm--value-stack test_param_4-interpret-pop-to-param)
                (list (atomic-cell-) #| params tail is here |# (atomic-cell-) (atomic-cell-) (atomic-cell-) (atomic-cell-) (cell-byte- 10))
                "replace last parameter keeping rest of value stack"))

;; bytecode: op idx-low:byte idx-high:byte, len: 3b
;; stack: [ val ... ] -> [ ...], growth: -1c
;; global@IDX = val
(define (interpret-pop-to-global [vm : vm-]) : vm-
  (define global-idx (peek-pc-int vm 1))
  (increment-pc
   (struct-copy vm- vm
                [value-stack (cdr (vm--value-stack vm))]
                [globals (cast (vector->immutable-vector
                                (vector-set/copy (vm--globals vm) global-idx (tos-value vm)))
                               (Immutable-Vectorof atomic-cell-))])
   3))

(module+ test #| interpret pop to global |#
  (define test-interpret-pop-to-global
    (interpret-pop-to-global
     (make-vm #:functions (vector-immutable (make-function-def #:byte-code (vector-immutable 0 2 0 0)))
              #:globals (vector-immutable (atomic-cell-)(atomic-cell-)(cell-byte- 9)(atomic-cell-))
              #:value-stack (list (cell-byte- 10) (atomic-cell-)))))
  (check-equal? (vm--globals test-interpret-pop-to-global)
                (vector-immutable (atomic-cell-)(atomic-cell-)(cell-byte- 10)(atomic-cell-))))

(define TRUE (cell-byte- #xff))
(define FALSE (cell-byte- #x00))

;; bytecode: op
;; stack: [ cell-list-ptr ...] -> [ cell-bool ... ], growth: 0
(define (interpret-nil? [vm : vm-]) : vm-
  (define list-ptr (tos-value vm))
  (unless (cell-list-ptr-? list-ptr)
    (raise-user-error (format "operand for nil? is not a list but ~a" list-ptr)))
  (increment-pc
   (push-value (pop-value vm) (if (eq? list-ptr NIL_CELL) TRUE FALSE))))

(define (interpret-nil?-ret-param [vm : vm-]) : vm-
  (interpret-nil?-ret-param- vm (peek-pc-byte vm 1) 2))

(define (interpret-nil?-ret-param- [vm : vm-] [param-idx : Byte] [pc-inc : Byte]) : vm-
  (define list-ptr (tos-value vm))
  (unless (cell-list-ptr-? list-ptr)
    (raise-user-error (format "operand for nil? is not a list but ~a" list-ptr)))
  (if (eq? list-ptr NIL_CELL)
      (interpret-ret (push-value (pop-value vm) (car (drop (vm-frame--parameter-tail (car (vm--frame-stack vm))) param-idx))))
      (increment-pc vm pc-inc)))

(define (tos-eq-byte? (vm : vm-) (value : Byte)) : Boolean
  (define tos (tos-value vm))
  (= value
     (if (cell-byte-? tos)
         (cell-byte--value tos)
         (raise-user-error (format "expected byte as tos, got ~a" tos)) )))

(define (tos-value-true? (vm : vm-)) : Boolean
  (tos-eq-byte? vm (cell-byte--value TRUE)))

(define (tos-value-false? (vm : vm-)) : Boolean
  (tos-eq-byte? vm (cell-byte--value FALSE)))

;; bytecode: op two-complement-offset, len: 2b
;; stack: [ val:cell-byte ... ]  -> [ ... ], growth: -1c
(define (interpret-bra [vm : vm-]) : vm-
  (interpret-bra- vm (two-complement->signed-byte (peek-pc-byte vm 1)) 2))

(define (interpret-bra- [vm : vm-] [signed-byte : Integer] [pc-inc : Byte]) : vm-
  (define delta (if (tos-value-false? vm)
                    pc-inc
                    (+ 1 signed-byte)))
  (increment-pc (pop-value vm) delta))

;; bytecode: op two-complement-offset, len: 2b
;; stack: [ ... ]  -> [ ... ], growth: 0c
(define (interpret-goto [vm : vm-]) : vm-
  (interpret-goto- vm (two-complement->signed-byte (peek-pc-byte vm 1))))

(define (interpret-goto- [vm : vm-] [signed-byte : Integer]) : vm-
  (define delta (+ 1 signed-byte))
  (increment-pc vm delta))

;; bytecode: op, len: 1b
;; stack: [ cell-list-ptr ... ] -> [ car-of-list ... ], growth: 0c
(define (interpret-car [vm : vm-]) : vm-
  (define tos (tos-value vm))
  (define car-cell (if (cell-list-ptr-? tos)
                       (cell-list-ptr--car tos)
                       (raise-user-error (format "expected cell-list-ptr, got ~a" tos))))
  (increment-pc (push-value (pop-value vm) car-cell)))

;; bytecode: op, len: 1b
;; stack: [ cell-list-ptr ... ] -> [ cdr-of-list ... ], growth: 0c
(define (interpret-cdr [vm : vm-]) : vm-
  (define tos (tos-value vm))
  (define cdr-cell (if (cell-list-ptr-? tos)
                       (cell-list-ptr--cdr tos)
                       (raise-user-error (format "expected cell-list-ptr, got ~a" tos))))
  (increment-pc (push-value (pop-value vm) cdr-cell)))

;; bytecode: op, len: 1b
;; stack: [ car-cell cdr-cell ... ] -> [ cell-list-ptr ... ], growth: -1c
(define (interpret-cons [vm : vm-]) : vm-
  (match-define (list (list cdr-cell car-cell) next-vm) (pop-and-get-values vm 2))
  (increment-pc (push-value next-vm (cell-list-ptr- car-cell cdr-cell))))

(define (interpret-tail-call [vm : vm-]) : vm-
  (define value-stack (vm--value-stack vm))
  (define active-frame (car (vm--frame-stack vm)))
  (define parameter-tail (vm-frame--parameter-tail active-frame))
  (define param-number (active-function-param-no vm))
  (define new-params (take value-stack param-number))
  (define new-value-stack (append new-params
                                  (drop parameter-tail param-number)))
  (define new-frame (struct-copy vm-frame- active-frame
                                 [parameter-tail new-value-stack]
                                 [bc-idx 0]))
  (struct-copy vm- vm
               [value-stack new-value-stack]
               [frame-stack (cons new-frame (cdr (vm--frame-stack vm)))]))

(define (sPUSH_BYTEc [val : Integer]) : Byte
  (when (or (fx< val -1) (fx>= val sPUSH_BYTEn))
    (raise-user-error (format "byte value out of bounds for push byte short command (~a)" val)))
  (define new-val (if (fx< val 0) sPUSH_BYTEn val))
  (bitwise-xor sPUSH_BYTE (bitwise-and sPUSH_BYTEn new-val)))

(define (sPUSH_PARAMc [idx : Byte]) : Byte
  (when (fx> idx sPUSH_PARAMn)
    (raise-user-error (format "index out of bounds for push param short command (~a)" idx)))
  (bitwise-xor sPUSH_PARAM idx))

(define (sPOP_TO_PARAMc [idx : Byte]) : Byte
  (when (fx> idx sPOP_TO_PARAMn)
    (raise-user-error (format "index out of bounds for pop to param short command (~a)" idx)))
  (bitwise-xor sPOP_TO_PARAM idx))

(define (sBRAc [to : Fixnum]) : Byte
  (define to- (byte->two-complement to sBRAmsb))
  (when (fx> to- sBRAn)
    (raise-user-error (format "jump target out of bounds for bra short command (~a ~a)" to to-)))
  (bitwise-xor sBRA to-))

(define (sGOTOc [to : Fixnum]) : Byte
  (define to- (byte->two-complement to sGOTOmsb))
  (when (fx> to- sGOTOn)
    (raise-user-error (format "jump target out of bounds for goto short command (~a ~a)" to to-)))
  (bitwise-xor sGOTO to-))

(define (sNIL?-RET-PARAMc [idx : Byte]) : Byte
  (when (fx> idx sNIL?-RET-PARAMn)
    (raise-user-error (format "index out of bounds for short command nil?-ret-param (~a)" idx)))
  (bitwise-xor sNIL?-RET-PARAM idx))


;; op: byte field-idx, stack: [ struct-ref- cell-] -> []
(define (interpret-pop-to-array-field [vm : vm-]) : vm-
  (define ptr2array (tos-value vm))
  (unless (cell-ptr-? ptr2array)
    (raise-user-error (format "expected ptr to array, got \"~a\"" ptr2array)))
  (define array (cell-ptr--ref ptr2array))
  (define next-vm (pop-value vm))
  (define value (tos-value next-vm))
  (define field-idx (peek-pc-byte next-vm 1))
  (unless (cell-array-? array)
    (raise-user-error (format "expected cell-array- to access field ~a" field-idx)))
  (unless (fx< field-idx (cell-array--size array))
    (raise-user-error (format "cell-array- too small (~a) to access field ~a" (cell-array--size array) field-idx)))
  (increment-pc (set-array-value (pop-value next-vm) array field-idx value) 2))

;; op: byte field-idx, stack: [ struct-ref- ] -> [cell-]
(define (interpret-push-array-field [vm : vm-]) : vm-
  (define ptr2array (tos-value vm))
  (unless (cell-ptr-? ptr2array)
    (raise-user-error (format "expected cell-ptr- as tos ~a" ptr2array)))
  (define array (cell-ptr--ref ptr2array))
  (define field-idx (peek-pc-byte vm 1))
  (unless (cell-array-? array)
    (raise-user-error (format "expected cell-array- to access field ~a" field-idx)))
  (unless (fx< field-idx (cell-array--size array))
    (raise-user-error (format "cell-array- too small (~a) to access field ~a" (cell-array--size array) field-idx)))
  (define value (get-array-value vm array field-idx))
  (increment-pc (push-value (pop-value vm) value) 2))

(module+ test #| interpret-...-field |#
  (define interpret-field-test--vm
    (make-vm
     #:value-stack (list (cell-ptr- (cell-array- 2 0)) (cell-int- 30))
     #:functions (vector-immutable (make-function-def #:byte-code (vector-immutable #x00 1)))
     #:arrays (vector-immutable (vector (ann (cell-byte- 10) atomic-cell-) (ann (cell-byte- 20) atomic-cell-)))))

  (check-equal? (tos-value (interpret-push-array-field interpret-field-test--vm))
                (cell-byte- 20))

  (check-equal? (vm--arrays (interpret-pop-to-array-field interpret-field-test--vm))
                (vector-immutable (vector (ann (cell-byte- 10) atomic-cell-) (ann (cell-int- 30) atomic-cell-)))))

;; op: byte array len, stack [] -> [ array-cell- ]
(define (interpret-allocate-array [vm : vm-]) : vm-
  (define len (peek-pc-byte vm 1))
  (define actual-vector (make-vector len (atomic-cell-)))
  (interpret-allocate-array- vm actual-vector 2))

(define (interpret-allocate-array- [vm : vm-] [actual-vector : (Vectorof atomic-cell-)] [inc-pc : Byte]) : vm-
  (define new-arrays (vector-append (vm--arrays vm) (vector actual-vector)))
  (define id (sub1 (vector-length new-arrays)))
  (when(fx< id 0)
    (raise-user-error (format "array id ~a must be >= 0" id)))
  (increment-pc
   (push-value
    (struct-copy vm- vm
                 [arrays (vector->immutable-vector new-arrays)])
    (cell-ptr- (cell-array- (vector-length actual-vector) id)))
   inc-pc))

(module+ test #| interpret-allocate-array |#
  (define interpret-allocate-array-test--vm
    (make-vm #:functions (vector-immutable (make-function-def #:byte-code (vector-immutable 0 10)))))

  (check-equal? (tos-value (interpret-allocate-array interpret-allocate-array-test--vm))
                (cell-ptr- (cell-array- 10 0))))

(define (interpret-pop-to-local [vm : vm-]) : vm-
  (define active-frame (car (vm--frame-stack vm)))
  (define tos (tos-value vm))
  (define idx (peek-pc-byte vm 1))
  (define new-frame (struct-copy vm-frame- active-frame
                                 [locals (vector->immutable-vector (vector-set/copy (vm-frame--locals active-frame) idx tos))]))
  (increment-pc
   (pop-value
    (struct-copy vm- vm
                 [frame-stack (cons new-frame (cdr (vm--frame-stack vm)))]))
   2))

(define (interpret-push-local [vm : vm-]) : vm-
  (define active-frame (car (vm--frame-stack vm)))
  (define idx (peek-pc-byte vm 1))
  (define value (vector-ref (vm-frame--locals active-frame) idx))
  (increment-pc
   (push-value vm value)
   2))

(define (interpret-create-struct [vm : vm-]) : vm-
  (define struct-idx (peek-pc-int vm 1))
  (define struct-def (vector-ref (vm--structs vm) struct-idx))
  (match-define (list struct-fields next-vm) (pop-and-get-values vm (vm-struct-def--field-no struct-def)))
  (interpret-allocate-array- vm (list->vector struct-fields) 3))

(module+ test #|interpret-create-struct |#
  (define interpret-create-struct-test--vm
    (make-vm #:structs (vector-immutable (vm-struct-def- "point" 2))
             #:value-stack (list (cell-byte- 20) (cell-byte- 10))
             #:functions (vector-immutable (make-function-def #:byte-code (vector-immutable CREATE_STRUCT 0 0)))))

  (check-equal? (vm--arrays (interpret-create-struct interpret-create-struct-test--vm))
                (vector-immutable (vector (cell-byte- 10) (cell-byte- 20)))))

(define (list->cell-list-ptr [a-list : (Listof atomic-cell-)]) : cell-list-ptr-
  (define (list->cell-list-ptr- [a-list : (Listof atomic-cell-)] [result : cell-list-ptr-]) : cell-list-ptr-
    (if (empty? a-list)
        result
        (list->cell-list-ptr- (cdr a-list) (cell-list-ptr- (car a-list) result))))
  (list->cell-list-ptr- (reverse a-list) NIL_CELL))

(define (cell-list-ptr->list [a-list : cell-]) : (Listof cell-)
  (define (cell-list-ptr->list- [a-list : cell-] [result : (Listof cell-)]) : (Listof cell-)
    (if (eq? NIL_CELL a-list)
        result
        (if (cell-list-ptr-? a-list)
            (cell-list-ptr->list- (cell-list-ptr--cdr a-list) (cons (cell-list-ptr--car a-list) result))
            (raise-user-error (format "expected cell-list-ptr, got ~a" a-list)))))
  (reverse (cell-list-ptr->list- a-list '())))

(module+ test #| list->cell-list-ptr |#
  (check-equal? (list->cell-list-ptr (list (cell-byte- 1) (cell-int- 8)))
                (cell-list-ptr- (cell-byte- 1) (cell-list-ptr- (cell-int- 8) NIL_CELL)))

  (check-equal? (cell-list-ptr->list (cell-list-ptr- (cell-byte- 1) (cell-list-ptr- (cell-int- 8) NIL_CELL)))
                (list (cell-byte- 1) (cell-int- 8))))

(define (interpret-create-list [vm : vm-]) : vm-
  (define count (peek-pc-byte vm 1))
  (match-define (list allocated-list next-vm) (pop-and-get-values vm count))
  (increment-pc (push-value next-vm (list->cell-list-ptr allocated-list)) 2))

(module+ test #|interpret-create-list |#
  (define interpret-create-list-test--vm
    (make-vm
     #:value-stack (list (cell-int- 8) (cell-byte- 1))
     #:functions (vector-immutable (make-function-def #:byte-code (vector-immutable CREATE_LIST 2)))))

  (check-equal? (tos-value (interpret-create-list interpret-create-list-test--vm))
                (cell-list-ptr- (cell-byte- 1) (cell-list-ptr- (cell-int- 8) NIL_CELL))))

(define (interpret-push-nil [vm : vm-]) : vm-
  (increment-pc
   (push-value vm NIL_CELL)))

(define (disassemble-byte-code (vm : vm-)) : String
  (define byte-code (peek-pc-byte vm))
  (cond
    [(= byte-code ALLOCATE_ARRAY) (format "alloc-array size ~a" (peek-pc-byte vm 1))]
    [(= byte-code BRA) (format "bra ~a" (two-complement->signed-byte (peek-pc-byte vm 1)))]
    [(= byte-code BRK) "brk"]
    [(= byte-code BYTE+) "byte+"]
    [(= byte-code CALL) "call"]
    [(= byte-code CAR) "car"]
    [(= byte-code CDR) "cdr"]
    [(= byte-code CONS) "cons"]
    [(= byte-code CREATE_STRUCT) (format "create-struct ~a" (peek-pc-int vm 1))]
    [(= byte-code CREATE_LIST) (format "create-list ~a" (peek-pc-byte vm 1))]
    [(= byte-code GOTO) (format "goto ~a" (two-complement->signed-byte (peek-pc-byte vm 1)))]
    [(= byte-code NIL?) "nil?"]
    [(= byte-code NIL?-RET-PARAM) (format "nil? -> return p~a" (peek-pc-byte vm 1))]
    [(= byte-code POP_TO_ARRAY_FIELD) (format "pop to array[~a]" (peek-pc-byte vm 1))]
    [(= byte-code POP_TO_GLOBAL) (format "pop g-~a" (peek-pc-int vm 1))]
    [(= byte-code POP_TO_LOCAL) (format "pop l-~a" (peek-pc-byte vm 1))]
    [(= byte-code POP_TO_PARAM) (format "pop p-~a" (peek-pc-byte vm 1))]
    [(= byte-code PUSH_ARRAY_FIELD) (format "push array[~a]" (peek-pc-byte vm 1))]
    [(= byte-code PUSH_BYTE) (format "push #~a" (peek-pc-byte vm 1))]
    [(= byte-code PUSH_GLOBAL) (format "push g-~a" (peek-pc-int vm 1))]
    [(= byte-code PUSH_INT) (format "push #~a" (peek-pc-int vm 1))]
    [(= byte-code PUSH_LOCAL) (format "push l-~a" (peek-pc-byte vm 1))]
    [(= byte-code PUSH_NIL) (format "push nil")]
    [(= byte-code PUSH_PARAM) (format "push p-~a" (peek-pc-byte vm 1))]
    [(= byte-code RET) "ret"]
    [(= byte-code TAIL_CALL) "tail-call"]

    [(= (bitwise-and byte-code sPUSH_BYTEm) sPUSH_BYTE) (format "push #-~a  ;; short version" (bitwise-and sPUSH_BYTEn byte-code))]
    [(= (bitwise-and byte-code sPUSH_PARAMm) sPUSH_PARAM) (format "push p-~a  ;; short version" (bitwise-and sPUSH_PARAMn byte-code))]
    [(= (bitwise-and byte-code sPOP_TO_PARAMm) sPOP_TO_PARAM) (format "pop p-~a  ;; short version" (bitwise-and sPOP_TO_PARAMn byte-code))]
    [(= (bitwise-and byte-code sBRAm) sBRA) (format "bra ~a  ;; short version" (two-complement->signed-byte (bitwise-and sBRAn byte-code) sBRAmsb))]
    [(= (bitwise-and byte-code sGOTOm) sGOTO) (format "goto ~a  ;; short version" (two-complement->signed-byte (bitwise-and sGOTOn byte-code) sGOTOmsb))]
    [(= (bitwise-and byte-code sNIL?-RET-PARAMm) sNIL?-RET-PARAM) (format "nil? -> return p~a  ;; short version" (bitwise-and sNIL?-RET-PARAMn byte-code))]

    [else (raise-user-error (format "unknown byte code command during disassembly ~a" byte-code))]))

(define (display-vm [vm : vm-]) : Any
  (define cur-frame (car (vm--frame-stack vm)))
  (displayln (format "exec: (~a)\t ~a  \t@function: ~a, byte-offset: ~a, value-stack-size: ~a, tos: ~a" (peek-pc-byte vm)
                     (disassemble-byte-code vm)
                     (vm-frame--fun-idx cur-frame)
                     (vm-frame--bc-idx cur-frame)
                     (length (vm--value-stack vm))
                     (if (empty? (vm--value-stack vm))
                         "nil"
                         (tos-value vm)))))

(define (is-push-param-short [code : Byte]) : Boolean
  (= (bitwise-and code sPUSH_PARAMm) sPUSH_PARAM))

(define (interpret-byte-code [vm : vm-]) : vm-
  (define byte-code (peek-pc-byte vm))
  (when (member 'trace (vm--options vm)) (display-vm vm))
  (cond
    [(= byte-code ALLOCATE_ARRAY) (interpret-allocate-array vm)]
    [(= byte-code BRA) (interpret-bra vm)]
    [(= byte-code BRK) (raise-user-error "encountered BRK")]
    [(= byte-code BYTE+) (interpret-byte+ vm)]
    [(= byte-code CALL) (interpret-call vm)]
    [(= byte-code CAR) (interpret-car vm)]
    [(= byte-code CDR) (interpret-cdr vm)]
    [(= byte-code CONS) (interpret-cons vm)]
    [(= byte-code CREATE_STRUCT) (interpret-create-struct vm)]
    [(= byte-code CREATE_LIST) (interpret-create-list vm)]
    [(= byte-code GOTO) (interpret-goto vm)]
    [(= byte-code NIL?) (interpret-nil? vm)]
    [(= byte-code NIL?-RET-PARAM) (interpret-nil?-ret-param vm)]
    [(= byte-code POP_TO_ARRAY_FIELD) (interpret-pop-to-array-field vm)]
    [(= byte-code POP_TO_STRUCT_FIELD) (interpret-pop-to-array-field vm)]
    [(= byte-code POP_TO_GLOBAL) (interpret-pop-to-global vm)]
    [(= byte-code POP_TO_LOCAL) (interpret-pop-to-local vm)]
    [(= byte-code POP_TO_PARAM) (interpret-pop-to-param vm)]
    [(= byte-code PUSH_BYTE) (interpret-push-byte vm)]
    [(= byte-code PUSH_STRUCT_FIELD) (interpret-push-array-field vm)]
    [(= byte-code PUSH_ARRAY_FIELD) (interpret-push-array-field vm)]
    [(= byte-code PUSH_GLOBAL) (interpret-push-global vm)]
    [(= byte-code PUSH_INT) (interpret-push-int vm)]
    [(= byte-code PUSH_LOCAL) (interpret-push-local vm)]
    [(= byte-code PUSH_NIL) (interpret-push-nil vm)]
    [(= byte-code PUSH_PARAM) (interpret-push-param vm)]
    [(= byte-code RET) (interpret-ret vm)]
    [(= byte-code TAIL_CALL) (interpret-tail-call vm)]

    [(= (bitwise-and byte-code sPUSH_BYTEm) sPUSH_BYTE)
     (define bits (bitwise-and sPUSH_BYTEn byte-code))
     (define value (if (= bits sPUSH_BYTEn) #xff bits))
     (interpret-push-byte- vm value 1)]
    [(is-push-param-short byte-code)
     (interpret-push-param- vm (bitwise-and sPUSH_PARAMn byte-code) 1)]
    [(= (bitwise-and byte-code sPUSH_GLOBALm) sPUSH_GLOBAL)
     (interpret-push-global- vm (fx+ (arithmetic-shift (bitwise-and sPUSH_GLOBALn byte-code) 8)
                                     (peek-pc-byte vm 1))
                             2)]

    [(= (bitwise-and byte-code sPOP_TO_PARAMm) sPOP_TO_PARAM)
     (interpret-pop-to-param- vm (bitwise-and sPOP_TO_PARAMn byte-code) 1)]

    [(= (bitwise-and byte-code sBRAm) sBRA)
     (interpret-bra- vm (two-complement->signed-byte (bitwise-and sBRAn byte-code) sBRAmsb) 1)]

    [(= (bitwise-and byte-code sGOTOm) sGOTO)
     (interpret-goto- vm (two-complement->signed-byte (bitwise-and sBRAn byte-code) sGOTOmsb))]

    [(= (bitwise-and byte-code sNIL?-RET-PARAMm) sNIL?-RET-PARAM)
     (interpret-nil?-ret-param- vm (bitwise-and sNIL?-RET-PARAMn byte-code) 1)]

    [else (raise-user-error (format "unknown byte code command ~a" byte-code))]))

(define (byte->two-complement [value : Fixnum] [start-bit : Byte 8]) : Byte
  (define max-p1 (arithmetic-shift #x100 (fx- start-bit 8)))
  (define mask (arithmetic-shift #xff (fx- start-bit 8)))
  (define pre-result
    (cond
      [(fx< value 0) (bitwise-and (fx+ max-p1 value) mask)]
      [else (bitwise-and value mask)]))
  (if (byte? pre-result)
      pre-result
      (raise-user-error (format "signed byte out of range ~a" value))))

(module+ test #| vbyte->two-complement |#
  (check-equal? (two-complement->signed-byte (byte->two-complement -1 8) 8)
                -1)
  (check-equal? (two-complement->signed-byte (byte->two-complement -128 8) 8)
                -128)
  (check-equal? (two-complement->signed-byte (byte->two-complement 127 8) 8)
                127)
  (check-equal? (two-complement->signed-byte (byte->two-complement 0 8) 8)
                0)
  (check-equal? (two-complement->signed-byte (byte->two-complement 1 8) 8)
                1)

  (check-equal? (two-complement->signed-byte (byte->two-complement -1 6) 6)
                -1)
  (check-equal? (two-complement->signed-byte (byte->two-complement -32 6) 6)
                -32)
  (check-equal? (two-complement->signed-byte (byte->two-complement 31 6) 6)
                31)
  (check-equal? (two-complement->signed-byte (byte->two-complement 0 6) 6)
                0)
  (check-equal? (two-complement->signed-byte (byte->two-complement 1 6) 6)
                1))

(define (run-until-break (vm : vm-)) : vm-
  (cond [(fx= BRK (peek-pc-byte vm))
         (when (member 'trace (vm--options vm)) (display-vm vm))
         vm]
        [else
         (define next-vm (interpret-byte-code vm))
         (run-until-break next-vm)]))

(module+ test #| run-until-break |#
  (define test-byte+--run-until-break
    (run-until-break
     (make-vm
      #:options (list ) ;; 'trace
      #:functions
      (vector-immutable
       (make-function-def
        #:byte-code (vector-immutable PUSH_BYTE 10
                                      PUSH_BYTE 20
                                      BYTE+
                                      BRK))))))

  (check-equal? (vm--value-stack test-byte+--run-until-break)
                (list (cell-byte- 30))
                "the execution result on the value stack should be 30")
  (check-equal? (car (vm--frame-stack test-byte+--run-until-break))
                (make-frame #:bc-idx 5)
                "the program counter should point to the break instruction")

  (define test-call-return--run-until-break
    (run-until-break
     (make-vm
      #:options (list) ;;  'trace
      #:functions
      (vector-immutable
       (make-function-def
        #:byte-code (vector-immutable PUSH_BYTE 10
                                      PUSH_BYTE 20
                                      PUSH_INT   1 0 ;; function index 1
                                      CALL
                                      BRK))
       (make-function-def
        #:parameter-count 2
        #:byte-code (vector-immutable PUSH_PARAM 1
                                      PUSH_PARAM 0
                                      BYTE+
                                      RET))))))

  (check-equal? (vm--value-stack test-call-return--run-until-break)
                (list (cell-byte- 30))
                "the execution result on the value stack should be 30")
  (check-equal? (car (vm--frame-stack test-call-return--run-until-break))
                (make-frame #:bc-idx 8)
                "the program counter should point to the break instruction"))

(module+ test #| test tail recursion |#

  (define test-tail-recursion--value-stack
    (list (cell-byte- 0)
          (cell-list-ptr- (cell-byte- 5)
                          (cell-list-ptr- (cell-byte- 10)
                                          (cell-list-ptr- (cell-byte- 20)
                                                          NIL_CELL)))))

  ;; (m-def (sum (bl (list byte)) (acc byte 0) -> byte
  ;;        "tail recursive function with accumulator")
  ;;   (if (nil? bl)
  ;;       acc
  ;;     (sum (cdr bl) (byte+ acc (car bl)))))


  (define test-tail-recursion--run-until-break
    (run-until-break
     (make-vm
      #:options (list) ;;  'trace
      #:value-stack  test-tail-recursion--value-stack
      #:functions
      (vector-immutable
       (make-function-def
        #:byte-code (vector-immutable PUSH_INT   1 0 ;; function index 1
                                      CALL
                                      BRK))
       (make-function-def
        #:parameter-count 2 ;; param0 = accumulator, param1 = list of bytes
        #:byte-code (vector-immutable (sPUSH_PARAMc 1)
                                      (sNIL?-RET-PARAMc 0)
                                      (sPUSH_PARAMc 1)
                                      CDR
                                      (sPUSH_PARAMc 1)
                                      CAR
                                      (sPUSH_PARAMc 0)
                                      BYTE+
                                      TAIL_CALL))))))

  (check-equal? (vm--value-stack test-tail-recursion--run-until-break)
                (list (cell-byte- 35))
                "tos of the value stack is the sum of all bytes in the list in 'test-tail-recursive--value-stack'")


  ;; (m-def (reverse (a-list (list cell)) (b-list (list cell) '()) -> (list cell)
  ;;                  "reverse a-list, consing it into b-list")
  ;;   (if (nil? a-list)
  ;;       b-list
  ;;     (reverse (cdr a-list) (cons (car a-list) b-list))))


  (define test-tail-recursion--value-stack2
    (list (list->cell-list-ptr (list (cell-byte- 5) (cell-byte- 10) (cell-byte- 20)))
          NIL_CELL))

  (define test-tail-recursion--run-until-break2
    (run-until-break
     (make-vm
      #:options (list) ;;  'trace
      #:value-stack  test-tail-recursion--value-stack2
      #:functions
      (vector-immutable
       (make-function-def
        #:byte-code (vector-immutable PUSH_INT   1 0 ;; function index 1
                                      CALL
                                      BRK))
       (make-function-def
        #:parameter-count 2 ;; param0 = original list, param1 = nil (reversed list in the end)
        #:byte-code (vector-immutable (sPUSH_PARAMc 0)
                                      (sNIL?-RET-PARAMc 1)
                                      (sPUSH_PARAMc 1)
                                      (sPUSH_PARAMc 0)
                                      CAR
                                      CONS
                                      (sPUSH_PARAMc 0)
                                      CDR
                                      TAIL_CALL))))))

  (check-equal? (vm--value-stack test-tail-recursion--run-until-break2)
                (list (list->cell-list-ptr (list  (cell-byte- 20) (cell-byte- 10) (cell-byte- 5))))
                "tos is reversed list")

  (define test-tail-recursion--unoptimized-run-until-break
    (run-until-break
     (make-vm
      #:options (list) ;;  'trace
      #:value-stack  test-tail-recursion--value-stack2
      #:functions
      (vector-immutable
       (make-function-def
        #:byte-code (vector-immutable PUSH_INT   1 0 ;; function index 1
                                      CALL
                                      BRK))
       (make-function-def
        #:parameter-count 2 ;; param0 = accumulator, param1 = list of bytes
        #:byte-code (vector-immutable (sPUSH_PARAMc 0)     ;; a-list
                      NIL?
                      BRA 8
                      (sPUSH_PARAMc 1)
                      (sPUSH_PARAMc 0)
                      CAR
                      CONS
                      (sPUSH_PARAMc 0)
                      CDR
                      TAIL_CALL
                      (sPUSH_PARAMc 1)
                      RET))))))

  (check-equal? (vm--value-stack test-tail-recursion--unoptimized-run-until-break)
                (list (list->cell-list-ptr (list  (cell-byte- 20) (cell-byte- 10) (cell-byte- 5))))
                "tos is reversed list")
  )

(module+ test #| structure usage |#

  (define structure-usage-test--vm
    (make-vm
     #:options (list) ;; 'trace
     #:frame-stack (list (make-frame #:locals (vector-immutable (atomic-cell-))))
     #:functions
     (vector-immutable
      (make-function-def
       #:byte-code (vector-immutable ALLOCATE_ARRAY 10
                                     POP_TO_LOCAL 0
                                     PUSH_BYTE 1
                                     PUSH_LOCAL 0
                                     POP_TO_ARRAY_FIELD 5
                                     PUSH_BYTE 7
                                     PUSH_LOCAL 0
                                     POP_TO_ARRAY_FIELD 4
                                     PUSH_LOCAL 0
                                     PUSH_ARRAY_FIELD 5
                                     BRK)))))

  (check-equal? (tos-value (run-until-break structure-usage-test--vm))
                (cell-byte- 1)))

(module+ test #| higher order function |#
  ;; (m-def (mapr (a-fun (fun T -> S)) (a-list (listof T)) (b-list (listof S) '()) -> (listof S))
  ;;        (if (empty? a-list)
  ;;            b-list
  ;;            (mapr a-fun (cdr a-list) (cons (a-fun (car a-list)) b-list))))

  ;; (m-def (inc (a-byte byte) -> byte)
  ;;        (byte+ a-byte 1))

  ;; (mapr inc '(1 2)) ;; yield '(3 2), since this map version reverses the original list


  (define higher-order-function-test--vm
    (make-vm
     #:options (list) ;; 'trace
     #:functions
             (vector-immutable
              (make-function-def ;; (mapr inc '(1 2 3)) ;; yield '(4 3 2), since this map version reverses the original list
               #:parameter-count 0
               #:byte-code (vector-immutable PUSH_NIL      ;; initial result '()
                                             PUSH_BYTE 1
                                             PUSH_BYTE 2
                                             PUSH_BYTE 3
                                             CREATE_LIST 3 ;; '(1 2 3)
                                             PUSH_INT 2 0  ;; higher function passed (inc)
                                             PUSH_INT 1 0  ;; function to call (mapr)
                                             CALL
                                             BRK))
              (make-function-def ;; mapr
               #:parameter-count 3
               #:byte-code (vector-immutable (sPUSH_PARAMc 1)
                                             (sNIL?-RET-PARAMc 2)
                                             (sPUSH_PARAMc 2)
                                             (sPUSH_PARAMc 1)
                                             CAR
                                             (sPUSH_PARAMc 0)
                                             CALL
                                             CONS
                                             (sPUSH_PARAMc 1)
                                             CDR
                                             (sPUSH_PARAMc 0)
                                             TAIL_CALL))
              (make-function-def ;; inc
               #:parameter-count 1
               #:byte-code (vector-immutable (sPUSH_BYTEc 1)
                                             (sPUSH_PARAMc 0)
                                             BYTE+
                                             RET)))))

  (check-equal? (cell-list-ptr->list (tos-value (run-until-break higher-order-function-test--vm)))
                (list (cell-byte- 4) (cell-byte- 3) (cell-byte- 2))))


;;                                        low-byte  high byte  (low-byte is first)
(define CELL_PTR #b00000001)           ;; xxxx xxx1 xxxx xxxx (ptr to words/cells)
(define CELL_LIST_PTR #b00000010)      ;; xxxx xx10 xxxx xxxx (ptr to (cell-pairs, car and cdr cell)) NIL= 0000 0010 0000 0000
(define CELL_INT #b00000000)           ;; xxxx x000 xxxx xxxx 0..8191, -4096..+4095 [advantage 13-bit adding can be done easily l+l, h+h+carry]
(define CELL_BYTE #b00000100)          ;; 0000 0100 xxxx xxxx 0..255, -128..+127
(define CELL_BCD  #b00001100)          ;; 0000 1100 dddd dddd 00..99 binary coded decimal

;; alternative encoding
;; ASL|ASR          = encoding len: 1 byte, execution time: 2 cycles => is beneficial if result can be used
;; CMP Imm          = encoding len: 2 byte, execution time: 2 cycles => is beneficial if A has no further purpose (needs no postprocessing)
;; ORA|AND|EOR Imm  = encoding len: 2 byte, execution time: 2 cycles
;;
;; PTR #b10000000        ;; 1xxx xxxx xxxx xxxx   ;; LDA tagbyte, BMI -- is ptr : ASL -> low byte of ptr
;; LIST PTR #b01000000   ;; 01xx xxxx xxxx xxxx   ;; .. ASL, BMI -- is list ptr  : 2xASL -> low byte of ptr
;; CELL_INT #b00000000   ;; 00xx xxx0 xxxx xxxx   ;; .. LDA tagbyte, ASR, BCC -- is int
;; CELL_TRUE             ;; 0001 1111 ---- ----   ;; CMP #$1F, BEQ --true    : knowing it is a boolean: 2xASR BCC = false/BCS = true || AND #$02, BNZ -- true | B
;; CELL_FALSE            ;; 0000 0001 ---- ----   ;; CMP #$01, BEQ -- false
;; CELL_BYTE             ;; 0000 0010 xxxx xxxx   ;; ..

;; 6510 code to have fast decoding of these tag-fields
;; ptr detect:
;;   A = tag-byte (first byte)
;;   LSR put lowest bit into carry (0 into bit 7)
;;   BCS --is a pointer (then ASL to get low byte of ptr, and next byte to have high byte of pointer)

;; list ptr detect
;;   A = tag-byte (already shifted right)
;;   LSR
;;   BCS -- is a list ptr (then 2xASL to get low byte of ptr ...)

;; these pointers allow for relocatable data, data may become non relocatable (mostly interesting for startup encoding/constant pool)
(define CELL_PTR_REL #b00010100)       ;; 0001 0100 xxxx xxxx -256..+254 (relative pointer) *2
(define CELL_LIST_PTR_REL #b00011100)  ;; 0001 1100 xxxx xxxx -512..+508 (relative pointer) *4


;; implementation idea for vm: ptr = index into (e.g. globals, functions)
;; pointed to cells point into an array of index-> complex-cell-

;; ideas of pointed to cells (are cells) which can have bytes following (no represented in vm yet)
;; pointed to cells can be more complex array with max 127 elements each a cell (also used for structures)
;; all data is always allocated in words (2 bytes) steps -> cell_ptr can access any cell
(define CELL_ARRAY #b00100100)         ;; 0010 0100 xxxx xxxx size 1..256
                                       ;; element0: 16bit cell
                                       ;; element1: 16bit cell
                                       ;; ..
                                       ;; element255: 16 bit cell

(define CELL_FLOAT #b00101100)         ;; 0010 1100 xxxx xxxx ??
                                       ;; 6 byte float data following
(define CELL_NAT_ARRAY #b00111100)     ;; 0011 1100 xxxx xxxx size 0..255 (page boundaries may play a role), used for strings
                                       ;; up to 256 bytes of native array following (always allocated in 2 byte steps)
(define CELL_LARGE_ARRAY #b01000100)   ;; 0100 0100 xxxx xxxx size 1..65536
                                       ;; xxxx xxxx xxxx xxxx (even larger)
                                       ;; cell0, cell1 ... cell65535

;; idea for
;; hash-maps implementation could be trees constructed from lists
;; hash-map = cell-list-ptr -> hash-map-hash-node
;; (car hash-map-hash-node) = min hash for right, or exact hash if cdr is value (and no list ptr)
;; (cdr hash-map-hash-node) = cell-list-ptr to a hash-map-tree-node  or cell-ptr to content, or cell-byte/int/bcd content
;; (car hash-map-tree-node) = cell-list-ptr to left (or nil)
;; (car hash-map-tree-node) = cell-list-ptr to right (or nil)

;; (int, ->(left->(int, ...) ,right->(int, ...))

;; idea: compact representation of lists possible? fast conversion <-> ?

;; to convert a complete cell, one might need information of globals, complex-cell-array and their indices to correctly generated data for the vm
;; idea: input = cell X startidx for next free heap
;;       output = bytes X
(define (cell->byte-code (a : cell-)): (Immutable-Vectorof Byte)
  (cond
    [(cell-int-? a) (vector-immutable
                     (cast (arithmetic-shift (bitwise-and #b00011111 (cell-int--value a)) 3) Byte)
                     (cast (arithmetic-shift (bitwise-and #b0001111111100000 (cell-int--value a)) -5) Byte))]
    [(cell-byte-? a) (vector-immutable #b00000100 (cell-byte--value a))]
    [else (raise-user-error (format "unknown cell type \"~a\"" a))]))

(module+ test #| cell->byte-code |#
  (check-equal? (cell->byte-code (cell-int- 1025))
                (vector-immutable #b00001000 #b00100000)))

;; since cell-ptr may be in the bytes, all bytes need be accessible (the whole memory)
(define (byte-code->cell (bytes : (Immutable-Vectorof Byte))) : cell-
  (define byte0 (vector-ref bytes 0))
  (cond
    [(= 0 (bitwise-and #b00000111 byte0))
     (define byte1 (vector-ref bytes 1))
     (cell-int- (fx+ (arithmetic-shift byte1 5) (arithmetic-shift byte0 -3)))]
    [(= byte0 #b00000100) (cell-byte- (vector-ref bytes 1))]
    [else (raise-user-error (format "unknown byte encoding for data in byte 0 \"~a\"" byte0))]))

(module+ test #| byte-code->cell |#
  (check-equal? (cell-int--value (cast (byte-code->cell (cell->byte-code (cell-int- 1025))) cell-int-))
                1025)

  (check-equal? (cell-byte--value (cast (byte-code->cell (cell->byte-code (cell-byte- 129))) cell-byte-))
                129))
