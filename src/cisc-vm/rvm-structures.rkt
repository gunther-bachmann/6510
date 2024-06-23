#lang racket

(require (only-in data/pvector pvector pvector? make-pvector build-pvector))
(require (only-in data/collection nth set-nth length))
(require (only-in racket/fixnum fx+ fx=))
(require (only-in threading ~>>))
(require (only-in "../6510-utils.rkt" two-complement-of decimal-from-two-complement))

(provide disassemble
         make-vm
         CISC_VM_CONS
         CISC_VM_CAR
         CISC_VM_BRA_EMPTY_LIST
         CISC_VM_CDR
         CISC_VM_GOTO
         CISC_VM_RET
         CISC_VM_IMMB
         CISC_VM_BYTE_ADD
         CISC_VM_BRA
         CISC_VM_BRA_NOT
         CISC_VM_MOVE
         CISC_VM_CALL
         CISC_VM_NIL_P
         CISC_VM_NOT

         VM_L0
         VM_L1
         VM_L2
         VM_L3
         VM_L4
         VM_L5

         VM_P0
         VM_P1

         encode-idx
         l-local
         l-param
         l-global
         l-imm)

(module+ test #| require test utils |#
  (require "../6510-test-utils.rkt"))


;; [xxxx xxxx xxxx xxxx] : cell

;; [xxxx xxxx xxxx xxx1] : cell-ptr        points to any cell
;; [xxxx xxxx xxxx xx10] : cell-list-ptr   points to a pair of cells (car, cdr)
;; [xxxx xxxx xxxx x000] : cell-int        direct value (13 bit payload) (high 8, low 5)

;; complex cells
;; [xxxx xxxx xxx0 0100] : cell-map        meta-cell, cells following form the map
;; [xxxx xxxx xxx0 1100] : cell-array      meta-cell, cells following form the array [ cannot be part of a list ]
;; [xxxx xxxx xxx1 0100] : cell-struct     meta-cell, cells following form the structure
;; [            01 1100]
;; [           011 1100]
;; [          0111 1100]

;; [xxxx xxxx 1111 1100] : cell-byte       direct value (8 bit payload)

;; if an array is to be put into a list or another complex cell (e.g. map, array, struct), it is wrapped in a cell-ptr!
;; complex cells (e.g. list, array, map, struct) may contain (directly) only either cell-ptr, cell-list-ptr, cell-int or cell-byte

;; 16 bit element
(struct cell ()
  #:transparent)

;; arbitrary number of cells
(struct celln cell ()
  #:transparent)

(struct atomic-cell cell ()
  #:transparent)

;; pointer to a cell (
(struct cell-ptr atomic-cell (ref)
  #:transparent
  #:guard
  (struct-guard/c cell?))

;; pointer to a pair, which is 2 cells memory adjacent, car (any cell) and cdr cell (usually a cell-list-ptr)
(struct cell-list-ptr atomic-cell (car cdr)
  #:transparent
  #:guard
  (struct-guard/c atomic-cell? atomic-cell?))

(struct cell-nil cell-list-ptr ()
  #:transparent
  #:guard
  (struct-guard/c atomic-cell? atomic-cell?))

(define VM_NIL_CELL (cell-nil (atomic-cell) (atomic-cell)))

;; flat value 0..255 or -128..127 (used for byte, char and boolean)
(struct cell-byte atomic-cell (value)
  #:transparent
  #:guard
  (struct-guard/c byte?))

;; flat value 0..8191 or -4096...4095
(struct cell-int atomic-cell (value)
  #:transparent
  #:guard
  (struct-guard/c integer?))

(define/contract (cell-value a-cell)
  (-> (or/c cell-byte? cell-int?) exact-nonnegative-integer?)
  (cond
    [(cell-byte? a-cell) (cell-byte-value a-cell)]
    [(cell-int? a-cell)  (cell-int-value a-cell)]
    [else (raise-user-error (format "not a value cell ~a" a-cell))]))

(struct cell-array celln
  (size
   array)
  #:transparent
  #:guard
  (struct-guard/c
   byte?
   pvector?)) ;; vector of atomic cells!

;; current implementation is an array
;; (struct cell-struct cell-array
;;   ()
;;   #:transparent
;;   #:guard
;;   (struct-guard/c
;;    byte?
;;    pvector?))

;; keep in mind that strings are just byte arrays

;; current execution context (similar to program counter)
(struct continuation
  (byte-code-idx
   func-idx)
  #:transparent
  #:guard
  (struct-guard/c
   exact-nonnegative-integer?
   exact-nonnegative-integer?))

;; a call frame, including the parameters (actually passed) and the # of locals allocated for this call
(struct frame
  (continuation
   param-count
   locals-count
   locals
   params
   return-cell)
  #:transparent
  #:guard
  (struct-guard/c
   continuation?
   exact-nonnegative-integer?
   exact-nonnegative-integer?
   pvector?
   pvector?
   byte?))

;; a function implementation, its required number of local cells to execute and its name
(struct function
  (locals-count
   byte-code
   name)
  #:transparent
  #:guard
  (struct-guard/c
   exact-nonnegative-integer?
   pvector?
   string?))

;; the complete vm state
(struct vm
  (frame-stack
   functions
   frame
   globals
   structs
   options)
  #:transparent
  #:guard
  (struct-guard/c
   (listof frame?)
   pvector?
   frame?
   pvector?
   pvector?
   (listof symbol?)))

;; type ids
(define CISC_VM_TYPE_BYTE      1)
(define CISC_VM_TYPE_INT       2)
(define CISC_VM_TYPE_STRUCT    3)

;; byte code                         #bytes     encoding (..-reg = encoded reference to a cell)
(define CISC_VM_BRK            0) ;; 1          <byte-code>
(define CISC_VM_BYTE_ADD       1) ;; 4          <byte-code> <target-reg> <src-a-reg> <src-b-reg>
(define CISC_VM_CALL           2) ;; 5+paramc   <byte-code> <target-reg> <func-idx-low> <func-idx-high> param-count <param1-reg> <param2-reg> ... <paramn-reg>
(define CISC_VM_RET            3) ;; 2          <byte-code> <result-reg>
(define CISC_VM_IMMB           4) ;; 3          <byte-code> <target-reg> value
(define CISC_VM_MAKE_LIST      5) ;; 3+elemc    <byte-code> <target-reg> element-count <elem1-reg> <elem2-reg> ... <elemn-reg>
(define CISC_VM_BRA_EMPTY_LIST 6) ;; 3          <byte-code> <list-tested-reg> offset
(define CISC_VM_CAR            7) ;; 3          <byte-code> <target-reg> <src-list-reg>
(define CISC_VM_CDR            8) ;; 3          <byte-code> <target-reg> <src-list-reg>
(define CISC_VM_GOTO           9) ;; 2          <byte-code> offset
(define CISC_VM_STRUCT_CREATE 10) ;; 3+fieldc   <byte-code> <target-reg> <struct-def-idx> <field-1-reg> <field-2-reg> ... <field-n-reg>
(define CISC_VM_ARRAY_CREATE  11) ;; 4          <byte-code> <target-reg> <length-reg> <default-val-reg>
(define CISC_VM_ARRAY_GET     12) ;; 4          <byte-code> <target-reg> <array-reg> <idx-reg>
(define CISC_VM_ARRAY_SET     13) ;; 4          <byte-code> <array-reg> <idx-reg> <value-reg>
(define CISC_VM_PEEK_LBYTE    14) ;; 3          <byte-code> <target-reg> <cell-ptr-reg>
(define CISC_VM_PEEK_HBYTE    15) ;; 3          <byte-code> <target-reg> <cell-ptr-reg
(define CISC_VM_POKE_LBYTE    16) ;; 3          <byte-code> <cell-ptr-reg> <value-reg>
(define CISC_VM_POKE_HBYTE    17) ;; 3          <byte-code> <cell-ptr-reg> <value-reg>
(define CISC_VM_CONS          18) ;; 4          <byte-code> <target-reg> <value-reg> <list-ptr/val-reg>
(define CISC_VM_MAKE_INT      19) ;; 4          <byte-code> <target-reg> <low-byte-reg> <high-byte-reg>
(define CISC_VM_IMMI          20) ;; 3          <byte-code> <target-reg> low-value high-value
(define CISC_VM_INT_ADD       21) ;; 4          <byte-code> <target-reg> <int-a-reg> <int-b-reg>
(define CISC_VM_INT_INC       22) ;; 2          <byte-code> <target-reg>
(define CISC_VM_CASE          23) ;; 3+casen*3  <byte-code> <target-reg> <case-src-reg> case-no case-byte-value-0 <new-byte-idx> case-byte-value-1 <new-byte-idx> ... case-byte-value-n <new-byte-idx>
(define CISC_VM_THROW         24) ;; 3+paramn   <byte-code> <exception-str> <param-no> <param-1> <param-2> ... <param-n>
(define CISC_VM_BRA           25) ;; 3          <byte-code> <bool-tested-reg> offset
(define CISC_VM_MOVE          26) ;; 3          <byte-code> <target-reg> <source-reg>
(define CISC_VM_NIL_P         27) ;; 3          <byte-code> <target-reg> <list-reg>
(define CISC_VM_BRA_NOT       28) ;; 3          <byte-code> <bool-tested-reg> offset
(define CISC_VM_NOT           29) ;; 3          <byte-code> <bool-target-reg> <bool-source-reg>

;; INT_TIMES
;; INT->BYTE
;; BYTE->INT

;; MAP_CREATE (could be implemented as list if key/value pairs)
;; MAP_GET
;; MAP_PUT

;; runtime functions
;; char-out
;; byte-out
;; int-out
;; list-out
;; struct-out
;; array-out
;; map-out
;; string-out
;; string-len
;; string-concat
;; string-*


;; reference to locals
(define VM_L0 0)
(define VM_L1 1)
(define VM_L2 2)
(define VM_L3 3)
(define VM_L4 4)
(define VM_L5 5)
(define VM_L6 6)
(define VM_L7 7)
(define VM_L8 8)
(define VM_L9 9)
(define VM_L10 10)

;; reference to parameters
(define VM_P0 64)
(define VM_P1 65)
(define VM_P2 66)
(define VM_P3 67)
(define VM_P4 68)
(define VM_P5 69)
(define VM_P6 70)

;; reference to globals
(define VM_G0 128)
(define VM_G1 129)
(define VM_G2 130)
(define VM_G3 131)
(define VM_G4 132)
(define VM_G5 133)
(define VM_G6 134)

;; encoded byte constants
(define VM_I0 #b11000000)
(define VM_I1 #b11000001)
(define VM_I2 #b11000010)
(define VM_I3 #b11000011)

;; create a vm with the given locals (in the current execution context) and the given functions
;; it is set to execute the first function at byte-code-index 0
(define/contract (make-vm #:locals [locals (make-pvector 0 (cell))]
                          #:params [params (make-pvector 0 (cell))]
                          #:globals [globals (make-pvector 0 (cell))]
                          #:functions [functions (make-pvector 1 (function 0 (make-pvector 1 CISC_VM_BRK) "just-brk (0)"))]
                          #:structs [structs (make-pvector 0 (cell))]
                          #:options [options (list)])
  (->* [] [#:locals pvector? #:params pvector? #:globals pvector?  #:functions pvector? #:structs pvector? #:options (listof symbol?)] vm?)
  (vm '() functions (frame (continuation 0 0) 0 0 locals params 0) globals structs options))

(define/contract (set-locals-array a-vm new-locals)
  (-> vm? pvector? vm?)
  (struct-copy vm a-vm
               [frame (struct-copy frame (vm-frame a-vm)
                                   [locals new-locals])]))

;; get the current local at idx
(define/contract (get-local a-vm idx)
  (-> vm? nonnegative-integer? cell?)
  "get the given local of the vm"
  (nth (frame-locals (vm-frame a-vm)) idx))

(define/contract (get-param a-vm idx)
  (-> vm? nonnegative-integer? cell?)
  "get the given local of the vm"
  (nth (frame-params (vm-frame a-vm)) idx))

(define/contract (get-global a-vm idx)
  (-> vm? nonnegative-integer? cell?)
  "get the given local of the vm"
  (nth (vm-globals a-vm) idx))

(module+ test #| make-vm, set-locals!, get-local |#
  (define some-cell (cell))

  (check-eq?
   (get-local (make-vm #:locals (build-pvector 4 (lambda (at) (nth (list (cell) some-cell (cell) (cell)) at))))
              1)
   some-cell
   "ensure that idx 1 references some-cell, as initialized")

  (for-each (lambda (it)
              (check-not-eq?
               (get-local (make-vm #:locals (build-pvector 4 (lambda (at) (nth (list (cell) some-cell (cell) (cell)) at)))) it)
               some-cell
               "ensure idx does not reference 'some-cell'"))
            (list 0 2 3)))

;; set the local of the current call frame at idx
(define/contract (set-local a-vm idx a-cell)
  (-> vm? exact-nonnegative-integer? cell? vm?)
  (struct-copy vm a-vm
               [frame (struct-copy frame (vm-frame a-vm)
                                   [locals (set-nth (frame-locals (vm-frame a-vm)) idx a-cell)])]))

(module+ test #| set-local, get-local |#
  (check-equal?
   (get-local
    (set-local (make-vm #:locals (make-pvector 4 (cell))) 1 some-cell)
    1)
   some-cell))

;; set the global function at index
(define/contract (set-function a-vm idx a-function)
  (-> vm? exact-nonnegative-integer? function? vm?)
  (struct-copy vm a-vm
               [functions (set-nth (vm-functions a-vm) idx a-function)]))

(module+ test #| set-function |#
  (define someFunction (function 1 (pvector CISC_VM_BYTE_ADD 0 0 0) "some"))
  (define dummyFunction (function 0 (pvector CISC_VM_BRK) "brk"))

  (check-equal?
   (nth (vm-functions
         (set-function (make-vm #:functions (pvector dummyFunction))
                       0 someFunction))
        0)
   someFunction))

;; set the global value at idx
(define/contract (set-global a-vm idx a-cell)
  (-> vm? exact-nonnegative-integer? cell? vm?)
  (struct-copy vm a-vm
               [globals (set-nth (vm-globals a-vm) idx a-cell)]))

(module+ test #| set-global |#
  (check-equal?
   (get-global
    (set-global
     (make-vm #:globals (pvector 2 (cell)))
     0
     (cell-byte 1))
    0)
   (cell-byte 1)))

;; the the parameter of the current call frame at idx
(define/contract (set-param a-vm idx a-cell)
  (-> vm? exact-nonnegative-integer? cell? vm?)
  (struct-copy vm a-vm
               [frame (struct-copy frame (vm-frame a-vm)
                                   [params (set-nth (frame-params (vm-frame a-vm)) idx a-cell)])]))

(module+ test #| set-parameter |#
  (check-equal?
   (get-param
    (set-param
     (make-vm #:params (pvector 2 (cell)))
     0
     (cell-byte 1))
    0)
   (cell-byte 1)))

;; call the function by index:
;; push the old execution frame on the frame stack
;; pass the given parameters
;; allocate locals as defined by the function
;; make the current execution context point to the first byte of the called function
(define/contract (call-function a-vm function-idx params encoded-return-cell)
  (-> vm? exact-nonnegative-integer? (listof cell?) byte? vm?)
  (define called-function (nth (vm-functions a-vm) function-idx))
  (define locals-count (function-locals-count called-function))
  (struct-copy vm a-vm
               [frame-stack (cons (vm-frame a-vm) (vm-frame-stack a-vm))]
               [frame (frame (continuation 0 function-idx)
                             (length params)
                             locals-count
                             (make-pvector locals-count (cell))
                             (apply pvector params)
                             encoded-return-cell)]))

(module+ test #| call-function |#
  (define afterCall
    (call-function
     (make-vm #:locals (pvector (cell-byte 0)) #:functions (pvector dummyFunction (function 1 (pvector CISC_VM_BYTE_ADD VM_L0 VM_P0 VM_P1) "some")))
     1
     (list (cell-byte 1) (cell-byte 2))
     0))

  (check-equal? (frame-continuation (vm-frame afterCall))
                (continuation 0 1))

  (check-equal? (frame-param-count (vm-frame afterCall))
                2)

  (check-equal? (frame-locals-count (vm-frame afterCall))
                1)

  (check-equal? (length (vm-frame-stack afterCall))
                1)

  (check-equal? (nth (frame-params (vm-frame afterCall)) 0)
                (cell-byte 1))
  (check-equal? (nth (frame-params (vm-frame afterCall)) 1)
                (cell-byte 2)))

;; increment the byte code pointer of the currently executing function
(define/contract (increment-program-counter a-vm (delta 1))
  (->* [vm?] [exact-integer?] vm?)
  (define the-frame (vm-frame a-vm))
  (define the-cont (frame-continuation the-frame))
  (struct-copy
   vm a-vm
   [frame (struct-copy
           frame the-frame
           [continuation (struct-copy
                          continuation the-cont
                          [byte-code-idx (fx+ (continuation-byte-code-idx the-cont) delta)])])]))

(define/contract (set-byte-code-idx a-vm new-byte-code-idx)
  (-> vm? exact-nonnegative-integer? vm?)
  (define the-frame (vm-frame a-vm))
  (define the-cont (frame-continuation the-frame))
  (struct-copy
   vm a-vm
   [frame (struct-copy
           frame the-frame
           [continuation (struct-copy
                          continuation the-cont
                          [byte-code-idx new-byte-code-idx])])]))

(module+ test #| increment-program-counter |#
  (check-equal? (frame-continuation (vm-frame (increment-program-counter (make-vm))))
                (continuation 1 0))
  (check-equal? (frame-continuation (vm-frame (increment-program-counter (make-vm) 2)))
                (continuation 2 0)))

;; get bytecode of the currently executed function
(define/contract (get-current-byte-code a-vm (offset 0))
  (->* [vm?] [exact-integer?] byte?)
  (define cont (frame-continuation (vm-frame a-vm)))
  (define running-function (nth (vm-functions a-vm) (continuation-func-idx cont)))
  (nth (function-byte-code running-function)
       (fx+ (continuation-byte-code-idx cont) offset)))

(module+ test #| get-current-byte-code |#
  (define vmWithLotsOfBytecode (make-vm #:functions (pvector (function 0 (pvector 1 2 3 4) "withBytecode"))))

  (check-equal? (get-current-byte-code vmWithLotsOfBytecode 0)
                1)
  (check-equal? (get-current-byte-code vmWithLotsOfBytecode 2)
                3)
  (check-equal? (get-current-byte-code vmWithLotsOfBytecode 3)
                4))

;; restore pre-call frame, don't move any parameter
(define/contract (return-from-call a-vm encoded-result-cell)
  (-> vm? byte? vm?)
  (define result-cell (get-cell a-vm (decode-cell-idx encoded-result-cell)))
  (define frame-stack (vm-frame-stack a-vm))
  (define new-frame (car frame-stack))
  (define return-cell (frame-return-cell (vm-frame a-vm)))
  (set-cell
   (struct-copy vm a-vm
                [frame new-frame]
                [frame-stack (cdr frame-stack)])
   (decode-cell-idx return-cell)
   result-cell))

(module+ test #| return-from-call |#
  (define vmCallAndReturn (return-from-call afterCall 0))

  (check-equal? (length (vm-frame-stack vmCallAndReturn))
                0)
  (check-equal? (frame-continuation (vm-frame vmCallAndReturn))
                (continuation 0 0)))

(struct cell-idx
  (index
   location)
  #:transparent ;; for tests
  #:guard
  (struct-guard/c
   exact-nonnegative-integer? ;; 0..63
   exact-nonnegative-integer? ;; 0..3, 0 = local, 1 = parameter, 2 = global, (idea: 3 = immediate byte value => index is an immediate constant (no reg use))
   ;; (or/c 'local 'global 'parameter 'immediate)
   ))

(define l-local 0)
(define l-param 1)
(define l-global 2)
(define l-imm 3)

(define/contract (encode-idx idx location)
  (-> byte? byte? byte?)
  (bitwise-ior
   (arithmetic-shift (bitwise-and location #b11) 6)
   (bitwise-and idx #b00111111)))

(define/contract (decode-cell-idx encoded-idx)
  (-> byte? cell-idx?)
  (cell-idx
   (bitwise-and #b00111111 encoded-idx)
   (bitwise-bit-field encoded-idx 6 8)))

(module+ test #| decode-cell-idx |#
  (check-equal? (decode-cell-idx #b01000010)
                (cell-idx 2 1))
  (check-equal? (encode-idx (cell-idx-index (decode-cell-idx #b01000010))
                            (cell-idx-location (decode-cell-idx #b01000010)))
                #b01000010))

(define/contract (get-cell a-vm a-cell-idx)
  (-> vm? cell-idx? cell?)
  (define idx (cell-idx-index a-cell-idx))
  (define i-cell
    (case (cell-idx-location a-cell-idx)
      ((0) (nth (frame-locals (vm-frame a-vm)) idx))
      ((1) (nth (frame-params (vm-frame a-vm)) idx))
      ((2) (nth (vm-globals a-vm) idx))
      ((3) (cell-byte idx)) ;; create dummy cell to carry 0..63 as byte value
      (else (raise-user-error "unknown cell-index"))))
  (if (cell-ptr? i-cell)
      (cell-ptr-ref i-cell)
      i-cell))

(module+ test #| get-cell |#
  (define local1params1globals2
    (make-vm #:locals (make-pvector 1 (cell-byte 1))
             #:params (pvector (cell-byte 2) (cell-byte 5))
             #:globals (make-pvector 2 (cell-byte 3))
             #:functions (make-pvector 2 (function 0 (make-pvector 1 CISC_VM_BRK) "empty"))))

  (check-equal? (get-cell local1params1globals2 (cell-idx 0 0))
                (cell-byte 1))
  (check-equal? (get-cell local1params1globals2 (cell-idx 0 1))
                (cell-byte 2))
  (check-equal? (get-cell local1params1globals2 (cell-idx 1 2))
                (cell-byte 3)))

(define/contract (set-cell a-vm a-cell-index a-cell)
  (-> vm? cell-idx? cell? vm?)
  (case (cell-idx-location a-cell-index)
    ((0) (set-local a-vm (cell-idx-index a-cell-index) a-cell))
    ((1) (set-param a-vm (cell-idx-index a-cell-index) a-cell))
    ((2) (set-global a-vm (cell-idx-index a-cell-index) a-cell))
    (else (raise-user-error "unknown cell-index"))))

(module+ test #| set-cell |#
  (check-equal? (get-local (set-cell local1params1globals2 (cell-idx 0 0) (cell-byte 5)) 0)
                (cell-byte 5))
  (check-equal? (get-param (set-cell local1params1globals2 (cell-idx 0 1) (cell-byte 5)) 0)
                (cell-byte 5))
  (check-equal? (get-global (set-cell local1params1globals2 (cell-idx 0 2) (cell-byte 5)) 0)
                (cell-byte 5)))

(define/contract (interpret-byte+ a-vm)
  (-> vm? vm?)
  (define target-cell-idx (decode-cell-idx (get-current-byte-code a-vm 1)))
  (define arg-a (get-cell a-vm (decode-cell-idx (get-current-byte-code a-vm 2))))
  (define arg-b (get-cell a-vm (decode-cell-idx (get-current-byte-code a-vm 3))))
  (when (member 'trace (vm-options a-vm))
    (displayln (format "cell: ~a = ~a + ~a" target-cell-idx arg-a arg-b)))
  (increment-program-counter
   (set-cell a-vm
             target-cell-idx
             (cell-byte (fx+ (cell-byte-value arg-a)
                             (cell-byte-value arg-b))))
   4))

(module+ test #| interpret-byte+ |#
  (define add-function (function 1 (pvector CISC_VM_BYTE_ADD VM_L0 VM_P0 VM_P1) "just add"))
  (define vm-after-add (interpret-byte+ (set-function local1params1globals2 0 add-function)))

  (check-equal? (frame-continuation (vm-frame vm-after-add))
                (continuation 4 0)
                "program counter is increased by 4")
  (check-equal? (get-local vm-after-add 0)
                (cell-byte 7)))

(define/contract (interpret-call a-vm)
  (-> vm? vm?)
  (define into-reg (get-current-byte-code a-vm 1))
  (define function-index (fx+ (get-current-byte-code a-vm 2) (arithmetic-shift (get-current-byte-code a-vm 3) 8)))
  (define param-count (get-current-byte-code a-vm 4))
  (define params (get-next-decoded-cells a-vm param-count 5))
  (when (member 'trace (vm-options a-vm))
    (displayln (format "call: function '~a' with params ~a, result -> ~a" (function-name (nth (vm-functions a-vm) function-index)) params (decode-cell-idx into-reg))))
  (call-function (increment-program-counter a-vm (fx+ param-count 5)) function-index params into-reg))

(module+ test #| interpret-call |#
  (define adderVM
    (increment-program-counter
     (make-vm #:functions
              (pvector (function 3 (pvector CISC_VM_IMMB VM_L0 1
                                            CISC_VM_IMMB VM_L1 2
                                            CISC_VM_CALL VM_L2 1 0 2 VM_L0 VM_L1)
                                 "f0: call f1")
                       (function 1 (pvector CISC_VM_BYTE_ADD VM_L0 VM_P0 VM_P1
                                            CISC_VM_RET VM_L0)
                                 "just add"))
              #:locals (pvector (cell-byte 1) (cell-byte 2) (cell)))
     6))

  (define adderVMafterCall (interpret-call adderVM))

  (check-equal? (frame-continuation (vm-frame adderVMafterCall))
                (continuation 0 1))

  (check-equal? (length (frame-locals (vm-frame adderVMafterCall)))
                1))

(define/contract (interpret-return a-vm)
  (-> vm? vm?)
  (return-from-call a-vm (get-current-byte-code a-vm 1)))

(module+ test #| interpret-return |#
  (define adderVMafterCallReturn (interpret-return (set-local (increment-program-counter adderVMafterCall 4) 0 (cell-byte 88))))

  (check-equal?  (get-local adderVMafterCallReturn 2)
                 (cell-byte 88)))

(define/contract (interpret-load-immediate-byte a-vm)
  (-> vm? vm?)
  (define target-cell-idx (decode-cell-idx (get-current-byte-code a-vm 1)))
  (define immediate (get-current-byte-code a-vm 2))
  (increment-program-counter
   (set-cell a-vm target-cell-idx (cell-byte immediate))
   3))

(module+ test #| interpret-load-immediate-byte |#
  (check-equal? (get-local
                 (interpret-load-immediate-byte
                  (make-vm #:locals (pvector (cell))
                           #:functions (pvector (function 1 (pvector 4 0 117) "load l0 #117"))))
                 0)
                (cell-byte 117)))

(define (get-next-decoded-cells a-vm element-no (offset 0))
  (->* [vm? exact-nonnegative-integer?] [exact-integer?] (listof cell?))
  (map (lambda (idx) (get-cell a-vm (decode-cell-idx (get-current-byte-code a-vm (fx+ offset idx))))) (range element-no)))

(module+ test #| get-next-decoded-cells |#
  (check-equal?
   (get-next-decoded-cells
    (make-vm #:locals (pvector (cell) (cell-byte 1) (cell-byte 2))
             #:functions (pvector (function 0 (pvector 2 VM_L1 VM_L2) "dummy")))
    2
    1)
   (list (cell-byte 1) (cell-byte 2))))

(define (construct-rev-cell-list elements (cur-head VM_NIL_CELL))
  (->* [(listof atomic-cell?)] [cell-list-ptr?] cell-list-ptr?)
  (if (empty? elements)
      cur-head
      (construct-rev-cell-list (cdr elements)
                               (cell-list-ptr (car elements) cur-head))))

(module+ test #| construct-rev-cell-list |#
  (check-equal? (construct-rev-cell-list (list (cell-byte 1) (cell-byte 2) (cell-byte 3)))
                (cell-list-ptr (cell-byte 3)
                               (cell-list-ptr (cell-byte 2)
                                              (cell-list-ptr (cell-byte 1)
                                                             VM_NIL_CELL)))))

(define (interpret-make-list a-vm)
  (-> vm? vm?)
  (define target-cell-idx (decode-cell-idx (get-current-byte-code a-vm 1)))
  (define element-no (get-current-byte-code a-vm 2))
  (define elements (get-next-decoded-cells a-vm element-no 3))
  (increment-program-counter
   (set-cell a-vm target-cell-idx (construct-rev-cell-list (reverse elements)))
   (fx+ 3 element-no)))

(module+ test
  (check-equal?
   (get-local
    (interpret-make-list
     (make-vm #:locals (pvector (cell-byte 1) (cell-byte 2) (cell))
              #:functions (pvector (function 0 (pvector CISC_VM_MAKE_LIST VM_L2 2 VM_L0 VM_L1) "some"))))
    2)
   (cell-list-ptr (cell-byte 1)
                  (cell-list-ptr (cell-byte 2)
                                 VM_NIL_CELL))))

(define (interpret-bra-emtpy-list a-vm)
  (-> vm? vm?)
  (increment-program-counter
   a-vm
   (if (cell-nil? (get-cell a-vm (decode-cell-idx (get-current-byte-code a-vm 1))))
       (fx+ 2 (decimal-from-two-complement (get-current-byte-code a-vm 2)))
       3)))

(module+ test #| bra-empty-list |#
  (check-equal?
   (frame-continuation
    (vm-frame
     (interpret-bra-emtpy-list
      (make-vm #:locals (pvector VM_NIL_CELL)
               #:functions (pvector (function 1 (pvector CISC_VM_BRA_EMPTY_LIST VM_L0 (two-complement-of -2)) "some"))))))
   (continuation 0 0))
  (check-equal?
   (frame-continuation
    (vm-frame
     (interpret-bra-emtpy-list
      (make-vm #:locals (pvector VM_NIL_CELL)
               #:functions (pvector (function 1 (pvector CISC_VM_BRA_EMPTY_LIST VM_L0 (two-complement-of 1)) "some"))))))
   (continuation 3 0)))

(define/contract (interpret-car a-vm)
  (-> vm? vm?)
  (increment-program-counter
   (set-cell a-vm (decode-cell-idx (get-current-byte-code a-vm 1))
             (cell-list-ptr-car (get-cell a-vm (decode-cell-idx (get-current-byte-code a-vm 2)))))
   3))

(module+ test #| interpret-car |#
  (check-equal?
   (get-local
    (interpret-car
     (make-vm #:locals (pvector (cell-list-ptr (cell-byte 1) (cell-list-ptr (cell-byte 2) VM_NIL_CELL))
                                (cell))
              #:functions (pvector (function 1 (pvector CISC_VM_CAR VM_L1 VM_L0) "car"))))
    1)
   (cell-byte 1)))

(define (interpret-cdr a-vm)
  (-> vm? vm?)
  (increment-program-counter
   (set-cell a-vm (decode-cell-idx (get-current-byte-code a-vm 1))
             (cell-list-ptr-cdr (get-cell a-vm (decode-cell-idx (get-current-byte-code a-vm 2)))))
   3))

(module+ test #| interpret-cdr |#
  (check-equal?
   (get-local
    (interpret-cdr
     (make-vm #:locals (pvector (cell-list-ptr (cell-byte 1) (cell-list-ptr (cell-byte 2) VM_NIL_CELL))
                                (cell))
              #:functions (pvector (function 1 (pvector CISC_VM_CDR VM_L1 VM_L0) "car"))))
    1)
   (cell-list-ptr (cell-byte 2) VM_NIL_CELL)))

(define (interpret-goto a-vm)
  (-> vm? vm?)
  (define rel-goto (fx+ 1 (decimal-from-two-complement (get-current-byte-code a-vm 1))))
  (when (member 'trace (vm-options a-vm))
    (displayln (format "goto ~a" rel-goto)))
  (increment-program-counter a-vm rel-goto))

(module+ test #| interpret-goto |#
  (check-equal?
   (frame-continuation
    (vm-frame
     (interpret-goto
      (make-vm #:functions (pvector (function 0 (pvector CISC_VM_GOTO (two-complement-of -1)) "goto"))))))
   (continuation 0 0))
  (check-equal?
   (frame-continuation
    (vm-frame
     (interpret-goto
      (make-vm #:functions (pvector (function 0 (pvector CISC_VM_GOTO (two-complement-of 1)) "goto"))))))
   (continuation 2 0)))


(module+ test #| recursive list generating function |#
  (define vmRecF
    (make-vm #:functions (pvector (function 5 (pvector CISC_VM_IMMB VM_L0 10
                                                       CISC_VM_IMMB VM_L1 20
                                                       CISC_VM_MAKE_LIST VM_L2 2 VM_L0 VM_L1
                                                       CISC_VM_IMMB VM_L3 0
                                                       CISC_VM_CALL VM_L4 1 0 2 VM_L2 VM_L3
                                                       CISC_VM_BRK) "sum (list 10 20)")
                                  (function 1 (pvector CISC_VM_BRA_EMPTY_LIST VM_P0 (two-complement-of 13) ;; 3 bytes
                                                       CISC_VM_CAR VM_L0 VM_P0               ;; 3
                                                       CISC_VM_BYTE_ADD VM_P1 VM_L0 VM_P1    ;; 4
                                                       CISC_VM_CDR VM_P0 VM_P0               ;; 3
                                                       CISC_VM_GOTO (two-complement-of -14)  ;; 2
                                                       CISC_VM_RET VM_P1) "rec sum list of bytes with acc"))
             #:locals (make-pvector 5 (cell))
             #:options (list))) ;; add  'trace to options
  (define vmRecFAfterRun (run-until-break vmRecF))

  (check-equal? (get-local vmRecFAfterRun 4)
                (cell-byte 30)))

(define/contract (array-create a-vm target-reg len default-value)
  (-> vm? cell-idx? exact-nonnegative-integer? cell? vm?)
  (set-cell a-vm target-reg
            (cell-array len (make-pvector len default-value ))))

(module+ test #| array-create |#
  (check-equal?
   (get-cell
    (array-create
     (make-vm #:locals (pvector 2 (cell)))
     (cell-idx 0 0)
     2
     (cell-byte 42))
    (cell-idx 0 0))
   (cell-array 2 (pvector (cell-byte 42) (cell-byte 42)))))

(define/contract (interpret-array-create a-vm)
  (-> vm? vm?)
  (define target-reg (decode-cell-idx (get-current-byte-code a-vm 1)))
  (define len (cell-value (get-cell a-vm (decode-cell-idx (get-current-byte-code a-vm 2)))))
  (define default-value (get-cell a-vm (decode-cell-idx (get-current-byte-code a-vm 3))))
  (increment-program-counter
   (array-create a-vm target-reg len default-value) 4))

(module+ test #| interpret-array-create |#
  (check-equal?
   (get-cell
    (interpret-array-create
     (make-vm #:locals (pvector (cell) (cell-int 2) (cell-byte 10))
              #:functions (pvector (function 0 (pvector CISC_VM_ARRAY_CREATE VM_L0 VM_L1 VM_L2) "fun"))))
    (decode-cell-idx VM_L0))
   (cell-array 2 (pvector (cell-byte 10) (cell-byte 10)))))

(define/contract (array-get a-vm array-cell-idx idx)
  (-> vm? cell-idx? exact-nonnegative-integer? cell?)
  (nth (cell-array-array (get-cell a-vm array-cell-idx)) idx))

(define/contract (interpret-array-get a-vm)
  (-> vm? vm?)
  (define target-reg (decode-cell-idx (get-current-byte-code a-vm 1)))
  (define array-cell-idx (decode-cell-idx (get-current-byte-code a-vm 2)))
  (define idx (cell-value (get-cell a-vm (decode-cell-idx (get-current-byte-code a-vm 3)))))
  (increment-program-counter
   (set-cell a-vm target-reg (array-get a-vm array-cell-idx idx))
   4))

(define/contract (array-set a-vm target-idx array-cell-idx array-idx value)
  (-> vm? cell-idx? cell-idx? byte? cell? vm?)
  (define array-cell (get-cell a-vm array-cell-idx))
  (set-cell a-vm target-idx
            (cell-array (cell-array-size array-cell)
                        (set-nth (cell-array-array array-cell) array-idx value))))

(define/contract (interpret-array-set a-vm)
  (-> vm? vm?)
  (define array-cell-idx (decode-cell-idx (get-current-byte-code a-vm 2)))
  (define idx (cell-value (get-cell a-vm (decode-cell-idx (get-current-byte-code 3)))))
  (define value (array-get a-vm array-cell-idx idx))
  (increment-program-counter
   (array-set a-vm array-cell-idx array-cell-idx idx value)
   4))

(module+ test #| array get and set |#
  (check-equal?
   (array-get
    (array-create (make-vm #:locals (pvector (cell) (cell-byte 5))) (cell-idx 0 0) 10 (cell-byte 0))
    (cell-idx 0 0)
    5)
   (cell-byte 0))

  (check-equal?
   (array-get
    (array-set
     (array-create (make-vm #:locals (pvector (cell) (cell-byte 5))) (cell-idx 0 0) 10 (cell-byte 0))
     (cell-idx 0 0) ;; target reg
     (cell-idx 0 0) ;; array reg
     5              ;; index
     (cell-byte 7)) ;; value
    (cell-idx 0 0)
    5)
   (cell-byte 7))

  (check-equal?
   (array-get
    (array-set
     (array-create (make-vm #:locals (pvector (cell) (cell-byte 5))) (cell-idx 0 0) 10 (cell-byte 0))
     (cell-idx 0 0) ;; target reg
     (cell-idx 0 0) ;; array reg
     5              ;; index
     (cell-byte 7)) ;; value
    (cell-idx 0 0)
    6)
   (cell-byte 0)))

(define/contract (struct-create a-vm struct-def target-reg init-cells)
  (-> vm? cell-array? cell-idx? (listof cell?) vm?)
  (define field-num (cell-byte-value (nth (cell-array-array struct-def) 0)))
  (define vm-w-alloc-struct (array-create a-vm target-reg field-num (cell)))
  (foldl (lambda (idxd-cell-pair n-vm)
           (array-set n-vm target-reg target-reg (car idxd-cell-pair) (cdr idxd-cell-pair)))
         vm-w-alloc-struct
         (map cons (range field-num) init-cells)))

(define/contract (interpret-struct-create a-vm)
  (-> vm? vm?)
  (define target-reg (decode-cell-idx (get-current-byte-code a-vm 1)))
  (define struct-def (cell-ptr-ref (nth (vm-structs a-vm) (get-current-byte-code a-vm 2))))
  (define struct-size (cell-byte-value (nth (cell-array-array struct-def) 0)))
  (define init-cells (get-next-decoded-cells a-vm struct-size 3))
  (struct-create a-vm struct-def target-reg init-cells))

(module+ test #| structure create, access = array get |#
  (define vmStructPointDefinition
    (cell-ptr (cell-array 4 (pvector (cell-byte 2) ;; two fields
                                     (cell-ptr (cell-array 5 (pvector #\p #\o #\i #\n #\t))) ;; structure name
                                     (cell-ptr (cell-array 1 (pvector #\x)))     ;; field 0 name
                                     (cell-ptr (cell-array 1 (pvector #\y))))))) ;; field 1 name

  (define vmStructCreate
    (make-vm #:locals (pvector (cell) (cell-byte 10) (cell-byte 20))
             #:functions (pvector (function 1 (pvector CISC_VM_STRUCT_CREATE VM_L0 0 #| index to structure definition |# VM_L1 VM_L2) "some"))
             #:structs (pvector vmStructPointDefinition)))

  (define vmStructCreateAfterRun
    (interpret-struct-create vmStructCreate))

  (check-equal? (get-local vmStructCreateAfterRun 0)
                (cell-array 2 (pvector (cell-byte 10) (cell-byte 20)))))


(define/contract (string->pvector a-str)
  (-> string? pvector?)
  (apply pvector (string->list a-str)))

(define/contract (pvector->string a-vec)
  (-> pvector? string?)
  (apply string (sequence->list a-vec)))

(module+ test
  (check-equal? (pvector->string (string->pvector "hello"))
                "hello"))

(define/contract (string->cell-array a-str)
  (-> string? cell-array?)
  (cell-array (string-length a-str) (string->pvector a-str)))

;; creates a structure with vm data such that vm commands can introspect on this data
(define/contract (make-structure name fields)
  (-> string? (listof string?) cell-ptr?)
  (cell-ptr
   (cell-array
    (+ 2 (length fields))
    (apply pvector
           (cons (cell-byte (length fields))
                 (cons (cell-ptr (string->cell-array name))
                       (map (lambda (field) (cell-ptr (string->cell-array field))) fields)))))))

(define/contract (structure-field-num a-struc-def)
  (-> cell-ptr? byte?)
  (cell-byte-value (nth (cell-array-array (cell-ptr-ref a-struc-def)) 0)))

(define/contract (structure-name a-struc-def)
  (-> cell-ptr? string?)
  (pvector->string (cell-array-array (cell-ptr-ref (nth (cell-array-array (cell-ptr-ref a-struc-def)) 1)))))

(define/contract (structure-field a-struc-def idx)
  (-> cell-ptr? byte? string?)
  (pvector->string (cell-array-array (cell-ptr-ref (nth (cell-array-array (cell-ptr-ref a-struc-def)) (+ 2 idx))))))


(module+ test #| write part of the interpreter within itself, poc |#
  (define r-struct-vm-functions 1)
  (define r-struct-vm-frame 2)
  (define r_struct-vm (make-structure "vm"
                                      (list "frame-stack"
                                            "functions"
                                            "frame"
                                            "globals"
                                            "structs"
                                            "options")))

  (define r-struct-frame-cont 0)
  (define r-struct-frame-locals 3)
  (define r_struct-frame (make-structure "frame"
                                         (list "continuation"
                                               "param-count"
                                               "locals-count"
                                               "locals"
                                               "params"
                                               "return-cell")))

  (define r-struct-cont-byte-code-idx 0)
  (define r-struct-cont-func-idx 1)
  (define r_struct-continuation (make-structure "continuation"
                                                (list "byte-code-idx"
                                                      "func-idx")))

  (define r-struct-function-byte-code 1)
  (define r_struct-function (make-structure "function"
                                            (list "locals-count"
                                                  "byte-code"
                                                  "name")))


  (define r_function-get-local (function 3 (pvector CISC_VM_ARRAY_GET VM_L0 VM_P0 (encode-idx r-struct-vm-frame l-imm )
                                                    CISC_VM_ARRAY_GET VM_L1 VM_L0 (encode-idx r-struct-frame-locals l-imm)
                                                    CISC_VM_ARRAY_GET VM_L2 VM_L1 VM_P1
                                                    CISC_VM_RET VM_L2)
                                         "get-local"))

  (define r_function-test-get-local (function 1 (pvector CISC_VM_CALL VM_L0 1 0 2 VM_L3 VM_L4 ;; vm is local@3
                                                         CISC_VM_BRK)
                                              "test-get-local"))

  (define/contract (r_make-function a-function)
    (-> function? cell-ptr?)
    (cell-ptr
     (cell-array
      3
      (pvector (cell-byte (function-locals-count a-function))
               (cell-ptr (cell-array (length (function-byte-code a-function))
                                     (apply pvector (sequence->list (sequence-map (lambda (byte) (cell-byte byte)) (function-byte-code a-function))))))
               (cell-ptr (cell-array (length (function-name a-function))
                                     (apply pvector (sequence->list (sequence-map (lambda (byte) (cell-byte (char->integer byte))) (string->pvector (function-name a-function)))))))))))

  (check-equal? (r_make-function (function 2 (pvector 1 2 3) "some"))
                (cell-ptr
                 (cell-array
                  3
                  (pvector (cell-byte 2)
                           (cell-ptr (cell-array 3 (pvector (cell-byte 1) (cell-byte 2) (cell-byte 3))))
                           (cell-ptr (cell-array 4 (pvector (cell-byte (char->integer #\s))(cell-byte (char->integer #\o))(cell-byte (char->integer #\m))(cell-byte (char->integer #\e)))))))))

  (define/contract (r_make-vm #:frame [a-frame (frame (continuation 0 0) 0 0 (pvector) (pvector) 0)]
                              #:functions [functions (pvector)])
    (->* [] [#:frame frame? #:functions pvector?] cell-ptr?)
    (cell-ptr
     (cell-array
      6
      (pvector (cell) ;; frame-stack  - list of frames
               (cell-ptr   ;; functions    - vector of functions
                (cell-array
                 (length functions)
                 (apply pvector (sequence->list (sequence-map (lambda (a-function) (r_make-function a-function)) functions)))))
               (cell-ptr  ;; frame        - frame
                (cell-array
                 6
                 (pvector (cell-ptr
                           (cell-array ;; continuation
                            2
                            (pvector
                             (cell-int (continuation-byte-code-idx (frame-continuation a-frame)))
                             (cell-int (continuation-func-idx (frame-continuation a-frame)))
                             )))
                          (cell-byte (frame-param-count a-frame)) ;; param-count
                          (cell-byte (frame-locals-count a-frame)) ;; locals-count
                          (cell-ptr      ;; locals - vector of cells
                           (cell-array
                            (frame-locals-count a-frame)
                            (frame-locals a-frame)))
                          (cell-ptr
                           (cell-array
                            (frame-param-count a-frame)
                            (frame-params a-frame)))
                          (cell)  ;; TODO: (frame-result-cell frame) return cell (needs to be resolved to the actual target cell)
                          )))
               (cell) ;; globals      - vector of cells
               (cell) ;; structs      - vector of struct-definitions
               (cell) ;; options      - vector of cells (options)
               ))))

  (define test-get-local-vm
    (make-vm #:structs (pvector r_struct-vm
                                r_struct-frame
                                r_struct-continuation
                                r_struct-function)
             #:locals (pvector
                       (cell) ;; result of test function (local @0)
                       (cell-byte (encode-idx 2 l-local)) ;; encoded ref to local 2 in the virtual machine
                       (cell-byte 47) ;; local @2 (should NOT be fetched!)
                       ;; struct of running vm (just a frame with locals of which local@1 should be get
                       (r_make-vm #:frame (frame (continuation 0 0)
                                                 0 ;; param-count
                                                 3 ;; local-count
                                                 (pvector ;; locals
                                                  (cell-byte 11)
                                                  (cell-byte 21) ;; <-- value actually got
                                                  (cell-byte 31))
                                                 (pvector) ;; params
                                                 0)) ;; result
                       (cell-int 1)) ;; index into cell array, parameter to get-local
             #:options (list) ;; 'trace
             #:functions (pvector r_function-test-get-local ;; call function-get-local with VM_L3 and VM_L4 as parameter
                                  r_function-get-local)))   ;; get local out of passed virtual machine

  (define test-get-local-vm-run (run-until-break test-get-local-vm))
  (check-equal? (get-local test-get-local-vm-run 0)
                (cell-byte 21)))

(define/contract (cons-cells a-vm target-reg car-cell cdr-cell)
  (-> vm? cell-idx? cell? cell? vm?)
  (set-cell a-vm target-reg (cell-list-ptr car-cell cdr-cell)))

;; <target-reg> <value-reg> <list-ptr/value-reg>
(define/contract (interpret-cons a-vm)
  (-> vm? vm?)
  (define target-reg (decode-cell-idx (get-current-byte-code a-vm 1)))
  (define car-reg-cell (get-cell a-vm (decode-cell-idx (get-current-byte-code a-vm 2))))
  (define cdr-reg-cell  (get-cell a-vm (decode-cell-idx (get-current-byte-code a-vm 3))))
  (increment-program-counter
   (cons-cells a-vm target-reg car-reg-cell cdr-reg-cell)
   4))

(module+ test #| cons-cells, interpret-cons |#
  (check-equal? (get-local (cons-cells (make-vm #:locals (pvector (cell)))
                                       (decode-cell-idx VM_L0)
                                       (cell-byte 5)
                                       (cell-byte 6))
                           0)
                (cell-list-ptr (cell-byte 5) (cell-byte 6)))

  (check-equal? (get-local (cons-cells (make-vm #:locals (pvector (cell)))
                                       (decode-cell-idx VM_L0)
                                       (cell-byte 5)
                                       VM_NIL_CELL)
                           0)
                (cell-list-ptr (cell-byte 5) VM_NIL_CELL))

  (check-equal? (get-local (interpret-cons
                            (make-vm #:functions (pvector (function 0 (pvector CISC_VM_CONS VM_L0 VM_L1 VM_L2) "cons"))
                                     #:locals (pvector (cell) (cell-byte 5) (cell-byte 6))))
                           0)
                (cell-list-ptr (cell-byte 5) (cell-byte 6))))

(define/contract (make-int a-cell b-cell)
  (-> cell-byte? cell-byte? cell-int?)
  (cell-int (fx+ (arithmetic-shift (bitwise-and (cell-byte-value b-cell) #b00111111) 8) (cell-byte-value a-cell))))

(define/contract (2-bytes->int a-byte b-byte)
  (-> byte? byte? integer?)
  (fx+ (arithmetic-shift (bitwise-and b-byte #b00111111) 8) a-byte))

(define/contract (interpret-make-int a-vm)
  (-> vm? vm?)
  (define target-reg (decode-cell-idx (get-current-byte-code a-vm 1)))
  (define low-byte-cell (get-cell a-vm (decode-cell-idx (get-current-byte-code a-vm 2))))
  (define high-byte-cell (get-cell a-vm (decode-cell-idx (get-current-byte-code a-vm 3))))
  (increment-program-counter
   (set-cell a-vm target-reg (make-int low-byte-cell high-byte-cell))
   4))

(module+ test #| make-int, interpret-make-int |#
  (check-equal? (make-int (cell-byte 1) (cell-byte 2))
                (cell-int 513))

  (check-equal? (make-int (cell-byte 0) (cell-byte 0))
                (cell-int 0))

  (check-equal? (make-int (cell-byte 255) (cell-byte 63))
                (cell-int 16383))

  (check-equal? (make-int (cell-byte 255) (cell-byte 64))
                (cell-int 255)
                "bits 7 and 8 are ignored in high byte to make value fit in cell")

  (check-equal? (make-int (cell-byte 255) (cell-byte 128))
                (cell-int 255)
                "bits 7 and 8 are ignored in high byte to make value fit in cell")

  (check-equal? (get-local (interpret-make-int
                            (make-vm #:functions (pvector (function 0 (pvector CISC_VM_MAKE_INT VM_L0 VM_L1 VM_L2) "make-int"))
                                     #:locals (pvector (cell) (cell-byte 5) (cell-byte 6))))
                           0)
                (cell-int 1541)))

(define/contract (int-add cell-a cell-b)
  (-> cell-int? cell-int? cell-int?)
  (cell-int (bitwise-and (fx+ (cell-int-value cell-a) (cell-int-value cell-b)) #b0011111111111111)))

(define/contract (interpret-int-add a-vm)
  (-> vm? vm?)
  (define target-reg (decode-cell-idx (get-current-byte-code a-vm 1)))
  (define cell-a (get-cell a-vm (decode-cell-idx (get-current-byte-code a-vm 2))))
  (define cell-b (get-cell a-vm (decode-cell-idx (get-current-byte-code a-vm 3))))
  (increment-program-counter
   (set-cell a-vm target-reg (int-add cell-a cell-b))
   4))

(define/contract (int-inc a-cell)
  (-> cell-int? cell-int?)
  (cell-int (bitwise-and #b0011111111111111 (fx+ 1 (cell-int-value a-cell)))))

(define/contract (interpret-int-inc a-vm)
  (-> vm? vm?)
  (define target-reg (decode-cell-idx (get-current-byte-code a-vm 1)))
  (define orig-cell (get-cell a-vm target-reg))
  (increment-program-counter
   (set-cell a-vm target-reg (int-inc orig-cell))
   2))

(module+ test #| function byte->int conversion |#

  (define r_peek-inst-int (function 11 (pvector CISC_VM_ARRAY_GET VM_L0 VM_P0 (encode-idx r-struct-vm-frame l-imm)            ;; VM_L0 = frame
                                                CISC_VM_ARRAY_GET VM_L1 VM_L0 (encode-idx r-struct-frame-cont l-imm)          ;; VM_L1 = continuation
                                                CISC_VM_ARRAY_GET VM_L2 VM_P0 (encode-idx r-struct-vm-functions l-imm)        ;; VM_L2 = functions
                                                CISC_VM_ARRAY_GET VM_L3 VM_L1 (encode-idx r-struct-cont-func-idx l-imm)       ;; VM_L3 = current-function-index
                                                CISC_VM_ARRAY_GET VM_L4 VM_L2 VM_L3                                           ;; VM_L4 = current-function
                                                CISC_VM_ARRAY_GET VM_L5 VM_L1 (encode-idx r-struct-cont-byte-code-idx l-imm)  ;; VM_L5 = byte code idx
                                                CISC_VM_INT_ADD   VM_L6 VM_P1 VM_L5                                           ;; VM_L6 = P1 + byte code index
                                                CISC_VM_ARRAY_GET VM_L7 VM_L4 (encode-idx r-struct-function-byte-code l-imm)  ;; VM_L7 = byte code of function
                                                CISC_VM_ARRAY_GET VM_L8 VM_L7 VM_L6                                           ;; VM_L8 = byte code @ offset + byte code index (low)
                                                CISC_VM_INT_INC   VM_L6                                                       ;; VM_L6 ++
                                                CISC_VM_ARRAY_GET VM_L9 VM_L7 VM_L6                                           ;; VM_L9 = byte code @ offset + 1 + byte code index (high)
                                                CISC_VM_MAKE_INT  VM_L10 VM_L8 VM_L9                                          ;; VM_L10 = int (low , high)
                                                CISC_VM_RET       VM_L10)
                                    "peek-inst-int"))

  (define r_peek-inst-int-test-vm
    (make-vm #:locals (pvector (cell)
                               (r_make-vm #:frame (frame (continuation 2 1) ;; at byte 2 of function 1
                                                         0 1 (pvector (cell)) (pvector)
                                                         0)
                                          #:functions (pvector (function 0 (pvector) "dummy")
                                                               (function 0 (pvector 20 20 0 1 #xff #x0f) "some byte-code")))
                               (cell-int 2)) ;; offset
             #:functions (pvector (function 0 (pvector CISC_VM_CALL VM_L0 1 0 2 VM_L1 VM_L2
                                                       CISC_VM_BRK)
                                            "call peek-inst-int")
                                  r_peek-inst-int)
             #:options (list)))

  (define r_peek-inst-int-test-vm-run (run-until-break r_peek-inst-int-test-vm))

  (check-equal? (get-local r_peek-inst-int-test-vm-run 0)
                (cell-int #x0fff)))

(module+ test #| recursive reverse implementation |#
  (define r_reverse (function 1 (pvector CISC_VM_BRA_EMPTY_LIST VM_P0 13
                                         CISC_VM_CAR            VM_L0 VM_P0
                                         CISC_VM_CONS           VM_P1 VM_L0 VM_P1
                                         CISC_VM_CDR            VM_P0 VM_P0
                                         CISC_VM_GOTO           (two-complement-of -14)
                                         CISC_VM_RET            VM_P1)
                              "reverse"))

  (define/contract (r_make-list cells (i-result VM_NIL_CELL))
    (-> (listof cell?) cell-list-ptr?)
    (define (r_make-list-rev r-cells result)
      (if (empty? r-cells)
          result
          (r_make-list-rev (cdr r-cells) (cell-list-ptr (car r-cells) result))))
    (r_make-list-rev (reverse cells) i-result))

  (define r_reverse-test-vm
    (make-vm #:locals (pvector (cell)
                               (r_make-list (list (cell-byte 1) (cell-int 400) (cell-byte 3)))
                               VM_NIL_CELL)
             #:functions (pvector (function 0 (pvector CISC_VM_CALL VM_L0 1 0 2 VM_L1 VM_L2
                                                       CISC_VM_BRK)
                                            "call reverse")
                                  r_reverse)
             #:options (list)))

  (define r_reverse-test-vm-run (run-until-break r_reverse-test-vm))

  (check-equal? (get-local r_reverse-test-vm-run 0)
                (r_make-list (list (cell-byte 3) (cell-int 400) (cell-byte 1)))))

;; <byte-code> <case-src-reg> case-no case-value-0 <case-ptr> <case-value-1> <case-ptr> ... <case-value-n> <case-ptr>
(define/contract (interpret-case a-vm)
  (-> vm? vm?)
  (define source-cell-val (cell-value (get-cell a-vm (decode-cell-idx (get-current-byte-code a-vm 1)))))
  (define case-no (get-current-byte-code a-vm 2))
  (define case-found
    (findf (lambda (pair) (eq? (car pair) source-cell-val))
           (map (lambda (idx) (cons (get-current-byte-code a-vm (fx+ 3 (* 3 idx)))
                               (fx+  (get-current-byte-code a-vm (fx+ 4 (* 3 idx)) )
                                     (arithmetic-shift (get-current-byte-code a-vm (fx+ 5 (* 3 idx))) 8))))
                (range case-no))))
  (if case-found
      (set-byte-code-idx a-vm (cdr case-found))
      (increment-program-counter a-vm (fx+ 3 (* 3 case-no)))))

(module+ test #| interpret-case |#
  (define (case-test-vm check-val)
    (make-vm
     #:locals (pvector (cell-byte check-val))
     #:functions
     (pvector
      (function 1 (pvector CISC_VM_CASE VM_L0 3
                           1  #x01 #x01
                           15 #x20 #x01
                           20 #x00 #xff)
                "case-fun"))))

  (check-equal? (continuation-byte-code-idx
                 (frame-continuation
                  (vm-frame
                   (interpret-case (case-test-vm 15)))))
                #x0120)

  (check-equal? (continuation-byte-code-idx
                 (frame-continuation
                  (vm-frame
                   (interpret-case (case-test-vm 1)))))
                #x0101)

  (check-equal? (continuation-byte-code-idx
                 (frame-continuation
                  (vm-frame
                   (interpret-case (case-test-vm 20)))))
                #xFF00)

  (check-equal? (continuation-byte-code-idx
                 (frame-continuation
                  (vm-frame
                   (interpret-case (case-test-vm 30)))))
                12))

;; no idea how to handle this yet (e.g. register function to be called, have exception-handle-frames and unwind up to them)
(define/contract (interpret-throw-exception a-vm)
  (-> vm? vm?)
  (define error-string (get-cell a-vm (decode-cell-idx (get-current-byte-code a-vm 1))))
  (define param-no (get-current-byte-code a-vm 2))
  (define param-cells (get-next-decoded-cells a-vm param-no 3))
  (writeln (format "EXCEPTION with error: ~a params: ~a" error-string (string-join (flatten (map write-data param-cells)))))
  (increment-program-counter
   a-vm
   (fx+ 3 param-no)))

(module+ test #| throw exceptin |#
  (skip "throw exception does just some output currently (implementation incomplete)"
        (interpret-throw-exception
         (make-vm
          #:locals (pvector (cell-ptr (cell-array 5 (string->pvector "error")))
                            (cell-byte 1)
                            (cell-byte 10))
          #:functions (pvector (function 0 (pvector CISC_VM_THROW VM_L0 2 VM_L1 VM_L2)
                                         "fun"))))))


;; MEMORY TRACKING ---

;; allocate memory for a structure and write reference into local cell
;; (define (allocate-struct-into))

;; allocate memory for an array and write reference into local cell
;; (define (allocate-array-into))

;; allocate memory for a map and write reference into local cell
;; (define (allocate-map-into))

;; allocate cell-pair for list writing the list pointer into a local cell
;; (define (allocate-cell-pair-ref-into))

;; allocate single cell
;; (define (allocate-cell-ref-into))

;; allocate memory for float and write reference into local cell
;; (define (allocate-float-into))

;; free allocated memory pointed to by ref (could be a cell, a struct, ...)
;; first iteration (gc + reference counting is be part of another iteration)
;; (define (free-ref))


(define/contract (interpret-command a-vm)
  (-> vm? vm?)
  (define bc (get-current-byte-code a-vm))
  (when (member 'trace (vm-options a-vm))
    (define cur-frame (frame-continuation (vm-frame a-vm)))
    (displayln (format "exec: bc: ~a  @function: ~a, byte-offset: ~a" bc
                       (continuation-func-idx cur-frame)
                       (continuation-byte-code-idx cur-frame))))
  (cond
    [(= bc CISC_VM_BRK)             (raise-user-error "brk command encountered")]
    [(= bc CISC_VM_BYTE_ADD)        (interpret-byte+ a-vm)]
    [(= bc CISC_VM_CALL)            (interpret-call a-vm)]
    [(= bc CISC_VM_RET)             (interpret-return a-vm)]
    [(= bc CISC_VM_IMMB)            (interpret-load-immediate-byte a-vm)]
    [(= bc CISC_VM_MAKE_LIST)       (interpret-make-list a-vm)]
    [(= bc CISC_VM_BRA_EMPTY_LIST)  (interpret-bra-emtpy-list a-vm)]
    [(= bc CISC_VM_CAR)             (interpret-car a-vm)]
    [(= bc CISC_VM_CDR)             (interpret-cdr a-vm)]
    [(= bc CISC_VM_GOTO)            (interpret-goto a-vm)]
    [(= bc CISC_VM_ARRAY_CREATE)    (interpret-array-create a-vm)]
    [(= bc CISC_VM_ARRAY_GET)       (interpret-array-get a-vm)]
    [(= bc CISC_VM_ARRAY_SET)       (interpret-array-set a-vm)]
    [(= bc CISC_VM_STRUCT_CREATE)   (interpret-struct-create a-vm)] ;; creates an array of predefined "structure"
    [(= bc CISC_VM_CONS)            (interpret-cons a-vm)]
    [(= bc CISC_VM_INT_ADD)         (interpret-int-add a-vm)]
    [(= bc CISC_VM_INT_INC)         (interpret-int-inc a-vm)]
    [(= bc CISC_VM_MAKE_INT)        (interpret-make-int a-vm)]
    [(= bc CISC_VM_CASE)            (interpret-case a-vm)]
    [(= bc CISC_VM_THROW)           (interpret-throw-exception a-vm)]
    [else (raise-user-error (format "unknown byte command ~a" (get-current-byte-code a-vm)))]))

(module+ test #| interpret command |#
  (define vm-before-interpretation (set-function local1params1globals2 0 add-function))
  (define vm-after-interpretation (interpret-command vm-before-interpretation))

  (check-equal? (frame-continuation (vm-frame vm-after-interpretation))
                (continuation 4 0)
                "program counter is increased by 4")
  (check-equal? (get-local vm-after-interpretation 0)
                (cell-byte 7)))

(define/contract (run-until-break a-vm)
  (-> vm? vm?)
  (cond [(fx= 0 (get-current-byte-code a-vm)) a-vm]
        [else
         (define next-vm (interpret-command a-vm))
         (run-until-break next-vm)]))

(module+ test #| run-until-break |#
  (define vmBeforeRun
    (make-vm #:functions
             (pvector (function 3 (pvector CISC_VM_IMMB VM_L0 1
                                           CISC_VM_IMMB VM_L1 2
                                           CISC_VM_CALL VM_L2 1 0 2 VM_L0 VM_L1
                                           CISC_VM_BRK) "f0: call add, brk")
                      (function 1 (pvector CISC_VM_BYTE_ADD VM_L0 VM_P0 VM_P1
                                           CISC_VM_RET   VM_L0) "just add and return"))
             #:locals (pvector (cell-byte 1) (cell-byte 2) (cell))))
  (define vmAfterRun (run-until-break vmBeforeRun))

  (check-equal? (get-local vmAfterRun 2)
                (cell-byte 3))

  (check-equal? (frame-continuation (vm-frame vmAfterRun))
                (continuation 13 0)))

(define/contract (write-data a-cell (strings '()))
  (->* [cell?] [(listof string?)] (listof string?))
  (cond
    [(cell-byte? a-cell) (cons (format "byte: ~a" (cell-byte-value a-cell)) strings)]
    [(cell-ptr? a-cell)  (cons "->" (write-data (cell-ptr-ref a-cell) strings))]
    [(cell-nil? a-cell) (cons "nil" strings)]
    [(cell-list-ptr? a-cell) (cons "("
                                   (write-data (cell-list-ptr-car a-cell)
                                               (cons " . "
                                                     (write-data (cell-list-ptr-cdr a-cell)
                                                                 (cons ")" strings)))))]
    [else (raise-user-error (format "unknown cell type ~a" a-cell))]))

(module+ test #| write-data |#
  (check-equal? (string-join (write-data (cell-byte 20)) "")
                "byte: 20")
  (check-equal? (string-join (write-data (cell-list-ptr (cell-byte 20) (cell-list-ptr (cell-byte 30) VM_NIL_CELL))) "")
                "(byte: 20 . (byte: 30 . nil))")
  (check-equal? (string-join (write-data VM_NIL_CELL) "")
                "nil")
  (check-equal? (string-join (write-data (cell-ptr (cell-byte 10))) "")
                "->byte: 10"))

(define/contract (cell-idx->string a-cell-idx)
  (-> cell-idx? string?)
  (define loc (cell-idx-location a-cell-idx))
  (format "~a~a"
          (cond [(eq? loc l-local) "l"]
                [(eq? loc l-param) "p"]
                [(eq? loc l-global) "g"]
                [(eq? loc l-imm) " "]
                [else (raise-user-error (format "unknown cell-idx location ~a") loc)])
          (cell-idx-index a-cell-idx)))

(define/contract (reg-idx->str encoded-reg)
  (-> byte? string?)
  (define decoded-reg (decode-cell-idx encoded-reg))
  (cell-idx->string decoded-reg))

(define/contract (reg->str@ bytes idx)
  (-> (listof byte?) exact-nonnegative-integer? string?)
  (define reg (nth bytes idx))
  (reg-idx->str reg))

(define/contract (disassemble byte-codes a-vm (strings (list)))
  (->* [(listof byte?) vm?] [(listof string?)] (listof string?))
  (cond [(empty? byte-codes) strings]
        [else
         (define opcode (car byte-codes))
         (define-values (add-strings bytes-consumed)
           (cond
             [(eq? opcode CISC_VM_BRK)            (values (list "brk") 1)]
             [(eq? opcode CISC_VM_BYTE_ADD)       (values (list (format "byte+ ~a + ~a -> ~a" (reg->str@ byte-codes 2)(reg->str@ byte-codes 3)(reg->str@ byte-codes 1))) 4)] ;; 4          <byte-code> <target-reg> <src-a-reg> <src-b-reg>
             [(eq? opcode CISC_VM_CALL)
              (define param-count (nth byte-codes 4))
              (values (list (format "call ~a (~a) -> ~a"
                                    (2-bytes->int (nth byte-codes 2) (nth byte-codes 3))
                                    (string-join (map (lambda (idx) (reg->str@ byte-codes (+ idx 5))) (range param-count)) " ")
                                    (reg->str@ byte-codes 1))) (+ 5 param-count))] ;; 5+paramc   <byte-code> <target-reg> <func-idx-low> <func-idx-high> param-count <param1-reg> <param2-reg> ... <paramn-reg>
             [(eq? opcode CISC_VM_RET)            (values (list (format "ret ~a" (reg->str@ byte-codes 1))) 2)] ;; 2          <byte-code> <result-reg>
             [(eq? opcode CISC_VM_IMMB)           (values (list (format "immb ~a -> ~a" (nth byte-codes 2) (reg->str@ byte-codes 1))) 3)] ;; 3          <byte-code> <target-reg> value
             [(eq? opcode CISC_VM_MAKE_LIST)
              (define list-count (nth byte-codes 2))
              (values (list (format "make_list (~a) -> ~a"
                                    (string-join (map (lambda (idx) (reg->str@ byte-codes (+ idx 3))) (range list-count)) " ")
                                    (reg->str@ byte-codes 1))) (+ 3 list-count))] ;; 3+elemc    <byte-code> <target-reg> element-count <elem1-reg> <elem2-reg> ... <elemn-reg>
             [(eq? opcode CISC_VM_BRA_EMPTY_LIST) (values (list (format "bra_empty_list ~a? -> ~a" (reg->str@ byte-codes 1) (decimal-from-two-complement (nth byte-codes 2)))) 3)] ;; 3          <byte-code> <list-tested-reg> offset
             [(eq? opcode CISC_VM_CAR)            (values (list (format "car ~a -> ~a" (reg->str@ byte-codes 2) (reg->str@ byte-codes 1))) 3)] ;; 3          <byte-code> <target-reg> <src-list-reg>
             [(eq? opcode CISC_VM_CDR)            (values (list (format "cdr ~a -> ~a" (reg->str@ byte-codes 2) (reg->str@ byte-codes 1))) 3)] ;; 3          <byte-code> <target-reg> <src-list-reg>
             [(eq? opcode CISC_VM_GOTO)           (values (list (format "goto -> ~a" (decimal-from-two-complement (nth byte-codes 1)))) 2)] ;; 2          <byte-code> offset
             [(eq? opcode CISC_VM_STRUCT_CREATE)
              (define struct-idx (nth byte-codes 2))
              (define struct-def  (nth (vm-structs a-vm) struct-idx))
              (define field-num (structure-field-num struct-def))
              (define struct-name (structure-name struct-def))
              (values (list (format "struct-create ~a { ~a } -> ~a" struct-name (string-join (map (lambda (idx) (format "~a: ~a" (structure-field struct-def idx) (reg->str@ byte-codes (+ idx 3)))) (range  field-num) ) ", ") (reg->str@ byte-codes 1))) (+ 3 field-num))] ;; 3+fieldc   <byte-code> <target-reg> <struct-def-idx> <field-1-reg> <field-2-reg> ... <field-n-reg>
             [(eq? opcode CISC_VM_ARRAY_CREATE)   (values '("(i) array_create") 4)] ;; 4          <byte-code> <target-reg> <length-reg> <default-val-reg>
             [(eq? opcode CISC_VM_ARRAY_GET)      (values '("(i) array_get") 4)] ;; 4          <byte-code> <target-reg> <array-reg> <idx-reg>
             [(eq? opcode CISC_VM_ARRAY_SET)      (values '("(i) array_set") 4)] ;; 4          <byte-code> <array-reg> <idx-reg> <value-reg>
             [(eq? opcode CISC_VM_PEEK_LBYTE)     (values '("(i) peek_lbyte") 3)] ;; 3          <byte-code> <target-reg> <cell-ptr-reg>
             [(eq? opcode CISC_VM_PEEK_HBYTE)     (values '("(i) peek_hbyte") 3)] ;; 3          <byte-code> <target-reg> <cell-ptr-reg
             [(eq? opcode CISC_VM_POKE_LBYTE)     (values '("(i) poke_lbyte") 3)] ;; 3          <byte-code> <cell-ptr-reg> <value-reg>
             [(eq? opcode CISC_VM_POKE_HBYTE)     (values '("(i) poke_hbyte") 3)] ;; 3          <byte-code> <cell-ptr-reg> <value-reg>
             [(eq? opcode CISC_VM_CONS)           (values (list (format "cons ~a ~a -> ~a" (reg->str@ byte-codes 2)(reg->str@ byte-codes 3)(reg->str@ byte-codes 1))) 4)] ;; 4          <byte-code> <target-reg> <value-reg> <list-ptr/val-reg>
             [(eq? opcode CISC_VM_MAKE_INT)       (values (list (format "make_int ~a + ~a<<8 -> ~a" (reg->str@ byte-codes 2) (reg->str@ byte-codes 3) (reg->str@ byte-codes 1))) 4)] ;; 4          <byte-code> <target-reg> <low-byte-reg> <high-byte-reg>
             [(eq? opcode CISC_VM_IMMI)           (values (list (format "immi ~a -> ~a" (2-bytes->int (nth byte-codes 2) (nth byte-codes 3)) (reg->str@ byte-codes 1))) 4)] ;; 4          <byte-code> <target-reg> low-value high-value
             [(eq? opcode CISC_VM_INT_ADD)        (values '("(i) int_add") 4)] ;; 4          <byte-code> <target-reg> <int-a-reg> <int-b-reg>
             [(eq? opcode CISC_VM_INT_INC)        (values '("(i) int_inc") 2)] ;; 2          <byte-code> <target-reg>
             [(eq? opcode CISC_VM_CASE)           (values '("(i) case") (+ 3 (* 2 (nth byte-codes 3))))] ;; 3+casen*3  <byte-code> <target-reg> <case-src-reg> case-no case-byte-value-0 <new-byte-idx> case-byte-value-1 <new-byte-idx> ... case-byte-value-n <new-byte-idx>
             [(eq? opcode CISC_VM_THROW)          (values '("(i) throw") (+ 3 (nth byte-codes 2)))] ;; 3+paramn   <byte-code> <exception-str> <param-no> <param-1> <param-2> ... <param-n>
             [(eq? opcode CISC_VM_BRA)            (values (list (format "bra ~a? -> ~a" (reg->str@ byte-codes 1) (decimal-from-two-complement (nth byte-codes 2)))) 3)] ;; 3          <byte-code> <bool-tested-reg> offset
             [(eq? opcode CISC_VM_BRA_NOT)        (values (list (format "bra not ~a? -> ~a" (reg->str@ byte-codes 1) (decimal-from-two-complement (nth byte-codes 2)))) 3)] ;; 3          <byte-code> <bool-tested-reg> offset
             [(eq? opcode CISC_VM_MOVE)           (values (list (format "move ~a -> ~a" (reg->str@ byte-codes 2)(reg->str@ byte-codes 1))) 3)] ;; 3          <byte-code> <target-reg> <source-reg>
             [(eq? opcode CISC_VM_NIL_P)          (values (list (format "nil? ~a -> ~a" (reg->str@ byte-codes 2)(reg->str@ byte-codes 1))) 3)] ;; 3          <byte-code> <target-reg> <list-reg>
             [(eq? opcode CISC_VM_NOT)            (values (list (format "not! ~a -> ~a" (reg->str@ byte-codes 2)(reg->str@ byte-codes 1))) 3)] ;; 3          <byte-code> <target-reg> <bool-reg>
             [else (raise-user-error (format "unknown opcode ~a" opcode))]))
         (disassemble (drop byte-codes bytes-consumed) a-vm (append strings add-strings))]))

(module+ test
  (check-equal? (disassemble (list CISC_VM_BRK
                                   CISC_VM_BYTE_ADD VM_L0 VM_P1 VM_G1
                                   CISC_VM_CALL VM_L0 2 1 2 VM_L1 VM_P1
                                   CISC_VM_IMMB VM_L0 212
                                   CISC_VM_MAKE_LIST VM_L1 2 VM_P1 VM_P2
                                   CISC_VM_RET VM_L0
                                   CISC_VM_BRA_EMPTY_LIST VM_L0 (two-complement-of -12)
                                   CISC_VM_CAR VM_L0 VM_P1
                                   CISC_VM_CDR VM_L0 VM_P1
                                   CISC_VM_GOTO (two-complement-of 5)
                                   CISC_VM_CONS VM_L0 VM_P1 VM_L1
                                   CISC_VM_MAKE_INT VM_L0 VM_P0 VM_P1
                                   CISC_VM_IMMI VM_L0 1 2
                                   CISC_VM_MOVE VM_L0 VM_L1
                                   CISC_VM_STRUCT_CREATE VM_L0 0 VM_L1 VM_P1 VM_P0
                                   CISC_VM_NIL_P VM_L0 VM_L1
                                   CISC_VM_NOT VM_L0 VM_L1
                                   CISC_VM_BRA VM_L0 (two-complement-of -4)
                                   CISC_VM_BRA_NOT VM_L0 (two-complement-of -4)
                                   )
                             (make-vm #:structs (pvector (make-structure "point" (list "x" "y" "z")))))
                (list "brk"
                      "byte+ p1 + g1 -> l0"
                      "call 258 (l1 p1) -> l0"
                      "immb 212 -> l0"
                      "make_list (p1 p2) -> l1"
                      "ret l0"
                      "bra_empty_list l0? -> -12"
                      "car p1 -> l0"
                      "cdr p1 -> l0"
                      "goto -> 5"
                      "cons p1 l1 -> l0"
                      "make_int p0 + p1<<8 -> l0"
                      "immi 513 -> l0"
                      "move l1 -> l0"
                      "struct-create point { x: l1, y: p1, z: p0 } -> l0"
                      "nil? l1 -> l0"
                      "not! l1 -> l0"
                      "bra l0? -> -4"
                      "bra not l0? -> -4"
                      )))
