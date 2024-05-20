#lang racket

(require (only-in data/pvector pvector pvector? make-pvector build-pvector))
(require (only-in data/collection nth set-nth length))
(require (only-in racket/fixnum fx+ fx=))
(require (only-in threading ~>>))
(require (only-in "../6510-utils.rkt" two-complement-of decimal-from-two-complement))

(module+ test #| require test utils |#
  (require "../6510-test-utils.rkt"))

;; 16 bit element
(struct cell ()
  #:transparent)

;; pointer to a cell
(struct cell-ptr cell (ref)
  #:transparent
  #:guard
  (struct-guard/c cell?))

;; pointer to a list, which is 2 cells memory adjacent, car (any cell) and cdr cell (usually a cell-list-ptr)
(struct cell-list-ptr cell (car cdr)
  #:transparent
  #:guard
  (struct-guard/c cell? cell?))

(struct cell-nil cell-list-ptr ()
  #:transparent
  #:guard
  (struct-guard/c cell? cell?))

;; flat value 0..255 or -128..127 (used for byte, char and boolean)
(struct cell-byte cell (value)
  #:transparent
  #:guard
  (struct-guard/c byte?))

;; flat value 0..8191 or -4096...4095
(struct cell-int cell (value)
  #:transparent
  #:guard
  (struct-guard/c integer?))

;; meta data for arrays, structures, maps, always memory adjacent with its data (struct-data, array-data, map-data, float)
(struct cell-header cell ()
  #:transparent)

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
   options)
  #:transparent
  #:guard
  (struct-guard/c
   (listof frame?)
   pvector?
   frame?
   pvector?
   (listof symbol?)))

;; byte code
(define CISC_VM_BRK            0)
(define CISC_VM_BYTE_ADD       1)
(define CISC_VM_CALL           2)
(define CISC_VM_RET            3)
(define CISC_VM_IMMB           4)
(define CISC_VM_MAKE_LIST      5)
(define CISC_VM_BRA_EMPTY_LIST 6)
(define CISC_VM_CAR            7)
(define CISC_VM_CDR            8)
(define CISC_VM_GOTO           9)

;; reference to locals
(define VM_L0 0)
(define VM_L1 1)
(define VM_L2 2)
(define VM_L3 3)
(define VM_L4 4)
(define VM_L5 5)
(define VM_L6 6)

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

;; create a vm with the given locals (in the current execution context) and the given functions
;; it is set to execute the first function at byte-code-index 0
(define/contract (make-vm #:locals [locals (make-pvector 0 (cell))]
                          #:params [params (make-pvector 0 (cell))]
                          #:globals [globals (make-pvector 0 (cell))]
                          #:functions [functions (make-pvector 1 (function 0 (make-pvector 1 CISC_VM_BRK) "just-brk (0)"))]
                          #:options [options (list)])
  (->* [] [#:locals pvector? #:params pvector? #:globals pvector?  #:functions pvector? #:options (listof symbol?)] vm?)
  (vm '() functions (frame (continuation 0 0) 0 0 locals params 0) globals options))

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
   exact-nonnegative-integer? ;; 0..3, 0 = local, 1 = parameter, 2 = global
   ;; (or/c 'local 'global 'parameter)
   ))

(define/contract (decode-cell-idx encoded-idx)
  (-> byte? cell-idx?)
  (cell-idx
   (bitwise-and #b00111111 encoded-idx)
   (bitwise-bit-field encoded-idx 6 8)))

(module+ test #| decode-cell-idx |#
  (check-equal? (decode-cell-idx #b01000010)
                (cell-idx 2 1)))

(define/contract (get-cell a-vm a-cell-idx)
  (-> vm? cell-idx? cell?)
  (define idx (cell-idx-index a-cell-idx))
  (case (cell-idx-location a-cell-idx)
    ((0) (nth (frame-locals (vm-frame a-vm)) idx))
    ((1) (nth (frame-params (vm-frame a-vm)) idx))
    ((2) (nth (vm-globals a-vm) idx))
    (else (raise-user-error "unknown cell-index"))))

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
                                            CISC_VM_CALL VM_L2 1 0 2 VM_L0 VM_L1) "f0: call f1")
                       (function 1 (pvector CISC_VM_BYTE_ADD VM_L0 VM_P0 VM_P1
                                            CISC_VM_RET VM_L0) "just add"))
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

(define (construct-rev-cell-list elements (cur-head (cell-nil (cell) (cell))))
  (->* [(listof cell?)] [cell-list-ptr?] cell-list-ptr?)
  (if (empty? elements)
      cur-head
      (construct-rev-cell-list (cdr elements)
                               (cell-list-ptr (car elements) cur-head))))

(module+ test #| construct-rev-cell-list |#
  (check-equal? (construct-rev-cell-list (list (cell-byte 1) (cell-byte 2) (cell-byte 3)))
                (cell-list-ptr (cell-byte 3)
                               (cell-list-ptr (cell-byte 2)
                                              (cell-list-ptr (cell-byte 1)
                                                             (cell-nil (cell) (cell)))))))

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
                                 (cell-nil (cell) (cell))))))

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
      (make-vm #:locals (pvector (cell-nil (cell) (cell)))
               #:functions (pvector (function 1 (pvector CISC_VM_BRA_EMPTY_LIST VM_L0 (two-complement-of -2)) "some"))))))
   (continuation 0 0))
  (check-equal?
   (frame-continuation
    (vm-frame
     (interpret-bra-emtpy-list
      (make-vm #:locals (pvector (cell-nil (cell) (cell)))
               #:functions (pvector (function 1 (pvector CISC_VM_BRA_EMPTY_LIST VM_L0 (two-complement-of 1)) "some"))))))
   (continuation 3 0)))

(define (interpret-car a-vm)
  (-> vm? vm?)
  (increment-program-counter
   (set-cell a-vm (decode-cell-idx (get-current-byte-code a-vm 1))
             (cell-list-ptr-car (get-cell a-vm (decode-cell-idx (get-current-byte-code a-vm 2)))))
   3))

(module+ test #| interpret-car |#
  (check-equal?
   (get-local
    (interpret-car
     (make-vm #:locals (pvector (cell-list-ptr (cell-byte 1) (cell-list-ptr (cell-byte 2) (cell-nil (cell) (cell))))
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
     (make-vm #:locals (pvector (cell-list-ptr (cell-byte 1) (cell-list-ptr (cell-byte 2) (cell-nil (cell) (cell))))
                                (cell))
              #:functions (pvector (function 1 (pvector CISC_VM_CDR VM_L1 VM_L0) "car"))))
    1)
   (cell-list-ptr (cell-byte 2) (cell-nil (cell) (cell)))))

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
  (check-equal? (string-join (write-data (cell-list-ptr (cell-byte 20) (cell-list-ptr (cell-byte 30) (cell-nil (cell) (cell))))) "")
                "(byte: 20 . (byte: 30 . nil))")
  (check-equal? (string-join (write-data (cell-nil (cell) (cell))) "")
              "nil")
  (check-equal? (string-join (write-data (cell-ptr (cell-byte 10))) "")
              "->byte: 10"))

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
