#lang typed/racket

(require (only-in racket/fixnum fx+ fx= fx< fx- fx>))
(require/typed racket/kernel [vector-set/copy (All (a) (-> (Immutable-Vectorof a) Nonnegative-Integer a (Vectorof a)))])

(module+ test #| require test utils |#
  (require typed/rackunit))

;; 16 bit element
(struct cell- () #:transparent)

;; arbitrary number of cells
(struct celln- cell- ()
  #:transparent)

(struct atomic-cell- cell- ()
  #:transparent)

;; pointer to a cell (
(struct cell-ptr- atomic-cell-
  ([ref : cell-])
  #:transparent)

;; pointer to a pair, which is 2 cells memory adjacent, car (any cell) and cdr cell (usually a cell-list-ptr)
(struct cell-list-ptr- atomic-cell-
  ([car : cell-]
   [cdr : cell-])
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
    [else (raise-user-error (format "not a value cell ~a" a-cell))]))

(struct cell-array- celln-
  ([size : Integer]
   [array : (Vectorof cell-)])
  #:transparent) ;; vector of atomic cells!

(struct vm-frame-
  ([fun-idx : Nonnegative-Integer]
   [bc-idx  : Nonnegative-Integer]
   [parameter-tail : (Listof cell-)]  ;; is a pointer to the last parameter on the value stack, at the beginning of a call this is identical to value-stack!
   [locals : (Immutable-Vectorof cell-)]        ;; a list of locals identified by index
   )
  #:transparent)

(define (make-frame #:fun-idx        (fun-idx : Nonnegative-Integer 0)
                    #:bc-idx         (bc-idx : Nonnegative-Integer 0)
                    #:parameter-tail (parameter-tail : (Listof cell-) '())
                    #:locals         (locals : (Immutable-Vectorof cell-) (vector-immutable)))
        : vm-frame-
  (vm-frame- fun-idx bc-idx parameter-tail locals))

(struct vm-struct-def-
  ([name : String]
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

(struct vm-
  ([frame-stack : (Listof vm-frame-)]
   [value-stack : (Listof cell-)]
   [functions : (Immutable-Vectorof vm-function-def-)]
   [globals : (Immutable-Vectorof cell-)]
   [structs : (Immutable-Vectorof vm-struct-def-)]
   [options : (Listof Symbol)])
  #:transparent)

(define (make-vm
         #:frame-stack (frame-stack : (Listof vm-frame-) (list (make-frame)))
         #:value-stack (value-stack : (Listof cell-) '())
         #:functions (functions : (Immutable-Vectorof vm-function-def-) (vector-immutable))
         #:globals (globals : (Immutable-Vectorof cell-) (vector-immutable))
         #:structs (structs : (Immutable-Vectorof vm-struct-def-) (vector-immutable))
         #:options (options : (Listof Symbol) '()))
        : vm-
  (vm- frame-stack value-stack functions globals structs options))

(define BRK                  0) ;;

(define PUSH_BYTE            5) ;; op = byte value, stack [] -> [cell-byte]
(define PUSH_INT             6) ;; op1=low byte op2=high byte, stack [] -> [cell-int]
;; also used for struct-index or function-index
(define PUSH_PARAM          10) ;; op = param-idx from tail, stack [] -> [cell-]
(define PUSH_GLOBAL         11) ;; op1=low byte index op2=high byte index stack [] -> [cell-]
(define PUSH_LOCAL          12) ;; op = local-idx, stacl [] -> [cell-]

;; example of short (one byte instruction) for push
(define sPUSH_PARAM         #b10000000) ;; short push param, lower 2 bits
(define sPUSH_PARAMm        #b11111100)
(define sPUSH_PARAMn        #b00000011)
(define sPUSH_GLOBAL        #b10000100) ;; short push global, lower 2 bits + next byte
(define sPUSH_GLOBALm       #b11111100)
(define sPUSH_GLOBALn       #b00000011)
(define sPUSH_LOCAL         #b10001000) ;; short push local, lower 2 bits
(define sPUSH_LOCALm        #b11111100)
(define sPUSH_BYTE          #b10001100) ;; short push byte, lower 2 bits  #b00 = #x00, #b01 = #x01, #b10 = #x02, #b11 = #xff
(define sPUSH_BYTEm         #b11111100)

(define POP_TO_PARAM        15) ;; op= param-idx from tail, stack [cell-] -> []
(define POP_TO_GLOBAL       16) ;; op1=low byte index op2=high byte index, stack [cell-] -> []
(define POP_TO_LOCAL        17) ;; op = local-idx, stacl [cell-] -> []

(define sPOP_TO_PARAM         #b10010000) ;; short pop to param, lower 2 bits
(define sPOP_TO_PARAMm        #b11111100)
(define sPOP_TO_PARAMn        #b00000011)
(define sPOP_TO_GLOBAL        #b10010100) ;; short pop to global, lower 2 bits + next byte
(define sPOP_TO_GLOBALm       #b11111100)
(define sPOP_TO_GLOBALn       #b00000011)
(define sPOP_TO_LOCAL         #b10011000) ;; short pop to local, lower 2 bits
(define sPOP_TO_LOCALm        #b11111100)

(define NIL?                20) ;; stack [cell-list-ptr] -> [cell-boolean]

(define BRA                 31) ;; op = relative offset
(define GOTO                32) ;; op = relative offset
(define RET                 33) ;; stack [cell paramN, ... cell param1, cell param0] -> []
(define CALL                34) ;; stack [int-cell: function index, cell paramN, ... cell param1, cell param0] -> [cell paramN, ... cell param1, cell param0]
(define TAIL_CALL           35) ;; stack [new-paramN .. new-param0, ..., original-paramN ... original-param0] -> [new-paramN .. new-param0]

(define sBRA                #b11000000)
(define sBRAm               #b11100000)
(define sBRAn               #b00011111)
(define sBRAmsb             5)
(define sGOTO               #b11100000)
(define sGOTOm              #b11100000)
(define sGOTOn              #b00011111)
(define sGOTOmsb            5)

(define CAR                 40) ;; stack [cell-list-ptr] -> [cell- car of list pointed at]
(define CDR                 41) ;; stack [cell-list-ptr] -> [cell-list-ptr cdr of list pointed at]
(define CONS                42) ;; stack [cell- car, cell-list-ptr cdr] -> stack [cell-list-ptr new-list]

(define BYTE+               60) ;; stack [cell-byte a, cell-byte b] -> [sum]

(define (integer->two-complement [value : Integer]) : Nonnegative-Integer
  (cond
    [(fx< value 0)
     (define new-val (fx+ #x10000 value))
     (if (fx< new-val #x8000)
         (raise-user-error "integer out of range ~a" value)
         new-val)]
    [(fx< value #x8000) value]
    [else (raise-user-error "integer out of range ~a" value)]))

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

(define (tos-value [vm : vm-] [offset : Byte 0]) : cell-
  (car (drop (vm--value-stack vm) offset)))

(define (pop-value [vm : vm-] [count : Byte 1]) : vm-
  (struct-copy vm- vm [value-stack (drop (vm--value-stack vm) count)]))

;; inner function for tail call recursion required, because recursive typed call w/ optional field does not typecheck
(define (pop-and-get-values [vm : vm-] [count : Nonnegative-Integer]) : (List (Listof cell-) vm-)
  (define (pop-values-[vm : vm-] [count : Nonnegative-Integer] [acc : (Listof cell-)]) : (List (Listof cell-) vm-)
    (cond
      [(fx< 0 count)
       (pop-values- (ann (pop-value vm) vm-) (ann (sub1 count) Nonnegative-Integer) (ann (cons (tos-value vm) acc) (Listof cell-)))]
      [else (list acc vm)]))
  (pop-values- vm count '()))

(define (push-value [vm : vm-] [value : cell-]) : vm-
  (struct-copy vm- vm [value-stack (cons value (vm--value-stack vm))]))

;; ensure overflow is caught to enable type checking result to be Byte
(define (byte+ [a : Byte] [b : Byte]) : Byte
  (define result (fx+ a b))
  (if (byte? result)
      result
      (raise-user-error "byte+ overflow ~a + ~a" a b)))

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
                                #:locals (vector->immutable-vector (make-vector locals-count (cell-)))))
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
    (interpret-ret (make-vm #:value-stack (list (cell-byte- 10) (cell-) (cell-))
                            #:frame-stack (list (make-frame #:fun-idx 0 #:bc-idx 0 #:parameter-tail (list (cell-) (cell-))))
                            #:functions (vector-immutable (make-function-def #:parameter-count 2)))))

  (check-equal? (vm--value-stack interpret-ret--vm)
                (list (cell-byte- 10))
                "dropping 2 parameters from value stack should keep tos as value returned")
  (check-equal? (vm--frame-stack interpret-ret--vm)
                (list)
                "returning from the only function on the stack should leave an empty frame stack")

  (define interpret-ret--vm2
    (interpret-ret (make-vm #:value-stack (list (cell-byte- 10) (cell-) (cell-) (cell-byte- 20))
                            #:frame-stack (list (make-frame #:fun-idx 0 #:bc-idx 0 #:parameter-tail (list (cell-) (cell-) (cell-byte- 20)))
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
      (raise-user-error "byte+ encountered non byte in ~a or ~a" a b)))

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
              #:frame-stack (list (make-frame #:parameter-tail (list (cell-) (cell-) (cell-int- 10) (cell-)))))))

  (check-equal? (car (vm--value-stack test_interpret-push-param))
                (cell-int- 10)
                "the pushed value is ten (which was the 2nd parameter in the parameter-tail list")

  (check-equal? (vm-frame--bc-idx (car (vm--frame-stack test_interpret-push-param)))
                2
                "the program counter got incremented by 2 (push + operand)"))

;; bytecode: op val:byte, len: 2b
;; stack: [ ... pN .. p1 p0] -> [ val:cell-byte- ... pN .. p1 p0], growth: 1c
(define (interpret-push-byte [vm : vm-]) : vm-
  (define byte-value (peek-pc-byte vm 1))
  (increment-pc (push-value vm (cell-byte- byte-value)) 2))

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
    (list (cell-byte- 10) (cell-) #| params tail is here |# (cell-) (cell-) (cell-byte- 9) (cell-)))
  (define test_param_2-interpret-pop-to-param
    (interpret-pop-to-param
     (make-vm #:functions (vector-immutable (make-function-def #:byte-code (vector-immutable 0 2 0 0)))
              #:frame-stack (list (make-frame #:parameter-tail (cddr test_param_2-values-interpret-pop-to-param)))
              #:value-stack test_param_2-values-interpret-pop-to-param)))

  (check-equal? (vm-frame--parameter-tail (car (vm--frame-stack test_param_2-interpret-pop-to-param)))
                (list (cell-) (cell-) (cell-byte- 10) (cell-))
                "replace exactly the 2nd parameter (from tail)")

  (check-equal? (vm--value-stack test_param_2-interpret-pop-to-param)
                (list (cell-) #| params tail is here |# (cell-) (cell-) (cell-byte- 10) (cell-))
                "the value stack should contain the parameter tail (and have the 2nd parameter replaced)")

  (define test_param_0-values-interpret-pop-to-param
    (list (cell-byte- 10) #| params tail is here |# (cell-byte- 9) (cell-) (cell-) (cell-)))
  (define test_param_0-interpret-pop-to-param
    (interpret-pop-to-param
     (make-vm #:functions (vector-immutable (make-function-def #:byte-code (vector-immutable 0 0 0 0)))
              #:frame-stack (list (make-frame #:parameter-tail (cdr test_param_0-values-interpret-pop-to-param)))
              #:value-stack test_param_0-values-interpret-pop-to-param)))

  (check-equal? (vm-frame--parameter-tail (car (vm--frame-stack test_param_0-interpret-pop-to-param)))
                (list (cell-byte- 10) (cell-) (cell-) (cell-))
                "replace the 1st parameter (from tail)")

  (check-equal? (vm--value-stack test_param_0-interpret-pop-to-param)
                (list #| params tail is here |# (cell-byte- 10) (cell-) (cell-) (cell-))
                "replace 1st parameter being the tos after this pop!")

  (define test_param_4-values-interpret-pop-to-param
    (list (cell-byte- 10) (cell-) #| params tail is here |# (cell-) (cell-) (cell-) (cell-) (cell-byte- 9)))
  (define test_param_4-interpret-pop-to-param
    (interpret-pop-to-param
     (make-vm #:functions (vector-immutable (make-function-def #:byte-code (vector-immutable 0 4 0 0)))
              #:frame-stack (list (make-frame #:parameter-tail (cddr test_param_4-values-interpret-pop-to-param)))
              #:value-stack test_param_4-values-interpret-pop-to-param)))

  (check-equal? (vm-frame--parameter-tail (car (vm--frame-stack test_param_4-interpret-pop-to-param)))
                (list (cell-) (cell-) (cell-) (cell-) (cell-byte- 10))
                "replace the last parameter (from tail = first param passed)")

  (check-equal? (vm--value-stack test_param_4-interpret-pop-to-param)
                (list (cell-) #| params tail is here |# (cell-) (cell-) (cell-) (cell-) (cell-byte- 10))
                "replace last parameter keeping rest of value stack"))

;; bytecode: op idx-low:byte idx-high:byte, len: 3b
;; stack: [ val ... ] -> [ ...], growth: -1c
;; global@IDX = val
(define (interpret-pop-to-global [vm : vm-]) : vm-
  (define global-idx (peek-pc-int vm 1))
  (increment-pc
   (struct-copy vm- vm
                [value-stack (cdr (vm--value-stack vm))]
                [globals (vector->immutable-vector
                          (vector-set/copy (vm--globals vm) global-idx (tos-value vm)))])
   3))

(module+ test #| interpret pop to global |#
  (define test-interpret-pop-to-global
    (interpret-pop-to-global
     (make-vm #:functions (vector-immutable (make-function-def #:byte-code (vector-immutable 0 2 0 0)))
              #:globals (vector-immutable (cell-)(cell-)(cell-byte- 9)(cell-))
              #:value-stack (list (cell-byte- 10) (cell-)))))
  (check-equal? (vm--globals test-interpret-pop-to-global)
                (vector-immutable (cell-)(cell-)(cell-byte- 10)(cell-))))

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
                       (raise-user-error "expected cell-list-ptr, got ~a" tos)))
  (increment-pc (push-value (pop-value vm) car-cell)))

;; bytecode: op, len: 1b
;; stack: [ cell-list-ptr ... ] -> [ cdr-of-list ... ], growth: 0c
(define (interpret-cdr [vm : vm-]) : vm-
  (define tos (tos-value vm))
  (define cdr-cell (if (cell-list-ptr-? tos)
                       (cell-list-ptr--cdr tos)
                       (raise-user-error "expected cell-list-ptr, got ~a" tos)))
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

(define (sPUSH_PARAMc [idx : Byte]) : Byte
  (when (fx> idx sPUSH_PARAMn)
    (raise-user-error "index out of bounds for short command (~a)" idx))
  (bitwise-xor sPUSH_PARAM idx))

(define (sPOP_TO_PARAMc [idx : Byte]) : Byte
  (when (fx> idx sPOP_TO_PARAMn)
    (raise-user-error "index out of bounds for short command (~a)" idx))
  (bitwise-xor sPOP_TO_PARAM idx))

(define (sBRAc [to : Fixnum]) : Byte
  (define to- (byte->two-complement to sBRAmsb))
  (when (fx> to- sBRAn)
    (raise-user-error "jump target out of bounds for short command (~a ~a)" to to-))
  (bitwise-xor sBRA to-))

(define (sGOTOc [to : Fixnum]) : Byte
  (define to- (byte->two-complement to sGOTOmsb))
  (when (fx> to- sGOTOn)
    (raise-user-error "jump target out of bounds for short command (~a ~a)" to to-))
  (bitwise-xor sGOTO to-))

(define (dissassemble-byte-code (vm : vm-)) : String
  (define byte-code (peek-pc-byte vm))
  (cond
    [(= byte-code BRA) (format "bra ~a" (two-complement->signed-byte (peek-pc-byte vm 1)))]
    [(= byte-code BRK) "brk"]
    [(= byte-code BYTE+) "byte+"]
    [(= byte-code CALL) "call"]
    [(= byte-code CAR) "car"]
    [(= byte-code CDR) "cdr"]
    [(= byte-code CONS) "cons"]
    [(= byte-code GOTO) (format "goto ~a" (two-complement->signed-byte (peek-pc-byte vm 1)))]
    [(= byte-code NIL?) "nil?"]
    [(= byte-code POP_TO_GLOBAL) (format "popp g-~a" (fx+ (peek-pc-byte vm 1) (arithmetic-shift (peek-pc-byte vm 2) 8)))]
    [(= byte-code POP_TO_PARAM) (format "popp p-~a" (peek-pc-byte vm 1))]
    [(= byte-code PUSH_BYTE) (format "pushb #~a" (peek-pc-byte vm 1))]
    [(= byte-code PUSH_GLOBAL) (format "pushg g-~a" (fx+ (peek-pc-byte vm 1) (arithmetic-shift (peek-pc-byte vm 2) 8)))]
    [(= byte-code PUSH_INT) (format "pushi #~a" (fx+ (peek-pc-byte vm 1) (arithmetic-shift (peek-pc-byte vm 2) 8)))]
    [(= byte-code PUSH_PARAM) (format "pushp p-~a" (peek-pc-byte vm 1))]
    [(= byte-code RET) "ret"]
    [(= byte-code TAIL_CALL) "tail-call"]

    [(= (bitwise-and byte-code sPUSH_PARAMm) sPUSH_PARAM) (format "pushp p-~a  ;; short version" (bitwise-and sPUSH_PARAMn byte-code))]
    [(= (bitwise-and byte-code sPOP_TO_PARAMm) sPOP_TO_PARAM) (format "pop p-~a  ;; short version" (bitwise-and sPOP_TO_PARAMn byte-code))]
    [(= (bitwise-and byte-code sBRAm) sBRA) (format "bra ~a  ;; short version" (two-complement->signed-byte (bitwise-and sBRAn byte-code) sBRAmsb))]
    [(= (bitwise-and byte-code sGOTOm) sGOTO) (format "goto ~a  ;; short version" (two-complement->signed-byte (bitwise-and sGOTOn byte-code) sGOTOmsb))]

    [else (raise-user-error (format "unknown byte code command during disassembly ~a" byte-code))]))

(define (interpret-byte-code [vm : vm-]) : vm-
  (define byte-code (peek-pc-byte vm))
  (when (member 'trace (vm--options vm))
    (define cur-frame (car (vm--frame-stack vm)))
    (displayln (format "exec: (~a)\t ~a  \t@function: ~a, byte-offset: ~a, value-stack-size: ~a, tos: ~a" byte-code
                       (dissassemble-byte-code vm)
                       (vm-frame--fun-idx cur-frame)
                       (vm-frame--bc-idx cur-frame)
                       (length (vm--value-stack vm))
                       (if (empty? (vm--value-stack vm))
                           "nil"
                           (tos-value vm)))))
  (cond
    [(= byte-code BRA) (interpret-bra vm)]
    [(= byte-code BRK) (raise-user-error "encountered BRK")]
    [(= byte-code BYTE+) (interpret-byte+ vm)]
    [(= byte-code CALL) (interpret-call vm)]
    [(= byte-code CAR) (interpret-car vm)]
    [(= byte-code CDR) (interpret-cdr vm)]
    [(= byte-code CONS) (interpret-cons vm)]
    [(= byte-code GOTO) (interpret-goto vm)]
    [(= byte-code NIL?) (interpret-nil? vm)]
    [(= byte-code POP_TO_GLOBAL) (interpret-pop-to-global vm)]
    [(= byte-code POP_TO_PARAM) (interpret-pop-to-param vm)]
    [(= byte-code PUSH_BYTE) (interpret-push-byte vm)]
    [(= byte-code PUSH_GLOBAL) (interpret-push-global vm)]
    [(= byte-code PUSH_INT) (interpret-push-int vm)]
    [(= byte-code PUSH_PARAM) (interpret-push-param vm)]
    [(= byte-code RET) (interpret-ret vm)]
    [(= byte-code TAIL_CALL) (interpret-tail-call vm)]

    [(= (bitwise-and byte-code sPUSH_PARAMm) sPUSH_PARAM)
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
  (cond [(fx= BRK (peek-pc-byte vm)) vm]
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
                "the program counter should point to the break instruction")

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
                                      NIL?         ;;                 stack [nil?]
                                      (sBRAc 7)    ;; idea: combine nil check and branch command into one, how about nil?-ret combination <-
                                      (sPUSH_PARAMc 1)
                                      CDR          ;;                 stack [cdr new-acc]
                                      (sPUSH_PARAMc 1)
                                      CAR          ;; first value     stack [car]
                                      (sPUSH_PARAMc 0)
                                      BYTE+        ;;                 stack [new-acc]
                                      TAIL_CALL
                                      (sPUSH_PARAMc 0) ;; idea have a short return function, specifying what should be returned
                                      RET))))))

  (check-equal? (vm--value-stack test-tail-recursion--run-until-break)
                (list (cell-byte- 35))
                "tos of the value stack is the sum of all bytes in the list in 'test-tail-recursive--value-stack'")


  ;; (m-def (reverse (a-list (list cell)) (b-list (list cell) '()) -> (list cell)
  ;;                  "reverse a-list, consing it into b-list")
  ;;   (if (nil? a-list)
  ;;       b-list
  ;;     (reverse (cdr a-list) (cons (car a-list) b-list))))

  (define test-tail-recursion--value-stack2
    (list NIL_CELL
          (cell-list-ptr- (cell-byte- 5)
                          (cell-list-ptr- (cell-byte- 10)
                                          (cell-list-ptr- (cell-byte- 20)
                                                          NIL_CELL)))))

  (define test-tail-recursion--run-until-break2
    (run-until-break
     (make-vm
      #:options (list ) ;;  'trace
      #:value-stack  test-tail-recursion--value-stack2
      #:functions
      (vector-immutable
       (make-function-def
        #:byte-code (vector-immutable PUSH_INT   1 0 ;; function index 1
                                      CALL
                                      BRK))
       (make-function-def
        #:parameter-count 2 ;; param0 = accumulator, param1 = list of bytes
        #:byte-code (vector-immutable (sPUSH_PARAMc 1)
                                      NIL?         ;;
                                      (sBRAc 7)    ;;
                                      (sPUSH_PARAMc 1)
                                      CDR
                                      (sPUSH_PARAMc 0)
                                      (sPUSH_PARAMc 1)
                                      CAR          ;;
                                      CONS          ;;
                                      TAIL_CALL
                                      (sPUSH_PARAMc 0)
                                      RET))))))

  (check-equal? (vm--value-stack test-tail-recursion--run-until-break2)
                (list (cell-list-ptr- (cell-byte- 20)
                                      (cell-list-ptr- (cell-byte- 10)
                                                      (cell-list-ptr- (cell-byte- 5)
                                                                      NIL_CELL))))
                "tos is reversed list"))
