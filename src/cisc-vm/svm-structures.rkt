#lang typed/racket

(require (only-in racket/fixnum fx+ fx= fx< fx-))
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

(define VM_NIL_CELL (cell-nil- (atomic-cell-) (atomic-cell-)))

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
   )
  #:transparent)

(define (make-frame #:fun-idx (fun-idx : Nonnegative-Integer 0)
                    #:bc-idx (bc-idx : Nonnegative-Integer 0)
                    #:parameter-tail (parameter-tail : (Listof cell-) '()))
  : vm-frame-
  (vm-frame- fun-idx bc-idx parameter-tail))

(struct vm-struct-def-
  ([name : String]
   [field-no : Nonnegative-Integer])
  #:transparent)

(struct vm-function-def-
  ([stack-size-used : Byte] ;; how much (in addition to the parameters) is the stack used (max)
   [parameter-count : Byte] ;; how many parameters are expected (fix)
   [name : String]
   [byte-code : (Immutable-Vectorof Byte)])
  #:transparent)

(define (make-function-def #:stack-size-used (stack-size-used : Byte 0)
                           #:parameter-count (parameter-count : Byte 0)
                           #:name (name : String "some-func")
                           #:byte-code (byte-code : (Immutable-Vectorof Byte) (vector-immutable 0 1 2 3 4)))
  : vm-function-def-
  (vm-function-def- stack-size-used parameter-count name byte-code))

(struct vm-
  ([frame-stack : (Listof vm-frame-)]
   [value-stack : (Listof cell-)]
   [functions : (Immutable-Vectorof vm-function-def-)]
   [globals : (Immutable-Vectorof cell-)]
   [structs : (Immutable-Vectorof vm-struct-def-)]
   [options : (Listof Symbol)])
  #:transparent)

(define (make-vm
         #:frames (frames : (Listof vm-frame-) (list (make-frame)))
         #:values (values : (Listof cell-) '())
         #:functions (functions : (Immutable-Vectorof vm-function-def-) (vector-immutable))
         #:globals (globals : (Immutable-Vectorof cell-) (vector-immutable))
         #:structs (structs : (Immutable-Vectorof vm-struct-def-) (vector-immutable))
         #:options (options : (Listof Symbol) '()))
        : vm-
  (vm- frames values functions globals structs options))

(define SVMC_BRK                  0) ;;
(define SVMC_PUSH_PARAM           1) ;; op = param-idx from tail, stack [] -> [cell-]
(define SVMC_PUSH_BYTE            2) ;; op = byte value, stack [] -> [cell-byte]
(define SVMC_PUSH_INT             3) ;; op1=low byte op2=high byte, stack [] -> [cell-int]
                                     ;; also used for struct-index or function-index
(define SVMC_PUSH_GLOBAL          4) ;; op1=low byte index op2=high byte index stack [] -> [cell-]
(define SVMC_POP_TO_PARAM         5) ;; op= param-idx from tail, stack [cell-] -> []
(define SVMC_POP_TO_GLOBAL        6) ;; op1=low byte index op2=high byte index, stack [cell-] -> []
(define SVMC_NIL?                 7) ;; stack [cell-list-ptr] -> [cell-boolean]
(define SVMC_BRA                  8) ;; op = relative offset
(define SVMC_CAR                  9) ;; stack [cell-list-ptr] -> [cell- car of list pointed at]
(define SVMC_CDR                 10) ;; stack [cell-list-ptr] -> [cell-list-ptr cdr of list pointed at]
(define SVMC_CONS                11) ;; stack [cell- car, cell-list-ptr cdr] -> stack [cell-list-ptr new-list]
(define SVMC_GOTO                12) ;; op = relative offset
(define SVMC_BYTE+               13) ;; stack [cell-byte a, cell-byte b] -> [sum]
(define SVMC_RET                 14) ;; stack [cell paramN, ... cell param1, cell param0] -> []
(define SVMC_CALL                15) ;; stack [int-cell: function index, cell paramN, ... cell param1, cell param0] -> [cell paramN, ... cell param1, cell param0]

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

(define (two-complement->signed-byte (a : Byte)) : Integer
  (cond
    [(< a #x80) a]
    [else (fx- a #x100)]))

(module+ test #| two-complement->signed-byte |#
  (check-equal? (two-complement->signed-byte #x7f)
                127)
  (check-equal? (two-complement->signed-byte 0)
                0)
  (check-equal? (two-complement->signed-byte #x80)
                -128)
  (check-equal? (two-complement->signed-byte #xff)
                -1))

(define (peek-pc-byte [vm : vm-] [ delta : Nonnegative-Integer 0]) : Byte
  (define active-frame (car (vm--frame-stack vm)))
  (define function (vector-ref (vm--functions vm) (vm-frame--fun-idx active-frame)))
  (vector-ref (vm-function-def--byte-code function) (fx+ delta (vm-frame--bc-idx active-frame))))

(module+ test #| peek-pc-byte |#
  (check-equal? (peek-pc-byte
                 (make-vm #:frames (list (make-frame #:bc-idx 2))
                          #:functions (vector-immutable
                                       (make-function-def #:byte-code (vector-immutable 7 8 9 10 11)))))
                9)

  (check-equal? (peek-pc-byte
                 (make-vm #:frames (list (make-frame #:bc-idx 2))
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
                  (make-vm #:frames (list (make-frame #:bc-idx 2))
                           #:functions (vector-immutable
                                        (make-function-def #:byte-code (vector-immutable 7 8 9 10 11))))))
                10))

(define (peek-value [vm : vm-] [offset : Byte 0]) : cell-
  (car (drop (vm--value-stack vm) offset)))

(define (pop-value [vm : vm-] [count : Byte 1]) : vm-
  (struct-copy vm- vm [value-stack (drop (vm--value-stack vm) count)]))

;; inner function for tail call recursion required, because recursive typed call w/ optional field does not typecheck
(define (pop-values [vm : vm-] [count : Nonnegative-Integer]) : (List (Listof cell-) vm-)
  (define (pop-values-[vm : vm-] [count : Nonnegative-Integer] [acc : (Listof cell-)]) : (List (Listof cell-) vm-)
    (cond
      [(fx< 0 count)
       (pop-values- (ann (pop-value vm) vm-) (ann (sub1 count) Nonnegative-Integer) (ann (cons (peek-value vm) acc) (Listof cell-)))]
      [else (list acc vm)]))
  (pop-values- vm count '()))

(define (push-value [vm : vm-] [value : cell-]) : vm-
  (struct-copy vm- vm [value-stack (cons value (vm--value-stack vm))]))

(define (push-on-value-stack (vm : vm-) (value : cell-)) : vm-
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
  (define new-frame (make-frame #:fun-idx (integer->two-complement (cell--value (car (vm--value-stack vm))))
                                #:bc-idx  0
                                #:parameter-tail (cdr (vm--value-stack vm))))
  (struct-copy vm- vm
               [frame-stack (cons new-frame (vm--frame-stack (increment-pc vm)))]
               [value-stack (cdr (vm--value-stack vm))]))

;; bytecode: op, len: 1b
;; stack: [ res pN .. p1 p0 ...] -> [ res ... ], growth: -N-1 (for n+1 parameter)
(define (interpret-ret [vm : vm-]) : vm-
  ;; drop the parameters
  (define value-stack (vm--value-stack vm))
  (define active-frame (car (vm--frame-stack vm)))
  (define new-value-stack (append (list (car value-stack))
                                  (drop value-stack (fx+ 1 (vm-function-def--parameter-count (vector-ref (vm--functions vm) (vm-frame--fun-idx active-frame)))))))
  (struct-copy vm- vm [frame-stack (cdr (vm--frame-stack vm))]
                      [value-stack new-value-stack]))

;; bytecode: op, len: 1b
;; stack: [ a:cell-byte b:cell-byte ... ] -> [ a+b:cell-byte- ... ], growth: -1c
(define (interpret-byte+ [vm : vm-]) : vm-
  ;; replace tos and tos-1 with sum (of the two bytes)
  (match-define (list (list a b) new-vm) (pop-values vm 2))
  (if (and (cell-byte-? a)
         (cell-byte-? b))
      (increment-pc
       (push-value new-vm (cell-byte- (byte+ (cell-byte--value a)
                                             (cell-byte--value b)))))
      (raise-user-error "byte+ encountered non byte in ~a or ~a" a b)))

(module+ test #| interpret-byte+ |#
  (check-equal? (vm--value-stack (interpret-byte+ (make-vm #:values (list (cell-byte- 1) (cell-byte- 2))
                                                           #:frames (list (make-frame)))))
                (list (cell-byte- 3))))

;; bytecode: op idx:byte, len: 2b
;; stack: [ ... pN .. p1 p0] -> [ pIDX ... pN .. p1 p0], growth: 1c
(define (interpret-push-param [vm : vm-]) : vm-
  (define param-idx (peek-pc-byte vm 1))
  (define param-value (car (drop (vm-frame--parameter-tail (car (vm--frame-stack vm))) param-idx)))
  (increment-pc (push-on-value-stack vm param-value) 2))

(module+ test #| interpret-push-param |#
  (define test_interpret-push-param
    (interpret-push-param
     (make-vm #:functions (vector-immutable (make-function-def #:byte-code (vector-immutable 0 2 0 0)))
              #:frames (list (make-frame #:parameter-tail (list (cell-) (cell-) (cell-int- 10) (cell-)))))))

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
  (increment-pc (push-on-value-stack vm (cell-byte- byte-value)) 2))

;; bytecode: op val-low:byte val-high:byte, len: 3b
;; stack: [ ... ] -> [ val:cell-int- ... ], growth: 1c
(define (interpret-push-int [vm : vm-]) : vm-
  (define low-byte (peek-pc-byte vm 1))
  (define high-byte (peek-pc-byte vm 2))
  (define int-value (cell-int- (fx+ (arithmetic-shift high-byte 8) low-byte)))
  (increment-pc (push-on-value-stack vm int-value) 3))

;; bytecode: op idx-low:byte idx-high:byte, len: 3b
;; stack: [ ... ] -> [ global@IDX:cell- ... ], growth: 1c
(define (interpret-push-global [vm : vm-]) : vm-
  (define global-idx-low (peek-pc-byte vm 1))
  (define global-idx-high (peek-pc-byte vm 2))
  (define global-idx (fx+ (arithmetic-shift global-idx-high 8) global-idx-low))
  (define global-value (vector-ref (vm--globals vm) global-idx))
  (increment-pc (push-on-value-stack vm global-value) 3))

;; bytecode: op idx:byte, len: 2b
;; stack: [ val ... pN .. p1 p0] -> [ ... pN .. pIDX=val .. p1 p0], growth: -1c
(define (interpret-pop-to-param [vm : vm-]) : vm-
  (define parameter-offset-from-tail (peek-pc-byte vm 1))
  (define value-stack (vm--value-stack vm))
  (define active-frame (car (vm--frame-stack vm)))
  (define parameter-tail (vm-frame--parameter-tail active-frame))
  (define values-without-parameters (takef value-stack (lambda (elt) (not (eq? elt (car parameter-tail))))))
  (define new-parameter-tail (append (take parameter-tail parameter-offset-from-tail)
                                     (list (car value-stack))
                                     (drop parameter-tail (add1 parameter-offset-from-tail))))
  (define new-frame (struct-copy vm-frame- active-frame
                                 [parameter-tail new-parameter-tail]))
  (increment-pc
   (struct-copy vm- vm
                [value-stack (append (cdr values-without-parameters) new-parameter-tail)]
                [frame-stack (cons new-frame (cdr (vm--frame-stack vm)))])
   2))

(module+ test #| interpret pop to param |#
  (define test_param_2-values-interpret-pop-to-param
    (list (cell-byte- 10) (cell-) #| params tail is here |# (cell-) (cell-) (cell-byte- 9) (cell-)))
  (define test_param_2-interpret-pop-to-param
    (interpret-pop-to-param
     (make-vm #:functions (vector-immutable (make-function-def #:byte-code (vector-immutable 0 2 0 0)))
              #:frames (list (make-frame #:parameter-tail (cddr test_param_2-values-interpret-pop-to-param)))
              #:values test_param_2-values-interpret-pop-to-param)))

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
              #:frames (list (make-frame #:parameter-tail (cdr test_param_0-values-interpret-pop-to-param)))
              #:values test_param_0-values-interpret-pop-to-param)))

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
              #:frames (list (make-frame #:parameter-tail (cddr test_param_4-values-interpret-pop-to-param)))
              #:values test_param_4-values-interpret-pop-to-param)))

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
  (define global-idx-low (peek-pc-byte vm 1))
  (define global-idx-high (peek-pc-byte vm 2))
  (define global-idx (fx+ (arithmetic-shift global-idx-high 8) global-idx-low))
  (increment-pc
   (struct-copy vm- vm
                [value-stack (cdr (vm--value-stack vm))]
                [globals (vector->immutable-vector (vector-set/copy (vm--globals vm) global-idx (car (vm--value-stack vm))))])
   3))

(module+ test #| interpret pop to global |#
  (define test-interpret-pop-to-global
    (interpret-pop-to-global
    (make-vm #:functions (vector-immutable (make-function-def #:byte-code (vector-immutable 0 2 0 0)))
             #:globals (vector-immutable (cell-)(cell-)(cell-byte- 9)(cell-))
             #:values (list (cell-byte- 10) (cell-)))))
  (check-equal? (vm--globals test-interpret-pop-to-global)
                (vector-immutable (cell-)(cell-)(cell-byte- 10)(cell-))))

;; bytecode: op
;; stack: [ cell-list-ptr ...] -> [ cell-bool ... ], growth: 0
(define (interpret-nil? [vm : vm-]) : vm-
  (define list-ptr (peek-value vm))
  (unless (cell-list-ptr-? list-ptr)
    (raise-user-error (format "operand for nil? is not a list but ~a" list-ptr)))
  (increment-pc
   (push-value (pop-value vm) (cell-byte- (if (eq? list-ptr VM_NIL_CELL) #xff #x00)))))

;; bytecode: op two-complement-offset, len: 2b
;; stack: [ val:cell-byte ... ]  -> [ ... ], growth: -1c
(define (interpret-bra [vm : vm-]) : vm-
  (define tos (peek-value vm))
  (define delta (if (= 0 (if (cell-byte-? tos)
                         (cell-byte--value tos)
                         (raise-user-error (format "expected byte as tos, got ~a" tos)) ))
                2
                (+ 1 (two-complement->signed-byte (peek-pc-byte vm 1)))))
  (increment-pc (pop-value vm) delta))

;; bytecode: op two-complement-offset, len: 2b
;; stack: [ ... ]  -> [ ... ], growth: 0c
(define (interpret-goto [vm : vm-]) : vm-
  (define delta (+ 1 (two-complement->signed-byte (peek-pc-byte vm 1))))
  (increment-pc vm delta))

;; bytecode: op, len: 1b
;; stack: [ cell-list-ptr ... ] -> [ car-of-list ... ], growth: 0c
(define (interpret-car [vm : vm-]) : vm-
  (define tos (peek-value vm))
  (define car-cell (if (cell-list-ptr-? tos)
                       (cell-list-ptr--car tos)
                       (raise-user-error "expected cell-list-ptr, got ~a" tos)))
  (increment-pc (push-value (pop-value vm) car-cell)))

;; bytecode: op, len: 1b
;; stack: [ cell-list-ptr ... ] -> [ cdr-of-list ... ], growth: 0c
(define (interpret-cdr [vm : vm-]) : vm-
  (define tos (peek-value vm))
  (define cdr-cell (if (cell-list-ptr-? tos)
                       (cell-list-ptr--cdr tos)
                       (raise-user-error "expected cell-list-ptr, got ~a" tos)))
  (increment-pc (push-value (pop-value vm) cdr-cell)))

;; bytecode: op, len: 1b
;; stack: [ cdr-cell car-cell ... ] -> [ cell-list-ptr ... ], growth: -1c
(define (interpret-cons [vm : vm-]) : vm-
  (match-define (list (list cdr-cell car-cell) next-vm) (pop-values vm 2))
  (increment-pc (push-value next-vm (cell-list-ptr- car-cell cdr-cell))))

(define (dissassemble-byte-code (vm : vm-)) : String
  (define byte-code (peek-pc-byte vm))
    (cond
    [(= byte-code SVMC_BRK) "brk"]
    [(= byte-code SVMC_BYTE+) "byte+"]
    [(= byte-code SVMC_RET) "ret"]
    [(= byte-code SVMC_CALL) "call"]
    [(= byte-code SVMC_PUSH_PARAM) (format "pushp p-~a" (peek-pc-byte vm 1))]
    [(= byte-code SVMC_PUSH_BYTE) (format "pushb #~a" (peek-pc-byte vm 1))]
    [(= byte-code SVMC_PUSH_INT) (format "pushi #~a" (fx+ (peek-pc-byte vm 1) (arithmetic-shift (peek-pc-byte vm 2) 8)))]
    [(= byte-code SVMC_PUSH_GLOBAL) (format "pushg g-~a" (fx+ (peek-pc-byte vm 1) (arithmetic-shift (peek-pc-byte vm 2) 8)))]
    [(= byte-code SVMC_POP_TO_PARAM) (format "popp p-~a" (peek-pc-byte vm 1))]
    [(= byte-code SVMC_POP_TO_GLOBAL) (format "popp g-~a" (fx+ (peek-pc-byte vm 1) (arithmetic-shift (peek-pc-byte vm 2) 8)))]
    [(= byte-code SVMC_NIL?) "nil?"]
    [(= byte-code SVMC_BRA) (format "bra ~a" (two-complement->signed-byte (peek-pc-byte vm 1)))]
    [(= byte-code SVMC_CAR) "car"]
    [(= byte-code SVMC_CDR) "cdr"]
    [(= byte-code SVMC_CONS) "cons"]
    [(= byte-code SVMC_GOTO) (format "goto ~a" (two-complement->signed-byte (peek-pc-byte vm 1)))]

    [else (raise-user-error (format "unknown byte code command ~a" byte-code))]))

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
                           (car (vm--value-stack vm))))))
  (cond
    [(= byte-code SVMC_BRK) (raise-user-error "encountered BRK")]
    [(= byte-code SVMC_BYTE+) (interpret-byte+ vm)]
    [(= byte-code SVMC_RET) (interpret-ret vm)]
    [(= byte-code SVMC_CALL) (interpret-call vm)]
    [(= byte-code SVMC_PUSH_PARAM) (interpret-push-param vm)]
    [(= byte-code SVMC_PUSH_BYTE) (interpret-push-byte vm)]
    [(= byte-code SVMC_PUSH_INT) (interpret-push-int vm)]
    [(= byte-code SVMC_PUSH_GLOBAL) (interpret-push-global vm)]
    [(= byte-code SVMC_POP_TO_PARAM) (interpret-pop-to-param vm)]
    [(= byte-code SVMC_POP_TO_GLOBAL) (interpret-pop-to-global vm)]
    [(= byte-code SVMC_NIL?) (interpret-nil? vm)]
    [(= byte-code SVMC_BRA) (interpret-bra vm)]
    [(= byte-code SVMC_CAR) (interpret-car vm)]
    [(= byte-code SVMC_CDR) (interpret-cdr vm)]
    [(= byte-code SVMC_CONS) (interpret-cons vm)]
    [(= byte-code SVMC_GOTO) (interpret-goto vm)]

    [else (raise-user-error (format "unknown byte code command ~a" byte-code))]))

(define (run-until-break (vm : vm-)) : vm-
  (cond [(fx= SVMC_BRK (peek-pc-byte vm)) vm]
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
        #:byte-code (vector-immutable SVMC_PUSH_BYTE 10
                                      SVMC_PUSH_BYTE 20
                                      SVMC_BYTE+
                                      SVMC_BRK))))))

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
        #:byte-code (vector-immutable SVMC_PUSH_BYTE 10
                                      SVMC_PUSH_BYTE 20
                                      SVMC_PUSH_INT   1 0 ;; function index 1
                                      SVMC_CALL
                                      SVMC_BRK))
       (make-function-def
        #:parameter-count 2
        #:byte-code (vector-immutable SVMC_PUSH_PARAM 1
                                      SVMC_PUSH_PARAM 0
                                      SVMC_BYTE+
                                      SVMC_RET))))))

  (check-equal? (vm--value-stack test-call-return--run-until-break)
                (list (cell-byte- 30))
                "the execution result on the value stack should be 30")
  (check-equal? (car (vm--frame-stack test-call-return--run-until-break))
                (make-frame #:bc-idx 8)
                "the program counter should point to the break instruction")

  (define test-tail-recursio--value-stack
    (list (cell-byte- 0)
          (cell-list-ptr- (cell-byte- 10) (cell-list-ptr- (cell-byte- 20) VM_NIL_CELL))))

  (define (byte->two-complement (value : Fixnum)) : Byte
    (define pre-result
      (cond
        [(fx< value 0) (fx+ 256 value)]
        [else value]))
    (if (byte? pre-result)
        pre-result
       (raise-user-error (format "signed byte out of range ~a" value))))

  (define test-tail-recursion--run-until-break
    (run-until-break
     (make-vm
      #:options (list) ;;  'trace
      #:values  test-tail-recursio--value-stack
      #:functions
      (vector-immutable
       (make-function-def
        #:byte-code (vector-immutable SVMC_PUSH_INT   1 0 ;; function index 1
                                      SVMC_CALL
                                      SVMC_BRK))
       (make-function-def
        #:parameter-count 2
        #:byte-code (vector-immutable SVMC_PUSH_PARAM 1 ;; the list        stack [list]
                                      SVMC_NIL?         ;;                 stack [nil?]
                                      SVMC_BRA 16       ;;                 stack []
                                      SVMC_PUSH_PARAM 1 ;; the list        stack [list]
                                      SVMC_CAR          ;; first value     stack [car]
                                      SVMC_PUSH_PARAM 0 ;; the accumulator stack [acc car]
                                      SVMC_BYTE+        ;;                 stack [new-acc]
                                      SVMC_PUSH_PARAM 1 ;; the list        stack [list new-acc]
                                      SVMC_CDR          ;;                 stack [cdr new-acc]
                                      SVMC_POP_TO_PARAM 1 ;;               stack [new-acc]
                                      SVMC_POP_TO_PARAM 0 ;;               stack []
                                      SVMC_GOTO (byte->two-complement -19)
                                      SVMC_PUSH_PARAM 0 ;; acc             stack [acc]
                                      SVMC_RET))))))

  (check-equal? (vm--value-stack test-tail-recursion--run-until-break)
                (list (cell-byte- 30))))
