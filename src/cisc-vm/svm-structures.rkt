#lang typed/racket

(require (only-in data/pvector pvector pvector? make-pvector build-pvector))
(require (only-in racket/fixnum fx+ fx= fx<))


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
   [bc-idx  : Nonnegative-Integer])
  #:transparent)

(struct vm-struct-def-
  ([name : String]
   [field-no : Nonnegative-Integer])
  #:transparent)

(struct vm-function-def-
  ([stack-size-used : Byte]
   [name : String]
   [byte-code : (Immutable-Vectorof Byte)])
  #:transparent)

(struct vm-
  ([frame-stack : (Listof vm-frame-)]
   [value-stack : (Listof cell-)]
   [functions : (Immutable-Vectorof vm-function-def-)]
   [globals : (Immutable-Vectorof cell-)]
   [structs : (Immutable-Vectorof vm-struct-def-)]
   [options : (Listof Symbol)])
  #:transparent)

(define SVMC_PUSH                 1)
(define SVMC_POP_INTO             2)
(define SVMC_NIL?                 3)
(define SVMC_BRA                  4)
(define SVMC_CAR                  5)
(define SVMC_CDR                  6)
(define SVMC_CONS                 7)
(define SVMC_GOTO                 8)
(define SVMC_BYTE+                9)
(define SVMC_RET                 10)


(define (make-vm
         #:frames (frames : (Listof vm-frame-) '())
         #:values (values : (Listof cell-) '())
         #:functions (functions : (Immutable-Vectorof vm-function-def-) (vector-immutable))
         #:globals (globals : (Immutable-Vectorof cell-) (vector-immutable))
         #:structs (structs : (Immutable-Vectorof vm-struct-def-) (vector-immutable))
         #:options (options : (Listof Symbol) '()))
        : vm-
  (vm- frames values functions globals structs options))


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

(define (call-function [vm : vm-]) : vm-
  (define new-frame (vm-frame- (integer->two-complement (cell--value (car (vm--value-stack vm)))) 0))
  (struct-copy vm- vm
               [frame-stack (cons new-frame (vm--frame-stack vm))]
               [value-stack (cdr (vm--value-stack vm))]))

(define (peek-pc-byte [vm : vm-] [ delta : Nonnegative-Integer 0]) : Byte
  (define active-frame (car (vm--frame-stack vm)))
  (define function (vector-ref (vm--functions vm) (vm-frame--fun-idx active-frame)))
  (vector-ref (vm-function-def--byte-code function) (fx+ delta (vm-frame--bc-idx active-frame))))

(module+ test #| peek-pc-byte |#
  (check-equal? (peek-pc-byte
                 (make-vm #:frames (list (vm-frame- 0 2))
                          #:functions (vector-immutable
                                       (vm-function-def- 1 "some fun"
                                                         (vector-immutable 7 8 9 10 11)))))
                9)

  (check-equal? (peek-pc-byte
                 (make-vm #:frames (list (vm-frame- 0 2))
                          #:functions (vector-immutable
                                       (vm-function-def- 1 "some fun"
                                                         (vector-immutable 7 8 9 10 11))))
                 1)
                10))

(define (increment-pc [vm : vm-] [delta : Nonnegative-Integer 1]) : vm-
  (define active-frame (car (vm--frame-stack vm)))
  (define new-frame (vm-frame- (vm-frame--fun-idx active-frame)
                               (fx+ delta (vm-frame--bc-idx active-frame))))
  (struct-copy vm- vm
               [frame-stack (cons new-frame (cdr (vm--frame-stack vm)))]))

(module+ test #| increment-pc |#
  (check-equal? (peek-pc-byte
                 (increment-pc
                  (make-vm #:frames (list (vm-frame- 0 2))
                           #:functions (vector-immutable
                                        (vm-function-def- 1 "some fun"
                                                          (vector-immutable 7 8 9 10 11))))))
                10))

(define (return-from-call [vm : vm-]) : vm-
  (struct-copy vm- vm [frame-stack (cdr (vm--frame-stack vm))]))

(define (peek-value [vm : vm-]) : cell-
  (car (vm--value-stack vm)))

(define (pop-value [vm : vm-]) : vm-
  (struct-copy vm- vm [value-stack (cdr (vm--value-stack vm))]))

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
                                                           #:frames (list (vm-frame- 0 0)))))
                (list (cell-byte- 3))))
