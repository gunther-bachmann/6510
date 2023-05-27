#lang racket

(require data/pvector)
(require racket/generic)
(require (only-in data/collection
                  set-nth
                  nth))

(module+ test #| require test utils |#
  (require "../6510-test-utils.rkt"))

;; memory of memory pages
(struct memory
  (pages count)
  #:guard (struct-guard/c pvector? nonnegative-integer?))

;; generic page 
(struct page
  ())

;; unallocated page will always eq? nil-page
(define nil-page (page))

;; page of 256 byte values
(struct byte-page page
  (data)
  #:guard (struct-guard/c pvector?))

;; generic value type
(struct value
  ())

;; reference to a value
(struct value-reference value
  (page-no index)
  #:guard (struct-guard/c nonnegative-integer? byte?))

;; byte value
(struct byte-value value
  (byte)
  #:transparent
  #:guard (struct-guard/c byte?))

;; stack of values to be operated on
(struct value-stack
  (values)
  #:guard (struct-guard/c list?))

;; c-tor
(define (make-byte-page)
  (byte-page (make-pvector 256 0)))

;; write BYTE into A-BYTE-PAGE at INDEX
(define/contract (write-byte a-byte-page index byte)
  (-> byte-page? byte? byte? byte-page?)
  (byte-page (set-nth (byte-page-data a-byte-page) index byte)))

(define/contract (write-bytes a-byte-page index bytes)
  (-> byte-page? byte? (listof byte?) byte-page?)
  (if (empty? bytes)
      a-byte-page
      (write-bytes
       (write-byte a-byte-page index (car bytes))
       (add1 index)
       (cdr bytes))))

;; read byte from A-BYTE-PAGE at INDEX
(define/contract (read-byte a-byte-page index)
  (-> byte-page? byte? byte?)
  (nth (byte-page-data a-byte-page) index))

;; write BYTE into A-BYTE-PAGE at INDEX
(define/contract (write-byte-value a-byte-page index a-byte-value)
  (-> byte-page? byte? byte-value? byte-page?)
  (write-byte a-byte-page index (byte-value-byte a-byte-value)))

;; read byte from A-BYTE-PAGE at INDEX
(define/contract (read-byte-value a-byte-page index)
  (-> byte-page? byte? byte-value?)
  (byte-value (read-byte a-byte-page index)))

(module+ test #| write-byte, read-byte |#
  (check-equal? (read-byte (write-byte (make-byte-page) 5 #x20)
                           5)
                #x20
                "written byte can be read from byte-page"))

;; make/init a memory of TOTAL-PAGE-NO
(define (make-memory total-page-no)
  (-> nonnegative-integer? memory?)
  (memory (make-pvector total-page-no nil-page) total-page-no))

;; get the page at NO from the memory
(define (get-memory-page memory no)
  (-> memory? nonnegative-integer? page?)
  (nth (memory-pages memory) no))

;; free the page at NO of the memory
(define (free-memory-page a-memory no)
  (-> memory? nonnegative-integer? memory?)
  (memory (set-nth (memory-pages a-memory) no nil-page) (memory-count a-memory)))

;; allocate this PAGE at NO on the memory
(define (alloc-memory-page a-memory no page)
  (-> memory? nonnegative-integer? memory?)
  (unless (eq? (nth (memory-pages a-memory) no) nil-page)
    (raise-user-error "cannot (re)allocate, page already allocated"))
  (memory (set-nth (memory-pages a-memory) no page) (memory-count a-memory)))

(module+ test #| allocate, get, free-memory-page |#
  (check-true (byte-page? (get-memory-page (alloc-memory-page (make-memory 256) #xC0 (make-byte-page)) #xC0))
              "allocating a byte page ensures that get will return this byte page")
  (check-eq? (get-memory-page (make-memory 256) #xC0) nil-page
             "unallocated pages eq nil-page")
  (check-eq? (get-memory-page (free-memory-page (alloc-memory-page (make-memory 256) #xC0 (make-byte-page)) #xC0) #xC0) nil-page
             "allocated, then freed pages eq nil-page (again)"))

;; write A-VALUE into A-VALUE-REF using/within A-memory
(define/contract (write-into-value-ref a-memory a-value-ref a-value)
  (-> memory? value-reference? value? memory?)
  (define vpage
    (get-memory-page a-memory (value-reference-page-no a-value-ref)))
  (cond ((and (byte-value? a-value) (byte-page? vpage))
         (memory (set-nth (memory-pages a-memory)
                          (value-reference-page-no a-value-ref)
                          (write-byte vpage (value-reference-index a-value-ref) (byte-value-byte a-value)))
                 (memory-count a-memory)))
        (#t (raise-user-error "memory page not matching value or unknown implementation"))))

;; push A-VALUE onto A-VALUE-STACK
(define/contract (push-value a-value-stack a-value)
  (-> value-stack? value? value-stack?)
  (value-stack (cons a-value (value-stack-values a-value-stack))))

;; write the top of A-VALUE-STACK into A-VALUE-REF using/within A-memory
(define/contract (tos-value-into a-memory a-value-stack a-value-ref)
  (-> memory? value-stack? value-reference? memory?)
  (write-into-value-ref a-memory a-value-ref (car (value-stack-values a-value-stack))))

;; pop the top from A-VALUE-STACK
(define/contract (pop-value a-value-stack)
  (-> value-stack? value-stack?)
  (value-stack (cdr (value-stack-values a-value-stack))))

;; get the value of the top of A-VALUE-STACK
(define/contract (top-of-stack-value a-value-stack)
  (-> value-stack? value?)
  (car (value-stack-values a-value-stack)))

(module+ test #| push-value, pop-value, top-of-stack-value, pop-value-into |#
  (check-equal? (byte-value-byte
                 (top-of-stack-value
                  (push-value (value-stack '()) (byte-value #xCD))))
                #xCD)
  (check-equal? (byte-value-byte
                 (top-of-stack-value
                  (push-value (push-value (value-stack '()) (byte-value #xCD)) (byte-value #x23))))
                #x23)
  (check-equal? (byte-value-byte
                 (top-of-stack-value
                  (pop-value (push-value (push-value (value-stack '()) (byte-value #xCD)) (byte-value #x23)))))
                #xCD))


(module+ test #| write-into-value-ref |#
  (check-equal? (read-byte
                 (get-memory-page
                  (write-into-value-ref (alloc-memory-page (make-memory 2) 1 (make-byte-page))
                                        (value-reference 1 100)
                                        (byte-value #x37))
                  1)
                 100)
                #x37))

;; skeleton definitions (without implementation

;; dereference A-VALUE-REFERENCE
(define (dereference-value a-memory a-value-reference)
  (-> memory? value-reference? value?)
  (define page (get-memory-page a-memory (value-reference-page-no a-value-reference)))
  (cond ((byte-page? page)
         (read-byte-value page (value-reference-index a-value-reference)))
        (#t (raise-user-error "unknown memory page type"))))

(module+ test #| dereference-value |#
  (check-equal? (byte-value-byte
                 (dereference-value
                  (write-into-value-ref (alloc-memory-page (make-memory 2) 1 (make-byte-page))
                                        (value-reference 1 100)
                                        (byte-value #x37))
                  (value-reference 1 100)))                 
                #x37))

(struct code-reference
  (page-no
   index)
  #:transparent
  #:guard (struct-guard/c nonnegative-integer? byte?))

(struct call-stack
  (code-references)
  #:guard (struct-guard/c (listof code-reference?)))

(struct env
  (map-stack)
  #:guard (struct-guard/c (listof hash?)))

(define/contract (inc-pc a-pc delta)
  (-> code-reference? integer? code-reference?)
  (define new-index-unmodified (+ (code-reference-index a-pc) delta))
  (define new-page
    (cond ((< new-index-unmodified 0)
           (sub1 (code-reference-page-no a-pc)))
          ((> new-index-unmodified #xff)
           (add1 (code-reference-page-no a-pc)))
          (#t (code-reference-page-no a-pc))))
  (code-reference new-page (bitwise-and #xff new-index-unmodified)))

(module+ test #| inc-pc |#
  (check-equal? (code-reference-page-no (inc-pc (code-reference 5 255) 1))
                6)
  (check-equal? (code-reference-index (inc-pc (code-reference 5 255) 1))
                0)
  (check-equal? (code-reference-page-no (inc-pc (code-reference 5 255) 2))
                6)
  (check-equal? (code-reference-index (inc-pc (code-reference 5 255) 2))
                1)
  (check-equal? (code-reference-page-no (inc-pc (code-reference 5 254) 1))
                5)
  (check-equal? (code-reference-index (inc-pc (code-reference 5 254) 1))
                255)
  (check-equal? (code-reference-page-no (inc-pc (code-reference 5 0) -1))
                4)
  (check-equal? (code-reference-index (inc-pc (code-reference 5 0) -1))
                255)
  (check-equal? (code-reference-page-no (inc-pc (code-reference 5 0) -2))
                4)
  (check-equal? (code-reference-index (inc-pc (code-reference 5 0) -2))
                254)
  (check-equal? (code-reference-page-no (inc-pc (code-reference 5 1) -1))
                5)
  (check-equal? (code-reference-index (inc-pc (code-reference 5 1) -1))
                0))

(define/contract (deref-pc a-mem a-pc (rel 0))
  (->* (memory? code-reference?) (integer?) byte?)
  (define real-pc (inc-pc a-pc rel))
  (define page (get-memory-page a-mem (code-reference-page-no real-pc)))
  (nth (byte-page-data page) (code-reference-index real-pc)))

(module+ test #| deref-pc |#
  (check-equal?  (deref-pc
                  (alloc-memory-page (make-memory 10) 5 (write-bytes (make-byte-page) 2 '(#x21 #x4e #xc3)))
                  (code-reference 5 3))
                 #x4e)
  (check-equal?  (deref-pc
                  (alloc-memory-page (make-memory 10) 5 (write-bytes (make-byte-page) 2 '(#x21 #x4e #xc3)))
                  (code-reference 5 3)
                  1)
                 #xc3)
  (check-equal?  (deref-pc
                  (alloc-memory-page (make-memory 10) 5 (write-bytes (make-byte-page) 2 '(#x21 #x4e #xc3)))
                  (code-reference 5 3)
                  -1)
                 #x21))

(struct vm-state
  (value-stack ;; values
   call-stack  ;; stack with return addresses
   memory      ;; pages
   env         ;; nested environments of int->value references
   pc)         ;; current program counter (index into memory)
  #:guard (struct-guard/c value-stack? call-stack? memory? env? code-reference?))

