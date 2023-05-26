#lang racket

(require data/pvector)
(require racket/generic)
(require (only-in data/collection
                  set-nth
                  nth))

(module+ test #| require test utils |#
  (require "../6510-test-utils.rkt"))

;; heap of memory pages
(struct heap
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

;; make/init a heap of TOTAL-PAGE-NO
(define (make-heap total-page-no)
  (-> nonnegative-integer? heap?)
  (heap (make-pvector total-page-no nil-page) total-page-no))

;; get the page at NO from the HEAP
(define (get-heap-page heap no)
  (-> heap? nonnegative-integer? page?)
  (nth (heap-pages heap) no))

;; free the page at NO of the HEAP
(define (free-heap-page a-heap no)
  (-> heap? nonnegative-integer? heap?)
  (heap (set-nth (heap-pages a-heap) no nil-page) (heap-count a-heap)))

;; allocate this PAGE at NO on the HEAP
(define (alloc-heap-page a-heap no page)
  (-> heap? nonnegative-integer? heap?)
  (unless (eq? (nth (heap-pages a-heap) no) nil-page)
    (raise-user-error "cannot (re)allocate, page already allocated"))
  (heap (set-nth (heap-pages a-heap) no page) (heap-count a-heap)))

(module+ test #| allocate, get, free-heap-page |#
  (check-true (byte-page? (get-heap-page (alloc-heap-page (make-heap 256) #xC0 (make-byte-page)) #xC0))
              "allocating a byte page ensures that get will return this byte page")
  (check-eq? (get-heap-page (make-heap 256) #xC0) nil-page
             "unallocated pages eq nil-page")
  (check-eq? (get-heap-page (free-heap-page (alloc-heap-page (make-heap 256) #xC0 (make-byte-page)) #xC0) #xC0) nil-page
             "allocated, then freed pages eq nil-page (again)"))

;; write A-VALUE into A-VALUE-REF using/within A-HEAP
(define/contract (write-into-value-ref a-heap a-value-ref a-value)
  (-> heap? value-reference? value? heap?)
  (define vpage
    (get-heap-page a-heap (value-reference-page-no a-value-ref)))
  (cond ((and (byte-value? a-value) (byte-page? vpage))
         (heap (set-nth (heap-pages a-heap)
                        (value-reference-page-no a-value-ref)
                        (write-byte vpage (value-reference-index a-value-ref) (byte-value-byte a-value)))
               (heap-count a-heap)))
        (#t (raise-user-error "heap page not matching value or unknown implementation"))))

;; push A-VALUE onto A-VALUE-STACK
(define/contract (push-value a-value-stack a-value)
  (-> value-stack? value? value-stack?)
  (value-stack (cons a-value (value-stack-values a-value-stack))))

;; write the top of A-VALUE-STACK into A-VALUE-REF using/within A-HEAP
(define/contract (tos-value-into a-heap a-value-stack a-value-ref)
  (-> heap? value-stack? value-reference? heap?)
  (write-into-value-ref a-heap a-value-ref (car (value-stack-values a-value-stack))))

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
                 (get-heap-page
                  (write-into-value-ref (alloc-heap-page (make-heap 2) 1 (make-byte-page))
                                        (value-reference 1 100)
                                        (byte-value #x37))
                  1)
                 100)
                #x37))

;; skeleton definitions (without implementation

;; dereference A-VALUE-REFERENCE
(define (dereference-value a-heap a-value-reference)
  (-> heap? value-reference? value?)
  (define page (get-heap-page a-heap (value-reference-page-no a-value-reference)))
  (cond ((byte-page? page)
         (read-byte-value page (value-reference-index a-value-reference)))
        (#t (raise-user-error "unknown heap page type"))))

(module+ test #| dereference-value |#
  (check-equal? (byte-value-byte
                 (dereference-value
                  (write-into-value-ref (alloc-heap-page (make-heap 2) 1 (make-byte-page))
                                        (value-reference 1 100)
                                        (byte-value #x37))
                  (value-reference 1 100)))                 
                 #x37))

(struct call-stack ())
(struct env ())

(struct vm-state
  (value-stack ;; value 
   call-stack  ;; stack with return addresses
   code        ;; array of byte code
   heap        ;; heap with pages
   env         ;; nested environments of int->value references
   pc)         ;; current program counter (index into code)
  #:guard (struct-guard/c value-stack? call-stack? pvector? heap? env? nonnegative-integer?))

