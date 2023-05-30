#lang racket

(require data/pvector)
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

;; generic cell type
(struct cell
  (ref-count) ;; ref-count is only kept on cells in the heap, cells on the stack do not have ref-counts!
  #:transparent
  #:guard (struct-guard/c byte?))

;; when putting a cell-ref on the stack, the heap-cell ref-count is incremented,
;; when popping a cell-ref from the stack, the heap-cell ref-count is decremented,
;; when putting a cell-value on the stack no ref-count needs adjustments

;; reference to a cell 
(struct cell-reference cell
  ()
  #:transparent)

(struct cell-nil-reference cell-reference
  ()
  #:transparent
  #:guard (struct-guard/c byte?))

;; single instance 
(define nil-reference (cell-nil-reference 0))

(define/contract (nil-ref? a-val-ref)
  (-> cell-reference? boolean?)
  (eq? a-val-ref nil-reference))

(module+ test #| nil |#
  (check-true (nil-ref? nil-reference)
              "only the defined nil-reference is nil")
  (check-false (nil-ref? (cell-nil-reference 0))
               "another instance of cell-nil-reference is not nil!"))

;; reference to an allocated cell on the heap
(struct cell-bound-reference cell-reference
  (page-no index)
  #:guard (struct-guard/c byte? nonnegative-integer? byte?))

;; byte cell
(struct byte-cell cell
  (byte)
  #:transparent
  #:guard (struct-guard/c byte? byte?))

;; int-cell (can store values > 256 but definitely < 65535 to allow for type tag bits in the 16 bit)
;; char-cell = special byte cell? 
;; boolean-cell = special byte cell?
;; native-array-ref-cell is special cell-reference (type-info anywhere?)
;; string-cell = special native-array-ref-cell?
;; symbol-cell = special native-array-ref-cell?
;; function/code-ref cell is special cell-reference (type-info anywhere?)
;; float-cell = special cell-reference (type-info anywhere)

;; stack of cells to be operated on
(struct eval-stack
  (cells)
  #:guard (struct-guard/c (listof cell?)))

;; reference into memory
(struct byte-mem-reference
  (page-no
   index)
  #:transparent
  #:guard (struct-guard/c nonnegative-integer? byte?))

(define/contract (inc-byte-mem-ref a-byte-mem-ref (delta 1))
  (->* (byte-mem-reference?) (integer?) byte-mem-reference?)
  (define page-no (byte-mem-reference-page-no a-byte-mem-ref))
  (define new-index-unmodified (+ (byte-mem-reference-index a-byte-mem-ref) delta))
  (define new-page-no
    (cond ((< new-index-unmodified 0)
           (sub1 page-no))
          ((> new-index-unmodified #xff)
           (add1 page-no))
          (#t page-no)))
  (byte-mem-reference new-page-no (bitwise-and #xff new-index-unmodified)))

(module+ test #| inc-pc |#
  (check-equal? (byte-mem-reference-page-no (inc-byte-mem-ref (byte-mem-reference 5 255) 1))
                6)
  (check-equal? (byte-mem-reference-index (inc-byte-mem-ref (byte-mem-reference 5 255) 1))
                0)
  (check-equal? (byte-mem-reference-page-no (inc-byte-mem-ref (byte-mem-reference 5 255) 2))
                6)
  (check-equal? (byte-mem-reference-index (inc-byte-mem-ref (byte-mem-reference 5 255) 2))
                1)
  (check-equal? (byte-mem-reference-page-no (inc-byte-mem-ref (byte-mem-reference 5 254) 1))
                5)
  (check-equal? (byte-mem-reference-index (inc-byte-mem-ref (byte-mem-reference 5 254) 1))
                255)
  (check-equal? (byte-mem-reference-page-no (inc-byte-mem-ref (byte-mem-reference 5 0) -1))
                4)
  (check-equal? (byte-mem-reference-index (inc-byte-mem-ref (byte-mem-reference 5 0) -1))
                255)
  (check-equal? (byte-mem-reference-page-no (inc-byte-mem-ref (byte-mem-reference 5 0) -2))
                4)
  (check-equal? (byte-mem-reference-index (inc-byte-mem-ref (byte-mem-reference 5 0) -2))
                254)
  (check-equal? (byte-mem-reference-page-no (inc-byte-mem-ref (byte-mem-reference 5 1) -1))
                5)
  (check-equal? (byte-mem-reference-index (inc-byte-mem-ref (byte-mem-reference 5 1) -1))
                0))

;; c-tor
(define (make-byte-page)
  (byte-page (make-pvector 256 0)))

;; write BYTE into A-BYTE-PAGE at INDEX
(define/contract (write-byte-into-page a-byte-page index byte)
  (-> byte-page? byte? byte? byte-page?)
  (byte-page (set-nth (byte-page-data a-byte-page) index byte)))

(define/contract (write-bytes-into-page a-byte-page index bytes)
  (-> byte-page? byte? (listof byte?) byte-page?)
  (if (empty? bytes)
      a-byte-page
      (write-bytes-into-page
       (write-byte-into-page a-byte-page index (car bytes))
       (add1 index)
       (cdr bytes))))

(define/contract (memory-set-page mem page-no a-page)
  (-> memory? nonnegative-integer? page? memory?)
  (memory (set-nth (memory-pages mem)
                   page-no
                   a-page)
          (memory-count mem)))

;; write bytes (even over page borders)
(define/contract (write-bytes-into-memory mem mem-ref bytes)
  (-> memory? byte-mem-reference? (listof byte?) memory?)
  (if (empty? bytes)
      mem
      (write-bytes-into-memory
       (memory-set-page mem
                        (byte-mem-reference-page-no mem-ref)
                        (write-byte-into-page (get-memory-page mem (byte-mem-reference-page-no mem-ref))
                                              (byte-mem-reference-index mem-ref)
                                              (car bytes)))
       (inc-byte-mem-ref mem-ref)
       (cdr bytes))))

(module+ test #| write-bytes-into-memory |#
  (define mem-written-over-two-pages
    (write-bytes-into-memory 
                 (alloc-memory-page
                  (alloc-memory-page (make-memory 3) 0 (make-byte-page))
                  1 (make-byte-page))
                 (byte-mem-reference 0 254)
                 '(1 2 3 4)))
  (check-equal? (read-byte-from-page (get-memory-page mem-written-over-two-pages 0) 254)
                1)
  (check-equal? (read-byte-from-page (get-memory-page mem-written-over-two-pages 0) 255)
                2)
  (check-equal? (read-byte-from-page (get-memory-page mem-written-over-two-pages 1) 0)
                3)
  (check-equal? (read-byte-from-page (get-memory-page mem-written-over-two-pages 1) 1)
                4))

;; read byte from A-BYTE-PAGE at INDEX
(define/contract (read-byte-from-page a-byte-page index)
  (-> byte-page? byte? byte?)
  (nth (byte-page-data a-byte-page) index))

;; write BYTE into A-BYTE-PAGE at INDEX
(define/contract (write-byte-cell-into-page a-byte-page index a-byte-cell)
  (-> byte-page? byte? byte-cell? byte-page?)
  (write-byte-into-page a-byte-page index (byte-cell-byte a-byte-cell)))

;; read byte from A-BYTE-PAGE at INDEX
(define/contract (read-byte-cell-from-page a-byte-page index)
  (-> byte-page? byte? byte-cell?)
  (byte-cell 0 (read-byte-from-page a-byte-page index)))

(module+ test #| write-byte, read-byte |#
  (check-equal? (read-byte-from-page (write-byte-into-page (make-byte-page) 5 #x20)
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
  (memory-set-page a-memory no nil-page))

;; allocate this PAGE at NO on the memory
(define (alloc-memory-page a-memory no page)
  (-> memory? nonnegative-integer? page? memory?)
  (unless (eq? (get-memory-page a-memory no) nil-page)
    (raise-user-error "cannot (re)allocate, page already allocated"))
  (memory-set-page a-memory no page))

(module+ test #| allocate, get, free-memory-page |#
  (check-true (byte-page? (get-memory-page (alloc-memory-page (make-memory 256) #xC0 (make-byte-page)) #xC0))
              "allocating a byte page ensures that get will return this byte page")
  (check-eq? (get-memory-page (make-memory 256) #xC0) nil-page
             "unallocated pages eq nil-page")
  (check-eq? (get-memory-page (free-memory-page (alloc-memory-page (make-memory 256) #xC0 (make-byte-page)) #xC0) #xC0) nil-page
             "allocated, then freed pages eq nil-page (again)"))

;; write A-cell into A-cell-REF using/within A-memory
(define/contract (write-into-cell-ref a-memory a-cell-ref a-cell)
  (-> memory? cell-bound-reference? cell? memory?)
  (define vpage
    (get-memory-page a-memory (cell-bound-reference-page-no a-cell-ref)))
  (cond ((and (byte-cell? a-cell) (byte-page? vpage))
         (memory-set-page a-memory
                          (cell-bound-reference-page-no a-cell-ref)
                          (write-byte-into-page vpage (cell-bound-reference-index a-cell-ref) (byte-cell-byte a-cell))))
        (#t (raise-user-error "memory page not matching cell or unknown implementation"))))

;; push A-cell onto A-eval-stack
(define/contract (push-cell a-eval-stack a-cell)
  (-> eval-stack? cell? eval-stack?)
  (eval-stack (cons a-cell (eval-stack-cells a-eval-stack))))

;; write the top of A-eval-stack into A-cell-REF using/within A-memory
(define/contract (tos-cell-into a-memory a-eval-stack a-cell-ref)
  (-> memory? eval-stack? cell-bound-reference? memory?)
  (write-into-cell-ref a-memory a-cell-ref (car (eval-stack-cells a-eval-stack))))

;; pop the top from A-eval-stack
(define/contract (pop-cell a-eval-stack)
  (-> eval-stack? eval-stack?)
  (eval-stack (cdr (eval-stack-cells a-eval-stack))))

;; get the cell of the top of A-eval-stack
(define/contract (top-of-stack-cell a-eval-stack)
  (-> eval-stack? cell?)
  (car (eval-stack-cells a-eval-stack)))

(module+ test #| push-cell, pop-cell, top-of-stack-cell, pop-cell-into |#
  (check-equal? (byte-cell-byte
                 (top-of-stack-cell
                  (push-cell (eval-stack '()) (byte-cell 0 #xCD))))
                #xCD)
  (check-equal? (byte-cell-byte
                 (top-of-stack-cell
                  (push-cell (push-cell (eval-stack '()) (byte-cell 0 #xCD)) (byte-cell 0 #x23))))
                #x23)
  (check-equal? (byte-cell-byte
                 (top-of-stack-cell
                  (pop-cell (push-cell (push-cell (eval-stack '()) (byte-cell 0 #xCD)) (byte-cell 0 #x23)))))
                #xCD))


(module+ test #| write-into-cell-ref |#
  (check-equal? (read-byte-from-page
                 (get-memory-page
                  (write-into-cell-ref (alloc-memory-page (make-memory 2) 1 (make-byte-page))
                                        (cell-bound-reference 0 1 100)
                                        (byte-cell 0 #x37))
                  1)
                 100)
                #x37))

;; skeleton definitions (without implementation

;; dereference A-cell-REFERENCE
(define (dereference-cell a-memory a-cell-reference)
  (-> memory? cell-bound-reference? cell?)
  (define page (get-memory-page a-memory (cell-bound-reference-page-no a-cell-reference)))
  (cond ((byte-page? page)
         (read-byte-cell-from-page page (cell-bound-reference-index a-cell-reference)))
        (#t (raise-user-error "unknown memory page type"))))

(module+ test #| dereference-cell |#
  (check-equal? (byte-cell-byte
                 (dereference-cell
                  (write-into-cell-ref (alloc-memory-page (make-memory 2) 1 (make-byte-page))
                                        (cell-bound-reference 0 1 100)
                                        (byte-cell 0 #x37))
                  (cell-bound-reference 0 1 100)))
                #x37))

(define/contract (deref-pc a-mem a-pc (rel 0))
  (->* (memory? byte-mem-reference?) (integer?) byte?)
  (define real-pc (inc-byte-mem-ref a-pc rel))
  (define page (get-memory-page a-mem (byte-mem-reference-page-no real-pc)))
  (nth (byte-page-data page) (byte-mem-reference-index real-pc)))

(module+ test #| deref-pc |#
  (check-equal?  (deref-pc
                  (alloc-memory-page (make-memory 10) 5 (write-bytes-into-page (make-byte-page) 2 '(#x21 #x4e #xc3)))
                  (byte-mem-reference 5 3))
                 #x4e)
  (check-equal?  (deref-pc
                  (alloc-memory-page (make-memory 10) 5 (write-bytes-into-page (make-byte-page) 2 '(#x21 #x4e #xc3)))
                  (byte-mem-reference 5 3)
                  1)
                 #xc3)
  (check-equal?  (deref-pc
                  (alloc-memory-page (make-memory 10) 5 (write-bytes-into-page (make-byte-page) 2 '(#x21 #x4e #xc3)))
                  (byte-mem-reference 5 3)
                  -1)
                 #x21))

(struct env
  (map-stack)
  #:guard (struct-guard/c (listof hash?)))

(define/contract (env-push-frame an-env a-map)
  (-> env? hash? env?)
  (env (cons a-map (env-map-stack an-env))))

(define/contract (env-pop-frame an-env)
  (-> env? env?)
  (env (cdr (env-map-stack an-env))))

(define/contract (env-resolve an-env key)
  (-> env? any/c cell?)
  (if (empty? (env-map-stack an-env))
      nil-reference
      (let ((top-frame (car (env-map-stack an-env))))
        (cond ((hash-has-key? top-frame key)
               (hash-ref top-frame key))
              (#t (env-resolve (env (cdr (env-map-stack an-env))) key))))))

(module+ test #| env |#
  (check-eq?  (env-resolve (env '()) 'unknown-key)
              nil-reference)
  (check-true  (nil-ref? (env-resolve (env '()) 'unknown-key)))
  (check-equal?
   (byte-cell-byte (env-resolve (env-push-frame (env '()) (hash 'some-key (byte-cell 0 #x3e)))
                                 'some-key))
   #x3e)
  (check-equal?
   (byte-cell-byte (env-resolve
                     (env-push-frame
                      (env-push-frame (env '()) (hash 'some-key (byte-cell 0 #x73)))
                      (hash 'some-key (byte-cell 0 #x3e)))
                     'some-key))
   #x3e
   "second frame shadows key with its cell")
  (check-equal?
   (byte-cell-byte (env-resolve
                     (env-pop-frame
                      (env-push-frame
                       (env-push-frame (env '()) (hash 'some-key (byte-cell 0 #x73)))
                       (hash 'some-key (byte-cell 0 #x3e))))
                     'some-key))
   #x73
   "some key is no longer shadowed by second frame (since it is popped)"))

(struct call-stack
  (code-references)
  #:guard (struct-guard/c (listof byte-mem-reference?)))

(define/contract (call-stack-push-pc a-call-stack a-pc)
  (-> call-stack? byte-mem-reference? call-stack?)
  (call-stack (cons a-pc (call-stack-code-references a-call-stack))))

(define/contract (call-stack-pop a-call-stack a-pc)
  (-> call-stack? byte-mem-reference? call-stack?)
  (call-stack (cdr (call-stack-code-references a-call-stack))))

(define/contract (call-stack-get-return a-call-stack)
  (-> call-stack? byte-mem-reference?)
  (car (call-stack-code-references a-call-stack)))

(struct vm-state
  (eval-stack ;; cell stack
   call-stack  ;; stack with return addresses
   memory      ;; pages
   env         ;; nested environments of int|key->cell (references)
   pc)         ;; current program counter (index into memory)
  #:guard (struct-guard/c eval-stack? call-stack? memory? env? byte-mem-reference?))

(define vm-op-codes
  (hash #x00 'break   ;; no operand
        #x01 'push_b  ;; byte cell
        #x02 'pop     ;;
        #x03 'add))   ;; pop 2 cells, add them, then push

(define vm-code-ops
  (apply hash (flatten (map (lambda (pair) (list (cdr pair) (car pair))) (hash->list vm-op-codes)))))

(define/contract (vm-interpret-push_b a-vm)
  (-> vm-state? vm-state?)
  (struct-copy vm-state a-vm
               [eval-stack (push-cell (vm-state-eval-stack a-vm) (byte-cell 0 (deref-pc (vm-state-memory a-vm) (vm-state-pc a-vm) 1)))]
               [pc (inc-byte-mem-ref (vm-state-pc a-vm) 2)]))

(define/contract (vm-interpret-pop a-vm)
  (-> vm-state? vm-state?)
  (struct-copy vm-state a-vm
               [eval-stack (pop-cell (vm-state-eval-stack a-vm))]
               [pc (inc-byte-mem-ref (vm-state-pc a-vm))]))

(define/contract (vm-interpret-add a-vm)
  (-> vm-state? vm-state?)
  (define val-a (top-of-stack-cell (vm-state-eval-stack a-vm)))
  (define stack1 (pop-cell (vm-state-eval-stack a-vm)))
  (define val-b (top-of-stack-cell stack1))
  (define stack2 (pop-cell stack1))
  (define result (bitwise-and #xff (+ (byte-cell-byte val-a) (byte-cell-byte val-b))))
  (struct-copy vm-state a-vm
               [eval-stack (push-cell stack2 (byte-cell 0 result))]
               [pc (inc-byte-mem-ref (vm-state-pc a-vm))]))

;; create a virtual machine with just page 0 being allocated as byte page
(define/contract (make-vm)
  (-> vm-state?)
  (vm-state
   (eval-stack '())
   (call-stack '())
   (alloc-memory-page (make-memory #xff) 0 (make-byte-page))
   (env '())
   (byte-mem-reference 0 0)))

;; load the given code into the vm 
(define/contract (load-initial-code a-vm bytes (mem-ref (byte-mem-reference 0 0)))
  (->* (vm-state? (listof (or/c symbol? byte?))) (byte-mem-reference?) vm-state?)
  (define real-bytes (map (lambda (b-or-s)
                            (cond ((symbol? b-or-s)
                                   (hash-ref vm-code-ops b-or-s))
                                  (#t b-or-s)))
                          bytes))
  (struct-copy vm-state a-vm
               [memory
                (write-bytes-into-memory (vm-state-memory a-vm) mem-ref real-bytes)]
               [pc mem-ref]))

;; run the current vm until a break instruction is encountered
(define/contract (run-vm a-vm)
  (-> vm-state? vm-state?)
  (define instruction (deref-pc (vm-state-memory a-vm) (vm-state-pc a-vm)))
  (define instruction-symbol (hash-ref vm-op-codes instruction))
  (cond ((eq? 'break instruction-symbol)
         a-vm)
        ((eq? 'push_b instruction-symbol)
         (run-vm (vm-interpret-push_b a-vm)))
        ((eq? 'pop instruction-symbol)
         (run-vm (vm-interpret-pop a-vm)))
        ((eq? 'add instruction-symbol)
         (run-vm (vm-interpret-add a-vm)))
        (#t (raise-user-error "unknown opcode ~a" instruction-symbol))))

(module+ test #| vm |#
  (check-equal? 
   (byte-cell-byte
    (top-of-stack-cell
     (vm-state-eval-stack
      (run-vm
       (load-initial-code (make-vm)
                          '(push_b #x10
                            push_b #x12
                            add
                            break)
                          (byte-mem-reference 0 2))))))
   #x22))
