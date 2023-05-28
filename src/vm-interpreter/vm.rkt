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
  ()
  #:transparent)

(struct value-nil-reference value-reference
  ()
  #:transparent)

(define nil-reference (value-nil-reference))

(define/contract (nil-ref? a-val-ref)
  (-> value-reference? boolean?)
  (eq? a-val-ref nil-reference))

(module+ test #| nil |#
  (check-true (nil-ref? nil-reference)
              "only the defined nil-reference is nil")
  (check-false (nil-ref? (value-nil-reference))
               "another instance of value-nil-reference is not nil!"))

(struct value-bound-reference value-reference
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
  (-> memory? value-bound-reference? value? memory?)
  (define vpage
    (get-memory-page a-memory (value-bound-reference-page-no a-value-ref)))
  (cond ((and (byte-value? a-value) (byte-page? vpage))
         (memory (set-nth (memory-pages a-memory)
                          (value-bound-reference-page-no a-value-ref)
                          (write-byte vpage (value-bound-reference-index a-value-ref) (byte-value-byte a-value)))
                 (memory-count a-memory)))
        (#t (raise-user-error "memory page not matching value or unknown implementation"))))

;; push A-VALUE onto A-VALUE-STACK
(define/contract (push-value a-value-stack a-value)
  (-> value-stack? value? value-stack?)
  (value-stack (cons a-value (value-stack-values a-value-stack))))

;; write the top of A-VALUE-STACK into A-VALUE-REF using/within A-memory
(define/contract (tos-value-into a-memory a-value-stack a-value-ref)
  (-> memory? value-stack? value-bound-reference? memory?)
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
                                        (value-bound-reference 1 100)
                                        (byte-value #x37))
                  1)
                 100)
                #x37))

;; skeleton definitions (without implementation

;; dereference A-VALUE-REFERENCE
(define (dereference-value a-memory a-value-reference)
  (-> memory? value-bound-reference? value?)
  (define page (get-memory-page a-memory (value-bound-reference-page-no a-value-reference)))
  (cond ((byte-page? page)
         (read-byte-value page (value-bound-reference-index a-value-reference)))
        (#t (raise-user-error "unknown memory page type"))))

(module+ test #| dereference-value |#
  (check-equal? (byte-value-byte
                 (dereference-value
                  (write-into-value-ref (alloc-memory-page (make-memory 2) 1 (make-byte-page))
                                        (value-bound-reference 1 100)
                                        (byte-value #x37))
                  (value-bound-reference 1 100)))
                #x37))

(struct code-reference
  (page-no
   index)
  #:transparent
  #:guard (struct-guard/c nonnegative-integer? byte?))

(define/contract (inc-pc a-pc (delta 1))
  (->* (code-reference?) (integer?) code-reference?)
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
  (-> env? any/c value?)
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
   (byte-value-byte (env-resolve (env-push-frame (env '()) (hash 'some-key (byte-value #x3e)))
                                 'some-key))
   #x3e)
  (check-equal?
   (byte-value-byte (env-resolve
                     (env-push-frame
                      (env-push-frame (env '()) (hash 'some-key (byte-value #x73)))
                      (hash 'some-key (byte-value #x3e)))
                     'some-key))
   #x3e
   "second frame shadows key with its value")
  (check-equal?
   (byte-value-byte (env-resolve
                     (env-pop-frame
                      (env-push-frame
                       (env-push-frame (env '()) (hash 'some-key (byte-value #x73)))
                       (hash 'some-key (byte-value #x3e))))
                     'some-key))
   #x73
   "some key is no longer shadowed by second frame (since it is popped)"))

(struct call-stack
  (code-references)
  #:guard (struct-guard/c (listof code-reference?)))

(define/contract (call-stack-push-pc a-call-stack a-pc)
  (-> call-stack? code-reference? call-stack?)
  (call-stack (cons a-pc (call-stack-code-references a-call-stack))))

(define/contract (call-stack-pop a-call-stack a-pc)
  (-> call-stack? code-reference? call-stack?)
  (call-stack (cdr (call-stack-code-references a-call-stack))))

(define/contract (call-stack-get-return a-call-stack)
  (-> call-stack? code-reference?)
  (car (call-stack-code-references a-call-stack)))

(struct vm-state
  (value-stack ;; values
   call-stack  ;; stack with return addresses
   memory      ;; pages
   env         ;; nested environments of int->value references
   pc)         ;; current program counter (index into memory)
  #:guard (struct-guard/c value-stack? call-stack? memory? env? code-reference?))

(define vm-op-codes
  (hash #x00 'break   ;; no operand
        #x01 'push_b  ;; byte value
        #x02 'pop     ;;
        #x03 'add))   ;; pop 2 values, add them, then push

(define vm-code-ops
  (apply hash (flatten (map (lambda (pair) (list (cdr pair) (car pair))) (hash->list vm-op-codes)))))

(define/contract (vm-interpret-push_b a-vm)
  (-> vm-state? vm-state?)
  (struct-copy vm-state a-vm
               [value-stack (push-value (vm-state-value-stack a-vm) (byte-value (deref-pc (vm-state-memory a-vm) (vm-state-pc a-vm) 1)))]
               [pc (inc-pc (vm-state-pc a-vm) 2)]))

(define/contract (vm-interpret-pop a-vm)
  (-> vm-state? vm-state?)
  (struct-copy vm-state a-vm
               [value-stack (pop-value (vm-state-value-stack a-vm))]
               [pc (inc-pc (vm-state-pc a-vm))]))

(define/contract (vm-interpret-add a-vm)
  (-> vm-state? vm-state?)
  (define val-a (top-of-stack-value (vm-state-value-stack a-vm)))
  (define stack1 (pop-value (vm-state-value-stack a-vm)))
  (define val-b (top-of-stack-value stack1))
  (define stack2 (pop-value stack1))
  (define result (bitwise-and #xff (+ (byte-value-byte val-a) (byte-value-byte val-b))))
  (struct-copy vm-state a-vm
               [value-stack (push-value stack2 (byte-value result))]
               [pc (inc-pc (vm-state-pc a-vm))]))

(define/contract (make-vm)
  (-> vm-state?)
  (vm-state
   (value-stack '())
   (call-stack '())
   (alloc-memory-page (make-memory #xff) 0 (make-byte-page))
   (env '())
   (code-reference 0 0)))

(define/contract (load-initial-code a-vm bytes)
  (-> vm-state? (listof (or/c symbol? byte?)) vm-state?)
  (define real-bytes (map (lambda (b-or-s)
                            (cond ((symbol? b-or-s)
                                   (hash-ref vm-code-ops b-or-s))
                                  (#t b-or-s)))
                          bytes))
  (struct-copy vm-state a-vm
               [memory
                (memory
                 (set-nth (memory-pages (vm-state-memory a-vm))
                          0 ;; page no
                          (write-bytes (get-memory-page (vm-state-memory a-vm) 0) 0 real-bytes))
                 (memory-count (vm-state-memory a-vm)))]))

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
         (run-vm (vm-interpret-add a-vm)))))

(module+ test #| vm |#
  (check-equal? 
   (byte-value-byte
    (top-of-stack-value
     (vm-state-value-stack
      (run-vm
       (load-initial-code (make-vm)
                          '(push_b #x10
                            push_b #x12
                            add
                            break))))))
   #x22))
