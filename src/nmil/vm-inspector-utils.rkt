#lang racket/base

#|

  provide functions to inspect the current status of the virtual machine

|#

(require (only-in racket/list
                  flatten
                  take
                  empty?
                  drop
                  make-list)
         (only-in "../tools/6510-interpreter.rkt"
                  memory-list
                  peek-word-at-address
                  peek)
         (only-in "../util.rkt"
                  bytes->int
                  low-byte
                  high-byte
                  format-hex-byte
                  format-hex-word)
         (only-in "vm-runtime/vm-memory-map.rkt"
                  TAG_BYTE_BYTE_CELL
                  TAG_BYTE_CELL_ARRAY
                  TAGGED_NIL
                  ZP_RT
                  ZP_RA
                  ZP_RP
                  ZP_CELL_STACK_TOS
                  ZP_CELL_STACK_LB_PTR
                  ZP_CELL_STACK_HB_PTR))

(provide vm-cell-at-nil?
         vm-stack->strings
         vm-cell-at->string
         vm-cell->string
         vm-page->strings
         vm-page-n->strings
         vm-regt->string
         vm-regp->string
         vm-rega->string
         vm-deref-cell-pair-w->string
         vm-deref-cell-pair->string
         vm-deref-cell-w->string
         vm-refcount-cell-pair-ptr
         vm-refcount-cell-ptr
         vm-cell-pair-free-tree->string
         shorten-cell-string
         shorten-cell-strings)

(module+ test
  (require "../6510-test-utils.rkt"
           "../6510.rkt"))

;; write out the cells that are marked as reallocatable
(define (vm-cell-pair-free-tree->string state)
  (define cell-pair-root (peek-word-at-address state #xcec5))
  (cond
    [(= 0 (bitwise-and #xff00 cell-pair-root)) "root is initial"]
    [else
     (format "pair $~a -> [ ~a . ~a ]"
             (format-hex-word cell-pair-root)
             (vm-cell-w->string (peek-word-at-address state cell-pair-root))
             (vm-cell-w->string (peek-word-at-address state (+ 2 cell-pair-root))))]))

;; write a status string of a memory page
(define (vm-page->strings state page)
  (define page-type-enc (peek state (bytes->int 0 page)))
  (define next-free-slot (peek state (bytes->int page #xcf)))
  (define page-type
    (cond
      [(= #x10 (bitwise-and #xf8 page-type-enc))
       (format "m1 page p~a" (bitwise-and #x07 page-type-enc))]
      [(= #x80 (bitwise-and #x80 page-type-enc))
       "cell page"]
      [(= #x40 (bitwise-and #xc0 page-type-enc))
       "cell-pair page"]
      [(= #x20 (bitwise-and #xe0 page-type-enc))
       "s8 page"]
      [(= #x18 page-type-enc)
       "call-frame page"]
      [else (raise-user-error "unknown page type")]))
  (define previous-page
    (cond
      [(not (= 0 (bitwise-and #xc0 page-type-enc)))
       (peek state (bytes->int #xff page))]
      [else (peek state (bytes->int 1 page))]))
  (define slots-used
    (cond
      [(= #x10 (bitwise-and #xf0 page-type-enc))
       (peek state (bytes->int 2 page))]
      [(= #x80 (bitwise-and #x80 page-type-enc))
       (bitwise-and #x7f page-type-enc)]
      [(= #x40 (bitwise-and #xc0 page-type-enc))
       (bitwise-and #x3f page-type-enc)]
      [(= #x20 (bitwise-and #xe0 page-type-enc))
       (bitwise-and #x1f page-type-enc)]
      [else 0]
      ))
  (cond [(= #x18 page-type-enc)
         (list (format "page-type:      ~a" page-type)
               (format "previous page:  $~a" (format-hex-byte previous-page)))]
        [else
         (list (format "page-type:      ~a" page-type)
               (format "previous page:  $~a" (format-hex-byte previous-page))
               (format "slots used:     ~a" slots-used)
               (format "next free slot: $~a" (format-hex-byte next-free-slot)))]))

;; write a status string of a memory page
(define (vm-page-n->strings state page)
  (define page-type-enc (peek state (bytes->int 0 page)))
  (define next-free-slot (peek state (bytes->int #xfe page)))
  (define page-type
    (cond
      [(= #x20 (bitwise-and #xf0 page-type-enc))
       (format "m1 page p~a" (bitwise-and #x0f page-type-enc))]
      [(= #x18 page-type-enc)
       "call-frame page"]
      [else (raise-user-error "unknown page type")]))
  (define previous-page
    (cond
      [else (peek state (bytes->int #xff page))]))
  (define slots-used
    (cond
      [(= #x20 (bitwise-and #xf0 page-type-enc))
       (peek state (bytes->int 1 page))]
      [else 0]
      ))
  (cond [(= #x18 page-type-enc)
         (list (format "page-type:      ~a" page-type)
               (format "previous page:  $~a" (format-hex-byte previous-page)))]
        [else
         (list (format "page-type:      ~a" page-type)
               (format "previous page:  $~a" (format-hex-byte previous-page))
               (format "slots used:     ~a" slots-used)
               (format "next free slot: $~a" (format-hex-byte next-free-slot)))]))

;; shorten verbose strings (e.g. pair-ptr cells or int cells)
(define (shorten-cell-string str)
  (regexp-replace*
   #px"(pair-ptr(\\[[-0-9]*\\])? (\\$[0-9A-Fa-f]*)?|int \\$0{0,3})"
   str
   ""))

(define (shorten-cell-strings strings)
  (map shorten-cell-string strings))

;; produce strings describing the current cell-stack status
(define (vm-stack->strings state (max-count 10) (follow #f))
  (define stack-tos-idx (peek state ZP_CELL_STACK_TOS))
  (define stack-lb-page-start (peek-word-at-address state ZP_CELL_STACK_LB_PTR))
  (define stack-hb-page-start (peek-word-at-address state ZP_CELL_STACK_HB_PTR))
  (cond
    [(and (regt-empty? state) (= stack-tos-idx #x01) (= 0 (peek state (add1 stack-lb-page-start)))) (list "stack is empty")]
    [else
     (define values-count (min (- stack-tos-idx 1) max-count))
     (define low-bytes (memory-list state (+ stack-lb-page-start (add1 (- stack-tos-idx values-count))) (+ stack-lb-page-start stack-tos-idx)))
     (define high-bytes (memory-list state (+ stack-hb-page-start (add1 (- stack-tos-idx values-count))) (+ stack-hb-page-start stack-tos-idx)))
     (define stack-item-no (+ values-count (if (regt-empty? state) 0 1)))
     (define stack-strings (reverse (map (lambda (pair) (vm-cell->string (car pair) (cdr pair) state follow)) (map cons low-bytes high-bytes))))
     (cons (format "stack holds ~a ~a" stack-item-no (if (= 1 stack-item-no) "item" "items"))
           (if (regt-empty? state)
               (list "stack is empty")
               (cons (format "~a  (rt)" (vm-regt->string state follow)) stack-strings)))]))

;; make a list of adjacent pairs
(define (pairing list (paired-list '()))
  (if (< (length list) 2)
      (reverse paired-list)
      (pairing (drop list 2) (cons `(,(car list) . ,(cadr list)) paired-list))))

(module+ test #| pairing |#
  (check-equal? (pairing '())
                '())
  (check-equal? (pairing '(1 2 3 4 5 6))
                '((1 . 2) (3 . 4) (5 . 6))))

;; write the car, cdr cell of the cell-pair at word in memory
(define (vm-deref-cell-pair-w->string state word (follow #f) (visited (list)))
  (define derefed-word-car (peek-word-at-address state word))
  (define derefed-word-cdr (peek-word-at-address state (+ 2 word)))
  (format "(~a . ~a)"
          (vm-cell-w->string derefed-word-car state follow visited)
          (vm-cell-w->string derefed-word-cdr state follow visited)))

;; derefence word to cell and write cell as string
(define (vm-deref-cell-w->string state word)
  (define derefed-word (peek-word-at-address state word))
  (format "~a" (vm-cell-w->string derefed-word)))

;; write the car, cdr cell of the cell-pair at low/high in memory
(define (vm-deref-cell-pair->string state low high (follow #f) (visited (list)))
  (vm-deref-cell-pair-w->string state (bytes->int low high) follow visited))

;; dereference low high pointing to cell and write it as string
(define (vm-deref-cell->string state low high)
  (vm-deref-cell-w->string state (bytes->int low high)))

;; write decoded cell described by word
(define (vm-cell-w->string word (state '()) (follow #f) (visited (list)))
  (vm-cell->string (low-byte word) (high-byte word) state follow visited))

;; get the refcount of a cell pair
(define (refcount-of-cell-pair state low high)
  (define rc-offset (arithmetic-shift low -2))
  (peek state (bytes->int rc-offset high)))

;; get the refcount of a cell (either on cell page, or on m1 page)
(define (refcount-of-cell state low high)
  (define page-type (peek state (bytes->int 0 high)))
  (define rc-offset
    (cond [(= #x80 (bitwise-and page-type #x80)) ;; cell-ptr-page
           (arithmetic-shift low -1)]
          [(= #x00 (bitwise-and page-type #xec))
           (sub1 low)]
          [else (raise-user-error (format "unknown page type ~a" page-type))]))
  (peek state (bytes->int rc-offset high)))

;; write decoded cell described by low high
;; the low 2 bits are used for pointer tagging
(define (vm-cell->string low high (state '()) (follow #f) (visited (list)))
  (cond
    [(memq (bytes->int low high) visited)
     (format "RECURSION->$~a~a" (format-hex-byte high) (format-hex-byte low))]
    [(= 0 low) "empty"]
    [(= 0 (bitwise-and #x01 low)) (format "ptr[~a] $~a~a"
                                          (if (empty? state) "-" (refcount-of-cell state low high))
                                          (format-hex-byte high)
                                          (format-hex-byte (bitwise-and #xfe low)))]
    [(and (= 1 (bitwise-and #x03 low)) (= high 0)) "pair-ptr NIL"]
    [(= 1 (bitwise-and #x03 low))
     (string-append (format "pair-ptr[~a] $~a~a"
                            (if (empty? state) "-" (refcount-of-cell-pair state low high))
                            (format-hex-byte high)
                            (format-hex-byte (bitwise-and #xfd low)))
                    (if follow
                        (vm-deref-cell-pair->string state low high #t (cons (bytes->int low high) visited))
                        ""))]
    [(= 3 (bitwise-and #x83 low)) (format "int $~a~a"
                                          (format-hex-byte (arithmetic-shift low -2))
                                          (format-hex-byte high))]
    [(= TAG_BYTE_BYTE_CELL (bitwise-and #xff low)) (format "byte $~a" (format-hex-byte high))]
    [(= TAG_BYTE_CELL_ARRAY (bitwise-and #xff low))
     (define array-str (format "cell-array len=$~a" (format-hex-byte high)))
     (if follow
         (format "~a [...]" array-str)
         array-str)]
    ;; TODO: a structure has a special value + follow bytes
    ;; (= ? (bitwise-and #xfc low)) e.g. #x04 = structure, high byte = number of fields
    ;; the following number of fields * cells cannot be structure cells, but only atomic or pointer cells
    [else "?"]))


;; is RT empty?
(define (regt-empty? state)
  (= 0 (peek state ZP_RT)))

;; is cell at the given location = NIL?
(define (vm-cell-at-nil? state loc)
  (= TAGGED_NIL (peek-word-at-address state loc)))

;; print the cell at the given location (reverse endianess)
(define (vm-cell-at->string state loc (rev-endian #f) (follow #f))
  (vm-cell-w->string (peek-word-at-address state loc rev-endian) state follow))

;; write string of current RT
(define (vm-regt->string state (follow #f))
  (vm-cell->string
   (peek state ZP_RT)
   (peek state (add1 ZP_RT))
   state
   follow))

;; get the actual refcount of a cell-pair-ptr
(define (vm-refcount-cell-pair-ptr state cell-pair-ptr)
  (define lowb  (low-byte cell-pair-ptr))
  (define highb (high-byte cell-pair-ptr))
  (define refc  (arithmetic-shift lowb -2))
  (peek state (bytes->int refc highb) ))

;; get the actual refcount of a cell-pair-ptr
(define (vm-refcount-cell-ptr state cell-ptr)
  (define lowb  (low-byte cell-ptr))
  (define highb (high-byte cell-ptr))
  (define refc  (arithmetic-shift lowb -1))
  (peek state (bytes->int refc highb) ))

;; write string of current RA
(define (vm-rega->string state)
  (vm-cell->string
   (peek state ZP_RA)
   (peek state (add1 ZP_RA))
   state))

(define (vm-regp->string state)
  (vm-cell->string
   (peek state ZP_RP)
   (peek state (add1 ZP_RP))
   state))

(module+ test #| vm-cell->strings |#
  (check-equal? (vm-cell->string #xc4 #xc0)
                "ptr[-] $c0c4")
  (check-equal? (vm-cell->string #xc1 #xc0)
                "pair-ptr[-] $c0c1")
  (check-equal? (vm-cell->string #x7b #x15)
                "int $1e15")
  (check-equal? (vm-cell->string TAG_BYTE_BYTE_CELL #x15)
                "byte $15"))

;; transform cells (given by their byte-list) into a list of cells
(define (vm-cells->strings byte-list (result (list)))
  (if (empty? byte-list)
      (reverse result)
      (vm-cells->strings
       (cddr byte-list)
       (cons (vm-cell->string (car byte-list)
                             (cadr byte-list))
             result))))

(module+ test #| vm-cells->strings |#
  (check-equal? (vm-cells->strings '(#x01 #x00 #x03 #x01))
                '("pair-ptr NIL" "int $0001")))

;; (module+ test #| vm-stack->strings |#
;;   (define test-vm_stack_to_string-a-code
;;     (list (JSR PUSH_NIL_TO_EVLSTK)
;;           (JSR PUSH_NIL_TO_EVLSTK)
;;           (LDA !$01)
;;           (LDX !$03)
;;           (JSR PUSH_INT_TO_EVLSTK)
;;           (JSR PUSH_NIL_TO_EVLSTK)))
;;   (define test-vm_stack_to_string-a-state-after
;;     (run-code-in-test test-vm_stack_to_string-a-code))

;;   (check-equal? (vm-stack->strings test-vm_stack_to_string-a-state-after)
;;                 '("stack holds 4 items"
;;                   "pair-ptr NIL  (rt)"
;;                   "int $0301"
;;                   "pair-ptr NIL"
;;                   "pair-ptr NIL")))
