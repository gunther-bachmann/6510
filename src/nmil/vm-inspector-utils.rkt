#lang racket/base

(provide vm-cell-at-nil?
         vm-stack->strings
         vm-cell-at->string
         vm-cell->string
         vm-page->strings
         vm-regt->string
         vm-regp->string
         vm-rega->string
         vm-regb->string
         vm-regc->string
         vm-regz->string
         vm-deref-cell-pair-w->string
         vm-deref-cell-w->string
         shorten-cell-string
         shorten-cell-strings

         vm-slot->string)

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
                  ZP_RB
                  ZP_RC
                  ZP_RP
                  ZP_RZ
                  ZP_EVAL_STACK_TAIL_TOP
                  ZP_EVAL_STACK_TAIL_LB_PTR
                  ZP_EVAL_STACK_TAIL_HB_PTR))

(module+ test
  (require "../6510-test-utils.rkt"
           "../6510.rkt"))

;; write a status string of a memory page
(define (vm-page->strings state page)
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
   #px"(ptr(\\[[-0-9]*\\])? (\\$[0-9A-Fa-f]*)?|int \\$0{0,3})"
   str
   ""))

(define (shorten-cell-strings strings)
  (map shorten-cell-string strings))

;; produce strings describing the current cell-stack status
(define (vm-stack->strings state (max-count 10) (follow #f))
  (define stack-tos-idx (peek state ZP_EVAL_STACK_TAIL_TOP))
  (define stack-lb-page-start (peek-word-at-address state ZP_EVAL_STACK_TAIL_LB_PTR))
  (define stack-hb-page-start (peek-word-at-address state ZP_EVAL_STACK_TAIL_HB_PTR))
  (cond
    [(and (= stack-tos-idx #x01)
        (= 0 (peek state (add1 stack-lb-page-start)))) (list "stack is empty or tos=nil")]
    [else
     (define values-count (min (- stack-tos-idx 1) max-count))
     (define low-bytes (memory-list state (+ stack-lb-page-start (add1 (- stack-tos-idx values-count))) (+ stack-lb-page-start stack-tos-idx)))
     (define high-bytes (memory-list state (+ stack-hb-page-start (add1 (- stack-tos-idx values-count))) (+ stack-hb-page-start stack-tos-idx)))
     (define stack-item-no (+ values-count 1))
     (define stack-strings (reverse (map (lambda (pair) (vm-cell->string (car pair) (cdr pair) state follow)) (map cons low-bytes high-bytes))))
     (cons (format "stack holds ~a ~a" stack-item-no (if (= 1 stack-item-no) "item" "items"))
           (cons (format "~a  (rt)" (vm-regt->string state follow)) stack-strings))]))

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
  (define derefed-word-car (peek-word-at-address state (+ 2 word)))
  (define derefed-word-cdr (peek-word-at-address state (+ 4 word)))
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

(define (vm-cell-w->string word (state '()) (follow #f) (visited (list)))
  (vm-cell->string (low-byte word) (high-byte word) state follow visited))

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

;; get the refcount of a cell (either on cell page, or on m1 page)
(define (refcount-of-cell-n state low high)
  (define page-type (peek state (bytes->int 0 high)))
  (define rc-offset
    (cond [(= #x20 (bitwise-and page-type #xf0)) ;; m1 page
           low]
          [else (raise-user-error (format "unknown page type ~a" page-type))]))
  (peek state (bytes->int rc-offset high)))

(define (vm-slot->string state loc (follow #f) (visited (list)))
  (cond
    [(memq loc visited)
     (format "RECURSION->$~a~a" (format-hex-word loc))]
    [else
     (define refcnt (peek state loc))
     (define slot-type (peek state (+ 1 loc)))
     (cond
       [(= 0 (bitwise-and #xc0 slot-type)) ;; cell-array
        (define array-len (bitwise-and #x3f (peek state (+ 1 loc))))
        (format "ptr[~a] cell-array of len ~a" refcnt array-len)]
       [(= #x80 (bitwise-and #x80 slot-type))
        (define array-len (bitwise-and #x7f (peek state (+ 1 loc))))
        (format "ptr[~a] native-array of len ~a" refcnt array-len)]
       [else (format "ptr[~a] with unknown slot type ~a" refcnt slot-type)])]))

;; write decoded cell described by low high
;; the low 2 bits are used for pointer tagging
(define (vm-cell->string low high (state '()) (follow #f) (visited (list)))
  (cond
    [(memq (bytes->int low high) visited)
     (format "RECURSION->$~a~a" (format-hex-byte high) (format-hex-byte low))]
    [(= 0 low) "ptr NIL"]
    [(= 0 (bitwise-and #x01 low))
     (string-append
      (format "ptr[~a] $~a~a"
              (if (empty? state) "-" (refcount-of-cell-n state low high))
              (format-hex-byte high)
              (format-hex-byte (bitwise-and #xfe low)))
      (if follow
          (vm-deref-cell-pair->string state low high #t (cons (bytes->int low high) visited))
          ""))]
    [(= 3 (bitwise-and #x03 low)) (format "int $~a~a"
                                          (format-hex-byte (arithmetic-shift low -2))
                                          (format-hex-byte high))]
    [(= TAG_BYTE_BYTE_CELL (bitwise-and #xff low)) (format "byte $~a" (format-hex-byte high))]
    ;; TODO: a structure has a special value + follow bytes
    ;; (= ? (bitwise-and #xfc low)) e.g. #x04 = structure, high byte = number of fields
    ;; the following number of fields * cells cannot be structure cells, but only atomic or pointer cells
    [else "?"]))

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

;; write string of current RA
(define (vm-rega->string state)
  (vm-cell->string
   (peek state ZP_RA)
   (peek state (add1 ZP_RA))
   state))

(define (vm-regb->string state)
  (vm-cell->string
   (peek state ZP_RB)
   (peek state (add1 ZP_RB))
   state))

(define (vm-regc->string state)
  (vm-cell->string
   (peek state ZP_RC)
   (peek state (add1 ZP_RC))
   state))

(define (vm-regp->string state)
  (vm-cell->string
   (peek state ZP_RP)
   (peek state (add1 ZP_RP))
   state))

(define (vm-regz->string state)
  (vm-cell->string
   (peek state ZP_RZ)
   (peek state (add1 ZP_RZ))
   state))

(module+ test #| vm-cell->strings |#
  (check-equal? (vm-cell->string #xc4 #xc0)
                "ptr[-] $c0c4")
  (check-equal? (vm-cell->string #x02 #xc0)
                "ptr[-] $c002")
  (check-equal? (vm-cell->string #xfb #x15)
                "int $3e15")
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
  (check-equal? (vm-cells->strings '(#x00 #x00 #x03 #x01))
                '("ptr NIL" "int $0001")))
