#lang typed/racket

(require (only-in racket/fixnum fx+ fx= fx< fx<= fx- fx>= fx>))

(require (only-in racket/hash hash-union))

(require (only-in "./util.rkt" nested->list low-byte high-byte bytes->int))
(require "./svm-ast.rkt")
(require "./svm-parse.rkt")

(require (only-in "./svm-structures.rkt"
                  disassemble-byte-code
                  make-vm
                  CONS
                  CAR
                  CDR
                  GOTO
                  RET
                  BYTE+
                  BRA
                  CALL
                  NIL?
                  TAIL_CALL

                  PUSH_INT
                  PUSH_ARRAY_FIELD
                  PUSH_BYTE
                  PUSH_NIL
                  PUSH_LOCAL
                  PUSH_GLOBAL
                  PUSH_STRUCT_FIELD
                  PUSH_PARAM

                  POP_TO_PARAM
                  POP_TO_LOCAL
                  POP_TO_GLOBAL

                  TRUE
                  FALSE

                  cell-byte--value

                  sPUSH_PARAMc
                  sPUSH_PARAMn
                  sPOP_TO_PARAMc
                  sPOP_TO_PARAMn

                  sNIL?-RET-PARAMc))

(module+ test #| require test utils |#
  (require typed/rackunit))

(define (generate-push (reg-ref : register-ref-)) : (Immutable-Vectorof Byte)
  (define idx (register-ref--index reg-ref))
  (case (register-ref--register reg-ref)
    [(Param)  (cond [(fx<= idx sPUSH_PARAMn) (vector-immutable (sPUSH_PARAMc idx))]
                     [(byte? idx) (vector-immutable PUSH_PARAM idx)]
                     [else (raise-user-error (format "index too large for push param ~a" idx))])]
    [(Local)  (cond [(byte? idx) (vector-immutable PUSH_LOCAL idx)]
                     [else (raise-user-error (format "index too large for push local ~a" idx))])]
    [(Global) (cond [(byte? idx) (vector-immutable PUSH_GLOBAL idx)]
                     [else (raise-user-error (format "index too large for push global ~a" idx))])]
    [else (raise-user-error (format "unknown register ref ~a" reg-ref))]))

(module+ test #| generate push |#
  (check-equal? (generate-push (register-ref- 'Param 0))
                (vector-immutable (sPUSH_PARAMc 0)))

  (check-equal? (generate-push (register-ref- 'Param 3))
                (vector-immutable (sPUSH_PARAMc 3)))

  (check-equal? (generate-push (register-ref- 'Param 4))
                (vector-immutable PUSH_PARAM 4)))

(define (generate-pop-to (reg-ref : register-ref-)) : (Immutable-Vectorof Byte)
  (define idx (register-ref--index reg-ref))
  (case (register-ref--register reg-ref)
    [(Param)  (cond [(fx<= idx sPOP_TO_PARAMn) (vector-immutable (sPOP_TO_PARAMc idx))]
                     [(byte? idx) (vector-immutable POP_TO_PARAM idx)]
                     [else (raise-user-error (format "index too large for pop to param ~a" idx))])]
    [(Local)  (cond [(byte? idx) (vector-immutable POP_TO_LOCAL idx)]
                     [else (raise-user-error (format "index too large for pop to local ~a" idx))])]
    [(Global) (cond [(byte? idx) (vector-immutable POP_TO_GLOBAL idx)]
                     [else (raise-user-error (format "index too large for pop to global ~a" idx))])]
    [else (raise-user-error (format "unknown register ref ~a" reg-ref))]))

(module+ test #| generate pop to |#
  (check-equal? (generate-pop-to (register-ref- 'Param 0))
                (vector-immutable (sPOP_TO_PARAMc 0)))

  (check-equal? (generate-pop-to (register-ref- 'Param 4))
                (vector-immutable POP_TO_PARAM 4)))



;; do generation for stack machine
;; goal: transform the reverse function into stack machine code

;; compiler passes 0 1 2 .. n <- just increment and use it in the ast-node?
;; n = generate all information necessary to
;;       - execute in vm
;;       - write into file (loadable by runtime / vm)
;; all passes before that will (just) rewrite the ast



;; ids should be resolved at some pass
;; e.g. id -> param#, id -> global#, id -> local#

(define (gen-atom (atom : ast-ex-atomic-)) : (Immutable-Vectorof Byte)
  (cond
    [(ast-at-int-? atom)
     (define value (ast-at-int--value atom))
     (vector-immutable PUSH_INT (low-byte value) (high-byte value))]
    [(ast-at-bool-? atom)
     (vector-immutable PUSH_BYTE (if (ast-at-bool--bool atom) (cell-byte--value TRUE) (cell-byte--value FALSE)))]
    [(ast-at-id-? atom)
     (define reg-ref (hash-ref (ast-info--id-map (ast-node--info atom)) (ast-at-id--id atom)))
     (generate-push reg-ref)]
    [(ast-at-string-? atom)
     (raise-user-error "string not implemented yet")]
    [else (raise-user-error (format "unknown atomic value ~a" atom))]))

(module+ test #| gen-atom |#
  (check-equal? (gen-atom (ast-at-id- (make-ast-info #:id-map (hash 'some (register-ref- 'Param 2))) 'some))
                (vector-immutable (sPUSH_PARAMc 2)))

  (check-equal? (gen-atom (ast-at-id- (make-ast-info #:id-map (hash 'some (register-ref- 'Param 10))) 'some))
                (vector-immutable PUSH_PARAM 10))

  (check-exn exn:fail? (lambda () (gen-atom (ast-at-id- (make-ast-info #:id-map (hash 'some (register-ref- 'Param 10))) 'unknown))))

  (check-equal? (gen-atom (ast-at-int- (make-ast-info) #x02fe))
                (vector-immutable PUSH_INT #xfe #x02))

  (check-equal? (gen-atom (ast-at-bool- (make-ast-info) #t))
                (vector-immutable PUSH_BYTE (cell-byte--value TRUE))))

(define (svm-resolve-ids (node : ast-node-) (id-map : (Immutable-HashTable Symbol register-ref-))) : ast-node-
  (cond
    [(ast-ex-with-? node) node] ;; TODO: add new ids for locals, has expressions to resolve
    [(ast-ex-cond-? node) node] ;; TODO: has expressions to resolve
    [(ast-ex-if-? node) node]   ;; TODO: has expressions to resolve
    [(ast-ex-fun-call-? node) node] ;; TODO: has expressions to resolve

    [(ast-at-id-? node)
     (struct-copy ast-at-id- node
                  [info #:parent ast-node-
                        (struct-copy ast-info- (ast-node--info node)
                                     [id-map (hash (ast-at-id--id node) (hash-ref id-map (ast-at-id--id node)))])] )]

    [(ast-pa-defaulted-def-? node) node] ;; has expression to resolve

    [(ast-ex-fun-def-? node)
     (define ids (append (map (lambda (param) (ast-param-def--id param)) (ast-ex-fun-def--parameter node))
                         (map (lambda (d-param) (ast-param-def--id d-param)) (ast-ex-fun-def--def-params node))))
     (define id-offset-pairs (map (lambda: ((a : Symbol) (b : Nonnegative-Integer))
                                    (define idx (- (length ids) b 1))
                                    (if (>= idx 0)
                                        (cons a (register-ref- 'Param idx))
                                        (raise-user-error "generating param idx hash failed")))
                                  ids (range (length ids))))
     (define id-reg-map (make-hash id-offset-pairs))
     (define new-body (svm-resolve-ids (ast-ex-fun-def--body node) (hash-union id-map id-reg-map)))
     (struct-copy ast-ex-fun-def- node
                  [body (if (ast-expression-? new-body) new-body (raise-user-error "expected new-body to be of type ast-expression-"))])]
    [else node]))

(module+ test #| svm-resolve-ids |#
  (check-true
   (match (nested->list (svm-resolve-ids (m-fun-def (some (p0 int) (p1 bool #t) -> bool)
                                                   p1) (hash)))
     [(list _ ... (list ast-at-id- (list ast-info- _ ... (list 'hash 'p1 (list register-ref- 'Param 0))) 'p1)) #t]
     [_ #f])
   "p1 (second parameter) has offset 0, looking from the end of the list")

  (check-true
   (match (nested->list (svm-resolve-ids (m-fun-def (some (p0 int) (p1 bool #t) -> bool)
                                                   p0) (hash)))
     [(list _ ... (list ast-at-id- (list ast-info- _ ... (list 'hash 'p0 (list register-ref- 'Param 1))) 'p0)) #t]
     [_ #f])
   "p0 (first parameter) has offset 1, looking from the end of the list"))

;; optimization pattern:
;;   top-level-expr: if (nil? anything) <- can be negated
;;                      param/local <- then/else can be interchanged
;;                      tail-call <- in TC-position
;;   can be generated optimized with
;;     PUSH anything
;;     sNIL?-RET_PARAMc / sNIL?-RET-LOCALc
;;     <else ...>
;;     TAIL_CALL

(define (svm-generate-function (def : ast-ex-fun-def-)) : (Immutable-Vectorof Byte)
  ;; TODO: currently just a dummy implementation
  (vector-immutable 129 156 129 41 128 129 40 42 35))

(module+ test #| generate code for simple reverse function |#
  (check-equal?
   (svm-generate-function
    (m-fun-def (reverse (a-list cell*) (b-list cell* '()) -> cell*
                        "reverse a-list, consing it into b-list")
               (if (nil? a-list)
                   b-list
                   (reverse (cdr a-list) (cons (car a-list) b-list)))))
   (vector-immutable (sPUSH_PARAMc 1)
                     (sNIL?-RET-PARAMc 0)
                     (sPUSH_PARAMc 1)
                     CDR
                     (sPUSH_PARAMc 0)
                     (sPUSH_PARAMc 1)
                     CAR
                     CONS
                     TAIL_CALL)))
