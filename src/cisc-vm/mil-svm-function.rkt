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

;; TODO refactor this method
(: svm-resolve-ids (-> ast-node- (Immutable-HashTable Symbol register-ref-) ast-node-))
(define (svm-resolve-ids node id-map)
  (cond
    [(ast-ex-with-? node)
     (define ids (map (lambda (local) (ast-ex-with-local--id local)) (ast-ex-with--locals node)))
     (define id-offset-pairs (map (lambda: ((a : Symbol) (idx : Nonnegative-Integer))
                                    (cons a (register-ref- 'Local idx)) ;; TODO maybe add offset because of nested with constructs! => need to keep used local idx in parameter of this function! (or do i need this information elsewhere during generation?)
                                    )
                                  ids (range (length ids))))
     (define complete-id-map (hash-union id-map (make-hash id-offset-pairs)))
     (struct-copy ast-ex-with- node
                  [locals (map (lambda: ((local : ast-ex-with-local-) (i : Nonnegative-Integer))
                                 (struct-copy ast-ex-with-local- local
                                              [value (cast (svm-resolve-ids (ast-ex-with-local--value local) (hash-union id-map (make-hash (take id-offset-pairs i)))) ast-expression-)]))
                               (ast-ex-with--locals node) (range (length (ast-ex-with--locals node))))]
                  [body (cast (svm-resolve-ids (ast-ex-with--body node) complete-id-map) ast-expression-)])]

    [(ast-ex-cond-? node)
     (struct-copy ast-ex-cond- node
                  [clauses (map (lambda: ((clause : ast-ex-cond-clause-))
                                  (struct-copy ast-ex-cond-clause- clause
                                               [condition (cast (svm-resolve-ids (ast-ex-cond-clause--condition clause) id-map) ast-expression-)]
                                               [body (cast (svm-resolve-ids (ast-ex-cond-clause--body clause) id-map) ast-expression-)]))
                                (ast-ex-cond--clauses node))]
                  [else    (cast (svm-resolve-ids (ast-ex-cond--else node) id-map) ast-expression-)])]

    [(ast-ex-if-? node)
     (struct-copy ast-ex-if- node
                  [condition (cast (svm-resolve-ids (ast-ex-if--condition node) id-map) ast-expression-)]
                  [then      (cast (svm-resolve-ids (ast-ex-if--then node) id-map) ast-expression-)]
                  [else      (cast (svm-resolve-ids (ast-ex-if--else node) id-map) ast-expression-)])]

    [(ast-ex-fun-call-? node)
     (struct-copy ast-ex-fun-call- node
                  [parameters (map (lambda: ((expr : ast-expression-))
                                     (cast (svm-resolve-ids expr id-map) ast-expression-))
                                   (ast-ex-fun-call--parameters node))])]

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
     (define add-id-map (make-hash id-offset-pairs))
     (define new-body (svm-resolve-ids (ast-ex-fun-def--body node) (hash-union id-map add-id-map)))
     (struct-copy ast-ex-fun-def- node
                  [body (cast new-body ast-expression-)])]
    [else node]))

(module+ test #| svm-resolve-ids |#
  (check-true
   (match (nested->list (svm-resolve-ids (m-expression-def (with ((p0 1) (p1 (f p0))) p1)) (hash)))
     [(list 'ast-ex-with- _
              (list (list 'ast-ex-with-local- _ 'p0 _)
                    (list 'ast-ex-with-local- _ 'p1
                          (list 'ast-ex-fun-call-
                                  _ 'f
                                  (list (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p0 (list 'register-ref- 'Local 0))) 'p0)))))
              (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p1 (list 'register-ref- 'Local 1))) 'p1)) #t]
     [_ #f])
   "resolved locals in with forms: p0/p1 (first/second local) has offset 0/1")

  (check-true
   (match (nested->list (svm-resolve-ids (m-fun-def (some (p0 int) (p1 bool #t) -> bool)
                                                   (cond (p1 p0) (p0 p1) (_ p0))) (hash)))
     [(list _ ... (list 'ast-ex-cond- _
                      (list (list 'ast-ex-cond-clause- _
                                  (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p1 (list 'register-ref- 'Param 0))) 'p1)
                                  (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p0 (list 'register-ref- 'Param 1))) 'p0))
                            (list 'ast-ex-cond-clause- _
                                  (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p0 (list 'register-ref- 'Param 1))) 'p0)
                                  (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p1 (list 'register-ref- 'Param 0))) 'p1)))
                      (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p0 (list 'register-ref- 'Param 1))) 'p0))) #t]
     [_ #f])
   "resolved parameter in cond forms: p1/p0 (second/first parameter) has offset 0/1, looking from the end of the list")

  (check-true
   (match (nested->list (svm-resolve-ids (m-fun-def (some (p0 int) (p1 bool #t) -> bool)
                                                   (a p1)) (hash)))
     [(list _ ... (list 'ast-ex-fun-call- _ 'a (list (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p1 (list 'register-ref- 'Param 0))) 'p1)))) #t]
     [_ #f])
   "resolved parameter in function calls: p1 (second parameter) has offset 0, looking from the end of the list")

  (check-true
   (match (nested->list (svm-resolve-ids (m-fun-def (some (p0 int) (p1 bool #t) -> bool)
                                                   (if p1 p0 p1)) (hash)))
     [(list _ ... (list 'ast-ex-if- _
                      (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p1 (list 'register-ref- 'Param 0))) 'p1)
                      (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p0 (list 'register-ref- 'Param 1))) 'p0)
                      (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p1 (list 'register-ref- 'Param 0))) 'p1)
                      _)) #t]
     [_ #f])
   "resolved parameter in if forms: p1/0 (second/first parameter) has offset 0/1, looking from the end of the list")

  (check-true
   (match (nested->list (svm-resolve-ids (m-fun-def (some (p0 int) (p1 bool #t) -> bool)
                                                   p1) (hash)))
     [(list _ ... (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p1 (list 'register-ref- 'Param 0))) 'p1)) #t]
     [_ #f])
   "resolves parameter in id usage: p1 (second parameter) has offset 0, looking from the end of the list")

  (check-true
   (match (nested->list (svm-resolve-ids (m-fun-def (some (p0 int) (p1 bool #t) -> bool)
                                                   p0) (hash)))
     [(list _ ... (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p0 (list 'register-ref- 'Param 1))) 'p0)) #t]
     [_ #f])
   "resolves parameter in id usage: p0 (first parameter) has offset 1, looking from the end of the list"))

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
