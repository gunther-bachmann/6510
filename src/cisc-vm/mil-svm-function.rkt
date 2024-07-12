#lang typed/racket

(require (only-in racket/fixnum fx+ fx= fx< fx<= fx- fx>= fx>))

(require (for-syntax typed/racket syntax/parse))

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
  ;; (require "../6510-test-utils.rkt")
  (require typed/rackunit)

  (require/typed racket/struct (struct->list (Any -> (Listof Any))))

  ;; convert a deeply nested structure into a list that can easily be matched
  (define (nested->list (deeply-nested : Any)) : Any
    (cond
      [(struct? deeply-nested)
       (define-values (info _a) (struct-info deeply-nested))
       (unless info (raise-user-error "is no struct"))
       (define-values (struct-name _b _c _d _e _f _g _h) (struct-type-info info))
       (cons struct-name (nested->list (struct->list deeply-nested)))]

      [(list? deeply-nested)
       (map nested->list deeply-nested)]

      [(hash? deeply-nested)
       (foldl (lambda ((key : Any) (acc-hash : (HashTable Any Any)))
                (hash-set acc-hash key (nested->list (hash-ref deeply-nested key))))
            (hash) (hash-keys deeply-nested))]

      [else deeply-nested])))

(struct ast-node-
  ((info : ast-info-))
  #:transparent)

(define-type Register (U 'Param 'Global 'Local))

(struct register-ref-
  ((register : Register)
   (index    : Nonnegative-Integer))
  #:transparent)

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

(struct ast-info-
  ((pre-code     : (Listof ast-node-))        ;; code that is generated before this very node, (e.g. preparing locals to be passed as parameters to a function call)
   (post-code    : (Listof ast-node-))        ;; code that is generated after this very node
   (source-start : Nonnegative-Integer)       ;; reference to source code
   (source-end   : Nonnegative-Integer)       ;;
   (locals-used  : Nonnegative-Integer)       ;; number of locals used up by this node (and its sub nodes)
   (id-map       : (HashTable Symbol register-ref-))
   )
  #:transparent)

(define (make-ast-info #:pre-code (a-pre-code : (Listof ast-node-) (list))
                       #:post-code (a-post-code : (Listof ast-node-) (list))
                       #:source-start (a-source-start : Nonnegative-Integer 0)
                       #:source-end (a-source-end : Nonnegative-Integer 0)
                       #:locals-used (a-locals-used : Nonnegative-Integer 0)
                       #:id-map (an-id-map : (HashTable Symbol register-ref-) (hash))) : ast-info-
  (ast-info- a-pre-code
             a-post-code
             a-source-start
             a-source-end
             a-locals-used
             an-id-map))

(struct ast-type-def- ast-node-
  ()
  #:transparent)

;; int
(struct ast-td-simple- ast-type-def-
  ((id : Symbol))
  #:transparent)

;; (listof A)
(struct ast-td-complex- ast-type-def-
  ((id : Symbol)
   (params : (Listof ast-type-def-)))
  #:transparent)

;; (param type)
(struct ast-param-def- ast-node-
  ((id : Symbol)
   (type : ast-type-def-))
  #:transparent)

;; (param type value)
(struct ast-pa-defaulted-def- ast-param-def-
  ((default : ast-expression-))
  #:transparent)

(struct ast-expression- ast-node-
  ()
  #:transparent)

(struct ast-ex-atomic- ast-expression-
  ()
  #:transparent)

;; nil '()
(struct ast-at-nil- ast-ex-atomic-
  ()
  #:transparent)

;; #t #f
(struct ast-at-bool- ast-ex-atomic-
  ((bool : Boolean))
  #:transparent)

;; 42
(struct ast-at-byte- ast-ex-atomic-
  ((byte : Byte))
  #:transparent)

;; "A"
(struct ast-at-char- ast-ex-atomic-
  ((value : Byte))
  #:transparent)

;; 4711
(struct ast-at-int- ast-ex-atomic-
  ((value : Integer))
  #:transparent)

;; "..."
(struct ast-at-string- ast-ex-atomic-
  ((string : String))
  #:transparent)

(struct ast-at-id- ast-ex-atomic-
  ((id : Symbol))
  #:transparent)

;; (...)
(struct ast-ex-list- ast-expression-
  ((list : (Listof ast-expression-)))
  #:transparent)

;; (fun ...)
(struct ast-ex-fun-call- ast-expression-
  ((fun        : Symbol)
   (parameters : (Listof ast-expression-)))
  #:transparent)

;; '(...)
(struct ast-ex-quoted-list- ast-ex-list-
  ()
  #:transparent)

(struct ast-ex-cond-clause- ast-node-
  ((condition : ast-expression-)
   (body      : (Listof ast-expression-)))
  #:transparent)

;; (cond (bool ex) ...)
(struct ast-ex-cond- ast-expression-
  ((clauses : (Listof ast-ex-cond-clause-))
   (else    : ast-expression-))
  #:transparent)

;; (if bool then else)
(struct ast-ex-if- ast-expression-
  ((condition : ast-expression-)
   (then : ast-expression-)
   (else : ast-expression-)
   (negated : Boolean))
  #:transparent)

;; (m-def symbol expression)
(struct ast-ex-def- ast-ex-list-
  ()
  #:transparent)

;; (m-fun-def (symbol params ... -> type doc-string?) expression)
(struct ast-ex-fun-def- ast-expression-
  ((id          : Symbol)
   (parameter   : (Listof ast-param-def-))
   (def-params  : (Listof ast-pa-defaulted-def-))
   (return-type : ast-type-def-)
   (description : (Listof String))
   (body        : (Listof ast-expression-)))
  #:transparent)

(struct ast-ex-with-local- ast-node-
  ((id : Symbol)
   (value : ast-expression-))
  #:transparent)

(struct ast-ex-with- ast-expression-
  ((locals : (Listof ast-ex-with-local-))
   (body   : ast-expression-))
  #:transparent)

(define-syntax (m-type-def stx)
  (syntax-parse stx
    [(_ (cpx-type inner-type ...))
     #'(ast-td-complex- (make-ast-info) 'cpx-type (list (m-type-def inner-type) ...))]
    [(_ basic-type)
     (if (string-suffix? (symbol->string (syntax->datum #'basic-type)) "*")
         #'(ast-td-complex- (make-ast-info) 'list (list (ast-td-simple- (make-ast-info) (string->symbol (string-trim (symbol->string 'basic-type) "*" #:right? #t)))))
         #'(ast-td-simple- (make-ast-info) 'basic-type))]))

(module+ test #| m-type-def |#
  (check-true
     (match (nested->list (m-type-def A*))
       [(list ast-td-complex- _ 'list (list (list 'ast-td-simple- _ 'A))) #t]
       [_ #f]))

  (check-true
   (match (nested->list (m-type-def A))
     [(list 'ast-td-simple- _ 'A) #t]
     [_ #f]))

  (check-true
   (match (nested->list (m-type-def (listof A)))
     [(list 'ast-td-complex- _ 'listof (list (list 'ast-td-simple- _ 'A))) #t]
     [_ #f]))

  (check-equal? (m-type-def (listof A))
                (ast-td-complex- (make-ast-info) 'listof (list (ast-td-simple- (make-ast-info) 'A)))))

(define-syntax (m-expression-def stx)
  (syntax-parse stx
    [(_ '())
     #'(ast-at-nil- (make-ast-info))]
    [(_ (~literal nil))
     #'(ast-at-nil- (make-ast-info))]

    [(_ #t)
     #'(ast-at-bool- (make-ast-info) #t)]
    [(_ #f)
     #'(ast-at-bool- (make-ast-info) #f)]

    [(_ ((~literal with) ((id expression) ...) body))
     #'(ast-ex-with- (make-ast-info)
                     (list (ast-ex-with-local- (make-ast-info) 'id (m-expression-def expression)) ...)
                     (m-expression-def body))]
    [(_ ((~literal if) bool-param true-param false-param))

     #'(ast-ex-if- (make-ast-info)
                   (m-expression-def bool-param)
                   (m-expression-def true-param)
                   (m-expression-def false-param)
                   #f)]

    [(_ ((~literal cond) [case-cond case-expression ...] ... [(~literal _) else-expression]))
     #'(ast-ex-cond- (make-ast-info)
                     (list (ast-ex-cond-clause- (make-ast-info) (m-expression-def case-cond) (list (m-expression-def case-expression) ...)) ...)
                     (m-expression-def else-expression))]

    [(_ (id param ...))
     #'(ast-ex-fun-call- (make-ast-info) 'id (list (m-expression-def param) ...))]

    [(_ value)
     #'(cond ((string? 'value) (ast-at-string- (make-ast-info) 'value))
             ((boolean? 'value) (ast-at-bool- (make-ast-info) 'value))
             ((exact-integer? 'value) (ast-at-int- (make-ast-info) 'value))
             ((symbol? 'value) (ast-at-id- (make-ast-info) 'value))
             (else (raise-user-error (format "unknown expression value type ~a" 'value))))]))

(module+ test #| m-expression-def |#
  (check-true
   (match (nested->list (m-expression-def (with () "some")))
     [(list 'ast-ex-with- _ (list) (list 'ast-at-string- _ "some")) #t]
     [_ #f])
   "empty with is properly parsed as expression")

  (check-true
   (match (nested->list (m-expression-def (with ((a 42)) "some")))
     [(list 'ast-ex-with- _ (list (list 'ast-ex-with-local- _ 'a (list 'ast-at-int- _ 42))) (list 'ast-at-string- _ "some")) #t]
     [_ #f])
   "with is properly parsed as expression")

  (check-true
   (match (nested->list (m-expression-def #t))
     [(list 'ast-at-bool- _ #t) #t]
     [_ #f])
   "boolean true is properly parsed as expression")

  (check-true
   (match (nested->list (m-expression-def #f))
     [(list 'ast-at-bool- _ #f) #t]
     [_ #f])
   "boolean false is properly parsed as expression")

  (check-true
   (match (nested->list (m-expression-def (cond [_ 0])))
     [(list 'ast-ex-cond- _ (list) (list 'ast-at-int- _ 0)) #t]
     [_ #f])
   "empty cond expression is properly parsed as expression")

  (check-true
   (match (nested->list (m-expression-def (cond [a 1] [b 3] [_ 0])))
     [(list 'ast-ex-cond- _
            (list (list 'ast-ex-cond-clause- _ (list 'ast-at-id- _ 'a) (list (list 'ast-at-int- _ 1)))
                  (list 'ast-ex-cond-clause- _ (list 'ast-at-id- _ 'b) (list (list 'ast-at-int- _ 3))))
            (list 'ast-at-int- _ 0)) #t]
     [_ #f])
   "complex cond expression is properly parsed as expression")

  (check-true
   (match (nested->list (m-expression-def "some"))
     [(list 'ast-at-string- _ "some") #t]
     [_ #f])
   "string constants are properly parsed as expression")

  (check-true
   (match (nested->list (m-expression-def (a-fun p1 17)))
     [(list 'ast-ex-fun-call- _ 'a-fun (list (list 'ast-at-id- _ 'p1) (list 'ast-at-int- _ 17))) #t]
     [_ #f])
   "functions calls are properly parsed as expression")

  (check-true
   (match (nested->list (m-expression-def 180))
     [(list 'ast-at-int- _ 180) #t]
     [_ #f])
   "integer constants are properly parsed as expression")

  (check-true
   (match (nested->list (m-expression-def (if #t 10 20)))
     [(list 'ast-ex-if- _ (list 'ast-at-bool- _ #t) (list 'ast-at-int- _ 10) (list 'ast-at-int- _ 20) #f) #t]
     [_ #f])
   "if expressions are properly parsed as expression"))

(define-syntax (m-fun-def stx)
  (syntax-parse stx
      [(_ (id (p-id p-typ) ... (o-id o-typ o-val) ... -> r-typ desc ...) expr ...)
       #'(ast-ex-fun-def- (make-ast-info)
                       'id
                       (list (ast-param-def- (make-ast-info) 'p-id (m-type-def p-typ)) ...)
                       (list (ast-pa-defaulted-def- (make-ast-info) 'o-id (m-type-def o-typ) (m-expression-def o-val)) ...)
                       (m-type-def r-typ)
                       (list 'desc ...)
                       (list (m-expression-def expr) ...))]))

(module+ test #| m-fun-def |#
  (check-true
   (match (nested->list (m-fun-def (mf (p0 A) (p1 B) (p2 C 10) -> C "some")
                                  #f))
     [(list 'ast-ex-fun-def-
            _
            'mf
            (list (list 'ast-param-def- _ 'p0 (list 'ast-td-simple- _ 'A))
                  (list 'ast-param-def- _ 'p1 (list 'ast-td-simple- _ 'B)))
            (list (list 'ast-pa-defaulted-def- _ 'p2 (list 'ast-td-simple- _ 'C)
                        (list 'ast-at-int- _ 10)))
            (list 'ast-td-simple- _ 'C)
            (list "some")
            (list (list 'ast-at-bool- _ #f))) #t]
     [_ #f])))

(module+ test #| simple reverse function |#
  (check-true
   (match
       (nested->list
        (m-fun-def (reverse (a-list cell*) (b-list cell* '()) -> cell*
                            "reverse a-list, consing it into b-list")
                   (if (nil? a-list)
                       b-list
                       (reverse (cdr a-list) (cons (car a-list) b-list)))))

     [(list ast-ex-fun-def-
        _
        reverse
        (list (list ast-param-def-
          _
          a-list
          (list ast-td-complex- _ list (list (list ast-td-simple- _ cell)))))
        (list (list ast-pa-defaulted-def-
          _
          b-list
          (list ast-td-complex- _ list (list (list ast-td-simple- _ cell)))
          (list ast-at-nil- _)))
        (list ast-td-complex- _ list (list (list ast-td-simple- _ cell)))
        (list "reverse a-list, consing it into b-list")
        (list (list ast-ex-if-
          _
          (list ast-ex-fun-call- _ nil? (list (list ast-at-id- _ a-list)))
          (list ast-at-id- _ b-list)
          (list ast-ex-fun-call-
           _
           reverse
           (list (list ast-ex-fun-call- _ cdr (list (list ast-at-id- _ a-list)))
            (list ast-ex-fun-call-
             _
             cons
             (list `(ast-ex-fun-call- ,_ car ,(list `(ast-at-id- ,_ a-list)))
             `(ast-at-id- ,_ b-list)))))
          #f)))
      #t]
     [_ #f])))

;; do generation for stack machine
;; goal: transform the reverse function into stack machine code

;; compiler passes 0 1 2 .. n
;; n = generate all information necessary to
;;       - execute in vm
;;       - write into file (loadable by runtime / vm)
;; all passes before that will (just) rewrite the ast

(define (low-byte (int : Integer)) : Byte
  (define lb (bitwise-and #xff int))
  (if (byte? lb)
      lb
      (raise-user-error "low-byte error")))

(define (high-byte (int : Integer)) : Byte
  (define hb (arithmetic-shift int -8))
  (if (byte? hb)
      hb
      (raise-user-error "high-byte error")))

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
     ;; TODO push value behind id
     (raise-user-error "id not implemented yet")]
    [(ast-at-string-? atom)
     (raise-user-error "string not implemented yet")]
    [else (raise-user-error (format "unknown atomic value ~a" atom))]))

(module+ test #| gen-atom |#
  (check-equal? (gen-atom (ast-at-int- (make-ast-info) #x02fe))
                (vector-immutable PUSH_INT #xfe #x02))

  (check-equal? (gen-atom (ast-at-bool- (make-ast-info) #t))
                (vector-immutable PUSH_BYTE (cell-byte--value TRUE))))

(define (svm-generate-function (def : ast-ex-fun-def-)) : (Immutable-Vectorof Byte)
  (vector-immutable))

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
