#lang typed/racket

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
                  NIL?))

(module+ test #| require test utils |#
  (require typed/rackunit)

  (require/typed racket/struct (struct->list (Any -> (Listof Any))))

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

(struct ast-info-
  ((pre-code     : (Listof ast-node-))        ;; code that is generated before this very node, (e.g. preparing locals to be passed as parameters to a function call)
   (post-code    : (Listof ast-node-))        ;; code that is generated after this very node
   (source-start : Nonnegative-Integer)       ;; reference to source code
   (source-end   : Nonnegative-Integer)       ;;
   (locals-used  : Nonnegative-Integer))      ;; number of locals used up by this node (and its sub nodes)
  #:transparent)

(define (make-ast-info #:pre-code (a-pre-code : (Listof ast-node-) (list))
                       #:post-code (a-post-code : (Listof ast-node-) (list))
                       #:source-start (a-source-start : Nonnegative-Integer 0)
                       #:source-end (a-source-end : Nonnegative-Integer 0)
                       #:locals-used (a-locals-used : Nonnegative-Integer 0)) : ast-info-
  (ast-info- a-pre-code
             a-post-code
             a-source-start
             a-source-end
             a-locals-used))

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
  ((byte : Byte))
  #:transparent)

;; 4711
(struct ast-at-int- ast-ex-atomic-
  ((int : Integer))
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

;; '(...)
(struct ast-ex-quoted-list- ast-ex-list-
  ()
  #:transparent)

;; (cond (bool ex) ...)
(struct ast-ex-cond- ast-ex-list-
  ()
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


(define-syntax (m-type-def stx)
  (syntax-parse stx
    [(_ (cpx-type inner-type ...))
     #'(ast-td-complex- (make-ast-info) 'cpx-type (list (m-type-def inner-type) ...))]
    [(_ basic-type)
     #'(ast-td-simple- (make-ast-info) 'basic-type)]))

(module+ test #| m-type-def |#
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
    ;; [(_ '())
    ;;  #'(ast-e-nil agsi)]
    [(_ ((~literal if) bool-param true-param false-param))
     #'(ast-ex-if- (make-ast-info)
                   (m-expression-def bool-param)
                   (m-expression-def true-param)
                   (m-expression-def false-param)
                   #f)]
  [(_ ((~literal cond) ((case-cond) (case-expression) ...) ...))
   #'()]
  ;; [(_ (id param ...))
  ;;  #'(ast-ex-fun-call- agsi 'id (list (m-expression-def param) ...))]
  [(_ value)
   #'(cond ((string? 'value) (ast-at-string- (make-ast-info) 'value))
           ((boolean? 'value) (ast-at-bool- (make-ast-info) 'value))
           ((exact-integer? 'value) (ast-at-int- (make-ast-info) 'value))
           ((symbol? 'value) (ast-at-id- (make-ast-info) 'value))
           (else (raise-user-error (format "unknown expression value type ~a" 'value))))]))

(module+ test #| m-expression-def |#
  (check-true
   (match (nested->list (m-expression-def "some"))
     [(list 'ast-at-string- _ "some") #t]
     [_ #f]))

  (check-true
   (match (nested->list (m-expression-def 180))
     [(list 'ast-at-int- _ 180) #t]
     [_ #f]))

  (check-true
   (match (nested->list (m-expression-def (if #t 10 20)))
     [(list 'ast-ex-if- _ (list 'ast-at-bool- _ #t) (list 'ast-at-int- _ 10) (list 'ast-at-int- _ 20) #f) #t]
     [_ #f])))

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
