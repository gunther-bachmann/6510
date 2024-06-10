#lang racket

;; basic types
;;   bool, (char), byte, int, string
;; complex types
;;   array, list, map, struct

(require syntax/parse/define)

(require (only-in "./vm-structures.rkt"
                  CISC_VM_CONS
                  CISC_VM_CAR
                  CISC_VM_BRA_EMPTY_LIST
                  CISC_VM_CDR
                  CISC_VM_GOTO
                  CISC_VM_RET
                  CISC_VM_IMMB
                  CISC_VM_BYTE_ADD

                  VM_L0
                  VM_L1
                  VM_P0
                  VM_P1

                  encode-idx
                  l-local
                  l-param
                  l-global
                  l-imm))

(module+ test #| require test utils |#
  (require "../6510-test-utils.rkt")

  )

(define-syntax (-> stx)
  (raise-syntax-error #f "cannot be used as an expression" stx))

(struct ast-node
  ()
  #:transparent)

(struct ast-type-def ast-node
  ()
  #:transparent)

(struct ast-td-simple ast-type-def
  (id)
  #:transparent
  #:guard
  (struct-guard/c symbol?))

(struct ast-ts-complex ast-type-def
  (id params)
  #:transparent
  #:guard
  (struct-guard/c symbol? (listof ast-type-def?)))

(struct ast-param-def ast-node
  (id type)
  #:transparent
  #:guard
  (struct-guard/c symbol? ast-type-def?))

(struct ast-expression ast-node
  ()
  #:transparent)

(struct ast-e-nil ast-expression
  ;; the nil symbol/list
  ()
  #:transparent)

(struct ast-e-fun-call ast-expression
  ;; strict eval parameter evaluation
  (fun params pre-code post-code)
  #:transparent
  #:guard
  (struct-guard/c symbol? (listof ast-expression?) (listof ast-expression?) (listof ast-expression?)))

(struct ast-e-if ast-e-fun-call
  ;; special because parameters of if underly a sepcial execution order (no strict eval)
  ()
  #:transparent
  #:guard
  (struct-guard/c symbol? (listof ast-expression?) (listof ast-expression?) (listof ast-expression?)))

(struct ast-e-value ast-expression
  ;; atomic values not containing others
  ()
  #:transparent)

(struct ast-ev-string ast-e-value
  (str)
  #:transparent
  #:guard
  (struct-guard/c string?))

(struct ast-ev-bool ast-e-value
  (bool)
  #:transparent
  #:guard
  (struct-guard/c boolean?))

(struct ast-ev-id ast-e-value
  (id)
  #:transparent
  #:guard
  (struct-guard/c symbol?))

(struct ast-ev-number ast-e-value
  (number)
  #:transparent
  #:guard
  (struct-guard/c exact-integer?))

(struct ast-pd-default-param ast-param-def
  (value)
  #:transparent
  #:guard
  (struct-guard/c symbol? ast-type-def? ast-expression?))

(struct ast-function-def ast-node
  (id parameter default-parameter return-type description body)
  #:transparent
  #:guard
  (struct-guard/c symbol?
                  (listof ast-param-def?)
                  (listof ast-pd-default-param?)
                  ast-type-def?
                  (listof string?)
                  (listof ast-expression?)))

;; type definition parser
(define-syntax-parser m-type-def
  [(_ (cpx-type inner-type ...))
   #'(ast-ts-complex 'cpx-type (list (m-type-def inner-type) ...))]
  [(_ basic-type)
   #'(ast-td-simple 'basic-type)])

;; (function) definition parser
(define-syntax-parser m-def
  #:literals [->]
  [(_ (id (p-id p-typ) ... (o-id o-typ o-val) ... -> r-typ desc ...) expr ...)
   #'(ast-function-def 'id
                       (list (ast-param-def 'p-id (m-type-def p-typ)) ...)
                       (list (ast-pd-default-param 'o-id (m-type-def o-typ) (m-expression-def o-val)
                                                   ) ...)
                       (m-type-def r-typ)
                       (list 'desc ...)
                       (list (m-expression-def expr) ...))])

(module+ test #| m-def |#
  (define mf0 (m-def (f0 -> string "description1" "description2") "value-hello"))

  (check-equal? (ast-function-def-id mf0)
                'f0)
  (check-equal? (ast-function-def-parameter mf0)
                '())
  (check-equal? (ast-function-def-default-parameter mf0)
                '())
  (check-equal? (ast-function-def-return-type mf0)
                (ast-td-simple 'string))
  (check-equal? (ast-function-def-description mf0)
                (list "description1" "description2"))

  (define mf1 (m-def (f1 (a string) (b bool) (q string "h") -> string)
                     (hello)
                     (hello2)))

  (check-equal? (ast-function-def-id mf1)
                'f1)
  (check-equal? (ast-function-def-parameter mf1)
                (list (ast-param-def 'a (ast-td-simple 'string))
                      (ast-param-def 'b (ast-td-simple 'bool))))
  (check-equal? (ast-function-def-default-parameter mf1)
                (list (ast-pd-default-param 'q (ast-td-simple 'string) (ast-ev-string "h"))))
  (check-equal? (ast-function-def-return-type mf1)
                (ast-td-simple 'string))
  (check-equal? (ast-function-def-description mf1)
                '())
  (check-equal? (ast-function-def-body mf1)
                (list (ast-e-fun-call 'hello (list) '() '())
                      (ast-e-fun-call 'hello2 (list) '() '())))

  (define mf2 (m-def (f2 (a string) -> bool)))

  (check-equal? (ast-function-def-id mf2)
                'f2)
  (check-equal? (ast-function-def-parameter mf2)
                (list (ast-param-def 'a (ast-td-simple 'string))))
  (check-equal? (ast-function-def-default-parameter mf2)
                '())
  (check-equal? (ast-function-def-return-type mf2)
                (ast-td-simple 'bool))
  (check-equal? (ast-function-def-description mf2)
                '())

  (define mf3 (m-def (f3 (a string "init") -> (list int))))

  (check-equal? (ast-function-def-id mf3)
                'f3)
  (check-equal? (ast-function-def-parameter mf3)
                '()
                )
  (check-equal? (ast-function-def-default-parameter mf3)
                (list (ast-pd-default-param 'a (ast-td-simple 'string) (ast-ev-string "init"))))
  (check-equal? (ast-function-def-return-type mf3)
                (ast-ts-complex 'list (list (ast-td-simple 'int))))
  (check-equal? (ast-function-def-description mf3)
                '()))

;; expression parser
(define-syntax-parser m-expression-def
  [(_ '())
   #'(ast-e-nil)]
  [(_ ((~literal if) bool-param true-param false-param ...))
   #'(ast-e-if 'if (cons (m-expression-def bool-param)
                         (cons
                          (m-expression-def true-param)
                          (list (m-expression-def false-param) ...))) '() '())]
  [(_ ((~literal cond) ((case-cond) (case-expression) ...) ...))
   #'()]
  [(_ (id param ...))
   #'(ast-e-fun-call 'id (list (m-expression-def param) ...) '() '())]
  [(_ value)
   #'(cond ((string? 'value) (ast-ev-string 'value))
           ((boolean? 'value) (ast-ev-bool 'value))
           ((exact-integer? 'value) (ast-ev-number 'value))
           ((symbol? 'value) (ast-ev-id 'value))
           (else (raise-user-error (format "unknown expression value type ~a" 'value))))])

(module+ test #| m-expression-def |#
  (check-equal?
   (m-expression-def '())
   (ast-e-nil))

  (check-equal?
   (m-expression-def "hello")
   (ast-ev-string "hello"))

  (check-equal?
   (m-expression-def 123)
   (ast-ev-number 123))

  (check-equal?
   (m-expression-def #t)
   (ast-ev-bool #t))

  (check-equal?
   (m-expression-def (fn1 "A" #t))
   (ast-e-fun-call 'fn1 (list (ast-ev-string "A") (ast-ev-bool #t)) '() '()))

  (check-equal?
   (m-expression-def (if #t "A" "B"))
   (ast-e-if 'if (list (ast-ev-bool #t) (ast-ev-string "A") (ast-ev-string "B")) '() '()))

  (check-equal?
   (m-expression-def (fn2))
   (ast-e-fun-call 'fn2 (list) '() '()))

  (check-equal?
   (m-expression-def (fn3 (fn5 "A") (- 1 2)))
   (ast-e-fun-call 'fn3 (list (ast-e-fun-call 'fn5 (list (ast-ev-string "A")) '() '())
                              (ast-e-fun-call '- (list (ast-ev-number 1) (ast-ev-number 2)) '() '()))
                   '() '())))

(module+ test #| simple reverse function |#
  (check-equal?
   (m-def (reverse (a-list (list cell)) (b-list (list cell) '()) -> (list cell)
                   "reverse a-list, consing it into b-list")
          (if (nil? a-list)
              b-list
              (reverse (cdr a-list) (cons (car a-list) b-list))))
   (ast-function-def
    'reverse
    ;; parameter
    (list (ast-param-def 'a-list (ast-ts-complex 'list (list (ast-td-simple 'cell)))))
    ;; parameter with defaults
    (list
     (ast-pd-default-param
      'b-list
      (ast-ts-complex 'list (list (ast-td-simple 'cell)))
      (ast-e-nil)))
    ;; return type
    (ast-ts-complex 'list (list (ast-td-simple 'cell)))
    ;; doc
    '("reverse a-list, consing it into b-list")
    ;; body
    (list
     (ast-e-if
      'if
      (list
       (ast-e-fun-call 'nil? (list (ast-ev-id 'a-list)) '() '())
       (ast-ev-id 'b-list)
       (ast-e-fun-call
        'reverse
        (list
         (ast-e-fun-call 'cdr (list (ast-ev-id 'a-list)) '() '())
         (ast-e-fun-call
          'cons
          (list
           (ast-e-fun-call 'car (list (ast-ev-id 'a-list)) '() '())
           (ast-ev-id 'b-list)) '() '())) '() '())) '() '())))))

;; intermediate ast nodes (used during transformation)
(struct -loc-set ast-expression
  (id expr)
  #:transparent)

(struct loc-ref ast-expression
  (id)
  #:transparent)

(define step0 (list
               (ast-e-if
                'if
                (list
                 (ast-e-fun-call 'nil? (list (ast-ev-id 'a-list)) '() '())
                 (ast-ev-id 'b-list)
                 (ast-e-fun-call
                  'reverse
                  (list
                   (ast-e-fun-call 'cdr (list (ast-ev-id 'a-list)) '() '())
                   (ast-e-fun-call
                    'cons
                    (list
                     (ast-e-fun-call 'car (list (ast-ev-id 'a-list)) '() '()) ;; extract this inner call to some variable 'a
                     (ast-ev-id 'b-list)) '() '())) '() '())) '() '())))

(define step1 (list
               (ast-e-if
                'if
                (list
                 (ast-e-fun-call 'nil? (list (ast-ev-id 'a-list)) '() '())
                 (ast-ev-id 'b-list)
                 (ast-e-fun-call
                  'reverse
                  (list
                   (ast-e-fun-call 'cdr (list (ast-ev-id 'a-list)) '() '()) ;; call has only refs to ids (done)
                   (-loc-set 'a (ast-e-fun-call 'car (list (ast-ev-id 'a-list)) '() '()))
                   (ast-e-fun-call 'cons (list (loc-ref 'a) (ast-ev-id 'b-list)) '() '())) '() '())) '() '()))) ;; now this call has only refs to ids (done), extract call itself to id b'

(define step2 (list
               (ast-e-if
                'if
                (list
                 (ast-e-fun-call 'nil? (list (ast-ev-id 'a-list)) '() '())
                 (ast-ev-id 'b-list)
                 (ast-e-fun-call
                  'reverse
                  (list
                   (ast-e-fun-call 'cdr (list (ast-ev-id 'a-list)) '() '()) ;; extract call itself to id 'c
                   (-loc-set 'a (ast-e-fun-call 'car (list (ast-ev-id 'a-list)) '() '()))
                   (-loc-set 'b (ast-e-fun-call 'cons (list (loc-ref 'a) (ast-ev-id 'b-list)) '() '()))
                   (loc-ref 'b)) '() '())) '() '())))

(define step3 (list
               (ast-e-if
                'if
                (list
                 (ast-e-fun-call 'nil? (list (ast-ev-id 'a-list)) '() '())
                 (ast-ev-id 'b-list)
                 (-loc-set 'a (ast-e-fun-call 'car (list (ast-ev-id 'a-list)) '() '()))
                 (-loc-set 'b (ast-e-fun-call 'cons (list (loc-ref 'a) (ast-ev-id 'b-list)) '() '())) ;; p1 can be reused here
                 (-loc-set 'c (ast-e-fun-call 'cdr (list (ast-ev-id 'a-list)) '() '())) ;; p0 can be reused here
                 (ast-e-fun-call 'reverse (list (loc-ref 'c) (loc-ref 'b)) '() '())) '() '()))) ;; now reverse has only ref parameters (done)

;; - function calls with only ids or local-refs are not compacted any further
;; - if function is special
;;   (if (nil? some-id) a b) is translated into
;;   (bra-empty-list some-id :label)
;;   b code (last expression result going into returned register)
;;   goto :label-after-if
;;   a code (last expression result going into returned register)
;;   :labe-after-if
;;   last expression is returned (via RET register)
;;   recursive call must be transformed into a goto! <- define fail condition  (if it cannot be done readily)

(define/contract (ed-function-call--has-only-refs? fun-call)
  (->* [ast-e-fun-call?] [] boolean?)
  (foldl (lambda (l r) (and (or (loc-ref? l) (ast-ev-id? l)) r)) true (ast-e-fun-call-params fun-call)))

(module+ test
  (check-true (ed-function-call--has-only-refs? (ast-e-fun-call 'fn1 (list) '() '())))

  (check-true (ed-function-call--has-only-refs? (ast-e-fun-call 'fn1 (list (loc-ref 'a) (ast-ev-id 'b)) '() '())))

  (check-false (ed-function-call--has-only-refs? (ast-e-fun-call 'fn1 (list (loc-ref 'a) (ast-ev-string "10")) '() '()))))

;; recursively extract all non refs from the given parameters (of a function all)
;; return the
;; - resulting params (must all be refs)
;; - setters that need to be prepended to the call
;; - list of newly introduced references
(define/contract (ed-function-call-params--mapper params (symbol-generator gensym))
  (->* [(listof ast-expression?)] [any/c]
      (listof (list/c (or/c loc-ref? ast-ev-id?) (listof -loc-set?) (listof loc-ref?))))
  (map (lambda (param)
         (cond [(ast-e-fun-call? param)
                (define sym (symbol-generator))
                ;; (match-define (list new-func new-ref-list prepends)
                ;;   (ed-function-call--extract-refs param '() '() symbol-generator))
                (list (loc-ref sym) (list (-loc-set sym (transform-function-call param))) '())]
               [(ast-e-nil? param) (define sym (symbol-generator)) (list (loc-ref sym) (list (-loc-set sym param)) '())]
               [(ast-ev-number? param) (define sym (symbol-generator)) (list (loc-ref sym) (list (-loc-set sym param)) '())]
               [(ast-ev-string? param) (define sym (symbol-generator)) (list (loc-ref sym) (list (-loc-set sym param)) '())]
               [(ast-ev-bool? param) (define sym (symbol-generator)) (list (loc-ref sym) (list (-loc-set sym param)) '())]
               [(ast-ev-id? param) (list param '() '())]
               [(loc-ref? param) (list param '() '())]
               [else (raise-user-error (format "unknown param type ~a" param))]))
       params))

(module+ test
  (check-match (ed-function-call-params--mapper (list (ast-e-if 'if (list (ast-ev-bool #t) (ast-ev-string "true-val") (ast-ev-string "false-val")) '() '())))
               (list (list
                      (loc-ref a-sym)
                      (list
                       (-loc-set a-sym (ast-e-fun-call 'if (list (loc-ref b-sym) (ast-ev-string "true-val") (ast-ev-string "false-val"))
                                                       (list (-loc-set b-sym (ast-ev-bool #t))) '())))
                      '()))
               (and (symbol? a-sym) (symbol? b-sym)))

  (check-equal? (ed-function-call-params--mapper (list (ast-e-nil) (ast-ev-number 47) (ast-ev-string "x") (ast-ev-bool #t)) (lambda () 'a))
                (list (list (loc-ref 'a) (list (-loc-set 'a (ast-e-nil))) '())
                      (list (loc-ref 'a) (list (-loc-set 'a (ast-ev-number 47))) '())
                      (list (loc-ref 'a) (list (-loc-set 'a (ast-ev-string "x"))) '())
                      (list (loc-ref 'a) (list (-loc-set 'a (ast-ev-bool #t))) '()))
                "values are extracted into a set to a ref and the ref")

  (check-equal? (ed-function-call-params--mapper (list (loc-ref 'a) (ast-ev-id 'b)))
                (list (list (loc-ref 'a) '() '())
                      (list (ast-ev-id 'b) '() '()))
                "a reference is not transformed => no new refs etc")

  (check-equal? (ed-function-call-params--mapper (list (loc-ref 'q) (ast-ev-number 17)) (lambda () 'l1))
                (list (list (loc-ref 'q) '() '())
                      (list (loc-ref 'l1) (list (-loc-set 'l1 (ast-ev-number 17))) '())))

  (check-match (ed-function-call-params--mapper (list (loc-ref 'q) (ast-e-fun-call 'fn1 (list (ast-ev-number 1)) '() '())))
               (list (list (loc-ref 'q) '() '())
                     (list (loc-ref a-sym)
                           (list (-loc-set a-sym (ast-e-fun-call 'fn1 (list (loc-ref b-sym))
                                                                 (list (-loc-set b-sym (ast-ev-number 1))) '()))) '()))
               (and (symbol? a-sym) (symbol? b-sym)))

  (check-match (ed-function-call-params--mapper
                (list (loc-ref 'q) (ast-e-fun-call 'fn1 (list (ast-ev-id 'l) (ast-e-fun-call 'r2 (list (ast-ev-number 1)) '() '())) '() '())))
               (list (list (loc-ref 'q) '() '())
                     (list (loc-ref a-sym)
                           (list (-loc-set a-sym
                                           (ast-e-fun-call
                                            'fn1 (list (ast-ev-id 'l) (loc-ref b-sym))
                                            (list (-loc-set b-sym
                                                            (ast-e-fun-call
                                                             'r2
                                                             (list (loc-ref c-sym))
                                                             (list (-loc-set c-sym (ast-ev-number 1)))
                                                             '())))
                                            '())))
                           '()))
               (and (symbol? a-sym) (symbol? b-sym) (symbol? c-sym)))

  (check-match (ed-function-call-params--mapper
                (list
                 (ast-e-fun-call 'cdr (list (ast-ev-id 'a-list)) '() '())
                 (ast-e-fun-call
                  'cons
                  (list
                   (ast-e-fun-call 'car (list (ast-ev-id 'a-list)) '() '())
                   (ast-ev-id 'b-list)) '() '())))
               (list (list (loc-ref c-sym)
                           (list (-loc-set c-sym (ast-e-fun-call 'cdr (list (ast-ev-id 'a-list)) '() '())))
                           '())
                     (list (loc-ref b-sym)
                           (list (-loc-set b-sym (ast-e-fun-call
                                                  'cons
                                                  (list (loc-ref a-sym) (ast-ev-id 'b-list))
                                                  (list (-loc-set a-sym (ast-e-fun-call 'car (list (ast-ev-id 'a-list)) '() '())))
                                                  '())))
                           '()))
               (and (symbol? a-sym) (symbol? b-sym) (symbol? c-sym))))

(define/contract (ed-function-call--extract-refs fun-call (ref-list '()) (prepends '()) (symbol-generator gensym))
  (->* [ast-e-fun-call?]
      [(listof loc-ref?) (listof -loc-set?) any/c]
      (list/c ast-e-fun-call? (listof loc-ref?) (listof -loc-set?)))
  (cond [(ed-function-call--has-only-refs? fun-call) (list fun-call ref-list prepends)]
        [(eq? 'if (ast-e-fun-call-fun fun-call))
         (define transformed-bool-param (ed-function-call-params--mapper (list (car (ast-e-fun-call-params fun-call))) symbol-generator))
         (list (ast-e-fun-call
                'if
                (cons (caar transformed-bool-param) (cdr (ast-e-fun-call-params fun-call)))
                '() '())
               (append ref-list (flatten (map (lambda (param) (cddr param)) transformed-bool-param)))
               (append prepends (flatten (map (lambda (param) (cadr param)) transformed-bool-param))))]
        [else
         (define transformed-params (ed-function-call-params--mapper (ast-e-fun-call-params fun-call) symbol-generator))
         (list
          (ast-e-fun-call
           (ast-e-fun-call-fun fun-call)
           (map (lambda (param) (car param)) transformed-params)
           '() '())
          (append ref-list (flatten (map (lambda (param) (cddr param)) transformed-params)))
          (append prepends (flatten (map (lambda (param) (cadr param)) transformed-params))))]))


(module+ test
  (check-match (ed-function-call--extract-refs (ast-e-fun-call 'fn1 (list (ast-ev-bool #t) (ast-e-fun-call 'inner (list (ast-ev-string "a-string")) '() '())) '() '()))
               (list
                (ast-e-fun-call 'fn1 (list (loc-ref a-sym) (loc-ref b-sym)) '() '())
                '()
                (list
                 (-loc-set a-sym (ast-ev-bool #t))
                 (-loc-set b-sym (ast-e-fun-call
                                  'inner
                                  (list (loc-ref c-sym ))
                                  (list (-loc-set c-sym  (ast-ev-string "a-string")))
                                  '())))))

  (check-match (ed-function-call--extract-refs
                (ast-e-fun-call 'fn1 (list (ast-e-if 'if (list (ast-ev-bool #t) (ast-ev-number 10) (ast-ev-number 20)) '() '())
                                           (ast-e-fun-call 'inner (list (ast-ev-string "a-string")) '() '())) '() '()))

               (list
                (ast-e-fun-call 'fn1 (list (loc-ref a-sym) (loc-ref b-sym)) '() '())
                '()
                (list
                 (-loc-set a-sym (ast-e-fun-call
                                  'if
                                  (list (loc-ref c-sym) (ast-ev-number 10) (ast-ev-number 20))
                                  (list (-loc-set c-sym (ast-ev-bool #t)))
                                  '()))
                 (-loc-set b-sym (ast-e-fun-call
                                  'inner
                                  (list (loc-ref f-sym))
                                  (list (-loc-set f-sym (ast-ev-string "a-string")))
                                  '()))))))

(define/contract (transform-function-call fun-call)
  (->* [ast-e-fun-call?] [] ast-e-fun-call?)
  (match-define (list new-call loc-refs loc-sets)
    (ed-function-call--extract-refs fun-call))
  (struct-copy ast-e-fun-call new-call
               [pre-code (append (ast-e-fun-call-pre-code fun-call) (reverse loc-sets))]))

(module+ test #| transform function call |#
  (check-match (transform-function-call (ast-e-fun-call 'fn1 (list (ast-ev-bool #t)) '() '()))
               (ast-e-fun-call 'fn1
                               (list (loc-ref a-sym))
                               (list (-loc-set a-sym (ast-ev-bool #t)))
                               '())
               (symbol? a-sym))

  (check-match (transform-function-call (ast-e-fun-call 'fn1 (list (ast-ev-bool #t) (ast-e-fun-call 'fn2 (list (ast-ev-string "z")) '() '())) '() '()))
               (ast-e-fun-call 'fn1
                               (list (loc-ref a-sym) (loc-ref b-sym))
                               (list
                                (-loc-set b-sym (ast-e-fun-call 'fn2 (list (loc-ref c-sym)) (list (-loc-set c-sym (ast-ev-string "z"))) '()))
                                (-loc-set a-sym (ast-ev-bool #t)))
                               '())
               (symbol? a-sym)))

(define/contract (transform an-expression)
  (->* [ast-expression?] [] ast-expression?)
  (cond
    [(ast-e-if? an-expression) (raise-user-error "not implemented yet")]
    [(ast-e-fun-call? an-expression) (transform-function-call an-expression)]
    [else an-expression]))

(module+ test #| transform |#
  (check-match (transform (ast-e-fun-call 'byte+ (list (ast-ev-number 1) (ast-ev-id 'a-byte)) '() '()))
               (ast-e-fun-call 'byte+ (list (loc-ref a-sym) (ast-ev-id 'a-byte)) (list (-loc-set a-sym (ast-ev-number 1))) '())
               (symbol? a-sym)))

(struct gen-context
  (next-local local-id-map parameter-id-map gen-bytes)
  #:transparent
  #:guard
  (struct-guard/c byte? (hash/c symbol? byte?) (hash/c symbol? byte?) (listof byte?)))

;; generate references to parameters and encode them into one byte (VM_L0 ... VM_Lx, VM_P0 ... etc.)
(define/contract (generate-parameter parameter a-gen-context)
  (->* [ast-node? gen-context?] [] byte?)
  (cond
    [(ast-ev-id? parameter)
     (encode-idx (hash-ref (gen-context-parameter-id-map a-gen-context) (ast-ev-id-id parameter))
                 l-param)]
    [(loc-ref? parameter)
     (encode-idx (hash-ref (gen-context-local-id-map a-gen-context) (loc-ref-id parameter))
                 l-local)]
    [else (raise-user-error (format "unknown parameter ~a" parameter))]))

(define/contract (generate-loc-set a-loc-set a-gen-context)
  (->* [-loc-set? gen-context?] [] gen-context?)
  (define expr (-loc-set-expr a-loc-set))
  (define local (gen-context-next-local a-gen-context))
  (struct-copy gen-context a-gen-context
               [next-local (+ 1 local)]
               [local-id-map (hash-set (gen-context-local-id-map a-gen-context) (-loc-set-id a-loc-set) local)]
               [gen-bytes (append (gen-context-gen-bytes a-gen-context)
                                  (cond
                                    [(ast-ev-number? expr) (list CISC_VM_IMMB local (ast-ev-number-number expr))]
                                    [else (raise-user-error (format "unknown expression to set ~a" expr))]))]))

(module+ test #| generate-loc-set |#
  (check-equal? (generate-loc-set (-loc-set 'sym (ast-ev-number 15)) (gen-context 5 (hash) (hash) '()))
                (gen-context 6 '#hash((sym . 5)) '#hash() (list CISC_VM_IMMB 5 15))))

(define/contract (generate-fun-call target-reg a-fun-call a-gen-context)
  (->* [byte? ast-e-fun-call? gen-context?] [] gen-context?)
  (define fun-id (ast-e-fun-call-fun a-fun-call))
     (define fun-params (ast-e-fun-call-params a-fun-call))
     (define pre-code (ast-e-fun-call-pre-code a-fun-call))
     (define next-gen-context (foldl (lambda (pre-expression inner-gen-context) (generate 0 pre-expression inner-gen-context)) a-gen-context pre-code))
     (cond
       [(eq? fun-id 'byte+)
        (struct-copy gen-context next-gen-context
                     [gen-bytes (append (gen-context-gen-bytes next-gen-context)
                                        (list CISC_VM_BYTE_ADD target-reg (generate-parameter (first fun-params) next-gen-context)
                                              (generate-parameter (second fun-params) next-gen-context)))])]
       [else (raise-user-error (format "unknown function id ~a" fun-id))]))

(module+ test #| generate-fun-call |#
  (check-equal? (generate-fun-call VM_L1 (ast-e-fun-call 'byte+ (list (loc-ref 'sym) (ast-ev-id 'param))
                                                         (list (-loc-set 'sym (ast-ev-number 11)))
                                                         '())
                                   (gen-context 0 (hash) (hash 'param 1) '()))
                (gen-context 1 '#hash((sym . 0)) '#hash((param . 1))
                             (list CISC_VM_IMMB     VM_L0 11
                                   CISC_VM_BYTE_ADD VM_L1 VM_L0 VM_P1))))

;; generate code for an-expression, assigning result into target-reg within the given gen-context
(define/contract (generate target-reg an-expression a-gen-context)
  (->* [byte? ast-expression? gen-context?] [] gen-context?)
  (cond
    [(-loc-set? an-expression) ;; makes no use of target-reg
     (generate-loc-set an-expression a-gen-context)]
    [(ast-e-fun-call? an-expression)
     (generate-fun-call target-reg an-expression a-gen-context)]
    [else (raise-user-error (format "unknown expression to generate ~a" an-expression))]))

(module+ test #| gen-context |#
  (check-equal? (gen-context-gen-bytes (generate 0 (-loc-set 'sym (ast-ev-number 10)) (gen-context 0 (hash) (hash) '())))
                (list CISC_VM_IMMB VM_L0 10))

  (check-equal? (gen-context-gen-bytes
                 (generate VM_L1
                           (ast-e-fun-call 'byte+
                                           (list (loc-ref 'byte-10) (ast-ev-id 'a-byte))
                                           (list (-loc-set 'byte-10 (ast-ev-number 10)))
                                           '())
                           (gen-context 0 (hash) (hash 'a-byte 0) '())))
                (list
                 CISC_VM_IMMB VM_L0 10
                 CISC_VM_BYTE_ADD VM_L1 VM_L0 VM_P0)))

(define/contract (--prep-param-hashmap params)
  (->* [(listof ast-param-def?)] [] hash?)
  (define param-ids (map ast-param-def-id params))
  (foldl (lambda (param hash-map) (hash-set hash-map param (hash-count hash-map))) (hash) param-ids))

(module+ test #| --prep-param-hashmap |#
  (check-equal? (--prep-param-hashmap (list (ast-param-def 'p0 (ast-td-simple 'string))
                                            (ast-param-def 'other (ast-td-simple 'number))))
                (hash 'p0 0 'other 1)))

(define/contract (cisc-vm-transform fun)
  (->* [ast-function-def?] [] gen-context?)
  (define transformed-expressions (map transform (ast-function-def-body fun)))
  ;; register all parameter-id s into the generation context
  (define param-map (--prep-param-hashmap (ast-function-def-parameter fun)))
  ;; generate (into target local slot 0) expression(s)
  (define new-gen-context (foldl (lambda (body-expr a-gen-ctx) (generate VM_L0 body-expr a-gen-ctx)) (gen-context 0 (hash) param-map '()) transformed-expressions))
  ;; count the needed local slots (for later function registration)
  ;; generate return local slot 0
  (define locals-needed (gen-context-next-local new-gen-context))
  (struct-copy gen-context new-gen-context
               [gen-bytes (append (gen-context-gen-bytes new-gen-context)
                                  (list CISC_VM_RET (- locals-needed 1)))]))

(module+ test #| compile |#

  (require (only-in "../6510-utils.rkt" two-complement-of))

  (check-match
   (cisc-vm-transform
    (m-def (add1 (a-byte byte) -> byte
                 "add one to the given")
           (byte+ 1 a-byte)))
   (gen-context 1 a-hash (hash 'a-byte 0)
                (list CISC_VM_IMMB VM_L0 1
                      CISC_VM_BYTE_ADD VM_L0 VM_L0 VM_P0 ;; optimization: byte could be directly encoded into parameter (encode-idx 1 l-imm)
                      CISC_VM_RET VM_L0))
   (hash? a-hash))


  ;; next test: tail call recursion
  ;; next test: if/cond

  (skip "transformation (if, tail-recursion) not implemented yet"
        (check-equal?
         (cisc-vm-transform
          (m-def (reverse (a-list (list cell)) (b-list (list cell) '()) -> (list cell)
                          "reverse a-list, consing it into b-list")
                 (if (nil? a-list)
                     b-list
                     (reverse (cdr a-list) (cons (car a-list) b-list)))))
         (list CISC_VM_BRA_EMPTY_LIST VM_P0 13
               CISC_VM_CAR            VM_L0 VM_P0
               CISC_VM_CONS           VM_P1 VM_L0 VM_P1
               CISC_VM_CDR            VM_P0 VM_P0
               CISC_VM_GOTO           (two-complement-of -14)
               CISC_VM_RET            VM_P1))))


;; TODO: now transform this tree into a flat list of cisc vm commands
;; idea: recursively transform innermost nodes into cisc commands,
;; allocating locals,
;; (tracking liveliness,) <- optimize later on that
;; replacing tree nodes with cisc
