#lang typed/racket

(require (only-in "./util.rkt" nested->list low-byte high-byte bytes->int))
(require "./svm-ast.rkt")

(require (for-syntax typed/racket syntax/parse/pre))

(provide m-type-def
         m-expression-def
         m-fun-def)

(module+ test #| require test utils |#
  ;; (require "../6510-test-utils.rkt")
  (require typed/rackunit))

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

    [(_ ((~literal with) ((id type expression) ...) body))
     #'(ast-ex-with- (make-ast-info)
                     (list (ast-ex-with-local- (make-ast-info) 'id (m-type-def type) (m-expression-def expression)) ...)
                     (m-expression-def body))]

    [(_ ((~literal if) bool-param true-param false-param))
     #'(ast-ex-if- (make-ast-info)
                   (m-expression-def bool-param)
                   (m-expression-def true-param)
                   (m-expression-def false-param)
                   #f)]

    [(_ ((~literal cond) [case-cond case-expression] ... [(~literal _) else-expression]))
     #'(ast-ex-cond- (make-ast-info)
                     (list (ast-ex-cond-clause- (make-ast-info) (m-expression-def case-cond) (m-expression-def case-expression)) ...)
                     (m-expression-def else-expression))]

    [(_ ((~literal lambda) ((p-id p-typ) ... (o-id o-typ o-val) ... -> r-typ) expr))
     #'( ast-ex-fun-def- (make-ast-info)
                         'lambda
                         (list (ast-param-def- (make-ast-info) 'p-id (m-type-def p-typ)) ...)
                         (list (ast-pa-defaulted-def- (make-ast-info) 'o-id (m-type-def o-typ) (m-expression-def o-val)) ...)
                         (m-type-def r-typ)
                         (list)
                         (m-expression-def expr))]

    [(_ (id param ...))
     #'(ast-ex-fun-call- (make-ast-info) 'id (list (m-expression-def param) ...))]

    [(_ value)
     #'(cond ((string? 'value) (ast-at-string- (make-ast-info) 'value))
             ((boolean? 'value) (ast-at-bool- (make-ast-info) 'value))
             ((exact-integer? 'value) (ast-at-int- (make-ast-info) 'value))
             ((symbol? 'value) (ast-at-id- (make-ast-info) 'value))
             (else (raise-user-error (format "unknown expression value type ~a" 'value))))]))

(module+ test #| m-expression-def |#
  (check-true (match (nested->list (m-expression-def (lambda ((a : list*) (b : bool) -> bool) b)))
                [(list 'ast-ex-fun-def- _ 'lambda _ ...) #t]
                [_ #f])
              "parse anonymous lambda expression")

  (check-true
   (match (nested->list (m-expression-def (with () "some")))
     [(list 'ast-ex-with- _ (list) (list 'ast-at-string- _ "some")) #t]
     [_ #f])
   "empty with is properly parsed as expression")

  (check-true
   (match (nested->list (m-expression-def (with ((a byte 42)) "some")))
     [(list 'ast-ex-with- _ (list (list 'ast-ex-with-local- _ 'a _ (list 'ast-at-int- _ 42))) (list 'ast-at-string- _ "some")) #t]
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
            (list (list 'ast-ex-cond-clause- _ (list 'ast-at-id- _ 'a) (list 'ast-at-int- _ 1))
                  (list 'ast-ex-cond-clause- _ (list 'ast-at-id- _ 'b) (list 'ast-at-int- _ 3)))
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
    [(_ (id (p-id p-typ) ... (o-id o-typ o-val) ... -> r-typ desc ...) expr)
     #'( ast-ex-fun-def- (make-ast-info)
                         'id
                         (list (ast-param-def- (make-ast-info) 'p-id (m-type-def p-typ)) ...)
                         (list (ast-pa-defaulted-def- (make-ast-info) 'o-id (m-type-def o-typ) (m-expression-def o-val)) ...)
                         (m-type-def r-typ)
                         (list 'desc ...)
                         (m-expression-def expr))]))

(module+ test #| m-fun-def |#
  (check-true
   (match (nested->list (m-fun-def (mf (p0 A) (p1 B) (p2 C 10) -> C "some")
                                  #f))
     [(list  ast-ex-fun-def-
             _
             'mf
             (list (list 'ast-param-def- _ 'p0 (list 'ast-td-simple- _ 'A))
                   (list 'ast-param-def- _ 'p1 (list 'ast-td-simple- _ 'B)))
             (list (list 'ast-pa-defaulted-def- _ 'p2 (list 'ast-td-simple- _ 'C)
                         (list 'ast-at-int- _ 10)))
             (list 'ast-td-simple- _ 'C)
             (list "some")
             (list 'ast-at-bool- _ #f)) #t]
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
            (list ast-ex-if-
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
                  #f))
      #t]
     [_ #f])))
