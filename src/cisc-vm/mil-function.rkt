#lang racket

;; basic types
;;   bool, (char), byte, int, string
;; complex types
;;   array, list, map, struct

(require syntax/parse/define)

(module+ test #| require test utils |#
  (require "../6510-test-utils.rkt"))

(define-syntax (-> stx)
  (raise-syntax-error #f "cannot be used as an expression" stx))

(struct type-def
  ()
  #:transparent)

(struct td-simple type-def
  (id)
  #:transparent
  #:guard
  (struct-guard/c symbol?))

(struct td-complex type-def
  (id params)
  #:transparent
  #:guard
  (struct-guard/c symbol? (listof type-def?)))

(struct parameter-def
  (id type)
  #:transparent
  #:guard
  (struct-guard/c symbol? type-def?))

(struct expression-def
  ()
  #:transparent)

(struct ed-nil expression-def
  ;; the nil symbol/list
  ()
  #:transparent)

(struct ed-if-usage expression-def
  ;; special because parameters of if underly a sepcial execution order (no strict eval)
  (bool-expr true-expr false-exprs)
  #:transparent
  #:guard
  (struct-guard/c expression-def? expression-def? (listof expression-def?)))

(struct ed-function-call expression-def
  ;; strict eval parameter evaluation
  (fun params)
  #:transparent
  #:guard
  (struct-guard/c symbol? (listof expression-def?)))

(struct ed-value expression-def
  ;; atomic values not containing others
  ()
  #:transparent)

(struct edv-string ed-value
  (str)
  #:transparent
  #:guard
  (struct-guard/c string?))

(struct edv-boolean ed-value
  (bool)
  #:transparent
  #:guard
  (struct-guard/c boolean?))

(struct edv-id ed-value
  (id)
  #:transparent
  #:guard
  (struct-guard/c symbol?))

(struct edv-number ed-value
  (number)
  #:transparent
  #:guard
  (struct-guard/c exact-integer?))

(struct default-parameter-def parameter-def
  (value)
  #:transparent
  #:guard
  (struct-guard/c symbol? type-def? expression-def?))

(struct function-def
  (id parameter default-parameter return-type description body)
  #:transparent
  #:guard
  (struct-guard/c symbol?
                  (listof parameter-def?)
                  (listof default-parameter-def?)
                  type-def?
                  (listof string?)
                  (listof expression-def?)))

;; type definition parser
(define-syntax-parser m-type-def
  [(_ (cpx-type inner-type ...))
   #'(td-complex 'cpx-type (list (m-type-def inner-type) ...))]
  [(_ basic-type)
   #'(td-simple 'basic-type)])

;; (function) definition parser
(define-syntax-parser m-def
  #:literals [->]
  [(_ (id (p-id p-typ) ... (o-id o-typ o-val) ... -> r-typ desc ...) expr ...)
   #'(function-def 'id
                   (list (parameter-def 'p-id (m-type-def p-typ)) ...)
                   (list (default-parameter-def 'o-id (m-type-def o-typ) (m-expression-def o-val)
                           ) ...)
                   (m-type-def r-typ)
                   (list 'desc ...)
                   (list (m-expression-def expr) ...))])

(module+ test #| m-def |#
  (define mf0 (m-def (f0 -> string "description1" "description2") "value-hello"))

  (check-equal? (function-def-id mf0)
                'f0)
  (check-equal? (function-def-parameter mf0)
                '())
  (check-equal? (function-def-default-parameter mf0)
                '())
  (check-equal? (function-def-return-type mf0)
                (td-simple 'string))
  (check-equal? (function-def-description mf0)
                (list "description1" "description2"))

  (define mf1 (m-def (f1 (a string) (b bool) (q string "h") -> string)
                     (hello)
                     (hello2)))

  (check-equal? (function-def-id mf1)
                'f1)
  (check-equal? (function-def-parameter mf1)
                (list (parameter-def 'a (td-simple 'string))
                      (parameter-def 'b (td-simple 'bool))))
  (check-equal? (function-def-default-parameter mf1)
                (list (default-parameter-def 'q (td-simple 'string) (edv-string "h"))))
  (check-equal? (function-def-return-type mf1)
               (td-simple 'string))
  (check-equal? (function-def-description mf1)
                '())
  (check-equal? (function-def-body mf1)
                (list (ed-function-call 'hello (list))
                      (ed-function-call 'hello2 (list))))

  (define mf2 (m-def (f2 (a string) -> bool)))

  (check-equal? (function-def-id mf2)
                'f2)
  (check-equal? (function-def-parameter mf2)
                (list (parameter-def 'a (td-simple 'string))))
  (check-equal? (function-def-default-parameter mf2)
                '())
  (check-equal? (function-def-return-type mf2)
                (td-simple 'bool))
  (check-equal? (function-def-description mf2)
                '())

  (define mf3 (m-def (f3 (a string "init") -> (list int))))

  (check-equal? (function-def-id mf3)
                'f3)
  (check-equal? (function-def-parameter mf3)
                '()
                )
  (check-equal? (function-def-default-parameter mf3)
                (list (default-parameter-def 'a (td-simple 'string) (edv-string "init"))))
  (check-equal? (function-def-return-type mf3)
                (td-complex 'list (list (td-simple 'int))))
  (check-equal? (function-def-description mf3)
                '()))

;; expression parser
(define-syntax-parser m-expression-def
  [(_ '())
   #'(ed-nil)]
  [(_ ((~literal if) bool-param true-param false-param ...))
   #'(ed-if-usage (m-expression-def bool-param)
                                      (m-expression-def true-param)
                                      (list (m-expression-def false-param) ...))]
  [(_ (id param ...))
   #'(ed-function-call 'id (list (m-expression-def param) ...))]
  [(_ value)
   #'(cond ((string? 'value) (edv-string 'value))
           ((boolean? 'value) (edv-boolean 'value))
           ((exact-integer? 'value) (edv-number 'value))
           ((symbol? 'value) (edv-id 'value))
           (else (raise-user-error (format "unknown expression value type ~a" 'value))))])

(module+ test #| m-expression-def |#
  (check-equal?
   (m-expression-def '())
   (ed-nil))

  (check-equal?
   (m-expression-def "hello")
   (edv-string "hello"))

  (check-equal?
   (m-expression-def 123)
   (edv-number 123))

  (check-equal?
   (m-expression-def #t)
   (edv-boolean #t))

  (check-equal?
   (m-expression-def (fn1 "A" #t))
   (ed-function-call 'fn1 (list (edv-string "A") (edv-boolean #t))))

  (check-equal?
   (m-expression-def (if #t "A" "B"))
   (ed-if-usage (edv-boolean #t) (edv-string "A") (list (edv-string "B"))))

  (check-equal?
   (m-expression-def (fn2))
   (ed-function-call 'fn2 (list)))

  (check-equal?
   (m-expression-def (fn3 (fn5 "A") (- 1 2)))
   (ed-function-call 'fn3 (list (ed-function-call 'fn5 (list (edv-string "A")))
                                (ed-function-call '- (list (edv-number 1) (edv-number 2)))))))

(module+ test #| simple reverse function |#
  (check-equal?
   (m-def (reverse (a-list (list cell)) (b-list (list cell) '()) -> (list cell)
                   "reverse a-list, consing it into b-list")
          (if (nil? a-list)
              b-list
              (reverse (cdr a-list) (cons (car a-list) b-list))))
   (function-def
    'reverse
    ;; parameter
    (list (parameter-def 'a-list (td-complex 'list (list (td-simple 'cell)))))
    ;; parameter with defaults
    (list
     (default-parameter-def
       'b-list
       (td-complex 'list (list (td-simple 'cell)))
       (ed-nil)))
    ;; return type
    (td-complex 'list (list (td-simple 'cell)))
    ;; doc
    '("reverse a-list, consing it into b-list")
    ;; body
    (list
     (ed-if-usage
      (ed-function-call 'nil? (list (edv-id 'a-list)))
      (edv-id 'b-list)
      (list
       (ed-function-call
        'reverse
        (list
         (ed-function-call 'cdr (list (edv-id 'a-list)))
         (ed-function-call
          'cons
          (list
           (ed-function-call 'car (list (edv-id 'a-list)))
           (edv-id 'b-list)))))))))))

;; intermediate ast nodes (used during transformation)
(struct -loc-set expression-def
  (id expr)
  #:transparent)

(struct loc-ref expression-def
  (id)
  #:transparent)

(define step0 (list
               (ed-if-usage
                (ed-function-call 'nil? (list (edv-id 'a-list)))
                (edv-id 'b-list)
                (list
                 (ed-function-call
                  'reverse
                  (list
                   (ed-function-call 'cdr (list (edv-id 'a-list)))
                   (ed-function-call
                    'cons
                    (list
                     (ed-function-call 'car (list (edv-id 'a-list))) ;; extract this inner call to some variable 'a
                     (edv-id 'b-list)))))))))

(define step1 (list
     (ed-if-usage
      (ed-function-call 'nil? (list (edv-id 'a-list)))
      (edv-id 'b-list)
      (list
       (ed-function-call
        'reverse
        (list
         (ed-function-call 'cdr (list (edv-id 'a-list))) ;; call has only refs to ids (done)
         (-loc-set 'a (ed-function-call 'car (list (edv-id 'a-list))))
         (ed-function-call 'cons (list (loc-ref 'a) (edv-id 'b-list))))))))) ;; now this call has only refs to ids (done), extract call itself to id b'

(define step2 (list
     (ed-if-usage
      (ed-function-call 'nil? (list (edv-id 'a-list)))
      (edv-id 'b-list)
      (list
       (ed-function-call
        'reverse
        (list
         (ed-function-call 'cdr (list (edv-id 'a-list))) ;; extract call itself to id 'c
         (-loc-set 'a (ed-function-call 'car (list (edv-id 'a-list))))
         (-loc-set 'b (ed-function-call 'cons (list (loc-ref 'a) (edv-id 'b-list))))
         (loc-ref 'b)))))))

(define step3 (list
     (ed-if-usage
      (ed-function-call 'nil? (list (edv-id 'a-list)))
      (edv-id 'b-list)
      (list
       (-loc-set 'a (ed-function-call 'car (list (edv-id 'a-list))))
       (-loc-set 'b (ed-function-call 'cons (list (loc-ref 'a) (edv-id 'b-list)))) ;; p1 can be reused here
       (-loc-set 'c (ed-function-call 'cdr (list (edv-id 'a-list)))) ;; p0 can be reused here
       (ed-function-call 'reverse (list (loc-ref 'c) (loc-ref 'b))))))) ;; now reverse has only ref parameters (done)

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
  (->* [ed-function-call?] [] boolean?)
  (foldl (lambda (l r) (and (or (loc-ref? l) (edv-id? l)) r)) true (ed-function-call-params fun-call)))

(module+ test
  (check-true (ed-function-call--has-only-refs? (ed-function-call 'fn1 (list))))

  (check-true (ed-function-call--has-only-refs? (ed-function-call 'fn1 (list (loc-ref 'a) (edv-id 'b)))))

  (check-false (ed-function-call--has-only-refs? (ed-function-call 'fn1 (list (loc-ref 'a) (edv-string "10"))))))

;; recursively extract all non refs from the given parameters (of a function all)
;; return the
;; - resulting params (must all be refs)
;; - setters that need to be prepended to the call
;; - list of newly introduced references
(define/contract (ed-function-call-params--mapper params (symbol-generator gensym))
  (->* [(listof expression-def?)] [any/c]
      (listof (list/c (or/c loc-ref? edv-id?) (listof -loc-set?) (listof loc-ref?))))
  (map (lambda (param)
         (cond [(ed-function-call? param)
                ;; incomplete! (recursion into call params mapper does not keep prepends nor locrefs!
                (define sym (symbol-generator))
                (match-define (list new-func new-ref-list prepends)
                  (ed-function-call--extract-refs param '() '() symbol-generator))
                (list (loc-ref sym) (cons (-loc-set sym new-func) prepends) new-ref-list)]
               [(ed-nil? param) (define sym (symbol-generator)) (list (loc-ref sym) (list (-loc-set sym param)) '())]
               [(edv-number? param) (define sym (symbol-generator)) (list (loc-ref sym) (list (-loc-set sym param)) '())]
               [(edv-string? param) (define sym (symbol-generator)) (list (loc-ref sym) (list (-loc-set sym param)) '())]
               [(edv-boolean? param) (define sym (symbol-generator)) (list (loc-ref sym) (list (-loc-set sym param)) '())]
               [(edv-id? param) (list param '() '())]
               [(loc-ref? param) (list param '() '())]
               [else (raise-user-error (format "unknown param type ~a" param))]))
       params))

(module+ test
  (check-equal? (ed-function-call-params--mapper (list (ed-nil) (edv-number 47) (edv-string "x") (edv-boolean #t)) (lambda () 'a))
                (list (list (loc-ref 'a) (list (-loc-set 'a (ed-nil))) '())
                      (list (loc-ref 'a) (list (-loc-set 'a (edv-number 47))) '())
                      (list (loc-ref 'a) (list (-loc-set 'a (edv-string "x"))) '())
                      (list (loc-ref 'a) (list (-loc-set 'a (edv-boolean #t))) '()))
                "values are extracted into a set to a ref and the ref")

  (check-equal? (ed-function-call-params--mapper (list (loc-ref 'a) (edv-id 'b)))
                (list (list (loc-ref 'a) '() '())
                      (list (edv-id 'b) '() '()))
                "a reference is not transformed => no new refs etc")

  (check-equal? (ed-function-call-params--mapper (list (loc-ref 'q) (edv-number 17)) (lambda () 'l1))
                (list (list (loc-ref 'q) '() '())
                      (list (loc-ref 'l1) (list (-loc-set 'l1 (edv-number 17))) '())))

  (check-match (ed-function-call-params--mapper (list (loc-ref 'q) (ed-function-call 'fn1 (list (edv-number 1)))))
               (list (list (loc-ref 'q) '() '())
                     (list (loc-ref a-sym)
                           (list (-loc-set a-sym (ed-function-call 'fn1 (list (loc-ref b-sym))))
                                 (-loc-set b-sym (edv-number 1))) '()))
               (and (symbol? a-sym) (symbol? b-sym)))

  (check-match (ed-function-call-params--mapper
                (list (loc-ref 'q) (ed-function-call 'fn1 (list (edv-id 'l) (ed-function-call 'r2 (list (edv-number 1)))))))
               (list (list (loc-ref 'q) '() '())
                     (list (loc-ref a-sym)
                           (list (-loc-set a-sym (ed-function-call 'fn1 (list (edv-id 'l) (loc-ref b-sym))))
                                 (-loc-set b-sym (ed-function-call 'r2 (list (loc-ref c-sym))))
                                 (-loc-set c-sym (edv-number 1))) '()))
               (and (symbol? a-sym) (symbol? b-sym) (symbol? c-sym)))

  (check-match (ed-function-call-params--mapper
                (list
                 (ed-function-call 'cdr (list (edv-id 'a-list)))
                 (ed-function-call
                  'cons
                  (list
                   (ed-function-call 'car (list (edv-id 'a-list)))
                   (edv-id 'b-list)))))
               (list (list (loc-ref c-sym)
                           (list (-loc-set c-sym (ed-function-call 'cdr (list (edv-id 'a-list)))))
                           '())
                     (list (loc-ref b-sym)
                           (list (-loc-set b-sym (ed-function-call 'cons (list (loc-ref a-sym) (edv-id 'b-list))))
                                 (-loc-set a-sym (ed-function-call 'car (list (edv-id 'a-list)))))
                           '()))
               (and (symbol? a-sym) (symbol? b-sym) (symbol? c-sym))))

(define/contract (ed-function-call--extract-refs fun-call (ref-list '()) (prepends '()) (symbol-generator gensym))
  (->* [ed-function-call?]
      [(listof loc-ref?) (listof -loc-set?) any/c]
      (list/c ed-function-call? (listof loc-ref?) (listof -loc-set?)))
  (cond [(ed-function-call--has-only-refs? fun-call) (list fun-call ref-list prepends)]
        [else
         (define transformed-params
           (ed-function-call-params--mapper (ed-function-call-params fun-call) symbol-generator))
         (list
          (ed-function-call
           (ed-function-call-fun fun-call)
           (map (lambda (param) (car param)) transformed-params))
          (append ref-list (flatten (map (lambda (param) (cddr param)) transformed-params)))
          (append prepends (flatten (map (lambda (param) (cadr param)) transformed-params))))]))

(define/contract (cisc-vm-transform fun)
  (->* [function-def?] [] (listof byte?))
  (list 0))

(module+ test #| compile |#
  (require (only-in "./vm-structures.rkt"
                    CISC_VM_CONS
                    CISC_VM_CAR
                    CISC_VM_BRA_EMPTY_LIST
                    CISC_VM_CDR
                    CISC_VM_GOTO
                    CISC_VM_RET

                    VM_L0
                    VM_P0
                    VM_P1))

  (require (only-in "../6510-utils.rkt" two-complement-of))


  (skip "transformation is not implemented yet"
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
