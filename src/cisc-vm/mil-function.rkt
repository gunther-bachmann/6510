#lang racket

;; basic types
;;   bool, (char), byte, int, string
;; complex types
;;   array, list, map, struct

(require syntax/parse/define)

(require (only-in "../6510-utils.rkt" two-complement-of))

(require (only-in "./vm-structures.rkt"
                  disassemble
                  make-vm
                  CISC_VM_CONS
                  CISC_VM_CAR
                  CISC_VM_BRA_EMPTY_LIST
                  CISC_VM_CDR
                  CISC_VM_GOTO
                  CISC_VM_RET
                  CISC_VM_IMMB
                  CISC_VM_BYTE_ADD
                  CISC_VM_BRA
                  CISC_VM_BRA_NOT
                  CISC_VM_MOVE
                  CISC_VM_CALL
                  CISC_VM_NIL_P
                  CISC_VM_NOT

                  VM_L0
                  VM_L1
                  VM_L2
                  VM_L3
                  VM_L4
                  VM_L5

                  VM_P0
                  VM_P1

                  encode-idx
                  l-local
                  l-param
                  l-global
                  l-imm))

(module+ test #| require test utils |#
  (require "../6510-test-utils.rkt"))


(define-syntax (-> stx)
  (raise-syntax-error #f "cannot be used as an expression" stx))


(struct ast-gen-info
  (pre-code          ;; code that is generated before this very node, (e.g. preparing locals to be passed as parameters to a function call)
   post-code         ;; code that is generated after this very node
   target-reg        ;; register the final result of this node should be present (e.g. in order for the follow up node to pick it up)
   target-reg-pref   ;; unused yet
   locals-used)      ;; number of locals used up by this node (and its sub nodes)
  #:transparent
  #:guard (struct-guard/c (listof any/c) (listof any/c) byte? symbol? byte?))

(define (make-ast-gen-info #:pre-code (a-pre-code '())
                           #:post-code (a-post-code '())
                           #:target-reg (a-target-reg (encode-idx 0 l-local))
                           #:target-reg-pref (a-pref 'default)
                           #:locals-used (a-local-used 0))
  (ast-gen-info a-pre-code a-post-code a-target-reg a-pref a-local-used))

(define agsi (make-ast-gen-info))

;; generic root node for all ast nodes
(struct ast-node
  (gen-info)
  #:transparent
  #:guard (struct-guard/c ast-gen-info?))

;; type definition
(struct ast-type-def ast-node
  ()
  #:transparent
  #:guard (struct-guard/c ast-gen-info?))

;; simple atomic type definition (like bool, string, int, byte, symbol)
(struct ast-td-simple ast-type-def
  (id) ;; type name/symbol
  #:transparent
  #:guard
  (struct-guard/c ast-gen-info?    symbol?))

;; complex type definition with parameters (like (array byte), or (map string string))
(struct ast-ts-complex ast-type-def
  (id       ;; type name/symbol
   params)  ;; type parameters itself type definitions
  #:transparent
  #:guard
  (struct-guard/c ast-gen-info? symbol? (listof ast-type-def?)))

;; parameter definition as used by function definitions
(struct ast-param-def ast-node
  (id     ;; parameter name/symbol
   type)  ;; type of the parameter
  #:transparent
  #:guard
  (struct-guard/c ast-gen-info? symbol? ast-type-def?))

;; root of all expression ast nodes
(struct ast-expression ast-node
  ()
  #:transparent
  #:guard (struct-guard/c ast-gen-info?))

;; expression nil
(struct ast-e-nil ast-expression
  ;; the nil symbol/list
  ()
  #:transparent
  #:guard (struct-guard/c ast-gen-info?))

;; function call
(struct ast-e-fun-call ast-expression
  ;; strict eval parameter evaluation
  (fun         ;; function name/id
   params)     ;; parameters passed to the function
  #:transparent
  #:guard
  (struct-guard/c ast-gen-info? symbol? (listof ast-expression?)))

;; special form if (looking like a function call)
(struct ast-e-if ast-e-fun-call
  ;; special because parameters of 'if' have special execution order (no strict eval)
  (negated)
  #:transparent
  #:guard
  (struct-guard/c ast-gen-info? symbol? (listof ast-expression?) boolean?))

;; value expression, representation an atomic value
(struct ast-e-value ast-expression
  ;; atomic values not containing others
  ()
  #:transparent
  #:guard (struct-guard/c ast-gen-info?))

;; value expression string
(struct ast-ev-string ast-e-value
  (str) ;; actual string
  #:transparent
  #:guard
  (struct-guard/c ast-gen-info? string?))

;; value expression boolean (either true or false)
(struct ast-ev-bool ast-e-value
  (bool)
  #:transparent
  #:guard
  (struct-guard/c ast-gen-info? boolean?))

;; value expression id (referencing a value in scope)
(struct ast-ev-id ast-e-value
  (id) ;; symbol/name of value referenced
  #:transparent
  #:guard
  (struct-guard/c ast-gen-info? symbol?))

;; number value expression
(struct ast-ev-number ast-e-value
  (number) ;; TODO: split this up into small-byte (fitting into 4 bit), byte and int
  #:transparent
  #:guard
  (struct-guard/c ast-gen-info? exact-integer?))

;; parameter definition of a parameter with default value (expression)
(struct ast-pd-default-param ast-param-def
  (value)
  #:transparent
  #:guard
  (struct-guard/c ast-gen-info? symbol? ast-type-def? ast-expression?))

;; function definition
(struct ast-function-def ast-node
  (id                 ;; name/symbol of the function
   parameter          ;; list of typed parameters without default initializer
   default-parameter  ;; list of typed parameters with default initializers
   return-type        ;; type definition of the return value
   description        ;; list of strings describing this function
   body)              ;; list of expressions making up the body of the function
  #:transparent
  #:guard
  (struct-guard/c ast-gen-info?
                  symbol?
                  (listof ast-param-def?)
                  (listof ast-pd-default-param?)
                  ast-type-def?
                  (listof string?)
                  (listof ast-expression?)))

(module+ test #| deep-struct->list |#
  (require racket/struct)

  ;; function to make a nested structure into a nested list, making it easier for check-match to only specify the actual values wanted to check
  (define (nested->list deeply-nested)
    (cond
      [(struct? deeply-nested)
       (define-values (info _a) (struct-info deeply-nested))
       (define-values (struct-name _b _c _d _e _f _g _h) (struct-type-info info))
       (cons struct-name (nested->list (struct->list deeply-nested)))]

      [(list? deeply-nested)
       (map nested->list deeply-nested)]

      [(hash? deeply-nested)
       (foldl (lambda (key acc-hash) (hash-set acc-hash key (nested->list (hash-ref deeply-nested key))))
            (hash) (hash-keys deeply-nested))]

      [else deeply-nested]))

  ;; make sure last element in ast-ev-bool structure is true, ignoring the rest
  (check-match (nested->list (ast-ev-bool (make-ast-gen-info) #t))
               (list 'ast-ev-bool _ ... #t))

  ;; allow for match partial hashes! and convert structures in hashes to lists
  (check-match (nested->list (hash 'b "some" 'a (ast-ev-bool (make-ast-gen-info) #t)))
               (hash 'a (list 'ast-ev-bool (list 'ast-gen-info _ ...) #t) #:open))

  ;; check that target register is 0 in the gen info nested in ast-ev-bool
  ;; this is particularly useful since the number of elements in ast-gen-info may change (at tail)
  (check-match (nested->list (ast-ev-bool (make-ast-gen-info) #t))
               (list 'ast-ev-bool (list 'ast-gen-info _ _ 0 _ ... ) _ ...)))

;; type definition parser
(define-syntax-parser m-type-def
  [(_ (cpx-type inner-type ...))
   #'(ast-ts-complex agsi 'cpx-type (list (m-type-def inner-type) ...))]
  [(_ basic-type)
   #'(ast-td-simple agsi 'basic-type)])

;; (function) definition parser
(define-syntax-parser m-def
  #:literals [->]
  [(_ (id (p-id p-typ) ... (o-id o-typ o-val) ... -> r-typ desc ...) expr ...)
   #'(ast-function-def agsi
                       'id
                       (list (ast-param-def agsi 'p-id (m-type-def p-typ)) ...)
                       (list (ast-pd-default-param agsi 'o-id (m-type-def o-typ) (m-expression-def o-val)) ...)
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
                (ast-td-simple agsi 'string))
  (check-equal? (ast-function-def-description mf0)
                (list "description1" "description2"))

  (define mf1 (m-def (f1 (a string) (b bool) (q string "h") -> string)
                     (hello)
                     (hello2)))

  (check-equal? (ast-function-def-id mf1)
                'f1)
  (check-equal? (ast-function-def-parameter mf1)
                (list (ast-param-def agsi 'a (ast-td-simple agsi 'string))
                      (ast-param-def agsi 'b (ast-td-simple agsi 'bool))))
  (check-equal? (ast-function-def-default-parameter mf1)
                (list (ast-pd-default-param agsi 'q (ast-td-simple agsi 'string) (ast-ev-string agsi "h"))))
  (check-equal? (ast-function-def-return-type mf1)
                (ast-td-simple agsi 'string))
  (check-equal? (ast-function-def-description mf1)
                '())
  (check-equal? (ast-function-def-body mf1)
                (list (ast-e-fun-call agsi 'hello (list))
                      (ast-e-fun-call agsi 'hello2 (list))))

  (define mf2 (m-def (f2 (a string) -> bool)))

  (check-equal? (ast-function-def-id mf2)
                'f2)
  (check-equal? (ast-function-def-parameter mf2)
                (list (ast-param-def agsi 'a (ast-td-simple agsi 'string))))
  (check-equal? (ast-function-def-default-parameter mf2)
                '())
  (check-equal? (ast-function-def-return-type mf2)
                (ast-td-simple agsi 'bool))
  (check-equal? (ast-function-def-description mf2)
                '())

  (define mf3 (m-def (f3 (a string "init") -> (list int))))

  (check-equal? (ast-function-def-id mf3)
                'f3)
  (check-equal? (ast-function-def-parameter mf3)
                '()
                )
  (check-equal? (ast-function-def-default-parameter mf3)
                (list (ast-pd-default-param agsi 'a (ast-td-simple agsi 'string) (ast-ev-string agsi "init"))))
  (check-equal? (ast-function-def-return-type mf3)
                (ast-ts-complex agsi 'list (list (ast-td-simple agsi 'int))))
  (check-equal? (ast-function-def-description mf3)
                '()))

;; expression parser
(define-syntax-parser m-expression-def
  [(_ '())
   #'(ast-e-nil agsi)]
  [(_ ((~literal if) bool-param true-param false-param))
   #'(ast-e-if agsi 'if (cons (m-expression-def bool-param)
                              (list
                               (m-expression-def true-param)
                               (m-expression-def false-param)))
               #f)]
  [(_ ((~literal cond) ((case-cond) (case-expression) ...) ...))
   #'()]
  [(_ (id param ...))
   #'(ast-e-fun-call agsi 'id (list (m-expression-def param) ...))]
  [(_ value)
   #'(cond ((string? 'value) (ast-ev-string agsi 'value))
           ((boolean? 'value) (ast-ev-bool agsi 'value))
           ((exact-integer? 'value) (ast-ev-number agsi 'value))
           ((symbol? 'value) (ast-ev-id agsi 'value))
           (else (raise-user-error (format "unknown expression value type ~a" 'value))))])

(module+ test #| m-expression-def |#
  (check-equal?
   (m-expression-def '())
   (ast-e-nil agsi))

  (check-equal?
   (m-expression-def "hello")
   (ast-ev-string agsi "hello"))

  (check-equal?
   (m-expression-def 123)
   (ast-ev-number agsi 123))

  (check-equal?
   (m-expression-def #t)
   (ast-ev-bool agsi #t))

  (check-equal?
   (m-expression-def (fn1 "A" #t))
   (ast-e-fun-call agsi 'fn1 (list (ast-ev-string agsi "A") (ast-ev-bool agsi #t))))

  (check-equal?
   (m-expression-def (if #t "A" "B"))
   (ast-e-if agsi 'if (list (ast-ev-bool agsi #t) (ast-ev-string agsi "A") (ast-ev-string agsi "B")) #f))

  (check-equal?
   (m-expression-def (fn2))
   (ast-e-fun-call agsi 'fn2 (list)))

  (check-equal?
   (m-expression-def (fn3 (fn5 "A") (- 1 2)))
   (ast-e-fun-call agsi 'fn3 (list (ast-e-fun-call agsi 'fn5 (list (ast-ev-string agsi "A")))
                                   (ast-e-fun-call agsi '- (list (ast-ev-number agsi 1) (ast-ev-number agsi 2)))))))

(module+ test #| simple reverse function |#
  (check-equal?
   (m-def (reverse (a-list (list cell)) (b-list (list cell) '()) -> (list cell)
                   "reverse a-list, consing it into b-list")
          (if (nil? a-list)
              b-list
              (reverse (cdr a-list) (cons (car a-list) b-list))))
   (ast-function-def
    agsi
    'reverse
    ;; parameter
    (list (ast-param-def agsi 'a-list (ast-ts-complex agsi 'list (list (ast-td-simple agsi 'cell)))))
    ;; parameter with defaults
    (list
     (ast-pd-default-param agsi
                           'b-list
                           (ast-ts-complex agsi 'list (list (ast-td-simple agsi 'cell)))
                           (ast-e-nil agsi)))
    ;; return type
    (ast-ts-complex agsi 'list (list (ast-td-simple agsi 'cell)))
    ;; doc
    '("reverse a-list, consing it into b-list")
    ;; body
    (list
     (ast-e-if agsi
               'if
               (list
                (ast-e-fun-call agsi 'nil? (list (ast-ev-id agsi 'a-list)))
                (ast-ev-id agsi 'b-list)
                (ast-e-fun-call agsi
                                'reverse
                                (list
                                 (ast-e-fun-call agsi 'cdr (list (ast-ev-id agsi 'a-list)))
                                 (ast-e-fun-call agsi
                                                 'cons
                                                 (list
                                                  (ast-e-fun-call agsi 'car (list (ast-ev-id agsi 'a-list)))
                                                  (ast-ev-id agsi 'b-list)))))) #f)))))

;; --- intermediate ast nodes (used during transformation) e.g. in pre-code

;; set a variable in scope to an expression result
(struct ast-e-loc-set ast-expression
  (id     ;; variable to set
   expr)  ;; expression to eval
  #:transparent)

;; reference a variable in scope (duplicate of ast-ev-id?)
(struct ast-e-loc-ref ast-expression
  (id)
  #:transparent)

(module+ test #| documenting steps during code generation |#

  (define step0 (list
                 (ast-e-if
                  agsi
                  'if
                  (list
                   (ast-e-fun-call agsi 'nil? (list (ast-ev-id agsi 'a-list)))
                   (ast-ev-id  agsi 'b-list)
                   (ast-e-fun-call
                    agsi
                    'reverse
                    (list
                     (ast-e-fun-call agsi 'cdr (list (ast-ev-id agsi 'a-list)))
                     (ast-e-fun-call
                      agsi
                      'cons
                      (list
                       (ast-e-fun-call agsi 'car (list (ast-ev-id agsi 'a-list))) ;; extract this inner call to some variable 'a
                       (ast-ev-id agsi 'b-list))))))
                  #f)))

  (define step1 (list
                 (ast-e-if
                  agsi
                  'if
                  (list
                   (ast-e-fun-call agsi 'nil? (list (ast-ev-id agsi 'a-list)))
                   (ast-ev-id agsi 'b-list)
                   (ast-e-fun-call
                    agsi
                    'reverse
                    (list
                     (ast-e-fun-call agsi 'cdr (list (ast-ev-id agsi 'a-list))) ;; call has only refs to ids (done)
                     (ast-e-loc-set agsi 'a (ast-e-fun-call agsi 'car (list (ast-ev-id agsi 'a-list))))
                     (ast-e-fun-call agsi 'cons (list (ast-e-loc-ref agsi 'a) (ast-ev-id agsi 'b-list)) ))))
                  #f))) ;; now this call has only refs to ids (done), extract call itself to id b'

  (define step2 (list
                 (ast-e-if
                  agsi
                  'if
                  (list
                   (ast-e-fun-call agsi 'nil? (list (ast-ev-id agsi 'a-list)))
                   (ast-ev-id agsi 'b-list)
                   (ast-e-fun-call
                    agsi
                    'reverse
                    (list
                     (ast-e-fun-call agsi 'cdr (list (ast-ev-id agsi 'a-list))) ;; extract call itself to id 'c
                     (ast-e-loc-set agsi 'a (ast-e-fun-call agsi 'car (list (ast-ev-id agsi 'a-list))))
                     (ast-e-loc-set agsi 'b (ast-e-fun-call agsi 'cons (list (ast-e-loc-ref agsi 'a) (ast-ev-id agsi 'b-list))))
                     (ast-e-loc-ref agsi 'b))))
                  #f)))

  (define step3 (list
                 (ast-e-if
                  agsi
                  'if
                  (list
                   (ast-e-fun-call agsi 'nil? (list (ast-ev-id agsi 'a-list)) )
                   (ast-ev-id agsi 'b-list)
                   (ast-e-loc-set agsi 'a (ast-e-fun-call agsi 'car (list (ast-ev-id agsi 'a-list))))
                   (ast-e-loc-set agsi 'b (ast-e-fun-call agsi 'cons (list (ast-e-loc-ref agsi 'a) (ast-ev-id agsi 'b-list)))) ;; p1 can be reused here
                   (ast-e-loc-set agsi 'c (ast-e-fun-call agsi 'cdr (list (ast-ev-id agsi 'a-list)))) ;; p0 can be reused here
                   (ast-e-fun-call agsi 'reverse (list (ast-e-loc-ref agsi 'c) (ast-e-loc-ref agsi 'b))))
                  #f)))) ;; now reverse has only ref parameters (done)

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

;; parameters of this function call have only loc-ref or ast-ev-id ast nodes => no more extraction of precode before actual call
(define/contract (ed-function-call--has-only-refs? fun-call)
  (->* [ast-e-fun-call?] [] boolean?)
  (foldl (lambda (l r) (and (or (ast-e-loc-ref? l) (ast-ev-id? l)) r)) true (ast-e-fun-call-params fun-call)))

(module+ test #| ed-function-call--has-only-refs? |#
  (check-true (ed-function-call--has-only-refs? (ast-e-fun-call agsi 'fn1 (list) )))

  (check-true (ed-function-call--has-only-refs? (ast-e-fun-call agsi 'fn1 (list (ast-e-loc-ref agsi 'a) (ast-ev-id agsi 'b)) )))

  (check-false (ed-function-call--has-only-refs? (ast-e-fun-call agsi 'fn1 (list (ast-e-loc-ref agsi 'a) (ast-ev-string agsi "10")) ))))


(struct transformed-parameter
  (reference
   loc-set
   locals-used)
  #:transparent
  #:guard (struct-guard/c (or/c ast-e-loc-ref? ast-ev-id?)
                          (listof ast-e-loc-set?)
                          byte?))

(define/contract (call-param->loc-ref/set param sym idx)
  (->* [ast-expression? symbol? byte?] []
      ;; (listof (list/c (or/c loc-ref? ast-ev-id?)) (listof ast-e-loc-set?) byte?)
      transformed-parameter?
      )
  (define new-gen-info (ast-node-gen-info param))
  (cond [(ast-e-fun-call? param)
         (define transformed-inner-fn-call
           (transform-function-call (struct-copy ast-e-fun-call param
                                                 [gen-info #:parent ast-node new-gen-info])))
         (define used-up-locals (ast-gen-info-locals-used (ast-node-gen-info transformed-inner-fn-call)))
         (transformed-parameter
          (ast-e-loc-ref new-gen-info sym)
          (list (ast-e-loc-set new-gen-info sym transformed-inner-fn-call))
          used-up-locals)]
        [(ast-e-nil? param)  (transformed-parameter (ast-e-loc-ref new-gen-info sym) (list (ast-e-loc-set new-gen-info sym param)) 1)]
        [(ast-ev-number? param)  (transformed-parameter (ast-e-loc-ref new-gen-info sym) (list (ast-e-loc-set new-gen-info sym param)) 1)]
        [(ast-ev-string? param)  (transformed-parameter (ast-e-loc-ref new-gen-info sym) (list (ast-e-loc-set new-gen-info sym param)) 1)]
        [(ast-ev-bool? param)  (transformed-parameter (ast-e-loc-ref new-gen-info sym) (list (ast-e-loc-set new-gen-info sym param)) 1)]
        [(ast-ev-id? param) (transformed-parameter param '() 0)]
        [(ast-e-loc-ref? param) (transformed-parameter param '() 0)]
        [else (raise-user-error (format "unknown param type ~a" param))]))

;; recursively extract all non refs from the given parameters (of a function all)
;; return the
;; - resulting params (must all be refs)
;; - setters that need to be prepended to the call
;; - list of newly introduced references
(define/contract (ed-function-call-params--mapper params (symbol-generator gensym) (idx 0) (mapped-params '())  )
  (->* [(listof ast-expression?)] [any/c byte? (listof transformed-parameter?)]
      (listof transformed-parameter?))
  (cond [(empty? params) (reverse mapped-params)]
        [else
         (define mapped-param (call-param->loc-ref/set (car params) (symbol-generator) idx))
         (ed-function-call-params--mapper
          (cdr params)
          symbol-generator
          (+ idx (transformed-parameter-locals-used mapped-param))
          (cons mapped-param mapped-params))]))

(module+ test
  ;; make sure that the resulting parameter is just a reference
  ;; make sure that nested if function then and else body are not transformed (because of late execution)
  ;; make sure that the boolean condition of the if function is transformed (pre-code on the if-command)
  (check-match (ed-function-call-params--mapper
                (list (ast-e-if agsi 'if (list (ast-ev-bool agsi #t)
                                               (ast-ev-string agsi "true-val")
                                               (ast-ev-string agsi "false-val"))
                                #f)))
               (list (transformed-parameter
                      (ast-e-loc-ref _ a-sym) ;; resulting parameter
                      (list             ;; of setters to be executed before
                       (ast-e-loc-set _ a-sym (ast-e-fun-call gen 'if (list (ast-e-loc-ref _ b-sym)
                                                                       (ast-ev-string _ "true-val")
                                                                       (ast-ev-string _ "false-val")))))
                      0))
               (and (symbol? a-sym) (symbol? b-sym)
                  (match (ast-gen-info-pre-code gen)
                    [(list (ast-e-loc-set _ b-sym (ast-ev-bool _ #t))) #t] ;; precode associated with the if-command
                    [_ #f])))

  (check-equal? (ed-function-call-params--mapper
                 (list (ast-e-nil agsi)
                       (ast-ev-number agsi 47)
                       (ast-ev-string agsi "x")
                       (ast-ev-bool agsi #t))
                 (lambda () 'a))
                (list (transformed-parameter (ast-e-loc-ref agsi 'a) (list (ast-e-loc-set agsi 'a (ast-e-nil agsi))) 1)
                      (transformed-parameter (ast-e-loc-ref agsi 'a) (list (ast-e-loc-set agsi 'a (ast-ev-number agsi 47))) 1)
                      (transformed-parameter (ast-e-loc-ref agsi 'a) (list (ast-e-loc-set agsi 'a (ast-ev-string agsi "x"))) 1)
                      (transformed-parameter (ast-e-loc-ref agsi 'a) (list (ast-e-loc-set agsi 'a (ast-ev-bool agsi #t))) 1))
                "values are extracted into a set to a ref and the ref")

  (check-equal? (ed-function-call-params--mapper (list (ast-e-loc-ref agsi 'a) (ast-ev-id agsi 'b)))
                (list (transformed-parameter (ast-e-loc-ref agsi 'a) '() 0)
                      (transformed-parameter (ast-ev-id agsi 'b) '() 0))
                "a reference is not transformed => no new refs etc")

  (check-equal? (ed-function-call-params--mapper (list (ast-e-loc-ref agsi 'q) (ast-ev-number agsi 17)) (lambda () 'l1))
                (list (transformed-parameter (ast-e-loc-ref agsi 'q) '() 0)
                      (transformed-parameter (ast-e-loc-ref agsi 'l1) (list (ast-e-loc-set agsi 'l1 (ast-ev-number agsi 17))) 1)))

  (check-match (ed-function-call-params--mapper (list (ast-e-loc-ref agsi 'q) (ast-e-fun-call agsi 'fn1 (list (ast-ev-number agsi 1)) )))
               (list (transformed-parameter (ast-e-loc-ref _ 'q) '() 0)
                     (transformed-parameter (ast-e-loc-ref _ a-sym)
                           (list (ast-e-loc-set _ a-sym (ast-e-fun-call gen 'fn1 (list (ast-e-loc-ref _ b-sym))))) 1))
               (and (symbol? a-sym) (symbol? b-sym)
                  (match (ast-gen-info-pre-code gen)
                    [(list (ast-e-loc-set _ b-sym (ast-ev-number _ 1))) #t]
                    [_ #f])))

  (check-match (ed-function-call-params--mapper
                (list (ast-e-loc-ref agsi 'q)
                      (ast-e-fun-call agsi 'fn1 (list (ast-ev-id agsi 'l)
                                                      (ast-e-fun-call agsi 'r2 (list (ast-ev-number agsi 1)))))))
               (list (transformed-parameter (ast-e-loc-ref _ 'q) '() 0)
                     (transformed-parameter (ast-e-loc-ref _ a-sym)
                           (list (ast-e-loc-set _ a-sym
                                           (ast-e-fun-call
                                            f1-gen
                                            'fn1 (list (ast-ev-id _ 'l) (ast-e-loc-ref _ b-sym)))))
                           1))
               (and (symbol? a-sym) (symbol? b-sym)
                  (match (ast-gen-info-pre-code f1-gen)
                    [(list (ast-e-loc-set _ b-sym
                                       (ast-e-fun-call
                                        r2-gen
                                        'r2
                                        (list (ast-e-loc-ref _ c-sym)))))
                     (match (ast-gen-info-pre-code r2-gen)
                       [(list (ast-e-loc-set _ c-sym (ast-ev-number _ 1))) #t]
                       [_ #f])]
                    [_ #f])))

  (check-match (ed-function-call-params--mapper
                (list
                 (ast-e-fun-call agsi 'cdr (list (ast-ev-id agsi 'a-list)))
                 (ast-e-fun-call agsi
                                 'cons
                                 (list
                                  (ast-e-fun-call agsi 'car (list (ast-ev-id agsi 'a-list)))
                                  (ast-ev-id agsi 'b-list)))))
               (list (transformed-parameter (ast-e-loc-ref _ c-sym)
                           (list (ast-e-loc-set _ c-sym (ast-e-fun-call _ 'cdr (list (ast-ev-id _ 'a-list)))))
                           0)
                     (transformed-parameter (ast-e-loc-ref _ b-sym)
                           (list (ast-e-loc-set _ b-sym
                                           (ast-e-fun-call gen
                                                           'cons
                                                           (list (ast-e-loc-ref _ a-sym) (ast-ev-id _ 'b-list)))))
                           1))
               (and (symbol? a-sym) (symbol? b-sym) (symbol? c-sym)
                  (match (ast-gen-info-pre-code gen)
                    [(list (ast-e-loc-set _ a-sym (ast-e-fun-call _ 'car (list (ast-ev-id _ 'a-list))))) #t]
                    [_ #f]))))

(define/contract (ed-function-call--extract-refs fun-call (ref-list '()) (prepends '()) (symbol-generator gensym))
  (->* [ast-e-fun-call?]
      [(listof ast-e-loc-ref?) (listof ast-e-loc-set?) any/c]
      (list/c ast-e-fun-call? (listof (or/c ast-e-loc-ref? ast-ev-id?)) (listof ast-e-loc-set?)))
  (cond [(ed-function-call--has-only-refs? fun-call) (list fun-call ref-list prepends)]
        [(eq? 'if (ast-e-fun-call-fun fun-call))
         (define transformed-bool-param (ed-function-call-params--mapper (list (car (ast-e-fun-call-params fun-call))) symbol-generator))
         (list (ast-e-fun-call (ast-node-gen-info fun-call)
                               'if
                               (cons (transformed-parameter-reference (car transformed-bool-param)) (cdr (ast-e-fun-call-params fun-call)))
                               )
               (append (map (lambda (param) (transformed-parameter-reference param)) transformed-bool-param) ref-list )
               (append prepends (flatten (map (lambda (param) (transformed-parameter-loc-set param)) transformed-bool-param))))]
        [else
         (define transformed-params (ed-function-call-params--mapper (ast-e-fun-call-params fun-call) symbol-generator))
         (define new-refs (map (lambda (param) (transformed-parameter-reference param)) transformed-params))
         (define transformed-refs (append new-refs  ref-list))
         (define additional-locals (foldl (lambda (ref acc) (+ acc (if (ast-e-loc-ref? ref) 1 0))) 0  new-refs))
         (list
          (ast-e-fun-call (struct-copy ast-gen-info (ast-node-gen-info fun-call)
                                       [locals-used  additional-locals])
                          (ast-e-fun-call-fun fun-call)
                          (map (lambda (param) (transformed-parameter-reference param)) transformed-params))
          transformed-refs
          (append prepends (flatten (map (lambda (param) (transformed-parameter-loc-set param)) transformed-params))))]))

(module+ test
  (check-match (ed-function-call--extract-refs
                (ast-e-fun-call agsi
                                'fn1
                                (list (ast-ev-bool agsi #t)
                                      (ast-e-fun-call agsi 'inner (list (ast-ev-string agsi "a-string"))))))
               (list
                (ast-e-fun-call _ 'fn1 (list (ast-e-loc-ref _ a-sym) (ast-e-loc-ref _ b-sym)))
                (list (ast-e-loc-ref _ a-sym) (ast-e-loc-ref _ b-sym))
                (list
                 (ast-e-loc-set _ a-sym (ast-ev-bool _ #t))
                 (ast-e-loc-set _ b-sym (ast-e-fun-call gen 'inner (list (ast-e-loc-ref _ c-sym ))))))
               (match (ast-gen-info-pre-code gen)
                 [(list (ast-e-loc-set _ c-sym  (ast-ev-string _ "a-string"))) #t]
                 [_ #f]))

  (check-match (ed-function-call--extract-refs
                (ast-e-fun-call  agsi 'fn1 (list (ast-e-if agsi 'if (list (ast-ev-bool agsi #t) (ast-ev-number agsi 10) (ast-ev-number agsi 20)) #f)
                                                 (ast-e-fun-call agsi 'inner (list (ast-ev-string agsi "a-string"))))))

               (list
                (ast-e-fun-call _ 'fn1 (list (ast-e-loc-ref _ a-sym) (ast-e-loc-ref _ b-sym)))
                (list (ast-e-loc-ref _ a-sym) (ast-e-loc-ref _ b-sym))
                (list
                 (ast-e-loc-set _ a-sym (ast-e-fun-call if-gen 'if (list (ast-e-loc-ref _ c-sym) (ast-ev-number _ 10) (ast-ev-number _ 20))))
                 (ast-e-loc-set _ b-sym (ast-e-fun-call inner-gen 'inner (list (ast-e-loc-ref _ f-sym))))))
               (and (match (ast-gen-info-pre-code if-gen)
                    [(list (ast-e-loc-set _ c-sym (ast-ev-bool _ #t))) #t]
                    [_ #f])
                  (match (ast-gen-info-pre-code inner-gen)
                    [(list (ast-e-loc-set _ f-sym (ast-ev-string _ "a-string"))) #t]
                    [_ #f]))))

(define/contract (transform-function-call fun-call)
  (->* [ast-e-fun-call?] [] ast-e-fun-call?)
  (match-define (list new-call loc-refs loc-sets)
    (ed-function-call--extract-refs fun-call))
  (define new-gen-info (struct-copy ast-gen-info (ast-node-gen-info new-call)
                                    [pre-code (append (ast-gen-info-pre-code (ast-node-gen-info fun-call)) loc-sets)]))
  (struct-copy ast-e-fun-call new-call
               [gen-info #:parent ast-node new-gen-info]))

(module+ test #| transform function call |#
  (check-match (transform-function-call (ast-e-fun-call agsi 'fn1 (list (ast-ev-bool agsi #t))))
               (ast-e-fun-call gen 'fn1
                               (list (ast-e-loc-ref _ a-sym)))
               (and (symbol? a-sym)
                  (match (ast-gen-info-pre-code gen)
                    [(list (ast-e-loc-set _ a-sym (ast-ev-bool _ #t))) #t]
                    [_ #f])))

  (check-match (transform-function-call (ast-e-fun-call agsi 'fn1 (list (ast-ev-bool agsi #t) (ast-e-fun-call agsi 'fn2 (list (ast-ev-string agsi "z"))))))
               (ast-e-fun-call fn1-gen 'fn1
                               (list (ast-e-loc-ref _ a-sym) (ast-e-loc-ref _ b-sym)))
               (and (symbol? a-sym)
                  (match (ast-gen-info-pre-code fn1-gen)
                    [(list
                       (ast-e-loc-set _ a-sym (ast-ev-bool _ #t))
                       (ast-e-loc-set _ b-sym (ast-e-fun-call fn2-gen 'fn2 (list (ast-e-loc-ref _ c-sym)))))
                     (match (ast-gen-info-pre-code fn2-gen)
                       [(list (ast-e-loc-set _ c-sym (ast-ev-string _ "z"))) #t]
                       [_ #f]) #t]
                    [_ #f]))))

;; add ast-e-loc-set if boolean expression is not just a simple var reference or boolean value
(define/contract (transform-if if-expression)
  (->* [ast-e-if?] [] ast-e-if?)
  (define boolean-expr (car (ast-e-fun-call-params if-expression)))
  (cond
    [(ast-ev-bool? boolean-expr) if-expression]
    [(ast-ev-id? boolean-expr) if-expression]
    [else
     (define sym (gensym))
     ;; target reg can be used for boolean expression, since it will eventually be overwritten exactly after the condition was evaluated and used (the last time)
     ;; TODO used transformed instead of regular
     (ast-e-if (struct-copy ast-gen-info (ast-node-gen-info if-expression)
                            [pre-code (append (ast-gen-info-pre-code (ast-node-gen-info if-expression))
                                              (list (ast-e-loc-set agsi sym (transform boolean-expr))))])
               'if (cons (ast-e-loc-ref agsi sym)
                         (map transform (cdr (ast-e-fun-call-params if-expression))))
               (ast-e-if-negated if-expression))]))

(define/contract (transform an-expression)
  (->* [ast-expression?] [] ast-expression?)
  (cond
    [(ast-e-if? an-expression) (transform-if an-expression)]
    [(ast-e-fun-call? an-expression) (transform-function-call an-expression)]
    [(ast-ev-number? an-expression) (struct-copy ast-ev-number an-expression
                                                 [gen-info #:parent ast-node (struct-copy ast-gen-info (ast-node-gen-info an-expression)
                                                                                          [locals-used (+ 1 (ast-gen-info-locals-used (ast-node-gen-info an-expression)))])])]
    [(ast-ev-bool? an-expression) (struct-copy ast-ev-bool an-expression
                                                 [gen-info #:parent ast-node (struct-copy ast-gen-info (ast-node-gen-info an-expression)
                                                                                          [locals-used (+ 1 (ast-gen-info-locals-used (ast-node-gen-info an-expression)))])])]
    [(ast-ev-string? an-expression) (struct-copy ast-ev-string an-expression
                                                 [gen-info #:parent ast-node (struct-copy ast-gen-info (ast-node-gen-info an-expression)
                                                                                          [locals-used (+ 1 (ast-gen-info-locals-used (ast-node-gen-info an-expression)))])])]
    [else an-expression]))

(module+ test #| transform |#

  (check-match (nested->list (transform (ast-e-fun-call agsi 'byte+ (list (ast-ev-number agsi 1) (ast-ev-id agsi 'a-byte)))))
               (list 'ast-e-fun-call (list 'ast-gen-info
                                           (list (list'ast-e-loc-set _ a-sym (list 'ast-ev-number _ 1))) ;; precode
                                           _ ...) ;; this allows for ast-gen-info to change without this test to be adjusted
                     'byte+
                     (list (list 'ast-e-loc-ref _ a-sym)
                           (list 'ast-ev-id _ 'a-byte)))))

;; generation context keeping track of useful information during code generation
(struct gen-context
  (next-local         ;; byte marking the next free local register/variable
   local-id-map       ;; map of symbol->local index (register/variable)
   parameter-id-map   ;; map of symbol->parameter index (register/variable)
   gen-bytes          ;; list of already generated bytes
   function-id-map    ;; map of function symbols->function index
   current-function)  ;; symbol of the current function w/i which this generation takes place (useful for tail-call detection)
  #:transparent
  #:guard
  (struct-guard/c byte? (hash/c symbol? byte?) (hash/c symbol? byte?) (listof byte?) (hash/c symbol? integer?) symbol?))

(define/contract (make-gen-context #:next-local (a-next-local 0)
                                   #:local-id-map (a-local-id-map (hash))
                                   #:parameter-id-map (a-parameter-id-map (hash))
                                   #:gen-bytes (a-gen-bytes (list))
                                   #:function-id-map (a-function-id-map (hash))
                                   #:current-function (a-current-function 'nil))
  (->* [] [#:next-local byte?
          #:local-id-map hash?
          #:parameter-id-map hash?
          #:gen-bytes (listof byte?)
          #:function-id-map hash?
          #:current-function symbol?]
      gen-context?)
  (gen-context a-next-local a-local-id-map a-parameter-id-map a-gen-bytes a-function-id-map a-current-function))

;; generate references to parameters and encode them into one byte (VM_L0 ... VM_Lx, VM_P0 ... etc.)
(define/contract (generate-parameter parameter a-gen-context)
  (->* [ast-node? gen-context?] [] byte?)
  (cond
    [(ast-ev-id? parameter)
     (encode-idx (hash-ref (gen-context-parameter-id-map a-gen-context) (ast-ev-id-id parameter))
                 l-param)]
    [(ast-e-loc-ref? parameter)
     (encode-idx (hash-ref (gen-context-local-id-map a-gen-context) (ast-e-loc-ref-id parameter))
                 l-local)]
    [else (raise-user-error (format "unknown parameter ~a" parameter))]))

(define/contract (generate-loc-set a-loc-set a-gen-context)
  (->* [ast-e-loc-set? gen-context?] [] gen-context?)
  (define expr (ast-e-loc-set-expr a-loc-set))
  (define local (ast-gen-info-target-reg (ast-node-gen-info expr)))
  (struct-copy gen-context a-gen-context
               [next-local (+ 1 (gen-context-next-local a-gen-context))]
               [local-id-map (hash-set (gen-context-local-id-map a-gen-context) (ast-e-loc-set-id a-loc-set) local)]
               [gen-bytes (append (gen-context-gen-bytes a-gen-context)
                                  (cond
                                    [(ast-ev-number? expr)  (list CISC_VM_IMMB local (ast-ev-number-number expr))]
                                    [(ast-ev-bool? expr)    (list CISC_VM_IMMB local (if (ast-ev-bool-bool expr) #xff #x00))]
                                    [(ast-e-if? expr)       (raise-user-error (format "if not allowed in ast-e-loc-set position (yet) ~a" expr))]
                                    [(ast-e-fun-call? expr) (gen-context-gen-bytes (generate-fun-call expr (struct-copy gen-context a-gen-context [gen-bytes (list)])))]
                                    [(ast-ev-id? expr)      (if (= local (encode-idx (hash-ref (gen-context-parameter-id-map a-gen-context) (ast-ev-id-id expr)) l-param))
                                                                (list)
                                                                (list CISC_VM_MOVE local (encode-idx (hash-ref (gen-context-parameter-id-map a-gen-context) (ast-ev-id-id expr)) l-param)))]
                                    [else (raise-user-error (format "unknown expression to set ~a" expr))]))]))

(module+ test #| generate-loc-set |#
  (check-equal? (generate-loc-set (ast-e-loc-set (make-ast-gen-info #:target-reg VM_L5) 'sym (ast-ev-number (make-ast-gen-info #:target-reg VM_L5) 15)) (make-gen-context #:next-local 5))
                (make-gen-context #:next-local 6 #:local-id-map '#hash((sym . 5)) #:gen-bytes (list CISC_VM_IMMB 5 15)))

  (check-equal?
   (generate-loc-set
    (ast-e-loc-set agsi 'sym (ast-e-fun-call (make-ast-gen-info #:pre-code (list (ast-e-loc-set (make-ast-gen-info #:target-reg VM_L0) 'a (ast-ev-number (make-ast-gen-info #:target-reg VM_L0) 1))
                                                            (ast-e-loc-set (make-ast-gen-info #:target-reg VM_L1) 'b (ast-ev-number (make-ast-gen-info #:target-reg VM_L1) 2))))
                                        'byte+ (list (ast-e-loc-ref agsi 'a) (ast-e-loc-ref agsi 'b))))
    (make-gen-context #:function-id-map (hash 'byte+ 0)))
   (make-gen-context #:next-local 1
                     #:local-id-map (hash 'sym 0)
                     #:gen-bytes
                     (list CISC_VM_IMMB VM_L0 1
                           CISC_VM_IMMB VM_L1 2
                           CISC_VM_BYTE_ADD VM_L0 VM_L0 VM_L1)
                     #:function-id-map (hash 'byte+ 0))))

;; generate code for operation with one operand (and target-reg of course)
(define/contract (generate-one-op opcode target-reg param a-gen-context)
  (->* [byte? byte? ast-expression? gen-context?] [] gen-context?)
  (struct-copy gen-context a-gen-context
               [gen-bytes (append (gen-context-gen-bytes a-gen-context)
                                  (list opcode target-reg (generate-parameter param a-gen-context)))]))

;; generate code for operation with two operands (and target-reg of course)
(define/contract (generate-two-op opcode target-reg param1 param2 a-gen-context)
  (->* [byte? byte? ast-expression? ast-expression? gen-context?] [] gen-context?)
  (struct-copy gen-context a-gen-context
               [gen-bytes (append (gen-context-gen-bytes a-gen-context)
                                  (list opcode target-reg (generate-parameter param1 a-gen-context)
                                        (generate-parameter param2 a-gen-context)))]))

(define/contract (generate-fun-call a-fun-call a-gen-context)
  (->* [ast-e-fun-call? gen-context?] [] gen-context?)
  (define target-reg (ast-gen-info-target-reg (ast-node-gen-info a-fun-call)))
  (define fun-id (ast-e-fun-call-fun a-fun-call))
  (define fun-params (ast-e-fun-call-params a-fun-call))
  (define fun-params-len (length fun-params))
  (define pre-code (ast-gen-info-pre-code (ast-node-gen-info a-fun-call)))
  (define next-gen-context (foldl (lambda (pre-expression inner-gen-context)
                                    (generate pre-expression inner-gen-context))
                                  a-gen-context pre-code))
  (cond
    ;; built in (intrinsic) commands
    [(eq? fun-id 'cons) (generate-two-op CISC_VM_CONS target-reg (first fun-params) (second fun-params) next-gen-context)]
    [(eq? fun-id 'cdr) (generate-one-op CISC_VM_CDR target-reg (first fun-params) next-gen-context)]
    [(eq? fun-id 'car) (generate-one-op CISC_VM_CAR target-reg (first fun-params) next-gen-context)]
    [(eq? fun-id 'nil?) (generate-one-op CISC_VM_NIL_P target-reg (first fun-params) next-gen-context)]
    [(eq? fun-id 'not!) (generate-one-op CISC_VM_NOT target-reg (first fun-params) next-gen-context)]
    [(eq? fun-id 'byte+) (generate-two-op CISC_VM_BYTE_ADD target-reg (first fun-params) (second fun-params) next-gen-context)]
    ;; tail call
    [(eq? fun-id (gen-context-current-function a-gen-context))

     (define gen-bytes-up-to-goto
       (append
        (gen-context-gen-bytes next-gen-context)
        (flatten
         (map (lambda  (idx-param) (if (= (encode-idx (car idx-param) l-param) (cdr idx-param))
                                  (list)
                                  (list CISC_VM_MOVE (encode-idx (car idx-param) l-param) (cdr idx-param))))
              (map cons (range (length fun-params))
                   (map (lambda (param) (generate-parameter param next-gen-context)) fun-params))))))
     (define function-len (+ (length gen-bytes-up-to-goto) ;; generated up to here
                             1)) ;; goto itself
     ;; (println (format "function len ~a, gen bytes up to goto ~a" function-len gen-bytes-up-to-goto))
     (struct-copy gen-context next-gen-context
                  [gen-bytes (append gen-bytes-up-to-goto
                                     (list CISC_VM_GOTO (two-complement-of (- 0 function-len))))])]
    ;; regular function call
    [else
     (define func-idx (hash-ref (gen-context-function-id-map a-gen-context) fun-id))
     (define func-idx-low (bitwise-and func-idx #xff))
     (define func-idx-high (arithmetic-shift func-idx -8))
     (struct-copy gen-context next-gen-context
                  [gen-bytes (append (gen-context-gen-bytes next-gen-context)
                                     (list CISC_VM_CALL target-reg func-idx-low func-idx-high fun-params-len)
                                     (map (lambda (param) (generate-parameter param next-gen-context)) fun-params))])]))

(module+ test #| generate-fun-call |#
  (check-equal? (generate-fun-call (ast-e-fun-call (make-ast-gen-info #:pre-code (list (ast-e-loc-set agsi 'sym (ast-ev-number agsi 11)))
                                                                      #:target-reg VM_L1)
                                                   'byte+
                                                   (list (ast-e-loc-ref agsi 'sym) (ast-ev-id agsi 'param)))
                                   (make-gen-context  #:parameter-id-map (hash 'param 1)))
                (make-gen-context #:next-local 1
                                  #:local-id-map '#hash((sym . 0))
                                  #:parameter-id-map '#hash((param . 1))
                                  #:gen-bytes (list CISC_VM_IMMB     VM_L0 11
                                                    CISC_VM_BYTE_ADD VM_L1 VM_L0 VM_P1))))

(define/contract (generate-register-ref expr a-gen-context)
  (->* [ast-expression? gen-context?] [] byte?)
  (cond
    [(ast-e-loc-ref? expr) (encode-idx (hash-ref (gen-context-local-id-map a-gen-context) (ast-e-loc-ref-id expr)) l-local)]
    [(ast-ev-id? expr) (encode-idx (hash-ref (gen-context-parameter-id-map a-gen-context) (ast-ev-id-id expr)) l-param)]
    [else (raise-user-error (format "cannot resolve reference to register ~a" expr))]))

(define/contract (generate-if if-expr a-gen-context)
  (->* [ast-e-if? gen-context?] [] gen-context?)
  ;; TODO: if a block (else or then) contains a recursive call, the the goto needs to know all generated code =>
  ;; generating a block that may contain a goto must be done with a context that holds all bytes generated so far!
  (define pre-code (ast-gen-info-pre-code (ast-node-gen-info if-expr)))
  (define bool-ref-expression (car (ast-e-fun-call-params if-expr)))
  (define next-gen-context (foldl (lambda (pre-expression inner-gen-context) (generate pre-expression inner-gen-context)) a-gen-context pre-code))
  (define else-block-gen-context (struct-copy gen-context next-gen-context [gen-bytes (list)]))
  (define else-block-expression (transform (third (ast-e-fun-call-params if-expr))))
  (define final-else-block-gen-context (generate else-block-expression else-block-gen-context))
  (define then-block-gen-context (struct-copy gen-context final-else-block-gen-context [gen-bytes (list)]))
  (define then-block-expression (transform (cadr (ast-e-fun-call-params if-expr))))
  (define then-block-is-recursive-call (and (ast-e-fun-call? then-block-expression)
                                          (eq? (ast-e-fun-call-fun then-block-expression)
                                               (gen-context-current-function then-block-gen-context))))
  (define final-then-block-gen-context (generate then-block-expression then-block-gen-context))
  (define then-block-len (length (gen-context-gen-bytes final-then-block-gen-context)))
  (define else-block-len (length (gen-context-gen-bytes final-else-block-gen-context)))
  (define branch-decision-register (generate-register-ref bool-ref-expression next-gen-context))

  ;; possible workaround: make longjumps possible
  (when (> then-block-len 126)
    (raise-user-error "generated then block > 126 bytes, branch cannot be generated"))
  (when (> else-block-len 126)
    (raise-user-error "generated else block > 126 bytes, branch cannot be generated"))

  (foldl (lambda (byte-list-fun context) (apply byte-list-fun (list context)))
         next-gen-context
         (list (lambda (ctx) (struct-copy gen-context ctx
                                     [gen-bytes (append (gen-context-gen-bytes ctx)
                                                        (list (if (ast-e-if-negated if-expr) CISC_VM_BRA CISC_VM_BRA_NOT) branch-decision-register (+ 3 then-block-len)))]))
               (lambda (ctx) (generate then-block-expression ctx))
               (lambda (ctx) (struct-copy gen-context ctx
                                     [gen-bytes (append (gen-context-gen-bytes ctx)
                                                        (if then-block-is-recursive-call '() (list CISC_VM_GOTO (+ 1 else-block-len))))]))
               (lambda (ctx) (generate else-block-expression ctx)))))

(module+ test #| generate-if |#
  (check-equal? (generate-if (ast-e-if (make-ast-gen-info #:pre-code (list (ast-e-loc-set agsi 'sym (ast-ev-bool agsi #t))))
                                       'if
                                       (list (ast-e-loc-ref agsi 'sym) (ast-ev-number agsi 1) (ast-ev-number agsi 2))
                                       #f)
                             (make-gen-context))
                (make-gen-context #:next-local 1
                                  #:local-id-map (hash 'sym 0)
                                  #:gen-bytes (list CISC_VM_IMMB VM_L0 #xff
                                                    CISC_VM_BRA_NOT VM_L0 6
                                                    CISC_VM_IMMB VM_L0 1
                                                    CISC_VM_GOTO 4
                                                    CISC_VM_IMMB VM_L0 2)))

  (check-equal? (generate-if (ast-e-if agsi 'if
                                             (list (ast-ev-id agsi 'param) (ast-ev-number agsi 1) (ast-ev-number agsi 2))
                                             #f)
                             (make-gen-context #:parameter-id-map (hash 'param 1)))
                (make-gen-context #:parameter-id-map (hash 'param 1)
                                  #:gen-bytes (list CISC_VM_BRA_NOT VM_P1 6
                                                    CISC_VM_IMMB VM_L0 1
                                                    CISC_VM_GOTO 4
                                                    CISC_VM_IMMB VM_L0 2)))

  (check-match (generate-if (ast-e-if (make-ast-gen-info #:pre-code (list (ast-e-loc-set agsi 'sym (ast-ev-bool agsi #t))))
                                      'if
                                      (list (ast-e-loc-ref agsi 'sym)
                                            (ast-ev-number agsi 1)
                                            (ast-e-fun-call agsi 'byte+ (list (ast-ev-number (make-ast-gen-info #:target-reg VM_L2) 2)
                                                                              (ast-ev-number (make-ast-gen-info #:target-reg VM_L1) 3))))
                                      #f)
                            (make-gen-context))
               (gen-context 3 gen-hash (hash)
                            (list CISC_VM_IMMB VM_L0 #xff
                                  CISC_VM_BRA_NOT VM_L0 6
                                  CISC_VM_IMMB VM_L0 1
                                  CISC_VM_GOTO 11
                                  CISC_VM_IMMB VM_L2 2
                                  CISC_VM_IMMB VM_L1 3
                                  CISC_VM_BYTE_ADD VM_L0 VM_L2 VM_L1)
                            (hash)
                            'nil)
               (hash? gen-hash)))

(define/contract (generate-value-set an-expression a-gen-context)
  (->* [ast-e-value? gen-context?] [] gen-context?)
  (define target-reg (ast-gen-info-target-reg (ast-node-gen-info an-expression)))
  (cond
    [(ast-ev-number? an-expression)
     (struct-copy gen-context a-gen-context
                  [gen-bytes (append (gen-context-gen-bytes a-gen-context)
                                     (list CISC_VM_IMMB target-reg (ast-ev-number-number an-expression)))])]
    [(ast-ev-bool? an-expression)
     (struct-copy gen-context a-gen-context
                  [gen-bytes (append (gen-context-gen-bytes a-gen-context)
                                     (list CISC_VM_IMMB target-reg (if (ast-ev-bool-bool an-expression) #xff #x00)))])]
    [(ast-ev-id? an-expression)
     (unless (hash-has-key? (gen-context-parameter-id-map a-gen-context) (ast-ev-id-id an-expression))
       (raise-user-error (format "Key id ~a not found in parameter list ~a" (ast-ev-id-id an-expression) (gen-context-parameter-id-map a-gen-context))))
     (struct-copy gen-context a-gen-context
                  [gen-bytes (append (gen-context-gen-bytes a-gen-context)
                                     (if (= target-reg (encode-idx (hash-ref (gen-context-parameter-id-map a-gen-context) (ast-ev-id-id an-expression)) l-param))
                                         (list)
                                         (list CISC_VM_MOVE target-reg (encode-idx (hash-ref (gen-context-parameter-id-map a-gen-context) (ast-ev-id-id an-expression)) l-param))))])]
    ;; [(ast-ev-string an-expression)]
    [else (raise-user-error (format "unknown value ~a" an-expression))]
    ))

;; generate code for an-expression, assigning result into target-reg within the given gen-context
(define/contract (generate an-expression a-gen-context)
  (->* [ast-expression? gen-context?] [] gen-context?)
  (cond
    [(ast-e-value? an-expression)
     (generate-value-set an-expression a-gen-context)]
    [(ast-e-loc-set? an-expression)
     (generate-loc-set an-expression a-gen-context)]
    ;; special form (e.g. if)
    [(ast-e-if? an-expression)
     (generate-if an-expression a-gen-context)]
    ;; calls
    [(ast-e-fun-call? an-expression)
     (generate-fun-call an-expression a-gen-context)]
    [else (raise-user-error (format "unknown expression to generate ~a" an-expression))]))

(module+ test #| generate |#
  (check-equal? (gen-context-gen-bytes (generate (ast-e-loc-set agsi 'sym (ast-ev-number agsi 10)) (make-gen-context)))
                (list CISC_VM_IMMB VM_L0 10))

  (check-equal? (gen-context-gen-bytes
                 (generate (ast-e-fun-call (make-ast-gen-info #:pre-code (list (ast-e-loc-set agsi 'byte-10 (ast-ev-number agsi 10))) #:target-reg VM_L1) 'byte+
                                           (list (ast-e-loc-ref agsi 'byte-10) (ast-ev-id agsi 'a-byte)))
                           (make-gen-context #:parameter-id-map (hash 'a-byte 0))))
                (list
                 CISC_VM_IMMB VM_L0 10
                 CISC_VM_BYTE_ADD VM_L1 VM_L0 VM_P0)))

(define/contract (--prep-param-hashmap params)
  (->* [(listof ast-param-def?)] [] hash?)
  (apply hash (flatten (map list (map ast-param-def-id params) (range (length params))))))

(module+ test #| --prep-param-hashmap |#
  (check-equal? (--prep-param-hashmap (list (ast-param-def agsi 'p0 (ast-td-simple agsi 'string))
                                            (ast-param-def agsi 'other (ast-td-simple agsi 'number))))
                (hash 'p0 0 'other 1)))

(define/contract (cisc-vm-transform fun)
  (->* [ast-function-def?] [] gen-context?)
  ;; register all parameter-id s into the generation context
  (define param-map (--prep-param-hashmap (append (ast-function-def-parameter fun) (ast-function-def-default-parameter fun))))
  (define normalized-expressions (map (lambda (expr) (normalize-ast-expression expr)) (ast-function-def-body fun)))
  (define allocated-expressions (map (lambda (expr) (allocate-registers expr param-map)) normalized-expressions))
  (define transformed-expressions (map transform allocated-expressions))
  ;; generate (into target local slot 0) expression(s)
  (define new-gen-context (foldl (lambda (body-expr a-gen-ctx) (generate body-expr a-gen-ctx))
                                 (make-gen-context #:parameter-id-map param-map #:current-function (ast-function-def-id fun))
                                 transformed-expressions))
  (define last-expr (last transformed-expressions))
  ;; count the needed local slots (for later function registration)
  ;; generate return local slot 0
  (define locals-needed (gen-context-next-local new-gen-context))
  (struct-copy gen-context new-gen-context
               [gen-bytes (append (gen-context-gen-bytes new-gen-context)
                                  (list CISC_VM_RET (ast-gen-info-target-reg (ast-node-gen-info last-expr))))]))

(module+ test #| compile |#

  (check-equal?
   (gen-context-gen-bytes
    (cisc-vm-transform
     (m-def (add1 (a-byte byte) -> byte
                  "add one to the given")
            (byte+ 1 a-byte))))
   (list CISC_VM_IMMB VM_L0 1
         CISC_VM_BYTE_ADD VM_L0 VM_L0 VM_P0 ;; optimization: byte could be directly encoded into parameter (encode-idx 1 l-imm)
         CISC_VM_RET VM_L0)))

(module+ test #| if/cond |#

  (check-equal?
   (cisc-vm-transform
    (m-def (a-or-b (param bool) -> byte
                   "return first or second")
           (if param 1 2)))
   (make-gen-context #:parameter-id-map (hash 'param 0)
                     #:gen-bytes (list CISC_VM_BRA_NOT VM_P0 6
                                       CISC_VM_IMMB VM_L0 1
                                       CISC_VM_GOTO 4
                                       CISC_VM_IMMB VM_L0 2
                                       CISC_VM_RET  VM_L0)
                     #:current-function 'a-or-b)))

(module+ test #| tail call recursion (w/ infinite loop => without if) |#


  ;;(skip "register allocation currently broken")
  (check-equal?
   (disassemble
    (gen-context-gen-bytes
     (cisc-vm-transform
      (m-def (called-rec (a byte) (b byte) -> byte
                         "make infinite loop adding one to each parameter")
             (called-rec (byte+ a 2) (byte+ b 1)))))
    (make-vm))
   (list "immb 2 -> l1"
         "byte+ p0 + l1 -> l0"
         "immb 1 -> l2"
         "byte+ p1 + l2 -> l1"
         "move l0 -> p0"
         "move l1 -> p1"
         "goto -> -21"
         "ret l0")))

(define (update-gen-info an-ast-node new-gen-info)
  (cond
    [(ast-e-fun-call? an-ast-node) (struct-copy ast-e-fun-call an-ast-node
                                                [gen-info #:parent ast-node new-gen-info])]
    [(ast-ev-bool? an-ast-node) (struct-copy ast-ev-bool an-ast-node
                                             [gen-info #:parent ast-node new-gen-info])]
    [(ast-ev-number? an-ast-node) (struct-copy ast-ev-number an-ast-node
                                               [gen-info #:parent ast-node new-gen-info])]
    [(ast-ev-string? an-ast-node) (struct-copy ast-ev-string an-ast-node
                                               [gen-info #:parent ast-node new-gen-info])]
    [(ast-ev-id? an-ast-node) (struct-copy ast-ev-id an-ast-node
                                           [gen-info #:parent ast-node new-gen-info])]
    [(ast-e-if? an-ast-node) (struct-copy ast-e-if an-ast-node
                                          [gen-info #:parent ast-node new-gen-info])]
    [(ast-e-loc-ref? an-ast-node) (struct-copy ast-e-loc-ref an-ast-node
                                         [gen-info #:parent ast-node new-gen-info])]
    [else (raise-user-error (format "structure info ~a cannot update gen-info (yet)" an-ast-node))]))

(define (update-gen-info-pre-code an-ast-node value)
  (update-gen-info an-ast-node
                   (struct-copy ast-gen-info (ast-node-gen-info an-ast-node)
                                [pre-code value])))

(define (update-gen-info-post-code an-ast-node value)
  (update-gen-info an-ast-node
                   (struct-copy ast-gen-info (ast-node-gen-info an-ast-node)
                                [post-code value])))

(define (update-gen-info-target-reg an-ast-node value)
  (update-gen-info an-ast-node
                   (struct-copy ast-gen-info (ast-node-gen-info an-ast-node)
                                [target-reg value])))

(define (update-gen-info-target-reg-pref an-ast-node value)
  (update-gen-info an-ast-node
                   (struct-copy ast-gen-info (ast-node-gen-info an-ast-node)
                                [target-reg-pref value])))

(define (update-gen-info-locals-used an-ast-node value)
  (update-gen-info an-ast-node
                   (struct-copy ast-gen-info (ast-node-gen-info an-ast-node)
                                [locals-used value])))

(define/contract (allocate-regs-fun-call a-fun-call param-map allocate-at)
  (->* [ast-e-fun-call? hash? byte?] [] ast-e-fun-call?)
  ;; each parameter gets new allocate-at
  (define new-params  (reverse (cdr
                                (foldl (lambda (param allocate-at-param-list)
                                         (define updated-param (update-gen-info-target-reg (allocate-registers param param-map (car allocate-at-param-list)) (car allocate-at-param-list)))
                                         (cons (+ 1 (ast-gen-info-target-reg (ast-node-gen-info updated-param)))
                                               (cons updated-param (cdr allocate-at-param-list))))
                                       (cons allocate-at (list))
                                       (ast-e-fun-call-params a-fun-call)))))
  ;; TODO determine target registers for tail call optimization!
  ;; in case of a recursive call, the parameters may have target registers corresponding to the actual parameters
  ;; careful though, a parameter may be used in some parameter (pre) code => order precode to not clash (if possible)
  (define fun-id (ast-e-fun-call-fun a-fun-call))
  (ast-e-fun-call (ast-node-gen-info a-fun-call)
                  fun-id
                  new-params))

(module+ test #| allocate-regs-fun-call |#

  (check-equal? (allocate-regs-fun-call
                 (ast-e-fun-call (make-ast-gen-info)
                           'fun
                           '())
                 (hash)
                 0)
                (ast-e-fun-call (make-ast-gen-info)
                          'fun
                          '()))
  (check-equal? (allocate-regs-fun-call
                 (ast-e-fun-call (make-ast-gen-info)
                           'fun
                           (list (ast-ev-bool (make-ast-gen-info) #t)
                                 (ast-ev-number (make-ast-gen-info) 15)))
                 (hash)
                 0)
                (ast-e-fun-call (make-ast-gen-info)
                          'fun
                          (list (ast-ev-bool (make-ast-gen-info) #t)
                                (ast-ev-number (make-ast-gen-info #:target-reg 1) 15))))

    (check-equal? (allocate-regs-fun-call
                 (ast-e-fun-call (make-ast-gen-info)
                           'fun
                           (list (ast-ev-bool (make-ast-gen-info) #t)
                                 (ast-ev-number (make-ast-gen-info) 15)))
                 (hash)
                 5)
                (ast-e-fun-call (make-ast-gen-info)
                          'fun
                          (list (ast-ev-bool (make-ast-gen-info #:target-reg 5) #t)
                                (ast-ev-number (make-ast-gen-info #:target-reg 6) 15))))
    (check-equal? (allocate-regs-fun-call
                   (ast-e-fun-call (make-ast-gen-info)
                                   'fun
                                   (list (ast-ev-bool (make-ast-gen-info) #t)
                                         (ast-e-fun-call (make-ast-gen-info) 'inner
                                                         (list (ast-ev-string (make-ast-gen-info) "hello")
                                                               (ast-ev-string (make-ast-gen-info) "there")))
                                         (ast-ev-number (make-ast-gen-info) 15)))
                   (hash)
                   5)
                  (ast-e-fun-call (make-ast-gen-info)
                                  'fun
                                  (list (ast-ev-bool (make-ast-gen-info #:target-reg 5) #t)
                                        (ast-e-fun-call (make-ast-gen-info #:target-reg 6) 'inner
                                                        (list (ast-ev-string (make-ast-gen-info #:target-reg 6) "hello")
                                                              (ast-ev-string (make-ast-gen-info #:target-reg 7) "there")))
                                        (ast-ev-number (make-ast-gen-info #:target-reg 7) 15)))))

(define/contract (get-wanted-target-reg an-ast-expression param-map)
  (->* [ast-expression? hash?] [] byte?)
  (cond
    [(ast-ev-id? an-ast-expression) (encode-idx  (hash-ref param-map (ast-ev-id-id an-ast-expression)) l-param)]
    [(ast-e-loc-ref? an-ast-expression) (ast-gen-info-target-reg (ast-node-gen-info an-ast-expression))]
    [(ast-e-value? an-ast-expression) (ast-gen-info-target-reg (ast-node-gen-info an-ast-expression))]
    [else (raise-user-error (format "expression has no wanted target register ~a" an-ast-expression))]))

(define (allocate-regs-if-expr  an-if-expr param-map allocate-at)
  ;; opt: check whether branches can optimize the target reg in last expression
  ;; (define then-expr (second (ast-e-fun-call-params an-if-expr)))
  (define else-expr (third (ast-e-fun-call-params an-if-expr)))
  (define allocated-params (map (lambda (param) (allocate-registers param param-map allocate-at)) (ast-e-fun-call-params an-if-expr)))
  (cond
    [(retargetable-atomic-expression else-expr)
     (define wanted-target-register (get-wanted-target-reg else-expr param-map))
     (define new-params (list (first allocated-params) (second allocated-params)
                              (update-gen-info-target-reg (third allocated-params) wanted-target-register)))
     (ast-e-if (struct-copy ast-gen-info (ast-node-gen-info an-if-expr)
                            [target-reg wanted-target-register])
               'if
               new-params
               (ast-e-if-negated an-if-expr))     ]
    [else
     (ast-e-if (ast-node-gen-info an-if-expr)
               'if
               allocated-params
               (ast-e-if-negated an-if-expr))]))

(define/contract (allocate-registers an-ast-expression param-map (allocate-at 0))
  (->* [ast-expression? hash?] [byte?] ast-expression?)
  (cond
    [(ast-e-if? an-ast-expression) (allocate-regs-if-expr an-ast-expression param-map allocate-at)]
    [(ast-e-fun-call? an-ast-expression) (allocate-regs-fun-call an-ast-expression param-map allocate-at)]
    [else an-ast-expression]))

(module+ test #| allocate registers |# )

;; (strongest) 'fixed, 'open, 'indifferent (weakes)
;;             | fixed | open  | indifferent
;; ------------+-------+-------+------------
;; fixed       | fixed | fixed | fixed
;; open        | fixed | open  | open
;; indifferent | fixed | open  | indifferent

(define reg-status? (or/c 'fixed 'open 'indifferent))

(define/contract (infer-reg-status a-reg-status b-reg-status)
  (->* [reg-status? reg-status?] [] reg-status? )
  (cond
    [(or (eq? a-reg-status 'fixed) (eq? b-reg-status 'fixed)) 'fixed]
    [(or (eq? a-reg-status 'open) (eq? b-reg-status 'open)) 'open]
    [else 'indifferent]))

(define/contract (target-reg-status an-ast-expression)
  (->* [ast-expression?] [] reg-status?)
  (cond
    [(ast-e-if? an-ast-expression)
     (infer-reg-status (target-reg-status (second (ast-e-fun-call-params an-ast-expression)))
                       (target-reg-status (third (ast-e-fun-call-params an-ast-expression))))]
    [(ast-e-fun-call) 'fixed]
    [(ast-e-value? an-ast-expression) 'open]
    [else 'indifferent]))

(define/contract (retargetable-atomic-expression ast-expr)
  (->* [ast-expression?] [] boolean?)
  (or (ast-e-value? ast-expr)
         (ast-e-loc-ref? ast-expr)))

(define/contract (normalize-ast-e-if an-if-expression)
  (->* [ast-e-if?] [] ast-e-if?)
  (define parameters (ast-e-fun-call-params an-if-expression))
  (define then-expr (second parameters))
  (define else-expr (third parameters))
  (if (and (retargetable-atomic-expression then-expr)
         (not (retargetable-atomic-expression else-expr))) ;; switch then else branch
      (ast-e-if (ast-node-gen-info an-if-expression)
                (ast-e-fun-call-fun an-if-expression)
                (list (normalize-ast-expression (car parameters))
                      (normalize-ast-expression else-expr)
                      (normalize-ast-expression then-expr))
                (not (ast-e-if-negated an-if-expression)))
      (ast-e-if (ast-node-gen-info an-if-expression)
                (ast-e-fun-call-fun an-if-expression)
                (map normalize-ast-expression parameters)
                (ast-e-if-negated an-if-expression))))

(define/contract (normalize-ast-expression an-ast-expression)
  (->* [ast-expression?] [] ast-expression?)
  (cond
    [(ast-e-if? an-ast-expression) (normalize-ast-e-if an-ast-expression)]
    [(ast-e-fun-call? an-ast-expression)
     (struct-copy ast-e-fun-call an-ast-expression
                  [params (map normalize-ast-expression (ast-e-fun-call-params an-ast-expression))])]
    [else an-ast-expression]))

(module+ test #| normalize |#
  (check-match (normalize-ast-expression
                 (car (ast-function-def-body
                       (m-def (reverse (a-list (list cell)) (b-list (list cell) '()) -> (list cell)
                                       "reverse a-list, consing it into b-list")
                              (if (nil? a-list)
                                  b-list
                                  (reverse (cdr a-list) (cons (car a-list) b-list)))))))
               (ast-e-if _
                         'if
                         (list
                          (ast-e-fun-call _ 'nil? (list (ast-ev-id _ 'a-list)))
                          (ast-e-fun-call _
                                          'reverse
                                          (list
                                           (ast-e-fun-call _ 'cdr (list (ast-ev-id _ 'a-list)))
                                           (ast-e-fun-call _ 'cons
                                                           (list (ast-e-fun-call _ 'car (list (ast-ev-id _ 'a-list)))
                                                                 (ast-ev-id _ 'b-list)))))
                          (ast-ev-id _ 'b-list))
                         #t))

  (check-match (normalize-ast-expression
                 (car (ast-function-def-body
                       (m-def (reverse (a-list (list cell)) (b-list (list cell) '()) -> (list cell)
                                       "reverse a-list, consing it into b-list")
                              (if (not! (nil? a-list))
                                  (reverse (cdr a-list) (cons (car a-list) b-list))
                                  b-list)))))
               (ast-e-if _
                         'if
                         (list
                          (ast-e-fun-call _ 'not! (list
                                                   (ast-e-fun-call _ 'nil? (list (ast-ev-id _ 'a-list)))))
                          (ast-e-fun-call _
                                          'reverse
                                          (list
                                           (ast-e-fun-call _ 'cdr (list (ast-ev-id _ 'a-list)))
                                           (ast-e-fun-call _ 'cons
                                                           (list (ast-e-fun-call _ 'car (list (ast-ev-id _ 'a-list)))
                                                                 (ast-ev-id _ 'b-list)))))
                          (ast-ev-id _ 'b-list))
                         #f)))

(module+ test #| compile |#

  (check-match
   (transform
    (allocate-registers (car (ast-function-def-body
                              (m-def (reverse (a-list (list cell)) (b-list (list cell) '()) -> (list cell)
                                              "reverse a-list, consing it into b-list")
                                     (if (nil? a-list)
                                         b-list
                                         (reverse (cdr a-list) (cons (car a-list) b-list))))))
                        (hash)))
   (ast-e-if
    (ast-gen-info
     (list (ast-e-loc-set _ sym-1 (ast-e-fun-call _ 'nil? (list (ast-ev-id _ 'a-list))))) '() 0 'default 0)
    'if
    (list
     (ast-e-loc-ref _ sym-1)
     (ast-ev-id _ 'b-list) ;; then branch = just reference (no expression, no value etc)  => eligable for other target reg
     (ast-e-fun-call       ;; else branch = recursive call => no other target reg necessary
      (ast-gen-info
       (list
        (ast-e-loc-set _ sym-2 (ast-e-fun-call _ 'cdr (list (ast-ev-id _ 'a-list))))
        (ast-e-loc-set _ sym-3
                  (ast-e-fun-call
                   (ast-gen-info (list (ast-e-loc-set _ sym-4 (ast-e-fun-call _ 'car (list (ast-ev-id _ 'a-list)))))
                                 '() 1 'default 1)
                   'cons
                   (list
                    (ast-e-loc-ref _ sym-4)
                    (ast-ev-id _ 'b-list)))))
       '() 0 'default 2)
      'reverse
      (list
       (ast-e-loc-ref (ast-gen-info '() '() 0 'default 0) sym-2)
       (ast-e-loc-ref (ast-gen-info '() '() 1 'default 0) sym-3))))
    #f))

  (define t-opt-ret-fun (m-def (reverse (a-list (list cell)) (b-list (list cell) '()) -> (list cell)
                          "reverse a-list, consing it into b-list")
                 (if (nil? a-list)
                     b-list
                     (reverse (cdr a-list) (cons (car a-list) b-list)))))
  (define t-opt-ret-normalized-expressions
    (map (lambda (expr) (normalize-ast-expression expr))
         (ast-function-def-body t-opt-ret-fun)))

  (define t-opt-ret-param-map (--prep-param-hashmap (append (ast-function-def-parameter t-opt-ret-fun) (ast-function-def-default-parameter t-opt-ret-fun))))
  (define t-opt-ret-allocated-expressions (map (lambda (expr) (allocate-registers expr t-opt-ret-param-map)) t-opt-ret-normalized-expressions))
  (define t-opt-ret-transformed-expressions (map transform t-opt-ret-allocated-expressions))

  (check-equal?
   (disassemble
    (gen-context-gen-bytes
     (cisc-vm-transform
      (m-def (reverse (a-list (list cell)) (b-list (list cell) '()) -> (list cell)
                      "reverse a-list, consing it into b-list")
             (if (nil? a-list)
                 b-list
                 (reverse (cdr a-list) (cons (car a-list) b-list))))))
    (make-vm))
   (list "nil? p0 -> l0"
         "bra l0? -> 21"
         "cdr p0 -> l0"
         "car p0 -> l1"     ;; data flow (eliminate move by replacing last assignment with move target, reorder to allow)
         "cons l1 p1 -> l1" ;; (a1) cdr p0 -> l0    before (a2)      (b1) car p0 -> l1         before (b2)
         "move l0 -> p0"    ;;                                       (b2) cons l1 p1 -> l1     after (b1) before (b3)
         "move l1 -> p1"    ;; (a2) move l0 -> p0   after  (a1,b1)   (b3) move l1 -> p1        after (b2)
         "goto -> -23"      ;;
         "ret p1"))

  (skip ": optimization should yield this (someday)"
        (check-equal?
         (disassemble
          (gen-context-gen-bytes
           (cisc-vm-transform
            (m-def (reverse (a-list (list cell)) (b-list (list cell) '()) -> (list cell)
                            "reverse a-list into b-list (through cons ing)")
                   (if (nil? a-list)
                       b-list
                       (reverse (cdr a-list) (cons (car a-list) b-list))))))
          (make-vm))
         (list "nil? p0 -> l0"
               "bra l0? -> 13"
               "car p0 -> l0"
               "cons l0 p1 -> p1"
               "cdr p0 -> p0" ;; reordering this expression to the end allows for reuse of p0
               "goto -> -17"
               "ret p1"))))

(module+ test #| optimization ideas |#

  (define example-ast
    (ast-function-def
     agsi
     'reverse
     (list (ast-param-def agsi 'a-list (ast-ts-complex agsi 'list (list (ast-td-simple agsi 'cell)))))
     (list (ast-pd-default-param agsi 'b-list (ast-ts-complex agsi 'list (list (ast-td-simple agsi 'cell))) (ast-e-nil agsi)))
     (ast-ts-complex agsi 'list (list (ast-td-simple agsi 'cell)))
     '("reverse a-list, consing it into b-list")
     (list
      (ast-e-if

    agsi  ;; target reg of this command is l0 (default for an expression in the body, can be changed)
       'if
       (list
        (ast-e-fun-call agsi 'nil? (list (ast-ev-id agsi 'a-list)) )
        (ast-ev-id agsi 'b-list) ;; then branch has only reference => try to propagate target reg = 'b-list this up (to if)
        (ast-e-fun-call
         agsi ;; else branch is recursive call (has no requirement on target reg, since it does not use it)
         'reverse
         (list (ast-e-fun-call agsi 'cdr (list (ast-ev-id agsi 'a-list)) )
               (ast-e-fun-call agsi 'cons (list (ast-e-fun-call agsi 'car (list (ast-ev-id agsi 'a-list)) ) (ast-ev-id agsi 'b-list)) ))))
       #f))))

  #|
  pattern for optimization
  ast-function-def
  gen info (target reg VM_L0 = default)
  body list
  ...
  <any depth?>

  last-expression
  gen-info (target reg VM_L0 = default)
  ;; cases: id-ref => push new target reg up
  any value (bool, int, string ...) => use target reg (no push up)
  call (regular) => use target reg (no push up)
  call (recursive) => use target reg (no push up)
  if bool then-branch else-branch :: if both branches want to push, choose one, if one branch wants to push, do so, if none want to push, inore
  cond bool branch ... else branch :: if all branches want to push, choose one, if one branch wants to push, do so, if none want to push, ignore

  |#
  )





;; (tracking liveliness,) <- optimize later on that
;; replacing tree nodes with cisc
