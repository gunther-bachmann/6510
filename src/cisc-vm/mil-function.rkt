#lang racket


;; basic types
;;   bool, char, byte, int, string
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

(struct ed-function-call expression-def
  (fun params)
  #:transparent
  #:guard
  (struct-guard/c symbol? (listof expression-def?)))

(struct ed-value expression-def
  ()
  #:transparent)

(struct edv-string expression-def
  (str)
  #:transparent
  #:guard
  (struct-guard/c string?))

(struct edv-boolean expression-def
  (bool)
  #:transparent
  #:guard
  (struct-guard/c boolean?))

(struct edv-number expression-def
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

(define-syntax-parser m-def2
  #:literals [->]
  [(_ (id (p-id p-typ) ... (o-id o-typ o-val) ... -> r-typ desc ...) expr ...)
   #'(list 'id '((p-id p-typ) ...) '((o-id o-typ o-val) ...) 'r-typ (list 'desc ...) (list'expr ...))])


(define-syntax-parser m-type-def
  [(_ (cpx-type inner-type ...))
   #'(td-complex 'cpx-type (list (m-type-def inner-type) ...))]
  [(_ basic-type)
   #'(td-simple 'basic-type)])

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

(define-syntax-parser m-expression-def
  [(_ (id param ...))
   #'(ed-function-call 'id (list (m-expression-def param) ...))]
  [(_ value)
   #'(cond ((string? 'value) (edv-string 'value))
           ((boolean? 'value) (edv-boolean 'value))
           ((exact-integer? 'value) (edv-number 'value))
           (else (raise-user-error (format "unknown expression value type ~a" 'value))))])

(module+ test #| m-expression-def |#
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
   (m-expression-def (fn2))
   (ed-function-call 'fn2 (list)))

  (check-equal?
   (m-expression-def (fn3 (fn5 "A") (- 1 2)))
   (ed-function-call 'fn3 (list (ed-function-call 'fn5 (list (edv-string "A")))
                                (ed-function-call '- (list (edv-number 1) (edv-number 2)))))))
