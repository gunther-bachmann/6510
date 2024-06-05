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
  (id)
  #:transparent
  #:guard
  (struct-guard/c symbol?))

(struct parameter-def
  (id type)
  #:transparent
  #:guard
  (struct-guard/c symbol? type-def?))

(struct expression-def
  ()
  #:transparent)

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


(define-syntax-parser t-def
  [(_ basic-type)
   #'(type-def 'basic-type)]
  [(_ (cpx-type inner-type ...))
   #'(type-def (t-def inner-type ...))]) ;; TODO provide constructor for type-def

(define-syntax-parser m-def
  #:literals [->]
  [(_ (id (p-id p-typ) ... (o-id o-typ o-val) ... -> r-typ desc ...) expr ...)
   #'(function-def 'id
                   (list (parameter-def 'p-id (t-def p-typ)) ...)
                   (list (default-parameter-def 'o-id (t-def o-typ) (expression-def);; 'o-val
                           ) ...)
                   (t-def r-typ)
                   (list 'desc ...)
                   (list (expression-def ;; (list 'expr ...)
                          )))])

(module+ test #| m-def |#
  (define mf0 (m-def (f0 -> string "description1" "description2") "value-hello"))

  (check-equal? (function-def-id mf0)
                'f0)
  (check-equal? (function-def-parameter mf0)
                '())
  (check-equal? (function-def-default-parameter mf0)
                '())
  (check-equal? (function-def-return-type mf0)
               (type-def 'string))
  (check-equal? (function-def-description mf0)
                (list "description1" "description2"))

  (define mf1 (m-def (f1 (a string) (b bool) (q string "h") -> string)
                     (hello)
                     (hello2)))

  (check-equal? (function-def-id mf1)
                'f1)
  (check-equal? (function-def-parameter mf1)
                (list (parameter-def 'a (type-def 'string))
                      (parameter-def 'b (type-def 'bool))))
  (check-equal? (function-def-default-parameter mf1)
                (list (default-parameter-def 'q (type-def 'string) (expression-def))))
  (check-equal? (function-def-return-type mf1)
               (type-def 'string))
  (check-equal? (function-def-description mf1)
                '())

  (define mf2 (m-def (f2 (a string) -> bool)))

  (check-equal? (function-def-id mf2)
                'f2)
  (check-equal? (function-def-parameter mf2)
                (list (parameter-def 'a (type-def 'string))))
  (check-equal? (function-def-default-parameter mf2)
                '())
  (check-equal? (function-def-return-type mf2)
               (type-def 'bool))
  (check-equal? (function-def-description mf2)
                '())

  (define mf3 (m-def (f3 (a string "init") -> int)))

  (check-equal? (function-def-id mf3)
                'f3)
  (check-equal? (function-def-parameter mf3)
                '()
                )
  (check-equal? (function-def-default-parameter mf3)
                (list (default-parameter-def 'a (type-def 'string) (expression-def))))
  (check-equal? (function-def-return-type mf3)
               (type-def 'int))
  (check-equal? (function-def-description mf3)
                '()))
