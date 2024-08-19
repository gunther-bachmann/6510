#lang typed/racket/base

#|review: ignore|#
#|  review does show several false positives |#

#|

  Compiler for mil to stack based virtual machine

 |#

(require (only-in racket/fixnum fx+ fx= fx< fx<= fx- fx>= fx>))
(require (only-in racket/list range take))
(require (only-in racket/match match))
(require (only-in racket/vector vector-append))

(require (only-in racket/hash hash-union))

(require (only-in threading ~>>))

(require (only-in "./util.rkt" nested->list low-byte high-byte bytes->int))
(require "./mil-ast.rkt")
(require "./mil-parse.rkt")

(require (only-in "./stack-virtual-machine.rkt"
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

(define-type Id-Reg-Map (Immutable-HashTable Symbol register-ref-))

;; resolve ids on a single local value expression in with-forms
(define (svm-resolve-ids--with--map-local
         (local : ast-ex-with-local-) (i : Nonnegative-Integer) (id-map : Id-Reg-Map) (locals-used : Nonnegative-Integer))
        : ast-ex-with-local-
  (define value (ast-ex-with-local--value local))
  (struct-copy ast-ex-with-local- local
               [value (svm-resolve-ids->expression value id-map locals-used)]))

(module+ test #| svm-resolve-ids--with--map-local |#
  (check-exn exn:fail? (lambda () (svm-resolve-ids--with--map-local
                              (ast-ex-with-local- (make-ast-info) 'l0
                                                  (ast-td-simple- (make-ast-info) 'byte)
                                                  (ast-at-id- (make-ast-info) 'l0))
                              0
                              (hash)
                              7))
             "fail if id in local expression cannot be resolved from id-map")

  (check-true (match (nested->list (svm-resolve-ids--with--map-local
                        (ast-ex-with-local- (make-ast-info) 'l1
                                            (ast-td-simple- (make-ast-info) 'byte)
                                            (ast-at-id- (make-ast-info) 'l0))
                        1
                        (hash 'l0 (register-ref- 'Local 1))
                        7))
                [(list 'ast-ex-with-local- _ 'l1 _
                       (list 'ast-at-id-
                             (list 'ast-info- _ ...
                                   (list 'hash 'l0 (list 'register-ref- 'Local 1)))
                             'l0))
                 #t]
                [_ #f])
              "resolve id in local expression to reference found in id-map"))

;; resolve ids on with-forms, adding introduced ids to the id-map
(define (svm-resolve-ids--with
         (with-node : ast-ex-with-) (id-map : Id-Reg-Map) (locals-used : Nonnegative-Integer))
        : ast-ex-with-
  (define locals (ast-ex-with--locals with-node))
  (define ids (map (lambda (local) (ast-ex-with-local--id local)) locals))
  (define id-reg-ref-pairs (map (lambda: ((a : Symbol) (idx : Nonnegative-Integer))
                                  (cons a (register-ref- 'Local (+ idx locals-used))))
                               ids (range (length ids))))
  (define new-locals-used (+ locals-used (length locals)))
  (define complete-id-map (hash-union id-map (make-hash id-reg-ref-pairs)))
  (struct-copy ast-ex-with- with-node
               [info #:parent ast-node- (struct-copy ast-info- (ast-node--info with-node)
                                                     [locals-used new-locals-used])]
               [locals (map (lambda: ((local : ast-ex-with-local-) (i : Nonnegative-Integer))
                              (define id-map-up-to-value (hash-union id-map (make-hash (take id-reg-ref-pairs i))))
                              (svm-resolve-ids--with--map-local local i id-map-up-to-value new-locals-used))
                            locals (range (length locals)))]
               [body (svm-resolve-ids->expression (ast-ex-with--body with-node) complete-id-map new-locals-used)]))

(module+ test #| svm-resolve-ids--with |#

  (check-true (match (nested->list
                      (svm-resolve-ids--with
                       (cast (m-expression-def (with ((l0 byte 0)
                                                      (l1 byte 1))
                                                     (byte+ l0 l1)))
                             ast-ex-with-)
                       (hash)
                       2))
                [(list 'ast-ex-with- _ _
                       (list 'ast-ex-fun-call- _ 'byte+
                             (list (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'l0 (list 'register-ref- 'Local 2))) 'l0)
                                   (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'l1 (list 'register-ref- 'Local 3))) 'l1))))
                 #t]
                [_ #f])
              "ensure that new ids are used to resolve body expression")

  (check-true (match (nested->list
                      (svm-resolve-ids--with
                       (cast (m-expression-def (with ((l0 byte 0)
                                                      (l1 byte (byte+ l0 1)))
                                                     l1))
                             ast-ex-with-)
                       (hash)
                       2))
                [(list 'ast-ex-with- _
                       (list (list 'ast-ex-with-local- _ 'l0 _ _)
                             (list 'ast-ex-with-local- _ 'l1 _
                                   (list 'ast-ex-fun-call- _ 'byte+
                                         (list (list 'ast-at-id-
                                                     (list 'ast-info- _ ...
                                                           (list 'hash 'l0 (list 'register-ref- 'Local 2)))
                                                     'l0)
                                               (list 'ast-at-int- _ 1)))))
                       _)
                 #t]
                [_ #f])
              "ensure that new ids can be used in the definition of following locals")

  (check-exn exn:fail? (lambda () (svm-resolve-ids--with
                              (cast (m-expression-def (with ((l0 byte l0)
                                                             (l2 byte 17))
                                                            l2))
                                    ast-ex-with-)
                              (hash)
                              2))
             "ensure that new ids cannot be used in same or previous locals")

  (check-exn exn:fail? (lambda () (svm-resolve-ids--with
                              (cast (m-expression-def (with ((l1 byte (byte+ l2 1))
                                                             (l2 byte 17))
                                                            l2))
                                    ast-ex-with-)
                              (hash)
                              2))
             "ensure that new ids cannot be used in same or previous locals"))

;; resolve ids on a clause of a cond-form
(define (svm-resolve-ids--cond--clause
         (clause : ast-ex-cond-clause-) (id-map : Id-Reg-Map) (locals-used : Nonnegative-Integer))
        : ast-ex-cond-clause-
  (define condition (ast-ex-cond-clause--condition clause))
  (define body (ast-ex-cond-clause--body clause))
  (struct-copy ast-ex-cond-clause- clause
               [info #:parent ast-node- (struct-copy ast-info- (ast-node--info clause)
                                                     [locals-used locals-used])]
               [condition (svm-resolve-ids->expression condition id-map locals-used)]
               [body      (svm-resolve-ids->expression body id-map locals-used)]))

(module+ test #| svm-resolve-ids--cond-clause |#
  (check-true (match (nested->list
                      (svm-resolve-ids--cond--clause
                       (ast-ex-cond-clause- (make-ast-info) (m-expression-def l0) (m-expression-def l1))
                       (hash 'l0 (register-ref- 'Local 0) 'l1 (register-ref- 'Local 1))
                       1))
                [(list 'ast-ex-cond-clause- _
                       (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'l0 (list 'register-ref- 'Local 0))) 'l0)
                       (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'l1 (list 'register-ref- 'Local 1))) 'l1))
                 #t]
                [_ #f])
              "check that all conditions and body expressions are resolved")

  (check-true (match (nested->list
                      (svm-resolve-ids--cond--clause
                       (ast-ex-cond-clause- (make-ast-info) (m-expression-def 0) (m-expression-def 1))
                       (hash)
                       1))
                [(list 'ast-ex-cond-clause-
                       (list 'ast-info- _ _ _ _ 1 _)
                       _ _) #t]
                [_ #f])
              "ensure that locals-used is passed on"))

;; resolve ids on a cond-form
(define (svm-resolve-ids--cond
         (cond-node : ast-ex-cond-) (id-map : Id-Reg-Map) (locals-used : Nonnegative-Integer))
        : ast-ex-cond-
  (struct-copy ast-ex-cond- cond-node
               [info #:parent ast-node- (struct-copy ast-info- (ast-node--info cond-node)
                                                     [locals-used locals-used])]
               [clauses (map (lambda: ((clause : ast-ex-cond-clause-))
                               (svm-resolve-ids--cond--clause clause id-map locals-used))
                             (ast-ex-cond--clauses cond-node))]
               [else    (svm-resolve-ids->expression (ast-ex-cond--else cond-node) id-map locals-used)]))

(module+ test #| svm-resolve-ids--cond |#

  [check-true (match (nested->list
                      (svm-resolve-ids--cond
                       (cast (m-expression-def (cond (#t l0) (#f l1) (_ l2))) ast-ex-cond-)
                       (hash 'l0 (register-ref- 'Local 0)
                             'l1 (register-ref- 'Local 1)
                             'l2 (register-ref- 'Local 2))
                       1))
                [(list 'ast-ex-cond- _
                       (list (list 'ast-ex-cond-clause- _ _
                                   (list 'ast-at-id- (list 'ast-info- _ ...
                                                           (list 'hash 'l0 (list 'register-ref- 'Local 0)))
                                         'l0))
                             (list 'ast-ex-cond-clause- _ _
                                   (list 'ast-at-id- (list 'ast-info- _ ...
                                                           (list 'hash 'l1 (list 'register-ref- 'Local 1)))
                                         'l1)))
                       (list 'ast-at-id- (list 'ast-info- _ ...
                                               (list 'hash 'l2 (list 'register-ref- 'Local 2)))
                             'l2)) #t]
                [_ #f])
              "make sure default case and all other clauses is resolved"])

;; resolve ids on an if-form
(define (svm-resolve-ids--if
         (if-node : ast-ex-if-) (id-map : Id-Reg-Map) (locals-used : Nonnegative-Integer))
        : ast-ex-if-
  (struct-copy ast-ex-if- if-node
               [info #:parent ast-node- (struct-copy ast-info- (ast-node--info if-node)
                                                     [locals-used locals-used])]
               [condition (svm-resolve-ids->expression (ast-ex-if--condition if-node) id-map locals-used)]
               [then      (svm-resolve-ids->expression (ast-ex-if--then if-node) id-map locals-used)]
               [else      (svm-resolve-ids->expression (ast-ex-if--else if-node) id-map locals-used)]))

(module+ test #| svm-resolve-ids-- |# )

;; resolve ids on a funcion call form
(define (svm-resolve-ids--fun-call
         (fun-call-node : ast-ex-fun-call-) (id-map : Id-Reg-Map) (locals-used : Nonnegative-Integer))
        : ast-ex-fun-call-
  (struct-copy ast-ex-fun-call- fun-call-node
               [info #:parent ast-node- (struct-copy ast-info- (ast-node--info fun-call-node)
                                                     [locals-used locals-used])]
               [parameters (map (lambda: ((expr : ast-expression-))
                                  (svm-resolve-ids->expression expr id-map locals-used))
                                (ast-ex-fun-call--parameters fun-call-node))]))

(module+ test #| svm-resolve-ids-- |# )

;; add information in which register this id is allocated, thus allowing generation  push, pop-to commands
(define (svm-resolve-ids--id
         (id-node : ast-at-id-) (id-map : Id-Reg-Map) (locals-used : Nonnegative-Integer))
        : ast-at-id-
  (struct-copy ast-at-id- id-node
               [info #:parent ast-node-
                     (struct-copy ast-info- (ast-node--info id-node)
                                  [locals-used locals-used]
                                  [id-map (hash (ast-at-id--id id-node)
                                                (hash-ref id-map (ast-at-id--id id-node)))])] ))

(module+ test #| svm-resolve-ids-- |# )

;; resolve ids in a function definition, adding the parameters as new id-reg-maps
(define (svm-resolve-ids--fun-def
         (fun-def-node : ast-ex-fun-def-) (id-map : Id-Reg-Map) (locals-used : Nonnegative-Integer))
        : ast-ex-fun-def-
  (define param-ids (map (lambda (param) (ast-param-def--id param))
                         (append (ast-ex-fun-def--parameter fun-def-node)
                                 (ast-ex-fun-def--def-params fun-def-node))))
  (define param-ids-len (length param-ids))
  (define param-id-reg-ref-pairs
    (map (lambda: ((a : Symbol) (idx : Nonnegative-Integer))
           (cons a (register-ref- 'Param idx)))
         param-ids (range param-ids-len)))
  (define new-param-id-map (hash-union id-map (make-hash param-id-reg-ref-pairs)))
  (struct-copy ast-ex-fun-def- fun-def-node
               [info #:parent ast-node- (struct-copy ast-info- (ast-node--info fun-def-node)
                                                     [locals-used locals-used])]
               [body (svm-resolve-ids->expression
                      (ast-ex-fun-def--body fun-def-node)
                      new-param-id-map
                      locals-used)]))

(module+ test #| svm-resolve-ids-- |# )

;; resolve ids in defaulted parameters
(define (svm-resolve-ids--pa-defaulted
         (pa-def-node : ast-pa-defaulted-def-) (id-map : Id-Reg-Map) (locals-used : Nonnegative-Integer))
        : ast-pa-defaulted-def-
  (struct-copy ast-pa-defaulted-def- pa-def-node
               [info #:parent ast-node- (struct-copy ast-info- (ast-node--info pa-def-node)
                                                     [locals-used locals-used])]
               [default (svm-resolve-ids->expression
                         (ast-pa-defaulted-def--default pa-def-node)
                         id-map
                         locals-used)]))

(module+ test #| svm-resolve-ids-- |# )

;; TODO: idea: implement svm-resolve-ids with a visitor, rewriting expressions, and a visitor, collecting declarations?
;; resolve ids in any form, dispatching to forms introducing new ids and ones, having expressions that might use ids
(define (svm-resolve-ids
         (node : ast-node-) (id-map : Id-Reg-Map) (locals-used : Nonnegative-Integer))
        : ast-node-
  (cond
    [(ast-ex-with-? node)          (svm-resolve-ids--with node id-map locals-used)]
    [(ast-ex-cond-? node)          (svm-resolve-ids--cond node id-map locals-used)]
    [(ast-ex-if-? node)            (svm-resolve-ids--if node id-map locals-used)]
    [(ast-ex-fun-call-? node)      (svm-resolve-ids--fun-call node id-map locals-used)]
    [(ast-at-id-? node)            (svm-resolve-ids--id node id-map locals-used)]
    [(ast-pa-defaulted-def-? node) (svm-resolve-ids--pa-defaulted node id-map locals-used)]
    [(ast-ex-fun-def-? node)       (svm-resolve-ids--fun-def node id-map locals-used)]
    [else node]))

;; resolve ids in an expression casting to expression (syntatic sugar)
(define (svm-resolve-ids->expression
         (node : ast-expression-) (id-map : Id-Reg-Map) (locals-used : Nonnegative-Integer))
        : ast-expression-
  (cast (svm-resolve-ids node id-map locals-used) ast-expression-))

(module+ test #| svm-resolve-ids |#
  (check-true
   (match (nested->list (svm-resolve-ids (m-expression-def (with ((p0 byte 1) (p1 byte (f p0))) p1)) (hash) 0))
     [(list 'ast-ex-with- _
              (list (list 'ast-ex-with-local- _ 'p0 _ _)
                    (list 'ast-ex-with-local- _ 'p1 _
                          (list 'ast-ex-fun-call-
                                  _ 'f
                                  (list (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p0 (list 'register-ref- 'Local 0))) 'p0)))))
              (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p1 (list 'register-ref- 'Local 1))) 'p1)) #t]
     [_ #f])
   "resolved locals in with forms: p0/p1 (first/second local) has offset 0/1")

  (check-true
   (match (nested->list (svm-resolve-ids (m-fun-def (some (p0 int) (p1 bool #t) -> bool)
                                                   (cond (p1 p0) (p0 p1) (_ p0))) (hash) 0))
     [(list _ ... (list 'ast-ex-cond- _
                      (list (list 'ast-ex-cond-clause- _
                                  (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p1 (list 'register-ref- 'Param 1))) 'p1)
                                  (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p0 (list 'register-ref- 'Param 0))) 'p0))
                            (list 'ast-ex-cond-clause- _
                                  (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p0 (list 'register-ref- 'Param 0))) 'p0)
                                  (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p1 (list 'register-ref- 'Param 1))) 'p1)))
                      (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p0 (list 'register-ref- 'Param 0))) 'p0))) #t]
     [_ #f])
   "resolved parameter in cond forms: p1/p0 (second/first parameter) has offset 1/0, looking from the top of parameter stack")

  (check-true
   (match (nested->list (svm-resolve-ids (m-fun-def (some (p0 int) (p1 bool #t) -> bool)
                                                   (a p1)) (hash) 0))
     [(list _ ... (list 'ast-ex-fun-call- _ 'a (list (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p1 (list 'register-ref- 'Param 1))) 'p1)))) #t]
     [_ #f])
   "resolved parameter in function calls: p1 (second parameter) has offset 1, looking from the top of parameter stack")

  (check-true
   (match (nested->list (svm-resolve-ids (m-fun-def (some (p0 int) (p1 bool #t) -> bool)
                                                   (if p1 p0 p1)) (hash) 0))
     [(list _ ... (list 'ast-ex-if- _
                      (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p1 (list 'register-ref- 'Param 1))) 'p1)
                      (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p0 (list 'register-ref- 'Param 0))) 'p0)
                      (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p1 (list 'register-ref- 'Param 1))) 'p1)
                      _)) #t]
     [_ #f])
   "resolved parameter in if forms: p1/0 (second/first parameter) has offset 1/0, looking from the top of parameter stack")

  (check-true
   (match (nested->list (svm-resolve-ids (m-fun-def (some (p0 int) (p1 bool #t) -> bool)
                                                   p1) (hash) 0))
     [(list _ ... (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p1 (list 'register-ref- 'Param 1))) 'p1)) #t]
     [_ #f])
   "resolves parameter in id usage: p1 (second parameter) has offset 0, looking from the end of the list")

  (check-true
   (match (nested->list (svm-resolve-ids (m-fun-def (some (p0 int) (p1 bool #t) -> bool)
                                                   p0) (hash) 0))
     [(list _ ... (list 'ast-at-id- (list 'ast-info- _ ... (list 'hash 'p0 (list 'register-ref- 'Param 0))) 'p0)) #t]
     [_ #f])
   "resolves parameter in id usage: p0 (first parameter) has offset 0, looking from the top of parameter stack"))

;; optimization pattern:
;;   top-level-expr: if (nil? anything) <- can be negated
;;                      param/local <- then/else can be interchanged
;;                      tail-call <- in TC-position
;;   can be generated optimized with
;;     PUSH anything
;;     sNIL?-RET_PARAMc / sNIL?-RET-LOCALc
;;     <else ...>
;;     TAIL_CALL

(define (svm-generate--id (id : ast-at-id-)) : (Immutable-Vectorof Byte)
  (define reg (hash-ref (ast-info--id-map (ast-node--info id)) (ast-at-id--id id)))
  (generate-push reg))


(define (optimize-tail-call-if-nil--apply (if-node : ast-ex-if-)) : ast-ex-if-
  if-node)

;; (define (optimize-tail-call-if-nil (v-data : visitor-data-)) : visitor-data-
;;   ;; (define node (visited-ast-node--node v-data))
;;   ;; (define state (cast (visitor-data--state v-data) vs-optimize-tail-call-if-nil-))
;;   ;; (define cur-fun (vs-optimize-tail-call-if-nil--current-fun state))
;;   (match node
;;     [(ast-ex-if- _
;;                  (ast-ex-fun-call- _ nil-sym (list (ast-at-id- _ _)))
;;                  (ast-ex-fun-call- _ rec-sym _)
;;                  _ _)
;;      (if (and (eq? 'nil? nil-sym)
;;             (eq? cur-fun rec-sym))
;;          (struct-copy visitor-data- v-data
;;                       [node (optimize-tail-call-if-nil--apply (cast node ast-ex-if-))])
;;          v-data)]
;;     [(ast-ex-if- _
;;                  (ast-ex-fun-call- _ rec-sym _)
;;                  (ast-ex-fun-call- _ nil-sym (list (ast-at-id- _ nil-checked-sym)))
;;                  _ _)
;;      (define if-node (cast node ast-ex-if-))
;;      (if (and (eq? 'nil? nil-sym)
;;             (eq? cur-fun rec-sym))
;;          (struct-copy visitor-data- v-data
;;                       [node (optimize-tail-call-if-nil--apply
;;                              (struct-copy ast-ex-if- if-node
;;                                           [else (ast-ex-if--then if-node)]
;;                                           [then (ast-ex-if--else if-node)]
;;                                           [negated (not (ast-ex-if--negated if-node))]))])
;;          v-data)]
;;     [_ v-data]))

(struct visitor-state-
  ()
  #:transparent)

(struct visited-ast-node-
  ((node : ast-node-)
   (state : visitor-state-))
  #:transparent)

(define (svm-apply-expression-visitor
         (visitor : (-> visited-ast-node- visited-ast-node-))
         (visited-node : visited-ast-node-))
        : visited-ast-node-
  (define state (visited-ast-node--state visited-node))
  (define node (visited-ast-node--node visited-node))
  (cond
    [(ast-ex-if-? node)
     (define visited-cond (svm-apply-expression-visitor visitor (visited-ast-node- (ast-ex-if--condition node) (visited-ast-node--state (visitor visited-node)))))
     (define visited-then (svm-apply-expression-visitor visitor (visited-ast-node- (ast-ex-if--then node) (visited-ast-node--state visited-cond))))
     (define visited-else (svm-apply-expression-visitor visitor (visited-ast-node- (ast-ex-if--else node) (visited-ast-node--state visited-then))))
     (visited-ast-node-
      (struct-copy ast-ex-if- node
                   [condition (cast (visited-ast-node--node visited-cond) ast-expression-)]
                   [then (cast (visited-ast-node--node visited-then) ast-expression-)]
                   [else (cast (visited-ast-node--node visited-else) ast-expression-)])
      (visited-ast-node--state visited-else))]
    [else (visitor visited-node)]))

(module+ test #| svm-apply-expression-visitor |#
  ;; (svm-apply-expression-visitor optimize-tail-call-if-nil (vs-optimize-tail-call-if-nil- 'some) (ast-at-int- (make-ast-info) 5))
  (struct svm-expr-visitor--ints-state- visitor-state-
    ((ints : (Listof Integer)))
    #:transparent)
  (define (svm-expr-visitor--ints (visited-node : visited-ast-node- )) : visited-ast-node-
    (define state (cast (visited-ast-node--state visited-node) svm-expr-visitor--ints-state-))
    (define node (visited-ast-node--node visited-node))
    (cond
      [(ast-at-int-? node)
       (visited-ast-node-
        node
        (svm-expr-visitor--ints-state-
         (cons (ast-at-int--value node) (svm-expr-visitor--ints-state--ints state))))]
      [else visited-node]))

  (check-equal? (svm-expr-visitor--ints-state--ints
                 (cast (visited-ast-node--state
                        (svm-apply-expression-visitor
                         svm-expr-visitor--ints
                         (visited-ast-node- (ast-ex-if- (make-ast-info) (ast-at-int- (make-ast-info) 1)
                                                        (ast-at-int- (make-ast-info) 2)
                                                        (ast-at-int- (make-ast-info) 3)
                                                        #f)
                                            (svm-expr-visitor--ints-state- (list)))))
                       svm-expr-visitor--ints-state-))
                '(3 2 1)))

(define (svm-generate--function (def : ast-ex-fun-def-)) : (Immutable-Vectorof Byte)
  ;; TODO: currently just a dummy implementation
  (cast (vector->immutable-vector (vector-append (svm-generate (ast-ex-fun-def--body def)) (vector-immutable RET))) (Immutable-Vectorof Byte))
  ;; (vector-immutable 129 156 129 41 128 129 40 42 35)
  )

(define (svm-generate--fun-call (call : ast-ex-fun-call-)) : (Immutable-Vectorof Byte)
  ;; open: tail call, user function call, runtime function call, complete list of vm internal function calls
  (cast
   (vector->immutable-vector
    (apply vector-append
           (append (map svm-generate (reverse (ast-ex-fun-call--parameters call)))
                   (list (case (ast-ex-fun-call--fun call)
                           [(nil?) (vector-immutable NIL?)]
                           [(car) (vector-immutable CAR)]
                           [(cdr) (vector-immutable CDR)]
                           [(cons) (vector-immutable CONS)]
                           [(reverse) (vector-immutable TAIL_CALL)]
                           [else (raise-user-error (format "unknown function \"~a\""  (ast-ex-fun-call--fun call)))])))))
   (Immutable-Vectorof Byte)))

(module+ test #| svm-generate--fun-call |#
  (check-equal?
   (svm-generate--fun-call
    (ast-ex-fun-call- (make-ast-info) 'nil? (list (ast-at-int- (make-ast-info) 10) (ast-at-int- (make-ast-info) 20))))
   (vector-immutable PUSH_INT (low-byte 20) (high-byte 20)
                     PUSH_INT (low-byte 10) (high-byte 10)
                     NIL?)))

(define (svm-generate--int (a : ast-at-int-)) : (Immutable-Vectorof Byte)
  (define val (ast-at-int--value a))
  (vector-immutable PUSH_INT (low-byte val) (high-byte val)))

(module+ test #| svm-generate--int |#
  (check-equal? (svm-generate--int (ast-at-int- (make-ast-info) 300))
                (vector-immutable PUSH_INT 44 1)))

(define (svm-generate--if (node : ast-ex-if-)) : (Immutable-Vectorof Byte)
  ;; open: optimize for sNIL?-RET-PARAMc
  (define cond-code (svm-generate (ast-ex-if--condition node)))
  (define then-code (svm-generate (ast-ex-if--then node)))
  (define else-code (svm-generate (ast-ex-if--else node)))
  (define first-block (if (ast-ex-if--negated node) then-code else-code))
  (define second-block (if (ast-ex-if--negated node) else-code then-code))
  (cast (vector->immutable-vector
         (vector-append cond-code
                        (vector-immutable BRA (+ 2 #|len of goto @end of first block|# (vector-length first-block)))
                        first-block
                        (vector-immutable GOTO (vector-length second-block))
                        second-block))
        (Immutable-Vectorof Byte)))

(module+ test #| svm-generate--if |#
  (check-equal?
   (svm-generate--if
    (ast-ex-if- (make-ast-info)
                (ast-ex-fun-call- (make-ast-info) 'nil? (list (ast-at-int- (make-ast-info) 10) (ast-at-int- (make-ast-info) 20)))
                (ast-at-int- (make-ast-info) 100)
                (ast-at-int- (make-ast-info) 200)
                #f))

   (vector-immutable PUSH_INT (low-byte 20) (high-byte 20)
                     PUSH_INT (low-byte 10) (high-byte 10)
                     NIL?
                     BRA 5
                     PUSH_INT (low-byte 200) (high-byte 200)
                     GOTO 3
                     PUSH_INT (low-byte 100) (high-byte 100))))

(define (svm-generate (node : ast-node-)) : (Immutable-Vectorof Byte)
  (cond
    [(ast-ex-fun-def-? node) (svm-generate--function node)]
    [(ast-at-id-? node) (svm-generate--id node)]
    [(ast-ex-if-? node) (svm-generate--if node)]
    [(ast-ex-fun-call-? node) (svm-generate--fun-call node)]
    [(ast-at-int-? node) (svm-generate--int node)]
    [else (raise-user-error (format "unknown node type ~a for generate" node))]))

(define (svm-compile (node : ast-node-)) : ast-node-
  (~>>
   node
   (svm-resolve-ids _ (hash) 0)))

(module+ test #| generate code for simple reverse function |#
  (check-equal?
   (svm-generate
    (svm-compile
     (m-fun-def (reverse (a-list cell*) (b-list cell* '()) -> cell*
                         "reverse a-list, consing it into b-list")
                (if (nil? a-list)
                    b-list
                    (reverse (cdr a-list) (cons (car a-list) b-list))))))
   (vector-immutable (sPUSH_PARAMc 0)     ;; a-list
                     NIL?
                     BRA 9
                     (sPUSH_PARAMc 1)
                     (sPUSH_PARAMc 0)
                     CAR
                     CONS
                     (sPUSH_PARAMc 0)
                     CDR
                     TAIL_CALL
                     GOTO 1
                     (sPUSH_PARAMc 1)
                     RET)
   "optimization should reduce bytecode to this")

  ;; (skip "optimizing is not implemented yet"
  ;;  (check-equal?
  ;;   (svm-generate
  ;;    (svm-compile
  ;;     (m-fun-def (reverse (a-list cell*) (b-list cell* '()) -> cell*
  ;;                         "reverse a-list, consing it into b-list")
  ;;                (if (nil? a-list)
  ;;                    b-list
  ;;                    (reverse (cdr a-list) (cons (car a-list) b-list))))))
  ;;   (vector-immutable (sPUSH_PARAMc 0)     ;; a-list
  ;;                     (sNIL?-RET-PARAMc 1) ;; if a-list is nil, return b-list
  ;;                     (sPUSH_PARAMc 0)
  ;;                     CAR
  ;;                     (sPUSH_PARAMc 1)
  ;;                     CONS                 ;; (cons (car a-list) b-list)
  ;;                     (sPUSH_PARAMc 0)
  ;;                     CDR                  ;; (cdr a-list)
  ;;                     TAIL_CALL)           ;; write two stack values into param0 and 1 and jump to function start
  ;;   "optimization should reduce bytecode to this"))
  )