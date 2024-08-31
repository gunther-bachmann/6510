#lang typed/racket/base

#|review: ignore|#
#|  review does show several false positives |#

#|

  AST for minimal lisp (should actually be independent of VM)

 |#


(provide (struct-out ast-node-)
         (struct-out ast-info-)
         make-ast-info
         (struct-out register-ref-)
         (struct-out ast-td-simple-)
         (struct-out ast-td-complex-)
         (struct-out ast-param-def-)
         (struct-out ast-pa-defaulted-def-)
         (struct-out ast-expression-)
         (struct-out ast-ex-atomic-)
         (struct-out ast-at-nil-)
         (struct-out ast-at-bool-)
         (struct-out ast-at-byte-)
         (struct-out ast-at-char-)
         (struct-out ast-at-int-)
         (struct-out ast-at-string-)
         (struct-out ast-at-id-)
         (struct-out ast-ex-list-)
         (struct-out ast-ex-fun-call-)
         (struct-out ast-ex-quoted-list-)
         (struct-out ast-ex-cond-clause-)
         (struct-out ast-ex-cond-)
         (struct-out ast-ex-if-)
         (struct-out ast-ex-def-)
         (struct-out ast-ex-fun-def-)
         (struct-out ast-ex-with-local-)
         (struct-out ast-ex-with-)
         (struct-out ast-ex-value-def-)
         Register)

;; ast-node-    :: ast=info-
;;    +----- ast-type-def-
;;    |         +----- ast-td-simple-   :: id
;;    |         +----- ast-td-complex-  :: id x params
;;    +----- ast-param-def    :: id x type
;;    |         +----- ast-pa-defaulted-def-    :: default
;;    +----- ast-expression-
;;    |         +----- ast-ex-fun-def-   :: id x params x defaulted-params x return-type x docs x body
;;    |         +----- ast-ex-value-def- :: id x doc x body
;;    |         +----- ast-ex-cond-      :: cond-clauses x else
;;    |         +----- ast-ex-if-
;;    |         +----- ast-ex-with-      :: with-locals
;;    |         +----- ast-ex-list- ??
;;    |         |         +----- ast-ex-def-         :: id x value
;;    |         |         +----- ast-ex-quoted-list- :: values
;;    |         +----- ast-ex-fun-call-  :: id x params
;;    |         +----- ast-ex-atomic-
;;    |                   +----- ast-at-int-    :: int-value
;;    |                   +----- ast-at-id-     :: id
;;    |                   +----- ast-at-bool-   :: bool-value
;;    |                   +----- ast-at-string- :: string-value
;;    +----- ast-ex-cond-clause-  :: condition x body
;;    +----- ast-ex-with-local-   :: id x value

(define-type Register (U 'Param 'Global 'Local))

(struct register-ref-
  ((register : Register)
   (index    : Nonnegative-Integer))
  #:transparent)

(struct ast-node-
  ((info : ast-info-))
  #:transparent)

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
   (body      : ast-expression-))
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
   (body        : ast-expression-))
  #:transparent)

(struct ast-ex-value-def- ast-expression-
  ((id          : Symbol)
   (type        : ast-type-def-)
   (description : String)
   (body        : ast-expression-))
  #:transparent)

(struct ast-ex-with-local- ast-node-
  ((id : Symbol)
   (type : ast-type-def-)
   (value : ast-expression-))
  #:transparent)

(struct ast-ex-with- ast-expression-
  ((locals : (Listof ast-ex-with-local-))
   (body   : ast-expression-))
  #:transparent)
