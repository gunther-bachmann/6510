#lang racket

#|

 interpreter for mil ast command structure

 |#

(require "./mil-structures.rkt")
(require (only-in threading ~>>))
(require (only-in "../6510-utils.rkt" byte two-complement-of))
(provide interpret
         module-ctx
         (struct-out interpreter-ctx))

(module+ test
    (require "../6510-test-utils.rkt"))

(define atom-types '(uint8 string char bool))

(define atom-functions '(+ - display cdr car quote cons > < eq? not zero?))

(define special-forms '(let     ;; mil-elt
                        if      ;; mil-if
                        def     ;; mil-definition
                        and       ;; mil-and
                        or       ;; mil-or
                        progn   ;; mil-progn
                        list))  ;; mil-list

;; provide the type expression for the given expression
;; (define/contract (type-of expr)
;;   (-> mil-expression? mil-expression?)
;;   expr)

;; interpretation context
(struct interpreter-ctx
  (symbols)
  #:transparent
  #:guard (struct-guard/c hash?))

;; resolve SYM in the given CTXS
(define/contract (resolve sym ctxs)
  (-> mil-symbol? (listof interpreter-ctx?) mil-expression?)
  (when (empty? ctxs)
    (raise-user-error "symbol not found ~a" (mil-symbol-value sym)))

  (define result (hash-ref (interpreter-ctx-symbols (car ctxs)) (mil-symbol-value sym) #f))
  (or result 
     (resolve sym (cdr ctxs))))

(module+ test #| resolve |#
  (check-exn exn:fail? (lambda () (resolve (mil-symbol 'a) (list (interpreter-ctx (hash))))))
  (check-equal? (resolve (mil-symbol 'a) (list (interpreter-ctx (hash 'a (mil-symbol 'a-found)))))
                (mil-symbol 'a-found))
  (check-equal? (resolve (mil-symbol 'a) (list (interpreter-ctx (hash)) (interpreter-ctx (hash 'a (mil-symbol 'a-found)))))
                (mil-symbol 'a-found)))

(define/contract (unbox-mil-atomic-value value)
  (-> mil-atomic-value? (or/c string? byte? char? boolean?))
  (cond ((mil-string? value) (mil-string-value value))
        ((mil-uint8? value) (mil-uint8-value value))
        ((mil-char? value) (mil-char-value value))
        ((mil-bool? value) (mil-bool-value value))
        (#t (raise-user-error "unknown atomic value ~a" value))))

;; sum up all uint params
(define/contract (mil-+ params ctxs)
  (-> (listof mil-uint8?) (listof interpreter-ctx?) mil-uint8?)
  (mil-uint8 (byte (foldr (lambda (val acc) (+ acc (mil-uint8-value val))) 0 params))))

(module+ test #| mil-+ |#
    (check-equal? (mil-+ (list (mil-uint8 1) (mil-uint8 2)) (list))
                (mil-uint8 3)))

;; subtract from first parameter, or negative number
;; convert to two-complement if negative
(define/contract (mil-- params ctxs)
  (-> (listof mil-uint8?) (listof interpreter-ctx?) mil-uint8?)
  (define p1 (car params))
  (define rest (cdr params))
  (define result
    (cond ((empty? rest)
           (- 0 (mil-uint8-value p1)))
          (#t
           (foldr (lambda (val acc) (- acc (mil-uint8-value val))) (mil-uint8-value p1) rest))))
  (mil-uint8
   (if (< result 0)
       (two-complement-of result)
       (byte result))))

(module+ test #| mil-- |#
  (check-equal? (mil-- (list (mil-uint8 2) (mil-uint8 1)) (list))
                (mil-uint8 1))
  (check-equal? (mil-- (list (mil-uint8 2)) (list))
                (mil-uint8 (two-complement-of -2)))
  (check-equal? (mil-- (list (mil-uint8 2) (mil-uint8 3)) (list))
                (mil-uint8 (two-complement-of -1))))

;; printout some text (and variables)
;; the first parameter is a format string that
;; may contain ~a and must be followed by an equal 
;; number of atomic values to print
(define/contract (mil-display params ctxs)
  (-> (listof mil-expression?) (listof interpreter-ctx?) mil-void?)
  (define format (mil-string-value (car params)))
  (define values (map unbox-mil-atomic-value (cdr params)))
  (apply fprintf (cons (current-output-port) (cons format values)))
  (mil-void))

;; tail of a list
;; fails on empty lists
(define/contract (mil-cdr params ctxs)
  (-> (list/c (or/c mil-list? mil-cell?)) (listof interpreter-ctx?) mil-expression?)
  (if (empty? params)
      (raise-user-error "cdr w/o parameter")
      (cond ((mil-list? (car params))
             (mil-list (cdr (mil-list-elements (car params)))))
            ((mil-cell? (car params))
             (mil-cell-tail (car params)))
            (#t (raise-user-error "cdr works on list only ~a" params)))))

(module+ test #| mil-cdr |#
  (check-equal? (mil-cdr (list (mil-l (mil-uint8 2) (mil-uint8 3))) (list))
                (mil-l (mil-uint8 3)))
  (check-equal? (mil-cdr (list (mil-cell (mil-uint8 2) (mil-uint8 3))) (list))
                (mil-uint8 3)))

;; head of a list
;; fails on empty lists
(define/contract (mil-car params ctxs)
  (-> (list/c (or/c mil-list? mil-cell?)) (listof interpreter-ctx?) mil-expression?)
  (if (empty? params)
      (raise-user-error "car w/o parameter")
      (cond ((mil-list? (car params))
             (car (mil-list-elements (car params))))
            ((mil-cell? (car params))
             (mil-cell-head (car params)))
            (#t (raise-user-error "car works on list only ~a" params)))))

(module+ test #| mil-car |#
  (check-equal? (mil-car (list (mil-l (mil-uint8 2) (mil-uint8 3))) (list))
                (mil-uint8 2))
  (check-equal? (mil-car (list (mil-cell (mil-uint8 2) (mil-uint8 3))) (list))
                (mil-uint8 2)))

;; concatenate (as head) a value to a list returning a new list
(define/contract (mil-cons params ctxs)
  (-> (list/c mil-expression? mil-expression?) (listof interpreter-ctx?) (or/c mil-cell? mil-list?))
  (define p1 (car params))
  (define p2 (cadr params))
  (cond ((mil-list? p2)
         (mil-list (cons p1 (mil-list-elements p2))))
        ((mil-cell? p2)
         (mil-list (list p1 p2)))
        (#t (mil-cell p1 p2))))

(module+ test #| mil-cons |#
  (check-equal? (mil-cons (list (mil-uint8 1) (mil-l (mil-uint8 2) (mil-uint8 3))) (list))
                (mil-l (mil-uint8 1) (mil-uint8 2) (mil-uint8 3))))

;; compare two uint8 values
(define/contract (mil-> params ctxs)
  (-> (list/c mil-uint8? mil-uint8?) (listof interpreter-ctx?) mil-bool?)
  (define p1 (car params))
  (define p2 (cadr params))
  (mil-bool (> (mil-uint8-value p1) (mil-uint8-value p2))))

(module+ test #| mil-> |#
  (check-equal? (mil-> (list (mil-uint8 10) (mil-uint8 20)) (list))
                (mil-bool #f))
  (check-equal? (mil-> (list (mil-uint8 20) (mil-uint8 20)) (list))
                (mil-bool #f))
  (check-equal? (mil-> (list (mil-uint8 20) (mil-uint8 10)) (list))
                (mil-bool #t)))

;; compare two uint8 values
(define/contract (mil-< params ctxs)
  (-> (list/c mil-uint8? mil-uint8?) (listof interpreter-ctx?) mil-bool?)
  (define p1 (car params))
  (define p2 (cadr params))
  (mil-bool (< (mil-uint8-value p1) (mil-uint8-value p2))))

(module+ test #| mil-< |#
  (check-equal? (mil-< (list (mil-uint8 10) (mil-uint8 20)) (list))
                (mil-bool #t))
  (check-equal? (mil-< (list (mil-uint8 20) (mil-uint8 20)) (list))
                (mil-bool #f))
  (check-equal? (mil-< (list (mil-uint8 20) (mil-uint8 10)) (list))
                (mil-bool #f)))

;; compare two values (string, uint8, char, bool)
(define/contract (mil-eq? params ctxs)
  (-> (list/c mil-expression? mil-expression?) (listof interpreter-ctx?) mil-bool?)  
  (define p1 (car params))
  (define p2 (cadr params))
  (mil-bool (cond ((mil-uint8? p1)
                   (eq? (mil-uint8-value p1) (mil-uint8-value p2)))
                  ((mil-string? p1)
                   (equal? (mil-string-value p1) (mil-string-value p2)))
                  ((mil-char? p1)
                   (eq? (mil-char-value p1) (mil-char-value p2)))
                  ((mil-bool? p1)
                   (eq? (mil-bool-value p1) (mil-bool-value p2)))
                  (#t (raise-user-error "cannot compare non atomic values ~a ~a" p1 p2)))))

(module+ test #| mil-eq? |#
  (check-equal? (mil-eq? (list (mil-uint8 1) (mil-uint8 2)) (list))
                (mil-bool #f))
  (check-equal? (mil-eq? (list (mil-uint8 1) (mil-uint8 1)) (list))
                  (mil-bool #t)))

;; (try to) apply a user defined function from ctxs
(define/contract (apply-mil-user-function sym reduced-params ctxs)
  (-> mil-symbol? (listof mil-expression?) (listof interpreter-ctx?) mil-expression?)
  (define user-fun (resolve sym ctxs))
  (unless (mil-definition? user-fun)
    (raise-user-error "symbol not a function definition ~a" sym))
  ;; create hash with param names mapped to the
  (define key-value-pairs (map cons (map mil-parameter-id (mil-definition-parameters user-fun)) reduced-params))
  (define new-ctxs (cons (interpreter-ctx (make-hash key-value-pairs)) ctxs))  
  (interpret (mil-definition-body user-fun) new-ctxs))

(module+ test #| apply-mil-user-function |#
  (check-equal? (apply-mil-user-function
                 (mil-symbol 'afn)
                 '()
                 (list (interpreter-ctx (hash 'afn (mil-definition 'afn '() "doc" (mil-uint8 #x80))))))
                (mil-uint8 #x80)))

;; evaluate a SYMbol on PARAMS in CTXS
(define/contract (apply-mil-function sym params ctxs)
  (-> mil-symbol? (listof mil-expression?) (listof interpreter-ctx?) mil-expression?)
  ;; eager eval of parameter
  (define reduced-params (map (lambda (param) (interpret param ctxs)) params))
  (cond ((eq? '+ (mil-symbol-value sym))       (mil-+ reduced-params ctxs))
        ((eq? '- (mil-symbol-value sym))       (mil-- reduced-params ctxs))
        ((eq? 'display (mil-symbol-value sym)) (mil-display reduced-params ctxs))
        ((eq? 'cdr (mil-symbol-value sym))     (mil-cdr reduced-params ctxs))
        ((eq? 'car (mil-symbol-value sym))     (mil-car reduced-params ctxs))
        ((eq? 'cons (mil-symbol-value sym))    (mil-cons reduced-params ctxs))
        ((eq? '> (mil-symbol-value sym))       (mil-> reduced-params ctxs))
        ((eq? '< (mil-symbol-value sym))       (mil-< reduced-params ctxs))
        ((eq? 'eq? (mil-symbol-value sym))     (mil-eq? reduced-params ctxs))
        ((eq? 'not (mil-symbol-value sym))       (mil-bool (not (mil-bool-value (car reduced-params)))))
        ((eq? 'zero? (mil-symbol-value sym))   (mil-bool (zero? (mil-uint8-value (car reduced-params)))))
        (#t (apply-mil-user-function sym reduced-params ctxs))))

(module+ test #| apply-mil-function |#
  (check-equal? (apply-mil-function (mil-symbol 'zero?) (list (mil-uint8 0)) (list))
                (mil-bool #t))
  (check-equal? (apply-mil-function (mil-symbol 'zero?) (list (mil-uint8 1)) (list))
                (mil-bool #f))
  (check-equal? (apply-mil-function (mil-symbol 'not) (list (mil-bool #f)) (list))
                (mil-bool #t))
  (check-equal? (apply-mil-function (mil-symbol 'not) (list (mil-bool #t)) (list))
                (mil-bool #f)))

;; short circuit and function
(define/contract (mil-sf-and expr ctxs)
  (-> mil-and? (listof interpreter-ctx?) mil-bool?)
  (mil-bool
   (foldl (lambda (val acc) (and acc (mil-bool-value (interpret val ctxs))))
          (mil-bool-value (interpret (car (mil-list-elements expr)) ctxs))
          (cdr (mil-list-elements expr)))))

;; short circuit or function
(define/contract (mil-sf-or expr ctxs)
  (-> mil-or? (listof interpreter-ctx?) mil-bool?)
  (mil-bool
   (foldl (lambda (val acc) (or acc (mil-bool-value (interpret val ctxs))))
          (mil-bool-value (interpret (car (mil-list-elements expr)) ctxs))
          (cdr (mil-list-elements expr)))))

;; short circuit if function
(define/contract (mil-sf-if expr ctxs)
  (-> mil-if? (listof interpreter-ctx?) mil-expression?)
  (if (mil-bool-value (interpret (mil-if-predicate expr) ctxs))
      (interpret (mil-if-true-body expr) ctxs)
      (interpret (mil-if-false-body expr) ctxs)))

;; do evaluation of a  mil list
(define/contract (mil-sf-list expr ctxs)
  (-> mil-list? (listof interpreter-ctx?) mil-expression?)
  (apply-mil-function (car (mil-list-elements expr))
                      (cdr (mil-list-elements expr))
                      ctxs))

;; interpret/reduce the given EXPR in this CTXS
(define/contract (interpret expr ctxs)
  (-> mil-expression? (listof interpreter-ctx?) mil-expression?)
  (cond ((mil-atomic-value? expr) expr)
        ((mil-quote? expr)        (mil-quote-quoted expr))
        ((mil-symbol? expr)       (resolve expr ctxs))
        ((mil-progn? expr)        (last (map (lambda (val) (interpret val ctxs)) (mil-list-elements expr))))
        ((mil-and? expr)          (mil-sf-and expr ctxs))
        ((mil-or? expr)           (mil-sf-or expr ctxs))
        ((mil-let? expr)          (interpret (mil-let-body (cons (let-ctx (mil-let-bindings expr) ctxs) ctxs))))
        ((mil-if? expr)           (mil-sf-if expr ctxs))
        ((mil-list? expr)         (mil-sf-list expr ctxs))
        (#t (raise-user-error "cannot interpret expr ~a" expr))))

(module+ test #| interpret |#
  (check-exn exn:fail? (lambda () (interpret (mil-list (list 'abc)) (list (interpreter-ctx (hash))))))
  (check-equal? (interpret (mil-progn (list (mil-bool #t) (mil-string "some") (mil-uint8 #x80))) (list (interpreter-ctx (hash))))
                (mil-uint8 #x80))
  (check-equal? (interpret (mil-and (list (mil-bool #t) (mil-bool #f))) (list (interpreter-ctx (hash))))
                (mil-bool #f))
  (check-equal? (interpret (mil-and (list (mil-bool #f) (mil-string "some"))) (list (interpreter-ctx (hash))))
                (mil-bool #f))
  (check-equal? (interpret (mil-or (list (mil-bool #t) (mil-string "some"))) (list (interpreter-ctx (hash))))
                (mil-bool #t))
  (check-equal? (interpret (mil-symbol 'a) (list (interpreter-ctx (hash 'a (mil-uint8 #x80)))))
                (mil-uint8 #x80))
  (check-equal? (interpret (mil-list (list (mil-symbol 'a))) (list (interpreter-ctx (hash 'a (mil-definition 'a '() "doc" (mil-uint8 #x80))))))
                (mil-uint8 #x80)))

;; construct a new context for the given let BINDINGS
(define/contract (let-ctx bindings ctxs)
  (-> (listof mil-let-binding?) (listof interpreter-ctx?) interpreter-ctx?)
  (~>>
   (map cons (map mil-let-binding-id bindings)
        (map (lambda (val) (interpret val ctxs))  (map mil-let-binding-expr bindings)))
   (make-hash)
   (interpreter-ctx)))

(module+ test #| let-ctx |#
  (check-equal? (resolve (mil-symbol 'a) (list (let-ctx (list (mil-let-binding 'a (mil-uint8 #x80) )) (list (interpreter-ctx (hash))))))
                (mil-uint8 #x80)))

;; construct a new context for the given MODULE
;; processes only definitions currently
(define/contract (module-ctx module)
  (-> mil-module? interpreter-ctx?)
  (~>>
   (map cons
        (map mil-definition-id (mil-module-definitions module))
        (mil-module-definitions module))
   (make-hash)
   (interpreter-ctx)))

(module+ test #| module-ctx |#
  (check-equal? (resolve (mil-symbol 'a)
                         (list (module-ctx (mil-module 'my-module
                                                       (list)
                                                       (list)
                                                       (list (mil-definition 'a '() "doc" (mil-uint8 #x80)))
                                                       (list)
                                                       (list)))))
                (mil-definition 'a '() "doc" (mil-uint8 #x80)))
    (check-equal? (resolve (mil-symbol 'b)
                           (list (module-ctx (mil-module 'my-module
                                                         (list)
                                                         (list)
                                                         (list (mil-definition 'a '() "doc" (mil-uint8 #x80))
                                                               (mil-definition 'b '() "doc" (mil-uint8 #x20)))
                                                         (list)
                                                         (list)))))
                (mil-definition 'b '() "doc" (mil-uint8 #x20))))
