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

(define atom-types '(uint8 string char))

(define atom-functions '(+ - display cdr car cons quote identity > < eq? and or not zero?))

(define special-forms '(let if def list)) ;; actually modelled by special mil structures

;; provide the type expression for the given expression
;; (define/contract (type-of expr)
;;   (-> mil-expression? mil-expression?)
;;   expr)

;; interpretation context
(struct interpreter-ctx
  (symbols)
  #:transparent
  #:guard (struct-guard/c hash?))

(define/contract (resolve sym ctxs)
  (-> mil-symbol? (listof interpreter-ctx?) mil-expression?)
  (when (empty? ctxs)
    (raise-user-error "symbol not found"))
  (define result (hash-ref (interpreter-ctx-symbols (car ctxs)) (mil-symbol-value sym) #f))
  (if result
      result
      (resolve sym (cdr ctxs))))

(module+ test #| resolve |#
  (check-exn exn:fail? (lambda () (resolve (mil-symbol 'a) (list (interpreter-ctx (hash))))))
  (check-equal? (resolve (mil-symbol 'a) (list (interpreter-ctx (hash 'a (mil-symbol 'a-found)))))
                (mil-symbol 'a-found))
  (check-equal? (resolve (mil-symbol 'a) (list (interpreter-ctx (hash)) (interpreter-ctx (hash 'a (mil-symbol 'a-found)))))
                (mil-symbol 'a-found)))

(define/contract (unbox-mil-atomic-value value)
  (-> mil-atomic-value? any/c)
  (cond ((mil-string? value) (mil-string-value value))
        ((mil-uint8? value) (mil-uint8-value value))
        ((mil-char? value) (mil-char-value value))
        (#t (raise-user-error "unknown atomic value"))))

(define/contract (apply-mil-function sym params ctxs)
  (-> mil-symbol? (listof mil-expression?) (listof interpreter-ctx?) mil-expression?)
  ;; eager eval of parameter
  (define reduced-params (map (lambda (param) (interpret param ctxs)) params))
  (cond ((eq? '+ (mil-symbol-value sym)) (mil-uint8 (byte (foldr (lambda (val acc) (+ acc (mil-uint8-value val))) 0 reduced-params))))
        ((eq? '- (mil-symbol-value sym))
         (define p1 (car reduced-params))
         (define rest (cdr reduced-params))
         (define result
           (cond ((empty? rest)
                  (- 0 (mil-uint8-value p1)))
                 (#t 
                  (foldr (lambda (val acc) (- acc (mil-uint8-value val))) (mil-uint8-value p1) rest))))
         (if (< result 0)
             (mil-uint8 (two-complement-of result))
             (mil-uint8 (byte result))))
        ((eq? 'display (mil-symbol-value sym))
         (define format (mil-string-value (car reduced-params)))
         (define values (map unbox-mil-atomic-value (cdr reduced-params)))
         (apply fprintf (cons (current-output-port) (cons format values)))
         (mil-void))
        (#t (apply-mil-user-function sym reduced-params ctxs))))


(define/contract (apply-mil-user-function sym reduced-params ctxs)
  (-> mil-symbol? (listof mil-expression?) (listof interpreter-ctx?) mil-expression?)
  (define user-fun (resolve sym ctxs))
  (unless (mil-definition? user-fun)
    (raise-user-error "symbol not a function definition"))
  ;; create hash with param names mapped to the
  (define key-value-pairs (map cons (map mil-parameter-id (mil-definition-parameters user-fun)) reduced-params))
  (define new-ctxs (cons (interpreter-ctx (make-hash key-value-pairs)) ctxs))  
  (interpret (mil-definition-body user-fun) new-ctxs))

(module+ test
  (check-equal? (apply-mil-function (mil-symbol 'afn)
                                    '()
                                    (list (interpreter-ctx (hash 'afn (mil-definition 'afn '() "doc" (mil-uint8 #x80))))))
                (mil-uint8 #x80))
  (check-equal? (apply-mil-function (mil-symbol '+)
                                    (list (mil-uint8 1) (mil-uint8 2))
                                    (list))
                (mil-uint8 3))
  (check-equal? (apply-mil-function (mil-symbol '-)
                                    (list (mil-uint8 2) (mil-uint8 1))
                                    (list))
                (mil-uint8 1))
  (check-equal? (apply-mil-function (mil-symbol '-)
                                    (list (mil-uint8 2))
                                    (list))
                (mil-uint8 (two-complement-of -2)))
  (check-equal? (apply-mil-function (mil-symbol '-)
                                    (list (mil-uint8 2) (mil-uint8 3))
                                    (list))
                (mil-uint8 (two-complement-of -1))))

;; interpret/reduce the given expr in this context
(define/contract (interpret expr ctxs)
  (-> mil-expression? (listof interpreter-ctx?) mil-expression?)
  (cond ((mil-symbol? expr)    ;; resolve id
         (resolve expr ctxs))
        ((mil-atomic-value? expr)     ;; return the value
         expr)
        ((mil-list? expr)      ;; function call (currently no heeding of special symbols
         (apply-mil-function (car (mil-list-elements expr)) (cdr (mil-list-elements expr)) ctxs))
        (#t (raise-user-error "cannot interpret expr"))))

(module+ test #| interpret |#
  (check-exn exn:fail? (lambda () (interpret (mil-list (list 'abc)) (list (interpreter-ctx (hash))))))
  (check-equal? (interpret (mil-symbol 'a) (list (interpreter-ctx (hash 'a (mil-uint8 #x80)))))
                (mil-uint8 #x80))
  (check-equal? (interpret (mil-list (list (mil-symbol 'a))) (list (interpreter-ctx (hash 'a (mil-definition 'a '() "doc" (mil-uint8 #x80))))))
                (mil-uint8 #x80)))

(define/contract (module-ctx module)
  (-> mil-module? interpreter-ctx?)
  (~>>
   (map cons (map mil-definition-id (mil-module-definitions module))
        (mil-module-definitions module))
   (make-hash)
   (interpreter-ctx)))

(module+ test #| module-ctx |#
  (check-equal? (resolve (mil-symbol 'a) (list (module-ctx (mil-module 'my-module
                                                                       (list)
                                                                       (list)
                                                                       (list (mil-definition 'a '() "doc" (mil-uint8 #x80)))
                                                                       (list)
                                                                       (list)))))
                (mil-definition 'a '() "doc" (mil-uint8 #x80)))
    (check-equal? (resolve (mil-symbol 'b) (list (module-ctx (mil-module 'my-module
                                                                       (list)
                                                                       (list)
                                                                       (list (mil-definition 'a '() "doc" (mil-uint8 #x80))
                                                                             (mil-definition 'b '() "doc" (mil-uint8 #x20)))
                                                                       (list)
                                                                       (list)))))
                (mil-definition 'b '() "doc" (mil-uint8 #x20))))
