#lang racket

(require (for-syntax (only-in racket/list second empty? first)))
(require  (for-syntax "6510-command-utils.rkt"))
(require (for-syntax "6510-syntax-utils.rkt"))

(require  "6510-command-utils.rkt")

(require (for-syntax (rename-in "6510-syntax-utils.rkt" (one-arg-adr-modes-accumulator? accumulator?))))

(module+ test
  (require rackunit))

;; (define-for-syntax (construct-example opcode argument qargument description within-non-racket-syntax)
;;   (if within-non-racket-syntax
;;       (string-append "  " opcode " " argument "  ; " description "\n")
;;       (string-append "  (" opcode " " qargument  ") # " description "\n")))

;; (define-for-syntax (string-append-l list-of-strings)
;;   (foldr string-append "" (filter (lambda (elt) (not (void? elt))) list-of-strings)))

;; (define-for-syntax (error-string/single adr-modes opcode-string within-non-racket-syntax)
;;   (string-append-l
;;    (list
;;     "invalid syntax.\n"
;;     (when (accumulator? adr-modes)
;;       (construct-example opcode-string "A" "A" "accumulator mode" within-non-racket-syntax))

;;     "got: ")))

(define-for-syntax (collect-syntax-result list-of-syntax-objects)
  (foldl discard-void-syntax-object #'()  list-of-syntax-objects))

(define-for-syntax (opcode-with-addressing/single adr-modes opcode op stx non-racket-syn org-string)
  (with-syntax ([accres (when (accumulator? adr-modes) (accumulator-mode opcode op))])
    #'accres
    ;; (let* ([res #'accres;; (collect-syntax-result (list #'accres))
    ;;             ]
    ;;        ;; [opcode-string (symbol->string (syntax->datum opcode))]
    ;;        [error-string "";; (error-string/single adr-modes opcode-string non-racket-syn)
    ;;                      ])
    ;;   (if (empty? (syntax->datum res))
    ;;       (raise-syntax-error error-string opcode non-racket-syn org-string stx)
    ;;       res))
    ))

(define-for-syntax (list->one-arg-adr-modes option-list)
  (one-arg-adr-modes #f ;;(member 'relative option-list)
                     (member 'accumulator option-list)
                     #f ;;(member 'immediate option-list)
                     #f ;;(member 'zero-page option-list)
                     #f ;; (member 'absolute option-list)
                     ))

(define-syntax-rule (opcode-with-addressing opcode option-list)
  (define-syntax (opcode stx)
    (syntax-case stx ()
      [(opcode op)
       ;; (with-syntax (                    ;; [symbol-acc (symbol-append opcode '_acc)]
       ;;                                   )
       ;;     #'(ASL_acc)
       ;;   ;; (when (equal? 'A (syntax->datum #'op))
       ;;   ;;   #'(symbol-acc))
       ;;   )
       #'(ASL_acc)
       ;; ([res (accumulator-mode #'ASL #'A)])
       ;;   ;; res
       ;;   #'(ASL_acc)
       ;;   )
       ;; (accumulator-mode opcode #'A)
       ;; (opcode-with-addressing/single (list->one-arg-adr-modes option-list) #'opcode #'op stx #f #'"")
       ])))

(define-syntax (define-opcode-functions/macro stx)
  (syntax-case stx ()
    [(_ op option-list bytecode-list mode ext param body)
     (let* ([option-list-clean (second (syntax->datum #'option-list))]
            [bytecode-list-clean (second (syntax->datum #'bytecode-list))]
            [option-list-length (length option-list-clean)]
            [mode? (member `,(syntax->datum #'mode) option-list-clean)]
            [byte-code (when mode? (list-ref bytecode-list-clean (- option-list-length (length mode?))))])
       (with-syntax ([func-name (datum->syntax #'op (symbol-append #'op (syntax->datum #'ext)))]
                     [byte-code-sy byte-code])
         (with-syntax ([op-function
                        (when mode?
                          (if (equal? 'empty (syntax->datum #'param))
                              #'(define (func-name) (map (lambda (elt) (if (equal? elt 'byte-code-place) byte-code-sy elt)) body))
                              #'(define (func-name param) (map (lambda (elt) (if (equal? elt 'byte-code-place) byte-code-sy elt)) body))))])
           #'op-function)))]))


;; (define-syntax (ASL stx)
;;   (syntax-case stx ()
;;     [(op oprd)
;;      #'(ASL_acc)]))

(define-syntax (define-opcode-functions stx)
  (syntax-case stx ()
    [(_ op option-list bytecode-list)
     #'(begin
         (define-opcode-functions/macro op option-list bytecode-list accumulator "_acc" empty (list ''opcode 'byte-code-place))
         (opcode-with-addressing op option-list)
         )]))



(define-opcode-functions ASL
  '(accumulator )
  '(#x0a       ))


(module+ test
  (check-equal? (ASL_acc)
                '('opcode 10))
  (check-equal? (ASL A)
                '('opcode 10))
  )
