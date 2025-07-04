#lang racket

#|

 provide methods to do the assembly auf ast-commands holistically, resolving all to
 translate a program to raw-bytes, all resolved

 |#

(require (only-in racket/hash hash-union))
(require "../6510.rkt")
(require "6510-calc-opcode-facades.rkt")
(require "6510-resolver.rkt")
(require "6510-relocator.rkt")
(require "6510-constants.rkt")
(require (only-in "6510-command.rkt"
                  ast-command?
                  ast-org-command?
                  ast-org-command-org
                  ast-org-align-command?
                  ast-org-align-command-org-alignment
                  ast-label-def-cmd
                  ast-opcode-cmd))
(require (only-in "../6510-utils.rkt" word/c byte/c low-byte high-byte))

(provide assemble
         assemble-to-code-list
         translate-code-list-for-basic-loader
         org-for-code-seq
         new-assemble-to-code-list
         hash-constants
         (struct-out assembly-code-list))

(module+ test
  (require "../6510.rkt")
  (require "../6510-test-utils.rkt"))

(define/contract (split-into-code-list code (code-list '()))
  (->* ((listof ast-command?)) ((listof (listof ast-command?))) (listof (listof ast-command?)))
  (if (empty? code)
      (reverse code-list)
      (if (or (ast-org-command? (car code))
             (ast-org-align-command? (car code)))
          (split-into-code-list (cdr code) (cons (list (car code)) code-list))
          (split-into-code-list (cdr code) (cons (append (car code-list) (list (car code))) (cdr code-list))))))

(module+ test #| split-into-code-list |#
  (check-equal? (split-into-code-list (list (ast-org-command '() #xc000)
                                            (ast-command '())
                                            (ast-command '())
                                            (ast-org-command '() #xc008)
                                            (ast-command '())
                                            (ast-org-align-command '() #x10)
                                            (ast-command '())))
                (list (list (ast-org-command '() #xc000)
                                            (ast-command '())
                                            (ast-command '()))
                      (list (ast-org-command '() #xc008)
                                            (ast-command '()))
                      (list (ast-org-align-command '() #x10)
                                            (ast-command '())))))

(define/contract (org-for-code-seq code-seq (corg #xFFFF))
  (->* ((listof ast-command?)) (word/c) word/c)
  (cond [(ast-org-command? (car code-seq))
         (ast-org-command-org (car code-seq))]
        [(ast-org-align-command? (car code-seq))
         (define al (ast-org-align-command-org-alignment (car code-seq)))
         (if (= corg #xffff)
             #xFFFF
             (+ corg (- al (bitwise-and corg (sub1 al)))))]
        [else
         (raise-user-error (format "org-command for this code sequence needs to be at the head, found ~a" (car code-seq)))]))

(module+ test #| org-for-code-seq |#
  (check-equal? (org-for-code-seq (list (ast-org-command '() #xc000)
                                        (ast-command '())
                                        (ast-command '())))
                #xc000)
  (check-equal? (org-for-code-seq (list (ast-org-align-command '() #x100)
                                        (ast-command '())
                                        (ast-command '()))
                                  #xffff)
                #xffff)
  (check-equal? (org-for-code-seq (list (ast-org-align-command '() #x100)
                                        (ast-command '())
                                        (ast-command '()))
                                  #xc223)
                #xc300)
  (check-equal? (org-for-code-seq (list (ast-org-align-command '() #x100)
                                        (ast-command '())
                                        (ast-command '())))
                #xFFFF))


;; translate a code list to a codesequence with loader that puts each segment into the right memory location upon loading into the c64
;; TODO: put into 6510-prg-generator
(define/contract (translate-code-list-for-basic-loader code-list)
  (-> pair? (listof byte/c))
  (define code-list-wlen ;; (list len+offset org bytes)
    (cdr (reverse (foldl (lambda (el acc)
                           (cons `(,(+ (length (cdr el)) (caar acc)) ,(car el) ,(cdr el) ) acc))
                         '((0))
                         code-list))))
  (define assembled-prefix-w-info
    (assemble-with-info
     2064
     (list        (JMP NEXT)
                  (byte-const CPY_TARGET $FD) ;; zero page FD FE
                  (byte-const CPY_SOURCE $FB) ;; zero page FB FC
           (label COPY_LEN)
                  (word $0000)

           (label COPY)
                  (STA COPY_LEN)
                  (STX COPY_LEN+1)
                  (LDY !$00)
           (label INNER_LOOP)
                  (LDA (CPY_SOURCE),y)
                  (STA (CPY_TARGET),y)
                  (INY)
                  (DEC COPY_LEN)
                  (BNE INNER_LOOP)
                  (INC CPY_TARGET+1)
                  (INC CPY_SOURCE+1)
                  (DEC COPY_LEN+1)
                  (BPL INNER_LOOP)
                  (RTS)

           (label NEXT))))

  (define payloads (map cdr code-list))

  (define list-of-copy-routines
    (map
     (lambda (code)
       (assemble-with-info
        0 ;; offset is irrelevant here
        (list
                (LDA-immediate (low-byte (second code)))
                (STA CPY_TARGET)
                (LDA-immediate (high-byte (second code)))
                (STA CPY_TARGET+1)
         (label PATCH_LOW)
                (LDA !$00) ;; PATCH
                (STA CPY_SOURCE)
         (label PATCH_HIGH)
                (LDA !$00) ;; PATCH
                (STA CPY_SOURCE+1)
                (LDA-immediate (low-byte (length (third code))))   ;; len of code to copy
                (LDX-immediate (high-byte (length (third code))))
                (JSR COPY))
        (first assembled-prefix-w-info)
        (third assembled-prefix-w-info)
        (fourth assembled-prefix-w-info)))
     code-list-wlen))

  (define offset-initial-jump
    (+ 2064 (length (append (second assembled-prefix-w-info)
                            (flatten (map second list-of-copy-routines))))))

  (define initial-jump
    (assemble
     offset-initial-jump
     (append
      (list
              (LDA-immediate (low-byte (caar code-list)))
              (STA INITIAL_JUMP+1)
              (LDA-immediate (high-byte (caar code-list)))
              (STA INITIAL_JUMP+2)
       (label INITIAL_JUMP)
              (JMP $0000)

       (label PAYLOAD)))))

  (define payload-start (+ offset-initial-jump (length initial-jump))) ;; this is added to code-list-w-len

  (define patched-list-of-copy-routines
    (map (lambda (copy-routine code-wlen)
           (define patch-low (hash-ref (first copy-routine) "PATCH_LOW"))
           (define patch-high (hash-ref (first copy-routine) "PATCH_HIGH"))
           (define patch-value (+ payload-start (- (first code-wlen) (length (third code-wlen)))))
           (append
            (take (second copy-routine) (+ 1 patch-low))
            (list (low-byte patch-value))
            (take (drop (second copy-routine) (+ 2 patch-low)) (- patch-high patch-low 1))
            (list (high-byte patch-value))
            (drop (second copy-routine) (+ 2 patch-high))))
         list-of-copy-routines
         code-list-wlen))

  (append (second assembled-prefix-w-info)
          (flatten patched-list-of-copy-routines)
          initial-jump
          (flatten payloads)))

(struct assembly-code-list
  (org-code-sequences
   labels)
  #:transparent
  #:guard (struct-guard/c (listof pair?) hash?))

;; get all constants from the command list and return a hash label->value (byte or word)
(define/contract (hash-constants program)
  (-> (listof ast-command?) (hash/c string? word/c))
  (apply hash
           (flatten
            (map (lambda (c-instr)
                   (cond [(ast-const-byte-cmd? c-instr)
                          (list (ast-const-byte-cmd-label c-instr)
                                (ast-const-byte-cmd-byte c-instr))]
                         [(ast-const-word-cmd? c-instr)
                          (list (ast-const-word-cmd-label c-instr)
                                (ast-const-word-cmd-word c-instr))]
                         [else '()]))
                 (constant-instructions program)))))


;; assemble the given program into a list of org-code-sequences and
;; defined labels (and constants)
;; one may pass additional labels (and constants) into the assembly run
(define/contract (new-assemble-to-code-list program (labels (hash)))
  (->* [(listof ast-command?)] [(hash/c string? nonnegative-integer?)] assembly-code-list?)
  (hash)
  (define constants (hash-constants program))
  (define program-p1 (->resolved-decisions (label-instructions program) program))
  (define lsoffsets (label-string-offsets (org-for-code-seq program-p1) program-p1))
  (define combined-offsets (hash-union lsoffsets labels #:combine (lambda (a _b) a)))
  (define program-p2 (->resolve-labels (org-for-code-seq program-p1) combined-offsets program-p1 '()))
  (define combined-constants (hash-union constants labels #:combine (lambda (a _b) a)))
  (define program-p3 (resolve-constants combined-constants program-p2))
  (define code-list (split-into-code-list program-p3))
  (define result-wo-align
    (map (lambda (code-seq) `(,(org-for-code-seq code-seq) . ,(resolved-program->bytes (cdr code-seq)))) code-list))
  (define cdr-result-w-align
    (assemble-to-code-list--resolve-org-aligns
     (cdr result-wo-align)
     (car result-wo-align)
     (cdr code-list)
     '()))
  (assembly-code-list
   (cons (car result-wo-align) cdr-result-w-align)
   (hash-union lsoffsets constants)))

;; deprecated, use new-assemble-to-code-list
(define/contract (assemble-to-code-list program)
  (-> (listof ast-command?) pair?)
  (assembly-code-list-org-code-sequences (new-assemble-to-code-list program)))

(define/contract (assemble-to-code-list--resolve-org-aligns result-wo-align prev-result code-list collected-result)
  (-> list? list? list? list? list?)
  (cond
    [(empty? result-wo-align)
     (reverse collected-result)]
    [else
     (define result-seq (car result-wo-align))
     (define code-seq (car code-list))
     (define result
       (if (= #xFFFF (car result-seq))
           `(,(org-for-code-seq
               code-seq
               (+ (car prev-result) (length (cdr prev-result))))
             . ,(resolved-program->bytes (cdr code-seq)))
           result-seq))
     ;; tail call
     (assemble-to-code-list--resolve-org-aligns
      (cdr result-wo-align)
      result
      (cdr code-list)
      (cons result collected-result))]))

(module+ test #| assemble-to-code-list |#
  (check-equal? (assemble-to-code-list (list (ast-org-command '() #xc000)
                                             (ast-opcode-cmd '() '(#x00 #x01))))
                `((#xc000 . (#x00 #x01))))
  (check-equal? (assemble-to-code-list (list (ast-org-command '() #xc000)
                                             (ast-opcode-cmd '() '(#x00 #x01))
                                             (ast-org-command '() #xc020)
                                             (ast-opcode-cmd '() '(#x02 #x03))
                                             ))
                `((#xc000 . (#x00 #x01))
                  (#xc020 . (#x02 #x03))
                  ))
  (check-equal? (assemble-to-code-list (list (ast-org-command '() #xc000)
                                             (ast-opcode-cmd '() '(#x00 #x01))
                                             (ast-org-command '() #xc020)
                                             (ast-opcode-cmd '() '(#x02 #x03))
                                             (ast-org-align-command '() #x100)
                                             (ast-opcode-cmd '() '(#x04 #x05))
                                             ))
                `((#xc000 . (#x00 #x01))
                  (#xc020 . (#x02 #x03))
                  (#xc100 . (#x04 #x05))
                  ))
  (check-equal? (assemble-to-code-list (list (ast-org-command '() #xc000)
                                             (ast-opcode-cmd '() '(#x00 #x01))
                                             (ast-org-command '() #xc020)
                                             (ast-opcode-cmd '() '(#x02 #x03))
                                             (ast-org-align-command '() #x100)
                                             (ast-opcode-cmd '() '(#x04 #x05))
                                             (ast-org-align-command '() #x100)
                                             (ast-opcode-cmd '() '(#x04 #x05))
                                             ))
                `((#xc000 . (#x00 #x01))
                  (#xc020 . (#x02 #x03))
                  (#xc100 . (#x04 #x05))
                  (#xc200 . (#x04 #x05))
                  ))
  (check-equal? (assemble-to-code-list (list (ast-org-command '() #xc000)
                                             (ast-opcode-cmd '() '(#x00 #x01))
                                             (ast-org-command '() #xc020)
                                             (ast-opcode-cmd '() '(#x02 #x03))
                                             (ast-org-align-command '() #x100)
                                             (ast-opcode-cmd '() '(#x04 #x05))
                                             (ast-org-align-command '() #x100)
                                             (ast-opcode-cmd '() '(#x04 #x05))
                                             (ast-org-align-command '() #x100)
                                             (ast-opcode-cmd '() '(#x06 #x07))
                                             (ast-org-command '() #xcf20)
                                             (ast-opcode-cmd '() '(#x08 #x09))
                                             (ast-org-align-command '() #x100)
                                             (ast-opcode-cmd '() '(#x0a #x0b))
                                             ))
                `((#xc000 . (#x00 #x01))
                  (#xc020 . (#x02 #x03))
                  (#xc100 . (#x04 #x05))
                  (#xc200 . (#x04 #x05))
                  (#xc300 . (#x06 #x07))
                  (#xcf20 . (#x08 #x09))
                  (#xd000 . (#x0a #x0b)))))

;; take a list of ast-command s and translate them to raw bytes
;; make sure that everything is resolved and decided such that
;; each instruction can be "assembled"
;; this command is complete as it decides, resolves labels, resolves constants and generates bytecode
(define/contract (assemble org program)
  (-> word/c (listof ast-command?) (listof byte/c))
  (second (assemble-with-info org program)))

;; returns additionally a list of label offsets collected by this assembly step
;; allowing additional label offsets to be passed into the assembly step
(define/contract (assemble-with-info org program (additional-label-offsets (hash)) (additional-label-instructions (list)) (additional-constants-defs (hash)))
  (->* (word/c (listof ast-command?)) (hash? list? hash?) (list/c hash? (listof byte/c) (listof ast-command?) hash?))
  (define complete-label-instructions (append additional-label-instructions (label-instructions program)))
  (define program-p1 (->resolved-decisions complete-label-instructions program))
  (define lsoffsets (hash-union additional-label-offsets (label-string-offsets org program-p1)))
  (define program-p2 (->resolve-labels org lsoffsets program-p1 '()))
  (define complete-constants-defs (hash-union additional-constants-defs (constant-definitions-hash program-p1)))
  (define program-p3 (resolve-constants complete-constants-defs program-p2))
  `(,lsoffsets ,(resolved-program->bytes program-p3) ,complete-label-instructions ,complete-constants-defs))

(module+ test #| assemble-with-info |#
  (check-equal?
   (car (assemble-with-info 2064
                            (list (JMP NEXT)
                                  (label CPY_TARGET)
                                  (word $0000)
                                  (label CPY_SOURCE)
                                  (word $0000)
                                  (label COPY_LEN)
                                  (word $0000)
                                  (label NEXT))))
   #hash(("COPY_LEN" . 2071)
       ("CPY_SOURCE" . 2069)
       ("CPY_TARGET" . 2067)
       ("NEXT" . 2073))))

(module+ test #| assemble |#
  (check-equal? (assemble 1000 (list (LDA !$10)))
                (list 169 16))
  (check-equal? (assemble 1000 (list (byte-const some_label $10) (LDA !some_label)))
                (list 169 16))
  (check-exn exn:fail? (lambda () (assemble 1000 (list (LDA !some_label)))))
  (check-exn exn:fail? (lambda () (assemble 1000 (list (byte-const some_label $10)(LDA !some_other_label)))))

  (check-equal? (assemble 1000 (list (STA $10)))
                (list 133 16))
  (check-equal? (assemble 1000 (list (byte-const some_label $10) (STA some_label)))
                (list 133 16))
  (check-exn exn:fail? (lambda () (assemble 1000 (list (STA some_label)))))
  (check-exn exn:fail? (lambda () (assemble 1000 (list (byte-const some_label $10)(STA some_other_label))))))

