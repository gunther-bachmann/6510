#lang racket

#|

 provide methods to do the assembly auf ast-commands holistically, resolving all to
 translate a program to raw-bytes, all resolved

 |#

(require "6510-resolver.rkt")
(require "6510-relocator.rkt")
(require "6510-constants.rkt")
(require (only-in "6510-command.rkt"
                  ast-command?
                  ast-org-command?
                  ast-org-command-org
                  ast-label-def-cmd
                  ast-opcode-cmd))
(require (only-in "../6510-utils.rkt" word/c byte/c))

(provide assemble assemble-to-code-list)

(module+ test
  (require "../6510.rkt")
  (require "../6510-test-utils.rkt"))

(define/contract (split-into-code-list code (code-list '()))
  (->* ((listof ast-command?)) ((listof (listof ast-command?))) (listof (listof ast-command?)))
  (if (empty? code)
      (reverse code-list)
      (if (ast-org-command? (car code))
          (split-into-code-list (cdr code) (cons (list (car code)) code-list))
          (split-into-code-list (cdr code) (cons (append (car code-list) (list (car code))) (cdr code-list))))))

(module+ test #| split-into-code-list |#
  (check-equal? (split-into-code-list (list (ast-org-command '() #xc000)
                                            (ast-command '())
                                            (ast-command '())
                                            (ast-org-command '() #xc008)
                                            (ast-command '())))
                (list (list (ast-org-command '() #xc000)
                                            (ast-command '())
                                            (ast-command '()))
                      (list (ast-org-command '() #xc008)
                                            (ast-command '())))))

(define/contract (org-for-code-seq code-seq)
  (-> (listof ast-command?) word/c)
  (if (ast-org-command? (car code-seq))
      (ast-org-command-org (car code-seq))
      (raise-user-error (format "org-command for this code sequence needs to be at the head, found ~a" (car code-seq)))))

(module+ test #| org-for-code-seq |#
  (check-equal? (org-for-code-seq (list (ast-org-command '() #xc000)
                                        (ast-command '())
                                        (ast-command '())))
                #xc000))

(define/contract (assemble-to-code-list program)
  (-> (listof ast-command?) pair?)
  (define program-p1 (->resolved-decisions (label-instructions program) program))
  (define lsoffsets (label-string-offsets (org-for-code-seq program-p1) program-p1))
  (define program-p2 (->resolve-labels (org-for-code-seq program-p1) lsoffsets program-p1 '()))
  (define program-p3 (resolve-constants (constant-definitions-hash program-p1) program-p2))
  (define code-list (split-into-code-list program-p3))
  (map (lambda (code-seq) `(,(org-for-code-seq code-seq) . ,(resolved-program->bytes (cdr code-seq)))) code-list))

(module+ test #| assemble-to-code-list |#
  (check-equal? (assemble-to-code-list (list (ast-org-command '() #xc000)
                                             (ast-opcode-cmd '() '(#x00 #x01))
                                             (ast-org-command '() #xc020)
                                             (ast-opcode-cmd '() '(#x02 #x03))))
                `((#xc000 . (#x00 #x01))
                  (#xc020 . (#x02 #x03)))))

;; take a list of ast-command s and translate them to raw bytes
;; make sure that everything is resolved and decided such that
;; each instruction can be "assembled"
;; this command is complete as it decides, resolves labels, resolves constants and generates bytecode
(define/contract (assemble org program)
  (-> word/c (listof ast-command?) (listof byte/c))
  (define program-p1 (->resolved-decisions (label-instructions program) program))
  (define lsoffsets (label-string-offsets org program-p1))
  (define program-p2 (->resolve-labels org lsoffsets program-p1 '()))
  (define program-p3 (resolve-constants (constant-definitions-hash program-p1) program-p2))
  (resolved-program->bytes program-p3))

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

