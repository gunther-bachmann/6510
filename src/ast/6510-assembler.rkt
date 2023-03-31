#lang racket

#|

 provide methods to do the assembly auf ast-commands holistically, resolving all to
 translate a program to raw-bytes, all resolved

 |#

(require "6510-resolver.rkt")
(require "6510-relocator.rkt")
(require "6510-constants.rkt")
(require (only-in "6510-command.rkt" ast-command?)) 
(require (only-in "../6510-utils.rkt" word/c byte/c))

(provide assemble)

(module+ test
  (require "../6510.rkt")
  (require "../6510-test-utils.rkt"))

;; take a list of ast-command s and translate them to raw bytes
;; make sure that everything is resolved and decided such that
;; each instruction can be "assembled"
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

