#lang racket

(require (rename-in  racket/contract [define/contract define/c]))
(require "6510-test-utils.rkt")

(define-struct reloc-entry
  (label         ;; label used
   use-position  ;; relative to code start (which is 0)
   byte-width    ;; target entry width (1 or 2 bytes)
   )
  #:transparent)

(define command/c (listof any/c))

;; (define/c (collect-reloc-entries program)
;;   (-> (listof command/c) (listof reloc-entry))
;;   '())

(define/c (command-len command)
  (-> command/c nonnegative-integer?)
  (define tag (car command))
  (define last-el (last command))
  (cond [(eq? tag 'rel-opcode) 2]
        [(eq? tag 'byte-value) (- (length command) 1)]
        [(eq? tag 'opcode)
         (+ (- (length command) 1)
            (if (and (list? last-el)
                   (eq? 'resolve-word (car last-el)))
                1
                0))]
        [#t 0]))

(module+ test #| command-len |#
  (check-equal? (command-len '(opcode 100))
                1)
  (check-equal? (command-len '(label-def "some"))
                0)
  (check-equal? (command-len '(word-const-def "some" #x2000))
                0)
  (check-equal? (command-len '(byte-const-def "some" #x20))
                0)
  (check-equal? (command-len '(byte-value #xd2 #xff))
                2)
  (check-equal? (command-len '(opcode #x20 #xff #xd2))
                3)
  (check-equal? (command-len '(rel-opcode #x20 #xff))
                2)
  (check-equal? (command-len '(rel-opcode #x20 (resolve-relative "some")))
                2)
  (check-equal? (command-len '(opcode #x20 (resolve-word "some")))
                3)
  (check-equal? (command-len '(opcode #x20 (resolve-byte "some")))
                2))

(define/c (collect-label-offsets offset collected-results commands)
  (-> nonnegative-integer? (listof any/c) (listof command/c) (listof any/c))
  (if (empty? commands)
      collected-results
      (let* ((command (car commands))
             (tag (car command)))
        (cond [(eq? tag 'label-def)
               (collect-label-offsets offset
                                     (cons (cons command offset) collected-results)
                                     (cdr commands))]
              [#t (collect-label-offsets (+ offset (command-len command))
                                        collected-results
                                        (cdr commands))]))))

(module+ test #| collect-label-offsets |#
  (check-equal? (collect-label-offsets 10 '() '((label-def "some")))
                '(((label-def "some") . 10)))
  (check-equal? (collect-label-offsets 10 '() '((opcode #x20 #xd2 #xff)
                                              (label-def "some")))
                '(((label-def "some") . 13)))
  (check-equal? (collect-label-offsets 10 '() '((label-def "hello")
                                              (opcode #x20 #xd2 #xff)
                                              (label-def "some")
                                              (opcode #x20 (resolve-word "hello"))))
                '(((label-def "some") . 13)
                  ((label-def "hello") . 10))))
