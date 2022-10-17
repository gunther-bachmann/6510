#lang racket

(require (rename-in  racket/contract [define/contract define/c]))
(require "6510-utils.rkt")
(require "6510-test-utils.rkt")

(provide label-string-offsets)

(define command/c (listof any/c))

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

(define/c (label-string-offsets offset collected-results commands)
  (-> nonnegative-integer? hash? (listof command/c) hash?)
  (if (empty? commands)
      collected-results
      (let* ((command (car commands))
             (tag (car command)))
        (cond [(eq? tag 'label-def)
               (label-string-offsets
                offset
                (hash-set collected-results (cadr command) offset)
                (cdr commands))]
              [#t
               (label-string-offsets
                (+ offset (command-len command))
                collected-results
                (cdr commands))]))))

(module+ test #| collect-label-offsets |#
  (check-equal? (label-string-offsets 10 (hash) '((label-def "some")))
                '#hash(("some" . 10)))
  (check-equal? (label-string-offsets 10 (hash) '((opcode #x20 #xd2 #xff)
                                              (label-def "some")))
                '#hash(("some" . 13)))
  (check-equal? (label-string-offsets 10 (hash) '((label-def "hello")
                                              (opcode #x20 #xd2 #xff)
                                              (label-def "some")
                                              (opcode #x20 (resolve-word "hello"))))
                '#hash(("some" . 13)
                  ("hello" . 10))))

(define (base-label-str full-label)
  (cond [(or (eq? #\> (string-ref full-label 0)) 
            (eq? #\< (string-ref full-label 0)))
         (substring full-label 1)]
        [#t full-label]))

(module+ test #| base-label-str |#
  (check-equal? (base-label-str "hello")
                "hello")
  (check-equal? (base-label-str ">hello")
                "hello")
  (check-equal? (base-label-str "<hello")
                "hello"))

(define (label->hilo-indicator full-label)
  (cond [(eq? #\> (string-ref full-label 0)) 1]
        [(eq? #\< (string-ref full-label 0)) 0]
        [#t (raise-user-error (format "full-label ~a has no hi/low prefix" full-label))]))

;; relocation table format
;; offset       data
;; 0            rel-position-low, rel-position-high, : position where the value has to be written to
;; 2            width (byte), (if width = 1 0:lowbyte 1:highbyte)?,
;; 3/4          rel-value-low, rel-value-high        : (val + origin) is the value to be written
(define (reloc-entry-bytes offset rel-offset . bytes)
  (append (list (low-byte (+ 1 offset))
                (high-byte (+ 1 offset)))
          bytes
          (list (low-byte rel-offset)
                (high-byte rel-offset))))

(define (resolve-word->reloc-bytes resolve label-offsets offset)
  (let ((rel-offset (hash-ref label-offsets (cadr resolve))))                 
    (reloc-entry-bytes offset rel-offset 2)))

(module+ test #| resolve-word->reloc-bytes |#
  (check-equal? (resolve-word->reloc-bytes '(resolve-word "some")
                                          '#hash(("some" . #x01d2))
                                          #xc040)
                '(#x41 #xc0 2 #xd2 #x01)))

(define (resolve-byte->reloc-bytes resolve label-offsets offset)
  (let* ((full-label (cadr resolve))
         (hilo-ind   (label->hilo-indicator full-label))
         (label      (base-label-str full-label))
         (rel-offset (hash-ref label-offsets label)))
    (reloc-entry-bytes offset rel-offset 1 hilo-ind)))

(module+ test #| resolve-byte->reloc-bytes |#
  (check-equal? (resolve-byte->reloc-bytes '(resolve-byte ">some")
                                          '#hash(("some" . #x01fa))
                                          #x0005)
                '(#x06 #x00 1 1 #xfa #x01))
  (check-equal? (resolve-byte->reloc-bytes '(resolve-byte "<some")
                                          '#hash(("some" . #x01fa))
                                          #x0005)
                '(#x06 #x00 1 0 #xfa #x01)))

(define (reloc-table-bytes offset collected-entries label-offsets commands)
  (if (empty? commands)
      collected-entries
      (let* ((command     (car commands))
             (res         (last command))
             (next-offset (+ offset (command-len command)))
             (next-entries
              (cond [(and (list? res)
                        (eq? 'resolve-word (car res)))
                     (append collected-entries
                             (resolve-word->reloc-bytes res label-offsets offset))]
                    [(and (list? res)
                        (eq? 'resolve-byte (car res)))
                     (append collected-entries
                             (resolve-byte->reloc-bytes res label-offsets offset))]
                    [#t collected-entries])))
        (reloc-table-bytes next-offset next-entries label-offsets (cdr commands)))))

(module+ test #| reloc-table-bytes |#
  (check-equal? (reloc-table-bytes #xc040 '() '#hash(("some" . #x01d2)("other" . #x01d9))
                                  '((opcode 20 (resolve-byte ">some"))
                                    (opcode 20 (resolve-word "other"))
                                    (opcode 20 (resolve-byte "<some"))))
                '(#x41 #xc0 1 1 #xd2 #x01
                  #x43 #xc0 2 #xd9 #x01
                  #x46 #xc0 1 0 #xd2 #x01)))
