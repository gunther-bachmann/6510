#lang racket

(require (rename-in  racket/contract [define/contract define/c]))
(require "6510-utils.rkt")
(require "6510-test-utils.rkt")

(provide label-string-offsets)

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

;; write commands into this structure
;; offset       data
;; 0            rl = reloc-len (word, low-then-high byte)
;; 2            relocation table
;; 2+rl         cbl = cmd-bytes-len (word, low-then-high byte)
;; 4+rl         command bytes
;; 4+rl+cbl-1

;; generate the bytes of the relocation table of these commands
;; offset       data
;; 0            rel-position-low, rel-position-high,
;; 2            width (byte), (if width = 1 0:lowbyte 1:highbyte)?, 
;; 4            rel-value-low, rel-value-high

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

(define (label-prefix->byte full-label)
  (cond [(eq? #\> (string-ref full-label 0)) 1]
        [(eq? #\< (string-ref full-label 0)) 0]
        [#t (raise-user-error (format "full-label ~a has no hi/low prefix" full-label))]))

(define (relocation-table offset collected-entries label-offsets commands)
  (if (empty? commands)
      collected-entries
      (let* ((command (car commands))
             (res (last command)))
        (cond [(and (list? res)
                  (eq? 'resolve-word (car res)))
               (let ((rel-offset (hash-ref label-offsets (cadr res))))
                 (relocation-table (+ offset (command-len command)) 
                                   (append (list (low-byte (+ 1 offset))
                                                 (high-byte (+ 1 offset))
                                                 2 
                                                 (low-byte rel-offset)
                                                 (high-byte rel-offset))
                                           collected-entries)
                                   label-offsets
                                   (cdr commands)))]
              [(and (list? res)
                  (eq? 'resolve-byte (car res)))
               (let* ((full-label (cadr res))
                      (hilo-ind (label-prefix->byte full-label))
                      (label (base-label-str full-label))
                      (rel-offset (hash-ref label-offsets label)))
                 (relocation-table (+ offset (command-len command)) 
                                   (append (list (low-byte (+ 1 offset))
                                                 (high-byte (+ 1 offset))
                                                 1 hilo-ind
                                                 (if (eq? 0 hilo-ind)
                                                     (low-byte rel-offset)
                                                     (high-byte rel-offset)))
                                           collected-entries)
                                   label-offsets
                                   (cdr commands)))]
              [#t
               (relocation-table (+ offset (command-len command)) collected-entries label-offsets (cdr commands))]))))

(module+ test #| relocation-table |#
  (check-equal? (relocation-table #xc040 '() '#hash(("some" . #xffd2))
                                  '((opcode 20 (resolve-word "some"))))
                '(#x41 #xc0 2 #xd2 #xff))
  (check-equal? (relocation-table #xc040 '() '#hash(("some" . #xffd2))
                                  '((opcode 20 (resolve-byte "<some"))))
                '(#x41 #xc0 1 0 #xd2))
  (check-equal? (relocation-table #xc040 '() '#hash(("some" . #xffd2))
                                  '((opcode 20 (resolve-byte ">some"))))
                '(#x41 #xc0 1 1 #xff)))
