#lang racket

;; todo: add .data command for byte arrays
;; todo: add branch commands
;; todo: add inc*/dec* commands
;; todo: add method descriptions (scrbl)
;; todo: define macros to ease implementation of all assembler opcodes (addressing modes)
;; planned: realize with typed racket

(require threading)

(require (for-syntax "6510-utils.rkt"))
(require "6510-utils.rkt")
(require "6510-interpreter.rkt")

(module+ test
  (require rackunit))

(provide parse-number-string replace-labels commands->bytes create-prg run-emulator
         ADC BRK LDA JSR RTS
         LABEL)


;; todo absolute address mode = 2
;; branches = 1 (e.g. beq)
(define (opcode-argument-length opcode)
  2)

(define (6510-byte-length command)
  (case (first command)
    [('opcode) (if (6510-label-string? (last command))
                   (+ 1 (opcode-argument-length (second command)))
                   (- (length command) 1))]
    [('label) 0]
    [else (error "uknown command" (first command))]))

(module+ test
  (check-match (6510-byte-length '('opcode 1 ":some"))
               3)

  (check-match (6510-byte-length '('opcode 1 ":other"))
               3)

  (check-match (6510-byte-length '('opcode 1 2 3))
               3)

  (check-match (6510-byte-length '('label ":test"))
               0))

(define (lo-sums list current-sum)
  (if (empty? list)
      '()
      (let* ([first-num (first list)]
             [new-sum (+ current-sum first-num)])
        (append `((,first-num ,current-sum)) (lo-sums (drop list 1) new-sum)))))

(define (collect-label-offset-map commands-bytes-list)
  (let* ([labels-bytes-list (filter (lambda (command-byte-pair)
                                      (case (first (first command-byte-pair))
                                        [('label) #t]
                                        [else #f])) commands-bytes-list)])
    labels-bytes-list))

(module+ test
  (check-match (collect-label-offset-map '((('opcode 1 2) (2 0))
                                           (('label ":some") (0 2))
                                           (('opcode 1 -2) (2 2))
                                           (('label ":other") (0 4))
                                           (('opcode 5 ":some") (3 4))
                                           (('label ":end") (0 7))))
               '((('label ":some") (0 2)) (('label ":other") (0 4)) (('label ":end") (0 7)))))

(define (get-label-offset labels-byte-list label)
  (let ([filtered (filter (lambda (label-byte-pair)
                            (equal? label (last (first label-byte-pair))))
                          labels-byte-list)])
    (when (empty? filtered)
      (error "label not found in list" label filtered))
    (last (last (last filtered)))))

(module+ test
  (check-match (get-label-offset '((('label ":some") (0 2))
                                   (('label ":other") (0 4))
                                   (('label ":end") (0 7)))
                                 ":some")
               2)

  (check-match (get-label-offset '((('label ":some") (0 2))
                                   (('label ":other") (0 4))
                                   (('label ":end") (0 7)))
                                 ":other")
               4)

  (check-exn
   exn:fail?
   (lambda () (get-label-offset '((('label ":some") (0 2))
                             (('label ":other") (0 4))
                             (('label ":end") (0 7)))
                           ":unknown"))))

(define (commands-bytes-list commands)
  (let* ([byte-lengths (map 6510-byte-length commands)]
         [byte-lengths/w-offset (lo-sums byte-lengths 0)])
    (map list commands byte-lengths/w-offset)))

(module+ test

  (check-match (commands-bytes-list '(('opcode 10 10 10)
                                      ('label ":some")
                                      ('opcode 0)
                                      ('opcode 10 10)))
               '((('opcode 10 10 10) (3 0))
                 (('label ":some") (0 3))
                 (('opcode 0) (1 3))
                 (('opcode 10 10) (2 4)))))

(define (replace-label command-byte-pair labels-bytes-list address)
  (let* ([command (first command-byte-pair)]
         [current-offset (last (last command-byte-pair))]
         [command-length (first (last command-byte-pair))])
    (if (>= 1 (length command))
        command-byte-pair
        (if (and (not (equal? ''label (first command))) (6510-label-string? (last command)))
            (let* ([label-offset (+ address (get-label-offset labels-bytes-list (last command)))])
              (list (append (drop-right command 1)
                            (list (low-byte label-offset) (high-byte label-offset)))
                    (last command-byte-pair)))
            command-byte-pair))))

(module+ test
  (check-match (replace-label '(('opcode 20 ":some") (3 10))
                              '((('label ":some") (0 8)))
                              100)
               '(('opcode 20 108 0) (3 10)))

  (check-match (replace-label '(('opcode 20 30 80) (3 10))
                              '((('label ":some") (0 8)))
                              100)
               '(('opcode 20 30 80) (3 10))))

(define (replace-labels commands address)
  (let* ([commands-bytes-list (commands-bytes-list commands)]
         [labels-bytes-list (collect-label-offset-map commands-bytes-list)])
    (map first
         (map (lambda (command-byte-pair) (replace-label command-byte-pair labels-bytes-list address))
              commands-bytes-list))))

(module+ test
  (check-match (replace-labels '(('opcode 1 2)
                                 ('label ":some")
                                 ('opcode 1 ":some")
                                 ('label ":other")
                                 ('opcode 5 ":other")
                                 ('label ":end"))
                               10)
               '(('opcode 1 2)
                 ('label ":some")
                 ('opcode 1 12 0)
                 ('label ":other")
                 ('opcode 5 15 0)
                 ('label ":end")))

  (check-match (replace-labels '(((quote label) ":some")
                                 ((quote opcode) 169 65)
                                 ((quote opcode) 32 ":some")
                                 ((quote opcode) 0)) 10)
               '(((quote label) ":some")
                 ('opcode 169 65)
                 ('opcode 32 10 0)
                 ('opcode 0))))

(define (resolve-statements commands)
  (let* [(label-offsets (collect-label-offset-map commands))]
    (replace-labels label-offsets commands)))

(define (command-is-label? command)
  (case (first command) [('label) #t] [else #f]))

(define (remove-resolved-statements commands)
  (filter-not command-is-label?
              (map (lambda (command)
                     (case (first command)
                       [('opcode) (drop command 1)]
                       [else command]))
                   commands)))

(module+ test
  (check-match (remove-resolved-statements '((1 2 3)
                                             ('opcode 2 3 4)
                                             (0)))
               '((1 2 3)
                 (2 3 4)
                 (0))))

(define (commands->bytes memory-address commands )
  (flatten (~>  (replace-labels commands memory-address)
                                          remove-resolved-statements)))

(define (JMP_abs absolute)
  (list ''opcode #x4C (high-byte absolute) (low-byte absolute)))


;; ================================================================================ JSR

(define (JSR_abs_label str)
  (list ''opcode #x20 str))

(define (JSR_abs absolute)
  (list ''opcode #x20 (low-byte absolute) (high-byte absolute)))

(define-syntax (JSR stx)
  (syntax-case stx ()
    [(JSR op)
     (if (6510-label-string? (syntax->datum #'op))
         #'(JSR_abs_label op)
         #'(JSR_abs (parse-number-string op)))]))

(define (RTS)
  (list ''opcode #x60))

;; ================================================================================ BRK

(define (BRK) (list ''opcode #x00))

(module+ test
  (check-match (BRK)
               '('opcode #x00)))

(define (LABEL_s label) (list ''label label))

(define-syntax (LABEL stx)
  (syntax-case stx ()
    [(LABEL op)
     #'(LABEL_s op)]))

;; ================================================================================ LDA

(define (LDA_abs absolute)
  (list ''opcode #xad (low-byte absolute) (high-byte absolute)))

(define (LDA_zp zero-page-address)
  (list ''opcode #xA5 (byte zero-page-address)))

(define (LDA_i immediate)
  (list ''opcode #xA9 (byte immediate)))

(define (LDA_zpx zero-page-address)
  (list ''opcode #xB5 (byte zero-page-address)))

(define (LDA_absx absolute)
  (list ''opcode #xBD (low-byte absolute) (high-byte absolute)))

(define (LDA_absy absolute)
  (list ''opcode #xB9 (low-byte absolute) (high-byte absolute)))

(define (LDA_indx absolute)
  (list ''opcode #xa1 (low-byte absolute) (high-byte absolute)))

(define (LDA_indy absolute)
  (list ''opcode #xb1 (low-byte absolute) (high-byte absolute)))

(define-syntax (LDA stx)
  (syntax-case stx ()
    [(LDA op)
     (if (equal? (substring (syntax-e #'op) 0 1) "#")
         #'(LDA_i (parse-number-string (substring op 1)))
         (let ([op-number (parse-number-string (syntax->datum #'op))])
           (if (> 256 op-number)
               #'(LDA_zp (parse-number-string op))
               #'(LDA_abs (parse-number-string op)))))]
    [(LDA open op close-or-var close-or-var2)
     (let ([close (syntax-e #'close-or-var)])
       (case close
         [(>) #'(LDA_indy (parse-number-string op))]
         [else #'(LDA_indx (parse-number-string op))]))]
    [(LDA op, idx)
     (let* ([indirect (syntax-e #'idx)]
            [op-number (parse-number-string (syntax->datum #'op))])
       (if (> 256 op-number)
           (case indirect
             [(x) #'(LDA_zpx (parse-number-string op))]
             [else (error "lda zero page index mode unknown" indirect)])
           (case indirect
             [(x) #'(LDA_absx (parse-number-string op))]
             [(y) #'(LDA_absy (parse-number-string op))]
             [else (error "lda absolute index mode unknown" indirect)])))]))

(module+ test
  (check-match (LDA "#$10")
               '('opcode #xA9 16))

  (check-match (LDA "$17")
               '('opcode #xa5 #x17))

  (check-match (LDA "$178F")
               '('opcode #xad #x8F #x17))

  (check-match (LDA "$10",x)
               '('opcode #xB5 16))

  (check-match (LDA "$A000",x)
               '('opcode #xBD #x00 #xA0))

  (check-match (LDA "$A000",y)
               '('opcode #xB9 #x00 #xA0))

  (check-match (LDA < "$A000" >,y )
               '('opcode #xB1 #x00 #xA0))

  (check-match (LDA < "$A000", x > )
               '('opcode #xA1 #x00 #xA0)))

;; ================================================================================ ADC

(define (ADC_i immediate)
  (list ''opcode  #x69 (byte immediate)))

(define (ADC_zpx zero-page-address)
  (list ''opcode #x75 (byte zero-page-address)))

(define (ADC_absx absolute)
  (list ''opcode #x7D (low-byte absolute) (high-byte absolute)))

(define (ADC_absy absolute)
  (list ''opcode #x79 (low-byte absolute) (high-byte absolute)))

(define (ADC_zp zero-page-address)
  (list ''opcode #x65 (byte zero-page-address)))

(define (ADC_abs absolute)
  (list ''opcode #x6D (low-byte absolute) (high-byte absolute)))

(define (ADC_indx absolute)
  (list ''opcode #x61 (low-byte absolute) (high-byte absolute)))

(define (ADC_indy absolute)
  (list ''opcode #x71 (low-byte absolute) (high-byte absolute)))

(define-syntax (ADC stx)
  (syntax-case stx ()
    [(ADC op)
     (if (equal? (substring (syntax-e #'op) 0 1) "#")
         #'(ADC_i (parse-number-string (substring op 1)))
         (let ([op-number (parse-number-string (syntax->datum #'op))])
           (if (> 256 op-number)
               #'(ADC_zp (parse-number-string op))
               #'(ADC_abs (parse-number-string op)))))]
    [(ADC open op close-or-var close-or-var2)
     (let ([close (syntax-e #'close-or-var)])
       (case close
         [(>) #'(ADC_indy (parse-number-string op))]
         [else #'(ADC_indx (parse-number-string op))]))]
    [(ADC op, idx)
     (let* ([indirect (syntax-e #'idx)]
            [op-number (parse-number-string (syntax->datum #'op))])
       (if (> 256 op-number)
           (case indirect
             [(x) #'(ADC_zpx (parse-number-string op))]
             [else (error "adc zero page index mode unknown" indirect)])
           (case indirect
             [(x) #'(ADC_absx (parse-number-string op))]
             [(y) #'(ADC_absy (parse-number-string op))]
             [else (error "adc absolute index mode unknown" indirect)])))]))

(module+ test
  (check-match (ADC "%10",x)
               '('opcode #x75 2))

  (check-match (ADC "$1237",y)
               '('opcode #x79 #x37 #x12))

  (check-match (ADC "#100")
               '('opcode #x69 100))

  (check-match (ADC "#$FF")
               '('opcode #x69 #xFF))

  (check-match (ADC "$FF")
               '('opcode #x65 #xFF))

  (check-match (ADC "$FFFF")
               '('opcode #x6d #xff #xff))

  (check-match (ADC < "$FFFF" > ,y)
               '('opcode #x71 #xff #xff))

  (check-match (ADC < "$FFFF" ,x >)
               '('opcode #x61 #xff #xff)))




; (run (assembler-program (initialize-cpu) 0 (list (LDA_i #x41) (JSR_abs #xFFFF) (BRK))))

(define (create-prg program org file-name)
  (display-to-file (list->bytes (append `(,(low-byte org) ,(high-byte org)) program))
                   file-name
                   #:mode 'binary
                   #:exists 'replace))

(define (run-emulator file-name)
  (system (string-append "x64 " file-name)))
