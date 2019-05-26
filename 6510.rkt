#lang racket

;; todo: add method descriptions
;; todo: realize with typed racket
;; todo: implement some syntax rules to make program-definition easier
;; todo: define macros to ease implementation of all assembler opcodes

(require racket/fixnum)
(require threading)

(require (for-syntax "6510-utils.rkt"))
(require "6510-utils.rkt")

(module+ test
  (require rackunit))

(provide parse-number-string assembler-program initialize-cpu replace-labels set-pc-in-state run
         ADC BRK LDA JSR RTS
         LABEL)


(struct cpu-state (program-counter flags memory accumulator x-index y-index stack-pointer))

(define (initialize-cpu)
  (cpu-state 0 0 (make-fxvector 65536) 0 0 0 #xff))

(define (peek state memory-address)
  (fxvector-ref (cpu-state-memory state) memory-address))

(define (poke state address value)
  (fxvector-set! (cpu-state-memory state) address (byte value))
  state)

(module+ test
  (check-match (peek (poke (initialize-cpu) #xc000 17) #xc000)
               17))

(define (6510-byte-length command)
  (case (first command)
    [('opcode) (case (last (drop-right command 1))
                 [('label-ref-relative) (- (length command) 2)]
                 [('label-ref-absolute) (- (length command) 1)]
                 [else (- (length command) 1)])]
    [('label) 0]
    [else (error "uknown command" (first command))]))

(module+ test
  (check-match (6510-byte-length '('opcode 1 'label-ref-relative "some"))
               2)

  (check-match (6510-byte-length '('opcode 1 'label-ref-absolute "other"))
               3)

  (check-match (6510-byte-length '('opcode 1 2 3))
               3)

  (check-match (6510-byte-length '('label "test"))
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
                                           (('label "some") (0 2))
                                           (('opcode 1 -2) (2 2))
                                           (('label "other") (0 4))
                                           (('opcode 5 'label-ref-absolute "some") (3 4))
                                           (('label "end") (0 7))))
               '((('label "some") (0 2)) (('label "other") (0 4)) (('label "end") (0 7)))))

(define (get-label-offset labels-byte-list label)
  (let ([filtered (filter (lambda (label-byte-pair)
                            (equal? label (last (first label-byte-pair))))
                          labels-byte-list)])
    (when (empty? filtered)
      (error "label not found in list" label filtered))
    (last (last (last filtered)))))

(module+ test
  (check-match (get-label-offset '((('label "some") (0 2))
                                   (('label "other") (0 4))
                                   (('label "end") (0 7)))
                                 "some")
               2)

  (check-match (get-label-offset '((('label "some") (0 2))
                                   (('label "other") (0 4))
                                   (('label "end") (0 7)))
                                 "other")
               4)

  (check-exn
   exn:fail?
   (lambda () (get-label-offset '((('label "some") (0 2))
                             (('label "other") (0 4))
                             (('label "end") (0 7)))
                           "unknown"))))

(define (commands-bytes-list commands)
  (let* ([byte-lengths (map 6510-byte-length commands)]
         [byte-lengths/w-offset (lo-sums byte-lengths 0)])
    (map list commands byte-lengths/w-offset)))

(module+ test

  (check-match (commands-bytes-list '(('opcode 10 10 10)
                                      ('label "some")
                                      ('opcode 0)
                                      ('opcode 10 10)))
               '((('opcode 10 10 10) (3 0))
                 (('label "some") (0 3))
                 (('opcode 0) (1 3))
                 (('opcode 10 10) (2 4)))))

(define (replace-label command-byte-pair labels-bytes-list address)
  (let* ([command (first command-byte-pair)]
         [current-offset (last (last command-byte-pair))]
         [command-length (first (last command-byte-pair))])
    (if (>= 1 (length command))
        command-byte-pair
        (case (last (drop-right command 1))
          [('label-ref-relative)
           (let* ([label-offset (get-label-offset labels-bytes-list (last command))])
             (list (append (drop-right command 2)
                           (list (- label-offset current-offset command-length)))
                   (last command-byte-pair)))]
          [('label-ref-absolute)
           (let* ([label-offset (+ address (get-label-offset labels-bytes-list (last command)))])
             (list (append (drop-right command 2)
                           (list (low-byte label-offset) (high-byte label-offset)))
                   (last command-byte-pair)))
           ]
          [else command-byte-pair]))))

(module+ test
  (check-match (replace-label '(('opcode 20 'label-ref-absolute "some") (3 10))
                              '((('label "some") (0 8)))
                              100)
               '(('opcode 20 108 0) (3 10)))

  (check-match (replace-label '(('opcode 20 30 80) (3 10))
                              '((('label "some") (0 8)))
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
                                 ('label "some")
                                 ('opcode 1 'label-ref-relative "some")
                                 ('label "other")
                                 ('opcode 5 'label-ref-absolute "some")
                                 ('label "end"))
                               10)
               '(('opcode 1 2)
                 ('label "some")
                 ('opcode 1 -2)
                 ('label "other")
                 ('opcode 5 12 0)
                 ('label "end")))

  (check-match (replace-labels '(((quote label) some)
                                 ((quote opcode) 169 65)
                                 ((quote opcode) 32 (quote label-ref-absolute) some)
                                 ((quote opcode) 0)) 10)
               '(((quote label) some)
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

(define (assembler-program state memory-address commands)
  (load state memory-address (flatten (~>  (replace-labels commands memory-address)
                                          remove-resolved-statements))))


(define (JMP_abs absolute)
  (list ''opcode #x4C (high-byte absolute) (low-byte absolute)))


(define (load state memory-address program)
  (map (lambda (pair) (fxvector-set! (cpu-state-memory state) (first pair) (last pair)))
       (map list
            (sequence->list (in-range memory-address (+ memory-address (length program))))
            program))
  state)

(define (peek-pc state)
  (peek state (cpu-state-program-counter state)))

(define (peek-pc+1 state)
  (peek state (+ 1 (cpu-state-program-counter state))))

(define (peek-pc+2 state)
  (peek state (+ 2 (cpu-state-program-counter state))))

(define (set-pc-in-state state pc)
  (struct-copy cpu-state state [program-counter (word pc)]))

(define (run state)
  (if (not (eq? 0 (peek-pc state)))
      (run (execute-cpu-step state))
      state))

(define (interpret-rts state)
  (let* ([sp (cpu-state-stack-pointer state)]
         [low-ret (peek state (+ #x100 (byte (+ 2 sp))))]
         [high-ret (peek state (+ #x100 (byte (+ 1 sp))))]
         [new-state (struct-copy cpu-state state
                                 [program-counter (word (+ 1 (absolute high-ret low-ret)))]
                                 [stack-pointer (byte (+ sp 2))])])
    new-state))

(define (interpret-jsr-abs high low state)
  (case (absolute high low)
    [(#xFFFF) (display (string (integer->char (cpu-state-accumulator state))))
              (struct-copy cpu-state state [program-counter (word (+ 3 (cpu-state-program-counter state)))])]
    [else (let* ([new-program-counter (absolute high low)]
                 [return-address (+ 2 (cpu-state-program-counter state))]
                 [sp (cpu-state-stack-pointer state)]
                 [new-state (struct-copy cpu-state state
                                         [program-counter (word new-program-counter)]
                                         [stack-pointer (byte (- sp 2))])])
            (poke new-state (+ #x100 sp) (low-byte return-address))
            (poke new-state (+ #x100 (byte (- sp 1))) (high-byte return-address))
            new-state)])) ;; TODO: put return address onto the stack and jump to address

(define (interpret-jmp-abs high low state)
  (struct-copy cpu-state state
               [program-counter (word (absolute high low))]))

(define (interpret-adc-i immediate state)
  (let* ([old-accumulator (cpu-state-accumulator state)]
         [intermediate-accumulator (+ immediate old-accumulator)]
         [new-accumulator (if (carry-flag? state) (+ 1 intermediate-accumulator) intermediate-accumulator)]
         [overflow        (> (+ immediate old-accumulator) 255 )])
    (struct-copy cpu-state state
                 [program-counter (word (+ 2 (cpu-state-program-counter state)))]
                 [flags           (if overflow
                                      (-set-carry-flag (cpu-state-flags state))
                                      (cpu-state-flags state))]
                 [accumulator     (byte new-accumulator)])))

(define (interpret-lda-i immediate state)
  (struct-copy cpu-state state
               [program-counter (word (+ 2 (cpu-state-program-counter state)))]
               [accumulator     (byte immediate)]))

(define (carry-flag? state)
  (eq? 1 (bitwise-and 1 (cpu-state-flags state))))

(define (-set-carry-flag flags)
  (bitwise-xor 1 flags))

(define (-clear-carry-flag flags)
  (bitwise-and #xfe flags))

(define (set-carry-flag state)
  (struct-copy cpu-state state [flags (-set-carry-flag (cpu-state-flags state))]))

(define (print-state state)
  (printf "A = ~a~n" (cpu-state-accumulator state))
  (printf "X = ~a~n" (cpu-state-x-index state))
  (printf "Y = ~a~n" (cpu-state-y-index state))
  (printf "SP = ~a~n" (cpu-state-stack-pointer state))
  (printf "PC = ~a~n" (cpu-state-program-counter state))
  (printf "C = ~s" (if (carry-flag? state) "X" " " )))

(define (execute-cpu-step state)
  (case (peek-pc state)
    [(#x20) (interpret-jsr-abs (peek-pc+2 state) (peek-pc+1 state) state)]
    [(#x4C) (interpret-jmp-abs (peek-pc+2 state) (peek-pc+1 state) state)]
    [(#x60) (interpret-rts state)]
    [(#x69) (interpret-adc-i (peek-pc+1 state) state)]
    [(#xA9) (interpret-lda-i (peek-pc+1 state) state)]
    [else state]))

;; ================================================================================ JSR

(define (JSR_abs_label ref str)
  (list ''opcode #x20 ''label-ref-absolute str))

(define (JSR_abs absolute)
  (list ''opcode #x20 (low-byte absolute) (high-byte absolute)))

(define-syntax (JSR stx)
  (syntax-case stx ()
    [(JSR op)  #'(JSR_abs (parse-number-string op))]
    [(JSR ref str) #'(JSR_abs_label ref str)]))

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

(module+ test
  (check-eq? (peek (assembler-program (initialize-cpu) 10 (list (ADC_i #x10) (ADC_i #x11))) 11)
             16
             "immediate operand 1 is $10 = 16")

  (check-true (carry-flag? (set-carry-flag (initialize-cpu))) "after setting, carry is set")

  (check-false (carry-flag? (initialize-cpu)) "carry initially clear")

  (check-eq? (cpu-state-accumulator (execute-cpu-step (execute-cpu-step (assembler-program (initialize-cpu) 0 (list (LDA_i #x80) (ADC_i #x81))))))
             1
             "accumulator should be 1 after adding 128 and 129 (overflow)")

  (check-true (carry-flag? (execute-cpu-step (execute-cpu-step (assembler-program (initialize-cpu) 0 (list (LDA_i #x80) (ADC_i #x81))))))
              "carry should be set after adding 128 and 129"))


; (run (assembler-program (initialize-cpu) 0 (list (LDA_i #x41) (JSR_abs #xFFFF) (BRK))))
