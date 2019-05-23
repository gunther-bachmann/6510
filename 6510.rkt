#lang racket

;; todo: add more unit tests
;; todo: add method descriptions
;; todo: realize with typed racket
;; todo: implement some syntax rules to make program-definition easier
;; todo: define macros to ease implementation of all assembler opcodes

(require racket/fixnum)
(require rackunit)
(require scribble/srcdoc)
(require (for-syntax "6510-utils.rkt"))
(require "6510-utils.rkt")

(provide parse-number-string ADC assembler-program initialize-cpu BRK run JSR LDA LABEL replace-labels set-pc-in-state)


(struct cpu-state (program-counter flags memory accumulator x-index y-index stack-pointer))

(define (initialize-cpu)
  (cpu-state 0 0 (make-fxvector 65536) 0 0 0 0))

(define (peek state memory-address)
  (fxvector-ref (cpu-state-memory state) memory-address))

(define (6510-byte-length command)
  (case (first command)
    [('opcode) (case (last (drop-right command 1))
                 [('label-ref-relative) (- (length command) 2)]
                 [('label-ref-absolute) (- (length command) 1)]
                 [else (- (length command) 1)])]
    [('label) 0]
    [else (error "uknown command" (first command))]))

(check-match (6510-byte-length '('opcode 1 'label-ref-relative "some"))
             2)

(check-match (6510-byte-length '('opcode 1 'label-ref-absolute "other"))
             3)

(check-match (6510-byte-length '('opcode 1 2 3))
             3)

(check-match (6510-byte-length '('label "test"))
             0)

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

(check-match (collect-label-offset-map '((('opcode 1 2) (2 0))
                                         (('label "some") (0 2))
                                         (('opcode 1 -2) (2 2))
                                         (('label "other") (0 4))
                                         (('opcode 5 'label-ref-absolute "some") (3 4))
                                         (('label "end") (0 7))))
             '((('label "some") (0 2)) (('label "other") (0 4)) (('label "end") (0 7))))

(define (get-label-offset labels-byte-list label)
  (let ([filtered (filter (lambda (label-byte-pair)
                            (equal? label (last (first label-byte-pair))))
                            labels-byte-list)])
    (when (empty? filtered)
      (error "label not found in list" label filtered))
    (last (last (last filtered)))))

(check-match (get-label-offset '((('label "some") (0 2)) (('label "other") (0 4)) (('label "end") (0 7))) "some")
             2)

(check-match (get-label-offset '((('label "some") (0 2)) (('label "other") (0 4)) (('label "end") (0 7))) "other")
             4)

(define (commands-bytes-list commands)
  (let* ([byte-lengths (map 6510-byte-length commands)]
         [byte-lengths/w-offset (lo-sums byte-lengths 0)])
    (map list commands byte-lengths/w-offset))
  )

(define (replace-labels commands address)
  (let* ([commands-bytes-list (commands-bytes-list commands)]
         [labels-bytes-list (collect-label-offset-map commands-bytes-list)])
    (filter (lambda (command) (case (first command) [('label) #f] [else #t]))
            (map first
                 (map (lambda (command-byte-pair)
                        (let* ([command (first command-byte-pair)]
                               [current-offset (last (last command-byte-pair))]
                               [command-length (first (last command-byte-pair))])
                          (if (< 1 (length command))
                              (case (last (drop-right command 1))
                                [('label-ref-relative)
                                 (let* ([label-offset (get-label-offset labels-bytes-list (last command))])
                                   `(,(append (drop-right command 2) `(,(- label-offset current-offset command-length))) ,(last command-byte-pair)))]
                                [('label-ref-absolute)
                                 (let* ([label-offset (+ address (get-label-offset labels-bytes-list (last command)))])
                                   `(,(append (drop-right command 2) `(,(high-byte label-offset) ,(low-byte label-offset))) ,(last command-byte-pair)))
                                 ]
                                [else command-byte-pair])
                              command-byte-pair)))
                      commands-bytes-list)))))

(check-match (replace-labels '(('opcode 1 2)('label "some")('opcode 1 'label-ref-relative "some")('label "other")('opcode 5 'label-ref-absolute "some")('label "end")) 10)
             '(('opcode 1 2) ('opcode 1 -2) ('opcode 5 0 12)))

(check-match (replace-labels '(((quote label) some) ((quote opcode) 169 65) ((quote opcode) 32 (quote label-ref-absolute) some) ((quote opcode) 0)) 10)
             '(('opcode 169 65) ('opcode 32 0 10) ('opcode 0)))

; '('label "label")
; '('opcode #x00 'label-ref-relative)
; '('opcode #x00 'label-ref-absolute)

(define (resolve-statements commands)
  (let* [(label-offsets (collect-label-offset-map commands))]
    (replace-labels label-offsets commands)))

(define (remove-resolved-statements commands)
  (map (lambda (command)
         (case (first command)
           [('opcode) (drop command 1)]
           [else command]))
       commands))

(check-match (remove-resolved-statements '((1 2 3) ('opcode 2 3 4) (0)))
             '((1 2 3) ( 2 3 4) (0)))

(define (assembler-program state memory-address commands)
  (load state memory-address (flatten (remove-resolved-statements (replace-labels commands memory-address)))))


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
  (struct-copy cpu-state state [program-counter pc]))

(define (run state)
  (if (not (eq? 0 (peek-pc state)))
      (run (execute-cpu-step state))
      state))

(define (execute-cpu-step state)
  (case (peek-pc state)
    [(#x20) (interpret-jsr-abs (peek-pc+1 state) (peek-pc+2 state) state)]
    [(#x4C) (interpret-jmp-abs (peek-pc+1 state) (peek-pc+2 state) state)]
    [(#x69) (interpret-adc-i (peek-pc+1 state) state)]
    [(#xA9) (interpret-lda-i (peek-pc+1 state) state)]
    [else state]))

(define (interpret-jsr-abs high low state)
  (case (absolute high low)
    [(#xFFFF) (print (integer->char (cpu-state-accumulator state)))
              (struct-copy cpu-state state [program-counter (+ 3 (cpu-state-program-counter state))])]
    [else state])) ;; TODO: put return address onto the stack and jump to address

(define (interpret-jmp-abs high low state)
  (struct-copy cpu-state state
               [program-counter (+ (* 256 high) low)]))

(define (interpret-adc-i immediate state)
  (let* ([old-accumulator (cpu-state-accumulator state)]
         [intermediate-accumulator (modulo (+ immediate old-accumulator) 256)]
         [new-accumulator (if (carry-flag? state) (+ 1 intermediate-accumulator) intermediate-accumulator)]
         [overflow        (> (+ immediate old-accumulator) 255 )])
    (struct-copy cpu-state state
                 [program-counter (+ 2 (cpu-state-program-counter state))]
                 [flags           (if overflow
                                      (-set-carry-flag (cpu-state-flags state))
                                      (cpu-state-flags state))]
                 [accumulator     new-accumulator])))

(define (interpret-lda-i immediate state)
  (struct-copy cpu-state state
               [program-counter (+ 2 (cpu-state-program-counter state))]
               [accumulator     immediate]))

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


;; ================================================================================ JSR

(define (JSR_abs_label ref str)
  (list ''opcode #x20 ''label-ref-absolute str))

(define (JSR_abs absolute)
  (list ''opcode #x20 (high-byte absolute) (low-byte absolute)))

(define-syntax (JSR stx)
  (syntax-case stx ()
    [(JSR op)  #'(JSR_abs (parse-number-string op))]
    [(JSR ref str) #'(JSR_abs_label ref str)]))

;; ================================================================================ BRK

(define (BRK) (list ''opcode #x00))

(check-match (BRK)
             '('opcode #x00))

(define (LABEL_s label) (list ''label label))

(define-syntax (LABEL stx)
  (syntax-case stx ()
    [(LABEL op)
     #'(LABEL_s op)]))

;; ================================================================================ LDA

(define (LDA_abs absolute)
  (list ''opcode #xad (high-byte absolute) (low-byte absolute)))

(define (LDA_zp zero-page-address)
  (list ''opcode #xA5 zero-page-address))

(define (LDA_i immediate)
  (list ''opcode #xA9 immediate))

(define (LDA_zpx zero-page-address)
  (list ''opcode #xB5 zero-page-address))

(define (LDA_absx absolute)
  (list ''opcode #xBD (high-byte absolute) (low-byte absolute)))

(define (LDA_absy absolute)
  (list ''opcode #xB9 (high-byte absolute) (low-byte absolute)))

(define-syntax (LDA stx)
  (syntax-case stx ()
    [(LDA op)
     (if (equal? (substring (syntax-e #'op) 0 1) "#")
         #'(LDA_i (parse-number-string (substring op 1)))
         (let ([op-number (parse-number-string (syntax->datum #'op))])
           (if (> 256 op-number)
               #'(LDA_zp (parse-number-string op))
               #'(LDA_abs (parse-number-string op)))))]
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

(check-match (LDA "#$10")
             '('opcode #xA9 16))

(check-match (LDA "$17")
             '('opcode #xa5 #x17))

(check-match (LDA "$178F")
             '('opcode #xad #x17 #x8F))

(check-match (LDA "$10",x)
             '('opcode #xB5 16))

(check-match (LDA "$A000",x)
             '('opcode #xBD #xA0 #x00))

(check-match (LDA "$A000",y)
             '('opcode #xB9 #xA0 #x00))

;; ================================================================================ ADC

(define (ADC_i immediate)
  (list ''opcode  #x69 immediate))

(define (ADC_zpx zero-page-address)
  (list ''opcode #x75 zero-page-address))

(define (ADC_absx absolute)
  (list ''opcode #x7D (high-byte absolute) (low-byte absolute)))

(define (ADC_absy absolute)
  (list ''opcode #x79 (high-byte absolute) (low-byte absolute)))

(define (ADC_zp zero-page-address)
  (list ''opcode #x65 zero-page-address))

(define (ADC_abs absolute)
  (list ''opcode #x6D (high-byte absolute) (low-byte absolute)))

(define-syntax (ADC stx)
  (syntax-case stx ()
    [(ADC op)
     (if (equal? (substring (syntax-e #'op) 0 1) "#")
         #'(ADC_i (parse-number-string (substring op 1)))
         (let ([op-number (parse-number-string (syntax->datum #'op))])
           (if (> 256 op-number)
               #'(ADC_zp (parse-number-string op))
               #'(ADC_abs (parse-number-string op)))))]
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

(check-match (ADC "%10",x)
             '('opcode #x75 2))

(check-match (ADC "$1237",y)
             '('opcode #x79 #x12 #x37))

(check-match (ADC "#100")
             '('opcode #x69 100))

(check-match (ADC "#$FF")
             '('opcode #x69 #xFF))

(check-match (ADC "$FF")
             '('opcode #x65 #xFF))

(check-match (ADC "$FFFF")
             '('opcode #x6d #xff #xff))

(check-eq? (peek (assembler-program (initialize-cpu) 10 (list (ADC_i #x10) (ADC_i #x11))) 11)
           16
           "immediate operand 1 is $10 = 16")

(check-true (carry-flag? (set-carry-flag (initialize-cpu))) "after setting, carry is set")

(check-false (carry-flag? (initialize-cpu)) "carry initially clear")

(check-eq? (cpu-state-accumulator (execute-cpu-step (execute-cpu-step (assembler-program (initialize-cpu) 0 (list (LDA_i #x80) (ADC_i #x81))))))
           1
           "accumulator should be 1 after adding 128 and 129 (overflow)")

(check-true (carry-flag? (execute-cpu-step (execute-cpu-step (assembler-program (initialize-cpu) 0 (list (LDA_i #x80) (ADC_i #x81))))))
            "carry should be set after adding 128 and 129")


; (run (assembler-program (initialize-cpu) 0 (list (LDA_i #x41) (JSR_abs #xFFFF) (BRK))))
