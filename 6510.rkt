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

(provide parse-number-string ADC assembler-program initialize-cpu)


(struct cpu-state (program-counter flags memory accumulator x-index y-index stack-pointer))

(define (initialize-cpu)
  (cpu-state 0 0 (make-fxvector 65536) 0 0 0 0))

(define (peek state memory-address)
  (fxvector-ref (cpu-state-memory state) memory-address))

(define (assembler-program state memory-address commands)
  (load state memory-address (flatten commands)))

(define (BRK) (list #x00))

(define (JMP_abs absolute)
  (list #x4C (high-byte absolute) (low-byte absolute)))

(define (JSR_abs absolute)
  (list #x20 (high-byte absolute) (low-byte absolute)))

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

;; (run (assembler-program (initialize-cpu) 0 (list (LDA_i #x41) (JSR_abs #xFFFF) (BRK))))

;; ================================================================================ LDA

(define (LDA_abs absolute)
  (list #xad (high-byte absolute) (low-byte absolute)))

(define (LDA_zp zero-page-address)
  (list #xA5 zero-page-address))

(define (LDA_i immediate)
  (list #xA9 immediate))

(define (LDA_zpx zero-page-address)
  (list #xB5 zero-page-address))

(define (LDA_absx absolute)
  (list #xBD (high-byte absolute) (low-byte absolute)))

(define (LDA_absy absolute)
  (list #xB9 (high-byte absolute) (low-byte absolute)))

(define-syntax (LDA stx)
  (syntax-case stx ()
    [(LDA op)
     (if (equal? (substring (syntax-e #'op) 0 1) "#")
         #'(LDA_i (parse-number-string (substring op 1)))
         (if (>= 5 (syntax-span #'op))
             #'(LDA_zp (parse-number-string op))
             #'(LDA_abs (parse-number-string op))))]
    [(LDA op, idx)
     (let ((indirect (syntax-e #'idx)))
       (if (>= 5 (syntax-span #'op))
           (case indirect
             [(x) #'(LDA_zpx (parse-number-string op))]
             [else (error "lda zero page index mode unknown" indirect)])
           (case indirect
             [(x) #'(LDA_absx (parse-number-string op))]
             [(y) #'(LDA_absy (parse-number-string op))]
             [else (error "lda absolute index mode unknown" indirect)])))]))

(check-match (LDA "#$10")
             '(#xA9 16))

(check-match (LDA "$17")
             '(#xa5 #x17))

(check-match (LDA "$178F")
             '(#xad #x17 #x8F))

(check-match (LDA "$10",x)
             '(#xB5 16))

(check-match (LDA "$A000",x)
             '(#xBD #xA0 #x00))

(check-match (LDA "$A000",y)
             '(#xB9 #xA0 #x00))

;; ================================================================================ ADC

(define (ADC_i immediate)
  (list #x69 immediate))

(define (ADC_zpx zero-page-address)
  (list #x75 zero-page-address))

(define (ADC_absx absolute)
  (list #x7D (high-byte absolute) (low-byte absolute)))

(define (ADC_absy absolute)
  (list #x79 (high-byte absolute) (low-byte absolute)))

(define (ADC_zp zero-page-address)
  (list #x65 zero-page-address))

(define (ADC_abs absolute)
  (list #x6D (high-byte absolute) (low-byte absolute)))

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
             '(#x75 2))
(check-match (ADC "$1237",y)
             '(#x79 #x12 #x37))
(check-match (ADC "#100")
             '(#x69 100))
(check-match (ADC "#$FF")
             '(#x69 #xFF))
(check-match (ADC "$FF")
             '(#x65 #xFF))
(check-match (ADC "$FFFF")
             '(#x6d #xff #xff))


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
