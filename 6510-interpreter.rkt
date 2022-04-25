#lang at-exp racket

;; todo: implement compare, increment (x,y), and branch commands (using flags correctly)
;; todo: check whether lense implementation is better (more efficient) than struct-copy (see https://docs.racket-lang.org/lens/struct-guide.html)
;; todo: check for mutation friendly persistent vector data, see (https://docs.racket-lang.org/pvector/index.html)

(require (only-in racket/fixnum make-fxvector fxvector-ref fxvector-set!))
(require (only-in threading ~>>))
(require "6510-utils.rkt")
(require scribble/srcdoc)
(require (for-doc scribble/base scribble/manual))

(module+ test 
  (require rackunit))

(provide run-interpreter reset-cpu initialize-cpu peek poke run set-pc-in-state 6510-load)

(struct cpu-state (program-counter ;; pointer to current program execution (16 bit)
                   flags           ;; flag register (8 bit)
                   memory          ;; 64kB memory byte vector
                   accumulator     ;; accumulator register (8 bit)
                   x-index         ;; x index register (8 bit)
                   y-index         ;; y index register (8 bit)
                   stack-pointer   ;; current stack pointer (8+1 bit) 1xx
                   ))
(provide (struct-doc cpu-state ([program-counter any/c]
                                [flags any/c]
                                [memory any/c]
                                [accumulator any/c]
                                [x-index any/c]
                                [y-index any/c]
                                [stack-pointer any/c]) @{Doc test}))

;; flags all negative, program counter at 0, registers all 0, sp = 0xFF
(define (initialize-cpu)
  (cpu-state 0 0 (make-fxvector 65536) 0 0 0 #xff))

;; execute a reset on the cpu 
(define (reset-cpu state)
  (let* ([new-pc (absolute (peek state #xFFFC) (peek state #xFFFD))])
    (struct-copy cpu-state state
                 [program-counter new-pc])))

;; documentation test with testfun
(define (testfun a)
  a)

(provide
 (proc-doc/names
  testfun
  (-> number? any/c)
  (a)
  @{Doc test}
  ))


;; give the byte at the given memory-address
(define (peek state memory-address)
  (fxvector-ref (cpu-state-memory state) memory-address))

;; set the byte at the given memory address (TODO replace with pvector pendant)
(define (poke state address value)
  (fxvector-set! (cpu-state-memory state) (word address) (byte value))
  state)

(module+ test #| peek and poke |#
  (check-match (peek (poke (initialize-cpu) #xc000 17) #xc000)
               17))

;; load program into memory using the 6510 state
(define (6510-load state memory-address program)
  (map (lambda (pair) (fxvector-set! (cpu-state-memory state) (first pair) (last pair)))
       (map list
            (sequence->list (in-range memory-address (+ memory-address (length program))))
            program))
  state)

(module+ test #| 6510-load |#
  (check-eq? (peek (6510-load (initialize-cpu) 10 (list #x00 #x10 #x00 #x11)) 11)
             16
             "immediate operand 1 is $10 = 16"))

;; peek into memory at the location the program counter points to (current point of execution)
(define (peek-pc state)
  (peek state (cpu-state-program-counter state)))

;; peek into memory at the location the program counter+1 points to (current point of execution+1)
(define (peek-pc+1 state)
  (peek state (+ 1 (cpu-state-program-counter state))))

;; peek into memory at the location the program counter+2 points to (current point of execution+2)
(define (peek-pc+2 state)
  (peek state (+ 2 (cpu-state-program-counter state))))

;; return state with modified program counter
(define (set-pc-in-state state pc)
  (struct-copy cpu-state state [program-counter (word pc)]))

;; execute if pc does not point at a 0 byte
(define (run state)
  (if (not (eq? 0 (peek-pc state)))
      (run (execute-cpu-step state))
      state))

;; interpret the RTS (return to subroutine) command
(define (interpret-rts state)
  (let* ([sp (cpu-state-stack-pointer state)]
         [low-ret (peek state (+ #x100 (byte (+ 2 sp))))]
         [high-ret (peek state (+ #x100 (byte (+ 1 sp))))]
         [new-state (struct-copy cpu-state state
                                 [program-counter (word (+ 1 (absolute high-ret low-ret)))]
                                 [stack-pointer (byte (+ sp 2))])])
    new-state))

;; interpret JSR absolute (jump to subroutine) command
;; mock kernel function FFD2 to print a string
(define (interpret-jsr-abs high low state)
  (case (absolute high low)
    [(#xFFD2) (display (string (integer->char (cpu-state-accumulator state))))
              (struct-copy cpu-state state [program-counter (word (+ 3 (cpu-state-program-counter state)))])]
    [else (let* ([new-program-counter (absolute high low)]
                 [return-address (+ 2 (cpu-state-program-counter state))]
                 [sp (cpu-state-stack-pointer state)]
                 [new-state (struct-copy cpu-state state
                                         [program-counter (word new-program-counter)]
                                         [stack-pointer (byte (- sp 2))])])
            (poke new-state (+ #x100 sp) (low-byte return-address))
            (poke new-state (+ #x100 (byte (- sp 1))) (high-byte return-address))
            new-state)]))

;; set/clear carry flag
(define (-adjust-carry-flag set flags)
  (if set
      (-set-carry-flag flags)
      (-clear-carry-flag flags)))

(module+ test #| -adjust-carry-flag |#
  (check-eq? (-adjust-carry-flag #t 0)
             1)
  (check-eq? (-adjust-carry-flag null 1)
             0))

;; set/clear zero flag
(define (-adjust-zero-flag set flags)
  (if set
      (-set-zero-flag flags)
      (-clear-zero-flag flags)))

;; set/clear overflow flag
(define (-adjust-overflow-flag set flags)
  (if set
      (-set-overflow-flag flags)
      (-clear-overflow-flag flags)))

;; set/clear negative flag
(define (-adjust-negative-flag set flags)
  (if set
      (-set-negative-flag flags)
      (-clear-negative-flag flags)))

;; interpret JMP absolute (jump)
(define (interpret-jmp-abs high low state)
  (struct-copy cpu-state state
               [program-counter (word (absolute high low))]))

;; derive overflow by looking at accumulator, operand and result
(define (derive-overflow acc oper new-acc)
  (let* ([input-has-same-sign (bitwise-not (bitwise-and #x80 (bitwise-xor acc oper)))]
         [in-out-has-different-sign (bitwise-and #x80 (bitwise-xor acc new-acc))])
    (not (zero? (bitwise-and input-has-same-sign in-out-has-different-sign)))))

;; interprets numbers a two complements
(define (derive-negative acc)
  (not (zero? (bitwise-and 128 (byte acc)))))

(module+ test #| derive-overlow |#
  (check-false (derive-overflow #x50 #x10 #x60))
  (check-true (derive-overflow #x50 #x50 #xa0))
  (check-false (derive-overflow #x50 #x90 #xe0))
  (check-false (derive-overflow #x50 #xd0 #x120))
  (check-false (derive-overflow #x50 #x10 #x60))
  (check-false (derive-overflow #xd0 #x10 #xe0))
  (check-false (derive-overflow #xd0 #x50 #x120))
  (check-true (derive-overflow #xd0 #x90 #x160))
  (check-false (derive-overflow #xd0 #xd0 #x1a0)))

;; interpret ADC immediate (add with carry)
;; c is set if result > 255
;; v is set if two complements interpretation produces overflow (e.g. 127 + 1 = $80 = two complements -127)
;; n is set if result is negative
;; z is set if result is zero (TODO check if this is also true for results with carry)
(define (interpret-adc-i immediate state)
  (let* ([old-accumulator (cpu-state-accumulator state)]
         [intermediate-accumulator (+ immediate old-accumulator)]
         [raw-new-accumulator (if (carry-flag? state) (+ 1 intermediate-accumulator) intermediate-accumulator)]
         [new-accumulator (byte raw-new-accumulator)]
         [carry?          (> raw-new-accumulator 255 )]
         [zero?           (zero? new-accumulator)]
         [negative?       (derive-negative raw-new-accumulator)]
         [overflow?       (derive-overflow old-accumulator immediate raw-new-accumulator)])
    (struct-copy cpu-state state
                 [program-counter (word (+ 2 (cpu-state-program-counter state)))]
                 [flags           (~>> (cpu-state-flags state)
                                      (-adjust-carry-flag carry?)
                                      (-adjust-zero-flag zero?)
                                      (-adjust-negative-flag negative?)
                                      (-adjust-overflow-flag overflow?))]
                 [accumulator     new-accumulator])))

(module+ test #| interpret-adc-i - checking carry flag related|#
  (check-equal? (cpu-state-accumulator (interpret-adc-i 10 (initialize-cpu)))
                10)

  (check-false (carry-flag? (interpret-adc-i 10 (set-carry-flag (initialize-cpu)))))
  (check-equal? (cpu-state-accumulator (interpret-adc-i 10 (set-carry-flag (initialize-cpu))))
                11)
  (check-true (carry-flag? (interpret-adc-i 10 (-set-accumulator 246 (initialize-cpu)))))
  (check-false (carry-flag? (interpret-adc-i 10 (-set-accumulator 245 (initialize-cpu))))))

(module+ test #| interpret-adc-i - checking zero flag related|#
  (check-true (zero-flag? (interpret-adc-i 10 (-set-accumulator 246 (initialize-cpu)))))
  (check-false (zero-flag? (interpret-adc-i 10 (-set-accumulator 245 (initialize-cpu)))))
  (check-true (zero-flag? (interpret-adc-i 10 (set-carry-flag (-set-accumulator 245 (initialize-cpu)))))))

(module+ test #| interpret-adc-i - checking overflow flag related|#
  (check-true (overflow-flag? (interpret-adc-i 10 (-set-accumulator 118 (initialize-cpu)))))
  (check-true (overflow-flag? (interpret-adc-i 10 (-set-accumulator 120 (initialize-cpu)))))
  (check-false (overflow-flag? (interpret-adc-i 10 (-set-accumulator 117 (initialize-cpu)))))
  (check-false (overflow-flag? (interpret-adc-i 10 (-set-accumulator 20 (initialize-cpu)))))

  (check-true (overflow-flag? (interpret-adc-i 1 (-set-accumulator 127 (initialize-cpu)))))
  (check-false (overflow-flag? (interpret-adc-i 1 (-set-accumulator 126 (initialize-cpu))))))

(module+ test #| interpret-adc-i - checking negative flag related|#
  (check-true (negative-flag?  (interpret-adc-i 1 (-set-accumulator (two-complement-of -2) (initialize-cpu)))))
  (check-false (negative-flag? (interpret-adc-i 1 (-set-accumulator (two-complement-of -1) (initialize-cpu)))))
  (check-false (negative-flag? (interpret-adc-i 1 (-set-accumulator (two-complement-of 1) (initialize-cpu)))))
  (check-false (negative-flag? (interpret-adc-i 1 (-set-accumulator (two-complement-of 0) (initialize-cpu))))))

;; return a state with accumulator set
(define (-set-accumulator new-accumulator state)
  (struct-copy cpu-state state
               [accumulator (byte new-accumulator)]))

;; interpret lda (load accumulator immediate)
(define (interpret-lda-i immediate state)
  (struct-copy cpu-state state
               [program-counter (word (+ 2 (cpu-state-program-counter state)))]
               [accumulator     (byte immediate)]))

(module+ test #| lda immediate |#
  (check-equal? (cpu-state-accumulator (interpret-lda-i 10 (initialize-cpu)))
                10)
  (check-equal? (cpu-state-accumulator (interpret-lda-i 0 (initialize-cpu)))
                0)
  (check-equal? (cpu-state-accumulator (interpret-lda-i 255 (initialize-cpu)))
                255))

(define (zero-flag? state)
  (eq? #x02 (bitwise-and #x02 (cpu-state-flags state))))

(define (carry-flag? state)
  (eq? 1 (bitwise-and 1 (cpu-state-flags state))))

(define (interrupt-flag? state)
  (eq? #x04 (bitwise-and #x04 (cpu-state-flags state))))

(define (decimal-flag? state)
  (eq? #x08 (bitwise-and #x08 (cpu-state-flags state))))

(define (negative-flag? state)
  (eq? #x80 (bitwise-and #x80 (cpu-state-flags state))))

(define (overflow-flag? state)
  (eq? #x40 (bitwise-and #x40 (cpu-state-flags state))))

(define (-set-carry-flag flags)
  (bitwise-xor 1 flags))

(define (-clear-carry-flag flags)
  (bitwise-and #xfe flags))

(define (-set-zero-flag flags)
  (bitwise-xor 2 flags))

(define (-clear-zero-flag flags)
  (bitwise-and #xfd flags))

(define (-set-overflow-flag flags)
  (bitwise-xor #x40 flags))

(define (-clear-overflow-flag flags)
  (bitwise-and #xbf flags))

(define (-set-negative-flag flags)
  (bitwise-xor #x80 flags))

(define (-clear-negative-flag flags)
  (bitwise-and #x7f flags))

(define (-set-interrupt-flag flags)
  (bitwise-xor #x04 flags))

(define (-clear-interrupt-flag flags)
  (bitwise-and #xfb flags))

(define (-set-decimal-flag flags)
  (bitwise-xor #x08 flags))

(define (-clear-decimal-flag flags)
  (bitwise-and #xf7 flags))

(define (set-carry-flag state)
  (struct-copy cpu-state state [flags (-set-carry-flag (cpu-state-flags state))]))

(define (print-state state)
  (printf "A = ~a~n" (cpu-state-accumulator state))
  (printf "X = ~a~n" (cpu-state-x-index state))
  (printf "Y = ~a~n" (cpu-state-y-index state))
  (printf "SP = ~a~n" (cpu-state-stack-pointer state))
  (printf "PC = ~a~n" (cpu-state-program-counter state))
  (printf "C = ~s, Z = ~s" (if (carry-flag? state) "X" " " ) (if (zero-flag? state) "X" " " )))

;; execute one cpu opcode and return the next state
(define (execute-cpu-step state)
  (case (peek-pc state)
    [(#x20) (interpret-jsr-abs (peek-pc+2 state) (peek-pc+1 state) state)]
    [(#x4C) (interpret-jmp-abs (peek-pc+2 state) (peek-pc+1 state) state)]
    [(#x60) (interpret-rts state)]
    [(#x69) (interpret-adc-i (peek-pc+1 state) state)]
    [(#xA9) (interpret-lda-i (peek-pc+1 state) state)]
    [(#xD0) (interpret-bne-rel (peek-pc+1 state) state)]
    [else (error "unknown opcode")]))

;; interpret bne (branch on not equal)
(define (interpret-bne-rel rel state)
  (let* ([pc (cpu-state-program-counter state)]
         [new-pc-on-jump (+ pc 2 (if (>= #x80 rel) (- 256 rel) rel))]
         [new-pc-no-jump (+ pc 2)]
         [new-pc (if (zero-flag? (cpu-state-flags state)) new-pc-no-jump new-pc-on-jump)])
    (struct-copy cpu-state state
                 [program-counter new-pc])))

;; put the raw bytes into memory (at org) and start running at org
(define (run-interpreter org raw-bytes)
  (displayln (format "loading program into interpreter at ~a" org))
  (define state (6510-load (initialize-cpu) org raw-bytes))
  (displayln "program execution:")
  (let ([_ (run (set-pc-in-state state org))])
    (void)))
