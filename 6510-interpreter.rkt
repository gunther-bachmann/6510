#lang at-exp racket

;; todo: implement compare, increment (x,y), and branch commands (using flags correctly)
;; todo: check whether lense implementation is better (more efficient) than struct-copy (see https://docs.racket-lang.org/lens/struct-guide.html)
;; reference: see c64os.com/post/6502instructions

;; (require (only-in racket/fixnum make-fxvector fxvector-ref fxvector-set!))
(require (only-in threading ~>>))
(require "6510-utils.rkt")
(require scribble/srcdoc)
(require (for-doc scribble/base scribble/manual))
(require data/pvector)
(require data/collection)

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
  (cpu-state 0 0 (make-pvector 65536 0) 0 0 0 #xff))

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
  (nth (cpu-state-memory state) memory-address))

;; set the byte at the given memory address (TODO replace with pvector pendant)
(define (poke state address value)
  (struct-copy cpu-state state 
               [memory (set-nth (cpu-state-memory state)
                                (word address)
                                (byte value))]))

(define (byte->hex-string num)
  (~a (number->string num 16)
      #:width 2 #:left-pad-string "0" #:align 'right))

(define (word->hex-string num)
  (~a (number->string num 16)
      #:width 4 #:left-pad-string "0" #:align 'right))

;; create a string formated with 'address byte+0 byte+1 ... byte+15' per line
(define (dump-memory from to state)
  (string-join
   (stream->list
    (map (lambda (it) (string-join
                  (stream->list
                   (append (list (word->hex-string (+ from (caar (stream->list it)))))
                           (map (lambda (pair) (cdr pair)) it)))
                  " "))
         (chunk 16
                 (indexed
                  (map (lambda (idx) (byte->hex-string (peek state idx)))
                         (range from (+ 1 to)))))))
   "\n"))

(module+ test #| dump-memory |#
  (check-equal? (dump-memory 266 286 (poke (initialize-cpu) #x10C #xFE))
                "010a 00 00 fe 00 00 00 00 00 00 00 00 00 00 00 00 00\n011a 00 00 00 00 00")
  (check-equal? (dump-memory 268 268 (poke (initialize-cpu) #x10C #xFE))
                "010c fe"))

(define (print-state state)
  (printf "A  = x~a,   " (byte->hex-string (cpu-state-accumulator state)))
  (printf " X = x~a, " (byte->hex-string (cpu-state-x-index state)))
  (printf "Y = x~a~n" (byte->hex-string (cpu-state-y-index state)))
  (printf "PC = x~a, " (word->hex-string (cpu-state-program-counter state)))
  (printf "SP = x~a~n" (byte->hex-string (cpu-state-stack-pointer state)))
  (printf "N=~a, O=~a, B=~a, D=~a, I=~a, Z=~a, C=~a"
          (if (negative-flag? state) "X" "_" )
          (if (overflow-flag? state) "X" "_" )
          (if (break-flag? state) "X" "_" )
          (if (decimal-flag? state) "X" "_" )
          (if (interrupt-flag? state) "X" "_" )
          (if (zero-flag? state) "X" "_" )
          (if (carry-flag? state) "X" "_" )))

(module+ test #| set-nth |#
  (check-equal? (nth (set-nth (cpu-state-memory (initialize-cpu)) 65535 1)
                     65535)
                1))

(module+ test #| peek and poke |#
  (check-match (peek (poke (initialize-cpu) #xc000 17) #xc000)
               17))

;; load program into memory using the 6510 state
(define (6510-load state memory-address program)
  (foldl (lambda (state pair) 
           (poke state (first pair) (last pair)))
         state 
         (map list
              (sequence->list (in-range memory-address (+ memory-address (length program))))
              program)))

(module+ test #| 6510-load |#
  (check-equal? (dump-memory 10 13 (6510-load (initialize-cpu) 10 (list #x00 #x10 #x00 #x11)))
             "000a 00 10 00 11"
             "load will put all bytes into memory"))

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

;; execute if pc does not point at a 0 byte (brk)
(define (run state)
  (if  (eq? 0 (peek-pc state))
      state
      (let ((next-state (execute-cpu-step state)))
        (run next-state))))

;; interpret the RTS (return from subroutine) command
(define (interpret-rts state)
  (let* ([sp (cpu-state-stack-pointer state)]
         [low-ret (peek state (+ #x100 (byte (+ 2 sp))))]
         [high-ret (peek state (+ #x100 (byte (+ 1 sp))))])
    (struct-copy cpu-state state
                                 [program-counter (word (+ 1 (absolute high-ret low-ret)))]
                                 [stack-pointer (byte (+ sp 2))])))

;; interpret JSR absolute (jump to subroutine) command
;; mock kernel function FFD2 to print a string
(define (interpret-jsr-abs high low state)
  (case (absolute high low)
    [(#xFFD2) (display (string (integer->char (cpu-state-accumulator state))))
              (struct-copy cpu-state state [program-counter (word (+ 3 (cpu-state-program-counter state)))])]
    [else (let* ([new-program-counter (absolute high low)]
                 [return-address (word (+ 2 (cpu-state-program-counter state)))]
                 [sp (cpu-state-stack-pointer state)]
                 [new-state (struct-copy cpu-state state
                                         [program-counter (word new-program-counter)]
                                         [stack-pointer (byte (- sp 2))])])
            (~>> new-state
                (poke _ (+ #x100 sp) (low-byte return-address))
                (poke _ (+ #x100 (byte (- sp 1))) (high-byte return-address))))]))

(module+ test #| jsr (jump to sub routine) |#
  (check-equal? (cpu-state-program-counter
                 (interpret-jsr-abs #x40 #x08 (set-pc-in-state (initialize-cpu) #x2001)))
                #x4008)
  (check-equal? (cpu-state-stack-pointer
                 (interpret-jsr-abs #x40 #x08 (set-pc-in-state (initialize-cpu) #x2001)))
                #xfd)
  (check-equal? (peek
                 (interpret-jsr-abs #x40 #x08 (set-pc-in-state (initialize-cpu) #x2001))
                 #x1FF)
                #x03)  
  (check-equal? (peek
                 (interpret-jsr-abs #x40 #x08 (set-pc-in-state (initialize-cpu) #x2001))
                 #x1FE)
                #x20))

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


(define (-push-on-stack value state)
  (let* ((old-sp (cpu-state-stack-pointer state)))
    (struct-copy cpu-state state
                 [stack-pointer (byte (- old-sp 1))]
                 [memory (set-nth (cpu-state-memory state)
                                  (word (+ #x100 old-sp))
                                  (byte value))])))
(define (-pop-from-stack state)
  (let* ((new-sp (byte (+ 1 (cpu-state-stack-pointer state))))
         (value (peek state (word (+ #x100 new-sp)))))
    `(,(struct-copy cpu-state state
                    [stack-pointer new-sp]) . ,value)))

(module+ test #| push and pop |#
  (check-eq? (cdr (-pop-from-stack (-push-on-stack #x56 (initialize-cpu))))
             #x56))

(define (interpret-rti state)
  (let* ((old-sp (cpu-state-stack-pointer state))
         (new-status-byte (peek state (+ #x101 old-sp)))
         (new-program-counter (absolute (peek state (+ #x102 old-sp))
                                        (peek state (+ #x103 old-sp)))))
    (struct-copy cpu-state state
                 [stack-pointer (byte (+ old-sp 3))]
                 [flags new-status-byte]
                 [program-counter new-program-counter])))

(module+ test #| rti |#
  (check-eq? (cpu-state-stack-pointer (interpret-rti (interpret-brk (initialize-cpu))))
             #xFF)
  (check-eq? (cpu-state-program-counter (interpret-rti (set-pc-in-state (interpret-brk (initialize-cpu)) #xABCD)))
             #x0000)
  (check-eq? (cpu-state-program-counter (interpret-rti (interpret-brk (set-pc-in-state (initialize-cpu) #xABCD))))
             #xABCD))

(define (interpret-brk state)
  (let* ((old-status-byte (cpu-state-flags state))
         (old-pc (cpu-state-program-counter state)))
    (~>>
     (struct-copy cpu-state state
                  [program-counter (absolute (peek state #xFFFE) (peek state #xFFFF))]
                  [flags (-set-brk-flag (cpu-state-flags state))])
     (-push-on-stack (low-byte old-pc) _)
     (-push-on-stack (high-byte old-pc) _)
     (-push-on-stack old-status-byte _))))

(module+ test #| brk |#
  (check-eq? (cpu-state-program-counter
              (interpret-brk (~>> (initialize-cpu)
                                 (poke _ #xFFFE #x01)
                                 (poke _ #xFFFF #x02))))
             #x0102
             "ensure brk will continue at adress $(FFFE)")
  (check-eq? (cpu-state-stack-pointer
              (interpret-brk (~>> (initialize-cpu))))
             (- #xFF 3)
             "ensure SP is reduced by 3"))

;; flags N O - B D I Z C
;;       negative               : result is negative (2 complements)
;;        overflow              : result produces an over/underflow
;;         unused               : unused
;;          break               : set when executing BRK (0x00)
;;           decimal            : when set, arithmetic is interpreted as bcd arithmetic
;;            interrupt disable : set to prevent interrupts
;;             zero             : result is zero
;;              carry           : carry over result bit otherwise lost
(define (carry-flag? state)
  (eq? #x01 (bitwise-and #x01 (cpu-state-flags state))))

(define (zero-flag? state)
  (eq? #x02 (bitwise-and #x02 (cpu-state-flags state))))

(define (interrupt-flag? state)
  (eq? #x04 (bitwise-and #x04 (cpu-state-flags state))))

(define (decimal-flag? state)
  (eq? #x08 (bitwise-and #x08 (cpu-state-flags state))))

(define (break-flag? state)
  (eq? #x10 (bitwise-and #x10 (cpu-state-flags state))))

(define (overflow-flag? state)
  (eq? #x40 (bitwise-and #x40 (cpu-state-flags state))))

(define (negative-flag? state)
  (eq? #x80 (bitwise-and #x80 (cpu-state-flags state))))

(define (-set-carry-flag flags)
  (bitwise-xor #x01 flags))

(define (-clear-carry-flag flags)
  (bitwise-and #xfe flags))

(define (-set-zero-flag flags)
  (bitwise-xor #x02 flags))

(define (-set-brk-flag flags)
  (bitwise-xor #x10 flags))

(define (-clear-brk-flag flags)
  (bitwise-and #xEF flags))

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


;; flags N O - B D I Z C

(define (interpret-clc state)
  (struct-copy cpu-state state
               [flags (-clear-carry-flag (cpu-state-flags state))]))

(define (interpret-sec state)
  (set-carry-flag state))

(define (interpret-cli state)
  (struct-copy cpu-state state
               [flags (-clear-interrupt-flag (cpu-state-flags state))]))

(define (interpret-sei state)
  (struct-copy cpu-state state
               [flags (-set-interrupt-flag (cpu-state-flags state))]))

(define (interpret-clv state)
  (struct-copy cpu-state state
               [flags (-clear-overflow-flag (cpu-state-flags state))]))

(define (interpret-cld state)
  (struct-copy cpu-state state
               [flags (-clear-decimal-flag (cpu-state-flags state))]))

(define (interpret-sed state)
  (struct-copy cpu-state state
               [flags (-set-decimal-flag (cpu-state-flags state))]))

(module+ test #| flags |#
  (check-true (carry-flag? (interpret-sec (initialize-cpu))))
  (check-false (carry-flag? (interpret-clc (interpret-sec (initialize-cpu)))))
  (check-true (interrupt-flag? (interpret-sei (initialize-cpu))))
  (check-false (interrupt-flag? (interpret-cli (interpret-sei (initialize-cpu)))))
  (check-true (decimal-flag? (interpret-sed (initialize-cpu))))
  (check-false (decimal-flag? (interpret-cld (interpret-sed (initialize-cpu)))))
  (check-true (overflow-flag? (interpret-adc-i 1 (interpret-adc-i #x7f (initialize-cpu)))))
  (check-false (overflow-flag? (interpret-clv (interpret-adc-i 1 (interpret-adc-i #x7f (initialize-cpu)))))))

;; execute one cpu opcode and return the next state
(define (execute-cpu-step state)
  (case (peek-pc state)
    [(#x00) (interpret-brk state)]
    [(#x18) (interpret-clc state)]
    [(#x20) (interpret-jsr-abs (peek-pc+2 state) (peek-pc+1 state) state)]
    [(#x38) (interpret-sec state)]
    [(#x40) (interpret-rti state)]
    [(#x4C) (interpret-jmp-abs (peek-pc+2 state) (peek-pc+1 state) state)]
    [(#x58) (interpret-cli state)]
    [(#x60) (interpret-rts state)]
    [(#x69) (interpret-adc-i (peek-pc+1 state) state)]
    [(#x78) (interpret-sei state)]
    [(#xA9) (interpret-lda-i (peek-pc+1 state) state)]
    [(#xB9) (interpret-clv state)]
    [(#xD0) (interpret-bne-rel (peek-pc+1 state) state)]
    [(#xD8) (interpret-cld state)]
    [(#xF8) (interpret-sed state)]
    [else (error "unknown opcode")]))

;; interpret bne (branch on not equal)
(define (interpret-bne-rel rel state)
  (let* ([pc (cpu-state-program-counter state)]
         [new-pc-on-jump (+ pc 2 (if (>= #x80 rel) (- 256 rel) rel))]
         [new-pc-no-jump (+ pc 2)]
         [new-pc (if (zero-flag? state) new-pc-no-jump new-pc-on-jump)])
    (struct-copy cpu-state state
                 [program-counter new-pc])))

;; put the raw bytes into memory (at org) and start running at org
(define (run-interpreter org raw-bytes)
  (displayln (format "loading program into interpreter at ~a" org))
  (define state (6510-load (initialize-cpu) org raw-bytes))
  (displayln "program execution:")
  (let ([_ (run (set-pc-in-state state org))])
    (void)))
