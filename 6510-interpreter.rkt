#lang at-exp racket

;; todo: check whether lense implementation is better (more efficient) than struct-copy (see https://docs.racket-lang.org/lens/struct-guide.html)
;; reference: see c64os.com/post/6502instructions
;; or: https://www.middle-engine.com/blog/posts/2020/06/23/programming-the-nes-the-6502-in-detail
;; control characters: https://www.c64-wiki.com/wiki/control_character

;; (require (only-in racket/fixnum make-fxvector fxvector-ref fxvector-set!))
(require (only-in threading ~>>))
(require "6510-utils.rkt")
(require scribble/srcdoc)
(require (for-doc scribble/base scribble/manual))
(require data/pvector)
(require data/collection)
(require racket/fixnum)
(require (rename-in  racket/contract [define/contract define/c]))

(module+ test #| include rackunit |#
  (require rackunit))

(provide print-state run-interpreter reset-cpu initialize-cpu peek poke run with-program-counter 6510-load byte->hex-string word->hex-string)

(define/c (in-word-range? word)
  (-> exact-integer? boolean?)
  (and (<= word 65535) (>= word 0)))

(define/c (in-byte-range? byte)
  (-> exact-integer? boolean?)
  (and (<= byte 255) (>= byte 0)))

(define byte/c (and/c exact-nonnegative-integer? in-byte-range?))

(define word/c (and/c exact-nonnegative-integer? in-word-range?))

(struct cpu-state (program-counter ;; pointer to current program execution (16 bit)
                   flags           ;; flag register (8 bit)
                   memory          ;; 64kB memory byte vector
                   accumulator     ;; accumulator register (8 bit)
                   x-index         ;; x index register (8 bit)
                   y-index         ;; y index register (8 bit)
                   stack-pointer)  ;; current stack pointer (8+1 bit) 1xx
  #:guard (struct-guard/c
           word/c
           byte/c
           pvector?
           byte/c
           byte/c
           byte/c
           byte/c))

(provide (struct-doc cpu-state ([program-counter word/c]
                                [flags byte/c]
                                [memory byte/c]
                                [accumulator byte/c]
                                [x-index byte/c]
                                [y-index byte/c]
                                [stack-pointer byte/c]) @{Doc test}))

;; flags all negative, program counter at 0, registers all 0, sp = 0xFF
(define/c (initialize-cpu)
  (-> cpu-state?)
  (cpu-state 0 0 (make-pvector 65536 0) 0 0 0 #xff))

;; return state with program-counter set to word
(define/c (with-program-counter state word)
  (-> cpu-state? word/c cpu-state?)
  (struct-copy cpu-state state
               [program-counter word]))

(module+ test #| with-program-counter |#
  (check-exn exn:fail:contract? (lambda () (with-program-counter (initialize-cpu) #xfffff)))
  (check-eq? (~>> (initialize-cpu)
                 (with-program-counter _ #xff00)
                 (cpu-state-program-counter _))
             #xff00))

;; return state with flags set to byte
(define/c (with-flags state byte)
  (-> cpu-state? byte/c cpu-state?)
  (struct-copy cpu-state state
               [flags byte]))

;; return state with accumulator set to byte
(define/c (with-accumulator state byte)
  (-> cpu-state? byte/c cpu-state?)
  (struct-copy cpu-state state
               [accumulator byte]))

(module+ test #| with-accumulator contract checks |#
  (check-exn exn:fail:contract? (lambda () (with-accumulator (initialize-cpu) 256)))
  (check-exn exn:fail:contract? (lambda () (with-accumulator (initialize-cpu) -1)))
  (check-eq? (cpu-state-accumulator (with-accumulator (initialize-cpu) 255))
             255)
  (check-eq? (cpu-state-accumulator (with-accumulator (initialize-cpu) 0))
             0))

;; return state with x-index set to byte
(define/c (with-x-index state byte)
  (-> cpu-state? byte/c cpu-state?)
  (struct-copy cpu-state state
               [x-index byte]))

;; return state with y-index set to byte
(define/c (with-y-index state byte)
  (-> cpu-state? byte/c cpu-state?)
  (struct-copy cpu-state state
               [y-index byte]))

;; return program-counter incremented by delta
(define/c (next-program-counter state delta)
  (-> cpu-state? exact-integer? word/c)
  (word (fx+ delta (cpu-state-program-counter state))))

;; execute a reset on the cpu 
(define/c (reset-cpu state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state
               [program-counter (peek-word-at-address state #xFFFC)]))

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

;; is bit7 set in value?
(define/c (bit7? value)
  (-> exact-nonnegative-integer? boolean?)
  (not (not-bit7? value)))

;; is bit0 set in value?
(define/c (bit0? value)
  (-> exact-nonnegative-integer? boolean?)
  (not (not-bit0? value)))

;; is bit7 blank in value?
(define/c (not-bit7? value)
  (-> exact-nonnegative-integer? boolean?)
  (zero? (bitwise-and #x80 value)))

;; is bit0 blank in value?
(define/c (not-bit0? value)
  (-> exact-nonnegative-integer? boolean?)
  (zero? (bitwise-and #x1 value)))

(define peeker/c (-> cpu-state? byte/c))

(define poker/c (-> cpu-state? byte/c cpu-state?))

;; give the byte at the given memory-address
(define/c (peek state memory-address)
  (-> cpu-state? word/c byte/c)
  (nth (cpu-state-memory state) memory-address))

;; set the byte at the given memory address (TODO replace with pvector pendant)
(define/c (-poke state address value)
  (-> cpu-state? word/c byte/c cpu-state?)
  (struct-copy cpu-state state 
               [memory (set-nth (cpu-state-memory state)
                                (word address)
                                (byte value))]))

;; poke values starting at address into memory
(define/c (-pokem state address values)
  (-> cpu-state? word/c (listof byte/c) cpu-state?)
  (if (empty? values)
      state
      (-pokem (-poke state address (car values))
              (word (fx+ 1 address))
              (cdr values))))

;; poke values starting at address into memory
(define/c (poke state address . values)
  (->* (cpu-state? word/c) () #:rest (listof byte/c) cpu-state?)
  (-pokem state address values))

;; peek byte at current stack pointer
(define/c (peek-stack state)
  (-> cpu-state? byte/c) 
  (peek state
        (fx+ #x100 (cpu-state-stack-pointer state))))

;; peek byte at current stack pointer + 1 (the value that would be popped off the stack)
(define/c (peek-stack+1 state)
  (-> cpu-state? byte/c) 
  (peek state
        (fx+ #x100 (byte (fx+ 1 (cpu-state-stack-pointer state))))))

;; peek byte at current stack pointer + 2 (the value that would be popped second off the stack)
(define/c (peek-stack+2 state)
  (-> cpu-state? byte/c) 
  (peek state
        (fx+ #x100 (byte (fx+ 2 (cpu-state-stack-pointer state))))))

;; peek byte at current stack pointer + 3 (the value that would be popped third off the stack)
(define/c (peek-stack+3 state)
  (-> cpu-state? byte/c) 
  (peek state
        (fx+ #x100 (byte (fx+ 3 (cpu-state-stack-pointer state))))))

;; put value onto the stack (w/o adjusting the stack pointer)
(define/c (poke-stack state value)
  (-> cpu-state? byte/c cpu-state?) 
  (poke state
        (fx+ #x100 (cpu-state-stack-pointer state))
        value))

;; put the second value onto the stack (w/o adjusting the stack pointer)
(define/c (poke-stack-1 state value)
  (-> cpu-state? byte/c cpu-state?) 
  (poke state
        (fx+ #x100 (byte (fx- (cpu-state-stack-pointer state) 1)))
        value))

;; transform a byte to a 2 digit hex string
(define/c (byte->hex-string num)
  (-> byte/c string?)
  (~a (number->string num 16)
      #:width 2 #:left-pad-string "0" #:align 'right))

;; transform a word (2 bytes) to a 4 digit hex string
(define/c (word->hex-string num)
  (-> word/c string?)
  (~a (number->string num 16)
      #:width 4 #:left-pad-string "0" #:align 'right))

(module+ test #| byte->hex-string, word->hex-string |#
  (check-equal? (byte->hex-string #x00) "00")
  (check-equal? (byte->hex-string #x01) "01")
  (check-equal? (byte->hex-string #x7f) "7f")
  (check-equal? (byte->hex-string #x80) "80")
  (check-equal? (byte->hex-string #x81) "81")
  (check-equal? (byte->hex-string #xa0) "a0")
  (check-equal? (byte->hex-string #xff) "ff")
  (check-equal? (word->hex-string #x0000) "0000")
  (check-equal? (word->hex-string #x0001) "0001")
  (check-equal? (word->hex-string #x0020) "0020")
  (check-equal? (word->hex-string #x0300) "0300")
  (check-equal? (word->hex-string #x4000) "4000")
  (check-equal? (word->hex-string #xffff) "ffff")
  (check-equal? (word->hex-string #x9999) "9999")
  (check-equal? (word->hex-string #x5e5f) "5e5f"))

;; create a string formated with 'address byte+0 byte+1 ... byte+15' per line
(define/c (memory->string from to state)
  (-> word/c word/c cpu-state? string?) 
  (string-join
   (stream->list
    (map (lambda (it) (string-join
                  (stream->list
                   (append (list (word->hex-string (fx+ from (caar (stream->list it)))))
                           (map (lambda (pair) (cdr pair)) it)))
                  " "))
         (chunk 16
                (indexed
                 (map (lambda (idx) (byte->hex-string (peek state idx)))
                      (range from (fx+ 1 to)))))))
   "\n"))

;; print memory starting at address FROM to address TO of STATE
(define/c (print-memory from to state)
  (-> word/c word/c cpu-state? cpu-state?)
  (printf "~a\n" (memory->string from to state))
  state)

(module+ test #| dump-memory |#
  (check-equal? (memory->string 266 286 (poke (initialize-cpu) #x10C #xFE))
                "010a 00 00 fe 00 00 00 00 00 00 00 00 00 00 00 00 00\n011a 00 00 00 00 00")
  (check-equal? (memory->string 268 268 (poke (initialize-cpu) #x10C #xFE))
                "010c fe"))

;; create the processor state as string (without the memory)
(define/c (state->string state)
  (-> cpu-state? string?)
  (string-join (list
                (format "A  = $~a,   " (byte->hex-string (cpu-state-accumulator state)))
                (format " X = $~a, " (byte->hex-string (cpu-state-x-index state)))
                (format "Y = $~a~n" (byte->hex-string (cpu-state-y-index state)))
                (format "PC = $~a, " (word->hex-string (cpu-state-program-counter state)))
                (format "SP = $~a~n" (byte->hex-string (cpu-state-stack-pointer state)))
                (format "N=~a, O=~a, B=~a, D=~a, I=~a, Z=~a, C=~a"
                        (if (negative-flag? state) "X" "_" )
                        (if (overflow-flag? state) "X" "_" )
                        (if (break-flag? state) "X" "_" )
                        (if (decimal-flag? state) "X" "_" )
                        (if (interrupt-flag? state) "X" "_" )
                        (if (zero-flag? state) "X" "_" )
                        (if (carry-flag? state) "X" "_" )))
               ""))

(module+ test #| state->string |#
  (check-equal? (state->string (initialize-cpu))
                "A  = $00,    X = $00, Y = $00\nPC = $0000, SP = $ff\nN=_, O=_, B=_, D=_, I=_, Z=_, C=_"))

;; print the state 
(define/c (print-state state)
  (-> cpu-state? cpu-state?)
  (printf "~a\n "(state->string state))
  state)

(module+ test #| set-nth |#
  (check-equal? (nth (set-nth (cpu-state-memory (initialize-cpu)) 65535 1)
                     65535)
                1))

(module+ test #| peek and poke |#
  (check-match (peek (poke (initialize-cpu) #xc000 17) #xc000)
               17))

;; load program into memory using the 6510 state
(define/c (6510-load state memory-address program)
  (-> cpu-state? word/c (listof byte/c) cpu-state?)
  (with-program-counter  
    (foldl (lambda (state pair) 
             (poke state (first pair) (last pair)))
           state 
           (map list
                (sequence->list (in-range memory-address (fx+ memory-address (length program))))
                program))
    memory-address))

(module+ test #| 6510-load |#
  (check-equal? (memory->string 10 13 (6510-load (initialize-cpu) 10 (list #x00 #x10 #x00 #x11)))
                "000a 00 10 00 11"
                "load will put all bytes into memory"))

;; peek into memory at the location the program counter points to (current point of execution)
(define/c (peek-pc state)
  (-> cpu-state? byte/c)
  (peek state (cpu-state-program-counter state)))

;; peek into memory at the location the program counter+1 points to (current point of execution+1)
(define/c (peek-pc+1 state)
  (-> cpu-state? byte/c)
  (peek state (word (fx+ 1 (cpu-state-program-counter state)))))

;; peek into memory at the location the program counter+2 points to (current point of execution+2)
(define/c (peek-pc+2 state)
  (-> cpu-state? byte/c)
  (peek state (word (fx+ 2 (cpu-state-program-counter state)))))

;; execute if pc does not point at a 0 byte (brk)
(define/c (run state)
  (-> cpu-state? cpu-state?)
  (if  (eq? 0 (peek-pc state))
       state
       (let ((next-state (execute-cpu-step state)))
         (run next-state))))

;; interpret the RTS (return from subroutine) command
;; pop low-byte, then high-byte form stack, inc by one and write this into the pc
(define/c (interpret-rts state)
  (-> cpu-state? cpu-state?)
  (let* ([sp (cpu-state-stack-pointer state)]
         [low-ret  (peek-stack+1 state)]
         [high-ret (peek-stack+2 state)])
    (struct-copy cpu-state state
                 [program-counter (word (fx+ 1 (absolute high-ret low-ret)))]
                 [stack-pointer   (byte (fx+ sp 2))])))

;; https://www.c64-wiki.com/wiki/control_character
(define/c (display-c64charcode byte)
  (-> byte/c any/c)
  (case byte
    [(#x0d) (displayln "")]
    [else (display (string (integer->char byte)))]))

;; interpret JSR absolute (jump to subroutine) command
;; mock kernel function FFD2 to print a string
(define/c (interpret-jsr-abs high low state)
  (-> byte/c byte/c cpu-state? cpu-state?)
  (case (absolute high low)
    [(#xFFD2) ;; (display (string (integer->char (cpu-state-accumulator state))))
     (~>> (cpu-state-accumulator state)
         (display-c64charcode _))
     (struct-copy cpu-state state [program-counter (next-program-counter state 3)])]
    [else (let* ([new-program-counter (absolute high low)]
                 [return-address (word (fx+ 2 (cpu-state-program-counter state)))]
                 [sp (cpu-state-stack-pointer state)])
            (struct-copy cpu-state (~>> state
                                       (poke-stack _ (high-byte return-address))
                                       (poke-stack-1 _ (low-byte return-address)))
                         [program-counter (word new-program-counter)]
                         [stack-pointer   (byte (fx- sp 2))]))]))

(module+ test #| jsr (jump to sub routine) |#
  (check-equal? (cpu-state-program-counter
                 (interpret-jsr-abs #x40 #x08 (with-program-counter (initialize-cpu) #x2001)))
                #x4008)
  (check-equal? (cpu-state-stack-pointer
                 (interpret-jsr-abs #x40 #x08 (with-program-counter (initialize-cpu) #x2001)))
                #xfd)
  (check-equal? (peek
                 (interpret-jsr-abs #x40 #x08 (with-program-counter (initialize-cpu) #x2001))
                 #x1FF)
                #x20)
  (check-equal? (peek
                 (interpret-jsr-abs #x40 #x08 (with-program-counter (initialize-cpu) #x2001))
                 #x1FE)
                #x03))

;; set/clear carry flag
(define/c (-adjust-carry-flag set flags)
  (-> boolean? byte/c byte/c)
  (if set
      (-set-carry-flag flags)
      (-clear-carry-flag flags)))

(module+ test #| -adjust-carry-flag |#
  (check-eq? (-adjust-carry-flag #t 0)
             1)
  (check-eq? (-adjust-carry-flag #f 1)
             0))

;; set/clear zero flag
(define/c (-adjust-zero-flag set flags)
  (-> boolean? byte/c byte/c)
  (if set
      (-set-zero-flag flags)
      (-clear-zero-flag flags)))

;; set/clear overflow flag
(define/c (-adjust-overflow-flag set flags)
  (-> boolean? byte/c byte/c)
  (if set
      (-set-overflow-flag flags)
      (-clear-overflow-flag flags)))

;; set/clear negative flag
(define/c (-adjust-negative-flag set flags)
  (-> boolean? byte/c byte/c)
  (if set
      (-set-negative-flag flags)
      (-clear-negative-flag flags)))

;; interpret JMP absolute (jump)
(define/c (interpret-jmp-abs high low state)
  (-> byte/c byte/c cpu-state? cpu-state?)
  (struct-copy cpu-state state
               [program-counter (word (absolute high low))]))

;; derive overflow by looking at accumulator, operand and result
(define/c (derive-overflow acc oper new-acc)
  (-> exact-integer? exact-integer? exact-integer? boolean?)
  (let* ([input-has-same-sign (bitwise-not (bitwise-and #x80 (bitwise-xor acc oper)))]
         [in-out-has-different-sign (bitwise-and #x80 (bitwise-xor acc new-acc))])
    (not (zero? (bitwise-and input-has-same-sign in-out-has-different-sign)))))

;; interprets numbers a two complements
(define/c (derive-negative acc)
  (-> exact-integer? boolean?)
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

(module+ test #| lda immediate |#

  (define (interpret-lda-i immediate state)
    (interpret-lda-mem state (lambda (_) (byte immediate)) 2))

  (check-equal? (cpu-state-accumulator (interpret-lda-i 10 (initialize-cpu)))
                10)
  (check-equal? (cpu-state-accumulator (interpret-lda-i 0 (initialize-cpu)))
                0)
  (check-equal? (cpu-state-accumulator (interpret-lda-i 255 (initialize-cpu)))
                255))

;; write the value into the stack and then increment the stack pointer
(define/c (-push-on-stack value state)
  (-> byte/c cpu-state? cpu-state?)
  (let* ((old-sp (cpu-state-stack-pointer state)))
    (struct-copy cpu-state state
                 [stack-pointer (byte (fx- old-sp 1))]
                 [memory (set-nth (cpu-state-memory state)
                                  (fx+ #x100 old-sp)
                                  (byte value))])))

;; pop return address and flags register from the stack
(define/c (interpret-rti state)
  (-> cpu-state? cpu-state?)
  (let* ((old-sp              (cpu-state-stack-pointer state))
         (new-flags           (peek-stack+3 state))
         (new-program-counter (absolute (peek-stack+2 state)
                                        (peek-stack+1 state))))
    (struct-copy cpu-state state
                 [stack-pointer   (byte (fx+ old-sp 3))]
                 [flags           new-flags]
                 [program-counter new-program-counter])))

(module+ test #| rti |#
  (check-eq? (cpu-state-stack-pointer (interpret-rti (interpret-brk (initialize-cpu))))
             #xFF)
  (check-eq? (cpu-state-program-counter (interpret-rti (with-program-counter (interpret-brk (initialize-cpu)) #xABCD)))
             #x0000)
  (check-eq? (cpu-state-program-counter (interpret-rti (interpret-brk (with-program-counter (initialize-cpu) #xABCD))))
             #xABCD))

;; push return address and flags onto the stack and continue at vector stored at FFFE
(define/c (interpret-brk state)
  (-> cpu-state? cpu-state?)
  (let* ((old-status-byte (cpu-state-flags state))
         (old-pc          (cpu-state-program-counter state)))
    (~>>
     (struct-copy cpu-state state
                  [program-counter (absolute (peek state #xFFFE) (peek state #xFFFF))]
                  [flags           (-set-brk-flag (cpu-state-flags state))])
     (-push-on-stack old-status-byte _)
     (-push-on-stack (high-byte old-pc) _)
     (-push-on-stack (low-byte old-pc) _))))

(module+ test #| brk |#
  (check-eq? (cpu-state-program-counter
              (interpret-brk (~>> (initialize-cpu)
                                 (poke _ #xFFFE #x01)
                                 (poke _ #xFFFF #x02))))
             #x0102
             "ensure brk will continue at adress $(FFFE)")
  (check-eq? (cpu-state-stack-pointer
              (interpret-brk (~>> (initialize-cpu))))
             (fx- #xFF 3)
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

;; is the carry flag set?
(define/c (carry-flag? state)
  (-> cpu-state? boolean?)
  (eq? #x01 (bitwise-and #x01 (cpu-state-flags state))))

;; is the carry flag clear?
(define/c (not-carry-flag? state)
  (-> cpu-state? boolean?)
  (not (carry-flag? state)))

;; is the zero flag set?
(define/c (zero-flag? state)
  (-> cpu-state? boolean?)
  (eq? #x02 (bitwise-and #x02 (cpu-state-flags state))))

;; is the zero flag clear?
(define/c (not-zero-flag? state)
  (-> cpu-state? boolean?)
  (not (zero-flag? state)))

;; is the interrupt flag set?
(define/c (interrupt-flag? state)
  (-> cpu-state? boolean?)
  (eq? #x04 (bitwise-and #x04 (cpu-state-flags state))))

;; is the decimal flag set?
(define/c (decimal-flag? state)
  (-> cpu-state? boolean?)
  (eq? #x08 (bitwise-and #x08 (cpu-state-flags state))))

;; is the break flag set?
(define/c (break-flag? state)
  (-> cpu-state? boolean?)
  (eq? #x10 (bitwise-and #x10 (cpu-state-flags state))))

;; is the overflow flag set?
(define/c (overflow-flag? state)
  (-> cpu-state? boolean?)
  (eq? #x40 (bitwise-and #x40 (cpu-state-flags state))))

;; is the overflow flag clear?
(define/c (not-overflow-flag? state)
  (-> cpu-state? boolean?)
  (not (overflow-flag? state)))

;; is the negative flag set?
(define/c (negative-flag? state)
  (-> cpu-state? boolean?)
  (eq? #x80 (bitwise-and #x80 (cpu-state-flags state))))

;; is the negative flag clear?
(define/c (not-negative-flag? state)
  (-> cpu-state? boolean?)
  (not (negative-flag? state)))

;; return flags with carry flag set
(define/c (-set-carry-flag flags)
  (-> byte/c byte/c)
  (bitwise-ior #x01 flags))

;; return flags with carry flag cleared
(define/c (-clear-carry-flag flags)
  (-> byte/c byte/c)
  (bitwise-and #xfe flags))

;; return flags with zero flag set
(define/c (-set-zero-flag flags)
  (-> byte/c byte/c)
  (bitwise-ior #x02 flags))

;; return flags with break flag set
(define/c (-set-brk-flag flags)
  (-> byte/c byte/c)
  (bitwise-ior #x10 flags))

;; return flags with break flag cleared
(define/c (-clear-brk-flag flags)
  (-> byte/c byte/c)
  (bitwise-and #xEF flags))

;; return flags with zero flag cleared
(define/c (-clear-zero-flag flags)
  (-> byte/c byte/c)
  (bitwise-and #xfd flags))

;; return flags with overflow flag set
(define/c (-set-overflow-flag flags)
  (-> byte/c byte/c)
  (bitwise-ior #x40 flags))

;; return flags with overflow flag cleared
(define/c (-clear-overflow-flag flags)
  (-> byte/c byte/c)
  (bitwise-and #xbf flags))

;; return flags with negative flag set
(define/c (-set-negative-flag flags)
  (-> byte/c byte/c)
  (bitwise-ior #x80 flags))

;; return flags with negative flag cleared
(define/c (-clear-negative-flag flags)
  (-> byte/c byte/c)
  (bitwise-and #x7f flags))

;; return flags with interrupt flag set
(define/c (-set-interrupt-flag flags)
  (-> byte/c byte/c)
  (bitwise-ior #x04 flags))

;; return flags with interrupt flag cleared
(define/c (-clear-interrupt-flag flags)
  (-> byte/c byte/c)
  (bitwise-and #xfb flags))

;; return flags with decimal flag set
(define/c (-set-decimal-flag flags)
  (-> byte/c byte/c)
  (bitwise-ior #x08 flags))

;; return flags with decimal flag cleared
(define/c (-clear-decimal-flag flags)
  (-> byte/c byte/c)
  (bitwise-and #xf7 flags))

;; return the state with carry flag set
(define/c (set-carry-flag state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state [flags (-set-carry-flag (cpu-state-flags state))]))

(define/c (clear-carry-flag state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state [flags (-clear-carry-flag (cpu-state-flags state))]))

(define/c (set-brk-flag state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state [flags (-set-brk-flag (cpu-state-flags state))]))

(define/c (clear-brk-flag state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state [flags (-clear-brk-flag (cpu-state-flags state))]))

(define/c (set-decimal-flag state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state [flags (-set-decimal-flag (cpu-state-flags state))]))

(define/c (clear-decimal-flag state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state [flags (-clear-decimal-flag (cpu-state-flags state))]))

(define/c (set-interrupt-flag state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state [flags (-set-interrupt-flag (cpu-state-flags state))]))

(define/c (clear-interrupt-flag state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state [flags (-clear-interrupt-flag (cpu-state-flags state))]))

(define/c (set-overflow-flag state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state [flags (-set-overflow-flag (cpu-state-flags state))]))

(define/c (clear-overflow-flag state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state [flags (-clear-overflow-flag (cpu-state-flags state))]))

(define/c (set-negative-flag state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state [flags (-set-negative-flag (cpu-state-flags state))]))

(define/c (clear-negative-flag state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state [flags (-clear-negative-flag (cpu-state-flags state))]))

(define/c (set-zero-flag state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state [flags (-set-zero-flag (cpu-state-flags state))]))

(define/c (clear-zero-flag state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state [flags (-clear-zero-flag (cpu-state-flags state))]))

;; return flags with carry, zero, negative and overflow flag set to the parameter values
(define/c (set-flags-cznv state carry? zero? negative? overflow?)
  (-> cpu-state? boolean? boolean? boolean? boolean? byte/c)
  (~>> (cpu-state-flags state)
      (-adjust-zero-flag zero?)
      (-adjust-negative-flag negative?)
      (-adjust-carry-flag carry?)
      (-adjust-overflow-flag overflow?)))

;; return flags with carry, zero and negative flag set to the parameter values
(define/c (set-flags-czn state carry? zero? negative?)
  (-> cpu-state? boolean? boolean? boolean? byte/c)
  (~>> (cpu-state-flags state)
      (-adjust-zero-flag zero?)
      (-adjust-negative-flag negative?)
      (-adjust-carry-flag carry?)))

;; return flags with zero and negative flag set to the parameter values
(define/c (set-flags-zn state zero? negative?)
  (-> cpu-state? boolean? boolean? byte/c)
  (~>> (cpu-state-flags state)
      (-adjust-zero-flag zero?)
      (-adjust-negative-flag negative?)))

;; flags N O - B D I Z C

;; clear carry flag
(define/c (interpret-clc state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state
               [flags           (-clear-carry-flag (cpu-state-flags state))]
               [program-counter (next-program-counter state 1)]))

;; set carry flag
(define/c (interpret-sec state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state
               [flags           (-set-carry-flag (cpu-state-flags state))]
               [program-counter (next-program-counter state 1)]))

;; clear interrupt flag
(define/c (interpret-cli state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state
               [flags           (-clear-interrupt-flag (cpu-state-flags state))]
               [program-counter (next-program-counter state 1)]))


;; set interrupt flag
(define/c (interpret-sei state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state
               [flags           (-set-interrupt-flag (cpu-state-flags state))]
               [program-counter (next-program-counter state 1)]))

;; clear overflow flag
(define/c (interpret-clv state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state
               [flags           (-clear-overflow-flag (cpu-state-flags state))]
               [program-counter (next-program-counter state 1)]))

;; clear decimal flag
(define/c (interpret-cld state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state
               [flags           (-clear-decimal-flag (cpu-state-flags state))]
               [program-counter (next-program-counter state 1)]))

;; set decimal flag
(define/c (interpret-sed state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state
               [flags           (-set-decimal-flag (cpu-state-flags state))]
               [program-counter (next-program-counter state 1)]))

(module+ test #| flags |#

  ;; interpret ADC immediate (add with carry)
  (define (interpret-adc-i state)
    (interpret-calc-op state fx+ (if (carry-flag? state) 1 0) peek-pc+1 derive-carry-after-addition 2))


  (check-true (carry-flag? (interpret-sec (initialize-cpu))))
  (check-false (carry-flag? (interpret-clc (interpret-sec (initialize-cpu)))))
  (check-true (interrupt-flag? (interpret-sei (initialize-cpu))))
  (check-false (interrupt-flag? (interpret-cli (interpret-sei (initialize-cpu)))))
  (check-true (decimal-flag? (interpret-sed (initialize-cpu))))
  (check-false (decimal-flag? (interpret-cld (interpret-sed (initialize-cpu)))))
  (check-true (overflow-flag? (interpret-adc-i (poke  (interpret-adc-i (poke  (initialize-cpu) 1 #x7f)) 3 1))))
  (check-false (overflow-flag? (interpret-clv (interpret-adc-i (poke  (interpret-adc-i (poke  (initialize-cpu) 1 #x7f)) 3 1))))))

;; return word (2 byte) value stored at ADDRESS
(define/c (peek-word-at-address state address)
  (-> cpu-state? word/c word/c)
  (let* [(low-byte  (peek state address))
         (high-byte (peek state (word (fx+ address 1))))]
    (absolute high-byte low-byte)))

;; return word (2 bytes) stored at program-counter + 1
(define/c (peek-word-at-pc+1 state)
  (-> cpu-state? word/c)
  (peek-word-at-address state (word (fx+ 1 (cpu-state-program-counter state)))))

;; get the byte that is stored at the memory address
;; that is stored low, high byte ordered at the given address
;;
;; address -> [ low ][ high ]
;; @high,low-> [ value ]
(define/c (peek-indirect state address)
  (-> cpu-state? word/c word/c)
  (peek state
        (peek-word-at-address state address)))

(define/c (peek-indirect-woffset state address offset)
  (-> cpu-state? word/c exact-integer? word/c)
  (peek state
        (word (fx+ offset (peek-word-at-address state address)))))

;; put the value at the address constructed from reading
;; low, high byte order from the address provided
;;
;; address -> [ low ][ high ]
;; @high, low <- value
(define/c (poke-indirect state address value)
  (-> cpu-state? word/c byte/c cpu-state?)
  (poke state
        (peek-word-at-address state address)
        value))

(define/c (poke-indirect-woffset state address offset value)
  (-> cpu-state? word/c exact-integer? byte/c cpu-state?)
  (poke state
        (word (fx+ offset (peek-word-at-address state address)))
        value))

;; (zp,x) ->
(define/c (peek-izx state)
  (-> cpu-state? byte/c)
  (let* [(zero-page-idx (peek-pc+1 state))
         (x             (cpu-state-x-index state))]
    (peek-indirect state (fx+ x zero-page-idx))))

;; (zp,x) <-
(define/c (poke-izx state value)
  (-> cpu-state? byte/c cpu-state?)
  (let* [(zero-page-idx (peek-pc+1 state))
         (x             (cpu-state-x-index state))]
    (poke-indirect state (fx+ x zero-page-idx) value)))

;; (zp),y ->
(define/c (peek-izy state)
  (-> cpu-state? byte/c)
  (let* [(zero-page-idx (peek-pc+1 state))
         (y             (cpu-state-y-index state))]
    (peek-indirect-woffset state zero-page-idx y)))

(define/c (poke-izy state value)
  (-> cpu-state? byte/c cpu-state?)
  (let* [(zero-page-idx (peek-pc+1 state))
         (idy           (cpu-state-y-index state))]
    (poke-indirect-woffset state zero-page-idx idy value)))

;; peek the value stored at the zero page given at pc+1
(define/c (peek-zp state)
  (-> cpu-state? byte/c)
  (peek state
        (peek-pc+1 state)))

;; poke a value at the zero page given at pc+1
(define/c (poke-zp state value)
  (-> cpu-state? byte/c cpu-state?)
  (poke state
        (peek-pc+1 state)
        value))

;; peek the value stored in the zero page given at pc+1 + x-index
(define/c (peek-zpx state)
  (-> cpu-state? byte/c)
  (peek state
        (fx+ (cpu-state-x-index state)
             (peek-pc+1 state))))

;; poke the given value into the zero page given at pc+1 + x-index
(define/c (poke-zpx state value)
  (-> cpu-state? byte/c cpu-state?)
  (poke state
        (fx+ (cpu-state-x-index state)
             (peek-pc+1 state))
        value))

;; poke the given value into the zero page given at pc+1 + y-index
(define/c (poke-zpy state value)
  (-> cpu-state? byte/c cpu-state?)
  (poke state
        (fx+ (cpu-state-y-index state)
             (peek-pc+1 state))
        value))

;; peek the given value at memory address given by the word at pc+1, pc+2  + x-index
(define/c (peek-absx state)
  (-> cpu-state? word/c)
  (peek state
        (word (fx+ (cpu-state-x-index state)
                   (peek-word-at-pc+1 state)))))

;; peek the given value at memory address given by the word at pc+1, pc+2  + y-index
(define/c (peek-absy state)
  (-> cpu-state? word/c)
  (peek state
        (word (fx+ (cpu-state-y-index state)
                   (peek-word-at-pc+1 state)))))

;; poke the given value at memory address given by the word at pc+1, pc+2  + x-index
(define/c (poke-absx state value)
  (-> cpu-state? byte/c cpu-state?)
  (poke state
        (word (fx+ (cpu-state-x-index state)
                   (peek-word-at-pc+1 state)))
        value))

;; poke the given value at memory address given by the word at pc+1, pc+2  + y-index
(define/c (poke-absy state value)
  (-> cpu-state? byte/c cpu-state?)
  (poke state
        (word (fx+ (cpu-state-y-index state)
                   (peek-word-at-pc+1 state)))
        value))

(define/c (peek-abs state)
  (-> cpu-state? word/c)
  (peek state (peek-word-at-pc+1 state)))

(define/c (poke-abs state value)
  (-> cpu-state? byte/c cpu-state?)
  (poke state
        (peek-word-at-pc+1 state)
        value))

(define/c (peek-zpy state)
  (-> cpu-state? byte/c)
  (peek state
        (fx+ (cpu-state-y-index state)
             (peek-pc+1 state))))

(define/c (derive-carry-after-addition raw-accumulator)
  (-> exact-integer? boolean?)
  (< 255 raw-accumulator))

(define/c (derive-carry-after-subtraction raw-accumulator)
  (-> exact-integer? boolean?)
  (<= 0 raw-accumulator))

;; interpret logical operators like AND, ORA, EOR
;; setting zero and negative flags (ZN)
(define/c (interpret-logic-op-mem state operation peeker pc-inc)
  (-> cpu-state?
     (-> exact-integer? exact-integer? exact-integer?)
     peeker/c
     exact-nonnegative-integer?
     cpu-state?)
  (let* [(raw-accumulator     (operation (peeker state) (cpu-state-accumulator state)))
         (new-accumulator     (byte raw-accumulator))
         (zero?               (zero? new-accumulator))
         (negative?           (derive-negative raw-accumulator))]
    (struct-copy cpu-state state
                 [accumulator     new-accumulator]
                 [flags           (set-flags-zn state zero? negative?)]
                 [program-counter (next-program-counter state pc-inc)])))

;; interpret calculating operations like ADC, SBC (currently without heeding the decimal flag
;; settting carry, zero, negative, overflow flag (CZNV)
(define/c (interpret-calc-op state operation add-calc-op peeker carry-deriver pc-inc)
  (-> cpu-state?
     (-> exact-integer? exact-integer? exact-integer? exact-integer?)
     exact-integer?
     peeker/c
     (-> exact-integer? boolean?) exact-nonnegative-integer? cpu-state?)
  (let* [(accumulator         (cpu-state-accumulator state))
         (operand             (peeker state))
         (raw-accumulator     (operation accumulator operand add-calc-op))
         (new-accumulator     (byte raw-accumulator))
         (carry?              (carry-deriver raw-accumulator))
         (zero?               (zero? new-accumulator))
         (negative?           (derive-negative raw-accumulator))
         [overflow?           (derive-overflow accumulator operand raw-accumulator)]]
    (struct-copy cpu-state state
                 [accumulator     new-accumulator]
                 [flags           (set-flags-cznv state carry? zero? negative? overflow?)]
                 [program-counter (next-program-counter state pc-inc)])))

(module+ test #| interpret-adc-i - checking carry flag related|#
  (check-equal? (cpu-state-accumulator (interpret-adc-i (poke (initialize-cpu) 1 10)))
                10)

  (check-false (carry-flag? (interpret-adc-i (poke  (set-carry-flag (initialize-cpu)) 1 10))))
  (check-equal? (cpu-state-accumulator (interpret-adc-i (poke  (set-carry-flag (initialize-cpu)) 1 10)))
                11)
  (check-true (carry-flag? (interpret-adc-i (poke  (with-accumulator (initialize-cpu) 246) 1 10))))
  (check-false (carry-flag? (interpret-adc-i (poke  (with-accumulator (initialize-cpu) 245) 1 10)))))

(module+ test #| interpret-adc-i - checking zero flag related|#
  (check-true (zero-flag? (interpret-adc-i (poke  (with-accumulator (initialize-cpu) 246) 1 10))))
  (check-false (zero-flag? (interpret-adc-i (poke  (with-accumulator (initialize-cpu) 245) 1 10))))
  (check-true (zero-flag? (interpret-adc-i (poke  (set-carry-flag (with-accumulator (initialize-cpu) 245)) 1 10)))))

(module+ test #| interpret-adc-i - checking overflow flag related|#
  (check-true (overflow-flag? (interpret-adc-i (poke  (with-accumulator (initialize-cpu) 118) 1 10))))
  (check-true (overflow-flag? (interpret-adc-i (poke  (with-accumulator (initialize-cpu) 120) 1 10))))
  (check-false (overflow-flag? (interpret-adc-i (poke  (with-accumulator (initialize-cpu) 117) 1 10))))
  (check-false (overflow-flag? (interpret-adc-i (poke  (with-accumulator (initialize-cpu) 20) 1 10))))

  (check-true (overflow-flag? (interpret-adc-i (poke  (with-accumulator (initialize-cpu) 127) 1 1))))
  (check-false (overflow-flag? (interpret-adc-i (poke  (with-accumulator (initialize-cpu) 126) 1 1)))))

(module+ test #| interpret-adc-i - checking negative flag related|#
  (check-true (negative-flag?  (interpret-adc-i (poke  (with-accumulator (initialize-cpu) (two-complement-of -2)) 1 1))))
  (check-false (negative-flag? (interpret-adc-i (poke  (with-accumulator (initialize-cpu) (two-complement-of -1)) 1 1))))
  (check-false (negative-flag? (interpret-adc-i (poke  (with-accumulator (initialize-cpu) (two-complement-of 1)) 1 1))))
  (check-false (negative-flag? (interpret-adc-i (poke  (with-accumulator (initialize-cpu) (two-complement-of 0)) 1 1)))))

(module+ test #| ora indirect zero page x - ora ($I,X) ) |#
  (define (interpret-ora-izx state)
    (interpret-logic-op-mem state bitwise-ior peek-izx 2))

  (define (-prepare-op-izx acc operand)
    (~>> (initialize-cpu)
        (with-accumulator _ acc)
        (with-x-index _ #x02)
        (poke _ #x01 #x70 ) ;; pc of ora itself is x0000 => operand at x0001
        (poke _ #x72 #x11 #x21)
        (poke _ #x2111 operand)))

  (check-eq? (~>> (-prepare-op-izx #xa5 #x5a)
                 (interpret-ora-izx _)
                 (cpu-state-accumulator _))
             #xff)
  (check-true (~>> (-prepare-op-izx #xa5 #x5a)
                  (interpret-ora-izx _)
                  (negative-flag? _)))
  (check-false (~>> (-prepare-op-izx #xa5 #x5a)
                   (interpret-ora-izx _)
                   (zero-flag? _))))

(module+ test #| ora indirect zero page y - ora ($I),Y ) |#
  (define (interpret-ora-izy state)
    (interpret-logic-op-mem state bitwise-ior peek-izy 2))

  (define (-prepare-op-izy acc operand)
    (~>> (initialize-cpu)
        (with-accumulator _ acc)
        (with-y-index _ #x02)
        (poke _ #x01 #x70 ) ;; pc of ora itself is x0000 => operand at x0001
        (poke _ #x70 #x11)
        (poke _ #x71 #x21)
        (poke _ #x2113 operand)))

  (check-eq? (~>> (-prepare-op-izy #xa5 #x5a)
                 (interpret-ora-izy _)
                 (cpu-state-accumulator _))
             #xff)
  (check-true (~>> (-prepare-op-izy #xa5 #x5a)
                  (interpret-ora-izy _)
                  (negative-flag? _)))
  (check-false (~>> (-prepare-op-izy #xa5 #x5a)
                   (interpret-ora-izy _)
                   (zero-flag? _))))

(module+ test #| and izx |#
  (define (interpret-and-izx state)
    (interpret-logic-op-mem state bitwise-and peek-izx 2))

  (check-eq? (~>> (-prepare-op-izx #xa5 #x5a)
                 (interpret-and-izx _)
                 (cpu-state-accumulator _))
             #x00)
  (check-false (~>> (-prepare-op-izx #xa5 #x5a)
                   (interpret-and-izx _)
                   (negative-flag? _)))
  (check-true (~>> (-prepare-op-izx #xa5 #x5a)
                  (interpret-and-izx _)
                  (zero-flag? _))))

(module+ test #| eor izx |#
  (define (interpret-eor-izx state)
    (interpret-logic-op-mem state bitwise-xor peek-izx 2))

  (check-eq? (~>> (-prepare-op-izx #xff #x5a)
                 (interpret-eor-izx _)
                 (cpu-state-accumulator _))
             #xa5)
  (check-true (~>> (-prepare-op-izx #xa5 #x5a)
                  (interpret-eor-izx _)
                  (negative-flag? _)))
  (check-false (~>> (-prepare-op-izx #xa5 #x5a)
                   (interpret-eor-izx _)
                   (zero-flag? _))))

(module+ test #| adc izx |#
  (define (interpret-adc-izx state)
    (let* [(cf-addon (if (carry-flag? state) 1 0))]
      (interpret-calc-op state fx+ cf-addon peek-izx derive-carry-after-addition 2)))

  (check-eq? (~>> (-prepare-op-izx #x1f #x22)
                 (interpret-adc-izx _)
                 (cpu-state-accumulator _))
             #x41)
  (check-eq? (~>> (-prepare-op-izx #x1f #x22)
                 (set-carry-flag _)
                 (interpret-adc-izx _)
                 (cpu-state-accumulator _))
             #x42
             "adding numbers with carry set will increase the result by 1")
  (check-eq? (~>> (-prepare-op-izx #xf8 #x08)
                 (interpret-adc-izx _)
                 (cpu-state-accumulator _))
             #x00
             "addition resulting in 256 will zield 0 in the accumulator")
  (check-true (~>> (-prepare-op-izx #xf8 #x08)
                  (interpret-adc-izx _)
                  (carry-flag? _))
              "when addition > 255, carry flag should be set")
  (check-false (~>> (-prepare-op-izx #xf8 #x07)
                   (interpret-adc-izx _)
                   (carry-flag? _))
               "when addition <= 255, carry flag should NOT be set")
  (check-false (~>> (-prepare-op-izx #x1f #x22)
                   (interpret-adc-izx _)
                   (negative-flag? _)))
  (check-false (~>> (-prepare-op-izx #x1f #x22)
                   (interpret-adc-izx _)
                   (zero-flag? _))))

;; load into accumulator
;; setting zero, negative flag (ZN)
(define/c (interpret-lda-mem state peeker pc-inc)
  (-> cpu-state? peeker/c exact-nonnegative-integer? cpu-state?)
  (let ((value (peeker state)))
    (struct-copy cpu-state state
                 [accumulator     value]
                 [flags           (set-flags-zn state (zero? value) (bit7? value))]
                 [program-counter (next-program-counter state pc-inc)])))

;; load into x-index
;; setting zero, negative flag (ZN)
(define/c (interpret-ldx-mem state peeker pc-inc)
  (-> cpu-state? peeker/c exact-nonnegative-integer? cpu-state?)
  (let ((value (peeker state)))
    (struct-copy cpu-state state
                 [x-index         value]
                 [flags           (set-flags-zn state (zero? value) (bit7? value))]
                 [program-counter (next-program-counter state pc-inc)])))

;; load into y-index
;; setting zero, negative flag (ZN)
(define/c (interpret-ldy-mem state peeker pc-inc)
  (-> cpu-state? peeker/c exact-nonnegative-integer? cpu-state?)
  (let ((value (peeker state)))
    (struct-copy cpu-state state
                 [y-index         value]
                 [flags           (set-flags-zn state (zero? value) (bit7? value))]
                 [program-counter (next-program-counter state pc-inc)])))

;; store accumulator into memory
;; setting no flags
(define/c (interpret-sta-mem state poker pc-inc)
  (-> cpu-state? poker/c exact-nonnegative-integer? cpu-state?)
  (struct-copy cpu-state (poker state (cpu-state-accumulator state))
               [program-counter (next-program-counter state pc-inc)]))

;; store y-index into memory
;; setting no flags
(define/c (interpret-sty-mem state poker pc-inc)  
  (-> cpu-state? poker/c exact-nonnegative-integer? cpu-state?)
  (struct-copy cpu-state (poker state (cpu-state-y-index state))
               [program-counter (next-program-counter state pc-inc)]))

;; store x-index into memory
;; setting no flags
(define/c (interpret-stx-mem state poker pc-inc)  
  (-> cpu-state? poker/c exact-nonnegative-integer? cpu-state?)
  (struct-copy cpu-state (poker state (cpu-state-x-index state))
               [program-counter (next-program-counter state pc-inc)]))

(module+ test #| sbc izx |#
  (define (interpret-sbc-izx state)
    (interpret-calc-op state fx- 0 peek-izx derive-carry-after-subtraction 2))

  (check-eq? (~>> (-prepare-op-izx #x1f #x22)
                 (interpret-sbc-izx _)
                 (cpu-state-accumulator _))
             (two-complement-of (fx- #x1f #x22)))
  (check-eq? (~>> (-prepare-op-izx #x1f #x22)
                 (set-carry-flag _)
                 (interpret-sbc-izx _)
                 (cpu-state-accumulator _))
             (two-complement-of (fx- #x1f #x22))
             "subtracting two numbers with carry set will not change the result")
  (check-false (~>> (-prepare-op-izx #x1f #x22)
                   (set-carry-flag _)
                   (interpret-sbc-izx _)
                   (carry-flag? _))
               "subtracting a larger from a smaller number will clear the carry since it borrows")
  (check-true (~>> (-prepare-op-izx #x1f #x22)
                  (interpret-sbc-izx _)
                  (negative-flag? _)))
  (check-false (~>> (-prepare-op-izx #x22 #x1f)
                   (interpret-sbc-izx _)
                   (negative-flag? _)))
  (check-false (~>> (-prepare-op-izx #x1f #x22)
                   (interpret-sbc-izx _)
                   (zero-flag? _)))
  (check-true (~>> (-prepare-op-izx #x1f #x1f)
                  (interpret-sbc-izx _)
                  (zero-flag? _))))

(define/c (compute-asl-result-n-flags state peeker)
  (-> cpu-state? peeker/c (values byte/c byte/c))
  (let* ((operand (peeker state))
         (result  (byte (fxlshift operand 1)))
         (flags   (set-flags-czn state (bit7? operand) (zero? result) (bit7? result))))
    (values result flags)))

;; compute arithmetic shift left (shift in 0 into bit0)
;; set carry, zero, negative flags (CZN)
(define/c (interpret-asl state)
  (-> cpu-state? cpu-state?)
  (let-values (((result new-flags) (compute-asl-result-n-flags state cpu-state-accumulator)))
    (struct-copy cpu-state state
                 [accumulator     result]
                 [flags           new-flags]
                 [program-counter (next-program-counter state 1)])))

(module+ test #| asl |#
  (check-eq? (~>> (initialize-cpu)
                 (with-accumulator _ #x11)
                 (interpret-asl _)
                 (cpu-state-accumulator _))
             #x22))

;; compute arithmetic shift left (shift in 0 into bit0)
;; set carry, zero, negative flags (CZN)
(define/c (interpret-asl-mem state peeker poker pc-inc)
  (-> cpu-state? peeker/c poker/c exact-nonnegative-integer? cpu-state?)
  (let-values (((result new-flags) (compute-asl-result-n-flags state peeker)))
    (struct-copy cpu-state (poker state result)
                 [flags           new-flags]
                 [program-counter (next-program-counter state pc-inc)])))

(module+ test #| interpret asl abs |#
  (define opcode-asl-abs #x0e)

  (check-eq? (~>> (initialize-cpu)
                 (poke _ #x0000 opcode-asl-abs #x0f #xf0)
                 (poke _ #xf00f #x11)
                 (execute-cpu-step _)
                 (peek _ #xf00f))
             #x22))

;; interpret relative branch (when test yields true)
;; setting no flags
(define/c (interpret-branch-rel state test)
  (-> cpu-state? (-> cpu-state? boolean?) cpu-state?)
  (let* ([pc             (cpu-state-program-counter state)]
         [rel            (peek-pc+1 state)]
         [new-pc-jump    (word (fx+ pc 2 (if (< #x80 rel) (fx- rel 256) rel)))]
         [new-pc-no-jump (word (fx+ pc 2))]
         [new-pc         (if (test state) new-pc-jump new-pc-no-jump)])
    (struct-copy cpu-state state
                 [program-counter new-pc])))

;; interpret bit test on memory
;; sets carry, zero, negative, overflow flag (CZNV)
(define/c (interpret-bit-mem state peeker pc-inc)
  (-> cpu-state? peeker/c exact-nonnegative-integer? cpu-state?)
  (let* ((peeked (peeker state))
         (zero?  (not (zero? (bitwise-and (cpu-state-accumulator state) peeked))))
         (bit6   (not (zero? (bitwise-and #x40 peeked))))
         (bit7   (bit7? peeked)))
    (struct-copy cpu-state state
                 [flags           (set-flags-cznv (carry-flag? state) zero? bit7 bit6)]
                 [program-counter (next-program-counter state pc-inc)])))

(define/c (compute-ror-result-n-flags state peeker)
  (-> cpu-state? peeker/c (values byte/c byte/c))
  (let* ((pre-value (peeker state))
         (value      (bitwise-xor (if (carry-flag? state) #x80 0)
                                  (fxrshift pre-value 1)))
         (flags      (set-flags-czn state (bit0? pre-value) (zero? value) (bit7? value))))
    (values value flags)))

(define/c (interpret-ror state)
  (-> cpu-state? cpu-state?)
  (let-values (((result new-flags) (compute-ror-result-n-flags state cpu-state-accumulator)))
    (struct-copy cpu-state state
                 [accumulator     result]
                 [flags           new-flags]
                 [program-counter (next-program-counter state 1)])))

(define/c (interpret-ror-mem state peeker poker pc-inc)
  (-> cpu-state? peeker/c poker/c exact-nonnegative-integer? cpu-state?)
  (let-values (((result new-flags) (compute-ror-result-n-flags state peeker)))
    (struct-copy cpu-state (poker state result)
                 [flags           new-flags]
                 [program-counter (next-program-counter state pc-inc)])))

(define/c (compute-rol-result-n-flags state peeker)
  (-> cpu-state? peeker/c (values byte/c byte/c))
  (let* ((pre-value (peeker state))
         (value     (bitwise-xor (if (carry-flag? state) 1 0)
                                 (byte (fxlshift pre-value 1))))
         (flags     (set-flags-czn state (bit7? pre-value) (zero? value) (bit7? value))))
    (values value flags)))

(define/c (interpret-rol state)
  (-> cpu-state? cpu-state?)
  (let-values (((result new-flags) (compute-rol-result-n-flags state cpu-state-accumulator)))
    (struct-copy cpu-state state
                 [accumulator     result]
                 [flags           new-flags]
                 [program-counter (next-program-counter state 1)])))

(define/c (interpret-rol-mem state peeker poker pc-inc)
  (-> cpu-state? peeker/c poker/c exact-nonnegative-integer? cpu-state?)
  (let-values (((result new-flags) (compute-rol-result-n-flags state peeker)))
    (struct-copy cpu-state (poker state result)
                 [flags           new-flags]
                 [program-counter (next-program-counter state pc-inc)])))

(define/c (compute-lsr-result-n-flags state peeker)
  (-> cpu-state? peeker/c (values byte/c byte/c))
  (let* ((pre-value (peeker state))
        (value (fxrshift pre-value 1)))
    (values
     value
     (set-flags-czn state (bit0? pre-value) (zero? value) (bit7? value)))))

(define/c (interpret-lsr state peeker)
  (-> cpu-state? peeker/c cpu-state?)
  (let-values (((result new-flags) (compute-lsr-result-n-flags state peeker)))
    (struct-copy cpu-state state
                 [accumulator     result]
                 [flags           new-flags]
                 [program-counter (next-program-counter state 1)])))

(define/c (interpret-lsr-mem state peeker poker pc-inc)
  (-> cpu-state? peeker/c poker/c exact-nonnegative-integer? cpu-state?)
  (let-values (((result new-flags) (compute-lsr-result-n-flags state peeker)))
    (struct-copy cpu-state (poker state result)
                 [flags           new-flags]
                 [program-counter (next-program-counter state pc-inc)])))

(define/c (interpret-plp state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state
               [flags           (peek-stack state)]
               [stack-pointer   (byte (fx+ 1 (cpu-state-stack-pointer state)))]
               [program-counter (next-program-counter state 1)]))

(define/c (interpret-pla state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state
               [accumulator     (peek-stack state)]
               [stack-pointer   (byte (fx+ 1 (cpu-state-stack-pointer state)))]
               [program-counter (next-program-counter state 1)]))

(define/c (interpret-pha state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state (poke-stack state (cpu-state-accumulator state))
               [stack-pointer   (byte (fx- (cpu-state-stack-pointer state) 1))]
               [program-counter (next-program-counter state 1)]))

(define/c (interpret-php state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state (poke-stack state (cpu-state-flags state))
               [stack-pointer   (byte (fx- (cpu-state-stack-pointer state) 1))]
               [program-counter (next-program-counter state 1)]))

(define/c (interpret-t_a state source)
  (-> cpu-state? (-> cpu-state? byte/c) cpu-state?)
  (let ((value (source state)))
    (struct-copy cpu-state state
                 [accumulator     value]
                 [flags           (set-flags-zn state (zero? value) (< 127 value))]
                 [program-counter (next-program-counter state 1)])))

(define/c (interpret-t_s state source)
  (-> cpu-state? (-> cpu-state? byte/c) cpu-state?)
  (let ((value (source state)))
    (struct-copy cpu-state state
                 [stack-pointer   value]
                 [program-counter (next-program-counter state 1)])))

(define/c (interpret-t_x state source)
  (-> cpu-state? (-> cpu-state? byte/c) cpu-state?)
  (let ((value (source state)))
    (struct-copy cpu-state state
                 [x-index         value]
                 [flags           (set-flags-zn state (zero? value) (bit7? value))]
                 [program-counter (next-program-counter state 1)])))

(define/c (interpret-t_y state source)
  (-> cpu-state? (-> cpu-state? byte/c) cpu-state?)
  (let ((value (source state)))
    (struct-copy cpu-state state
                 [y-index         value]
                 [flags           (set-flags-zn state (zero? value) (bit7? value))]
                 [program-counter (next-program-counter state 1)])))

(define/c (interpret-modify-x-index state delta)
  (-> cpu-state? exact-integer? cpu-state?)
  (let ((value (byte (fx+ delta (cpu-state-x-index state)))))
    (struct-copy cpu-state state
                 [x-index         value]
                 [flags           (set-flags-zn state (zero? value) (bit7? value))]
                 [program-counter (next-program-counter state 1)])))

(define/c (interpret-modify-y-index state delta)
  (-> cpu-state? exact-integer? cpu-state?)
  (let ((value (byte (fx+ delta (cpu-state-y-index)))))
    (struct-copy cpu-state state
                 [y-index         value]
                 [flags           (set-flags-zn state (zero? value) (bit7? value))]
                 [program-counter (next-program-counter state 1)])))

(module+ test #| t_a |#
  (check-eq? (cpu-state-accumulator
              (interpret-t_a
               (struct-copy cpu-state (initialize-cpu) [x-index 10])
               cpu-state-x-index))
             10)
  (check-eq? (cpu-state-accumulator
              (interpret-t_a
               (struct-copy cpu-state (initialize-cpu) [y-index 11])
               cpu-state-y-index))
             11))

(define/c (interpret-jmp-ind state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state
               [program-counter (peek-word-at-address state (peek-abs state))]))

(define/c (interpret-compare state peeker1 peeker2 pc-inc)
  (-> cpu-state? peeker/c peeker/c exact-nonnegative-integer? cpu-state?)
  (let* ((value1 (peeker1 state))
         (value2 (peeker2 state))
         (diff   (fx- value1 value2)))
    (struct-copy cpu-state state
                 [flags           (set-flags-czn state (not-bit7? diff) (zero? diff) (bit7? diff))]
                 [program-counter (next-program-counter state pc-inc)])))

(define/c (interpret-crement-mem state op peeker poker pc-inc)
  (-> cpu-state? (-> exact-integer? exact-integer? exact-integer?) peeker/c poker/c exact-nonnegative-integer? cpu-state?)
  (let* ((pre-value (peeker state))
         (value     (byte (op pre-value 1))))
    (struct-copy cpu-state (poker state value)
                 [flags           (set-flags-zn state (zero? value) (bit7? value))]
                 [program-counter (next-program-counter state pc-inc)])))

(module+ test #| crement-mem |#
  (check-eq? (peek (interpret-crement-mem (initialize-cpu)
                                           fx+
                                           (lambda (s) 10)
                                           (lambda (s v) (poke s #x2000 v))
                                           2)
                   #x2000)
             11)
  (check-eq? (cpu-state-program-counter (interpret-crement-mem (initialize-cpu) fx- peek-zp poke-zp 2))
             2 "make sure contract is valid on actual parameters"))

(define/c (interpret-nop state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state
               [program-counter (next-program-counter state 1)]))

;; execute one cpu opcode and return the next state (see http://www.oxyron.de/html/opcodes02.html)
;; imm = #$00
;; zp = $00
;; zpx = $00,X
;; zpy = $00,Y
;; izx = ($00,X)
;; izy = ($00),Y
;; abs = $0000
;; abx = $0000,X
;; aby = $0000,Y
;; ind = ($0000)
;; rel = $0000 (PC-relative)
;; io = illegal opcode
(define/c (execute-cpu-step state)
  (-> cpu-state? cpu-state?)
  (case (peek-pc state)
    [(#x00) (interpret-brk state)]
    [(#x01) (interpret-logic-op-mem state bitwise-ior peek-izx 2)]
    ;; #x02 -io KIL
    ;; #x03 -io SLO izx
    ;; #x04 -io NOP zp
    [(#x05) (interpret-logic-op-mem state bitwise-ior peek-zp 2)]
    [(#x06) (interpret-asl-mem state peek-zp poke-zp 2)]
    ;; #x07 -io SLO zp
    [(#x08) (interpret-php state)]
    [(#x09) (interpret-logic-op-mem state bitwise-ior peek-pc+1 2)]
    [(#x0a) (interpret-asl state)]
    ;; #x0b -io ANC imm
    ;; #x0c -io NOP abs
    [(#x0d) (interpret-logic-op-mem state bitwise-ior peek-abs 3)]
    [(#x0e) (interpret-asl-mem state peek-abs poke-abs 3)]
    ;; #x0f -io SLO abs
    [(#x10) (interpret-branch-rel state not-negative-flag?)]
    [(#x11) (interpret-logic-op-mem state bitwise-ior peek-izy 2)]
    ;; #x12 -io KIL
    ;; #x13 -io SLO izy
    ;; #x14 -io NOP zpx
    [(#x15) (interpret-logic-op-mem state bitwise-ior peek-zpx 2)]
    [(#x16) (interpret-asl-mem state peek-zpx poke-zpx 2)]
    ;; #x17 -io SLO zpx
    [(#x18) (interpret-clc state)]
    [(#x19) (interpret-logic-op-mem state bitwise-ior peek-absy 3)]
    ;; #x1a -io NOP
    ;; #x1b -io SLO abt
    ;; #x1c -io NOP abx
    [(#x1d) (interpret-logic-op-mem state bitwise-ior peek-absx 3)]
    [(#x1e) (interpret-asl-mem state peek-absx poke-absx 3)]
    ;; #x1f -io SLO abx
    [(#x20) (interpret-jsr-abs (peek-pc+2 state) (peek-pc+1 state) state)]
    [(#x21) (interpret-logic-op-mem state bitwise-and peek-izx 2)]
    ;; #x22 -io KIL
    ;; #x23 -io RLA izx
    [(#x24) (interpret-bit-mem state peek-zp 2)]
    [(#x25) (interpret-logic-op-mem state bitwise-and peek-zp 2)]
    [(#x26) (interpret-rol-mem state peek-zp poke-zp 2)]
    ;; #x27 -io RLA zp
    [(#x28) (interpret-plp state)]
    [(#x29) (interpret-logic-op-mem state bitwise-and peek-pc+1 2)]
    [(#x2a) (interpret-rol state)]
    ;; #x2b -io ANC imm
    [(#x2c) (interpret-bit-mem state peek-abs 3)]
    [(#x2d) (interpret-logic-op-mem state bitwise-and peek-abs 3)]
    [(#x2e) (interpret-rol-mem state peek-abs poke-abs 3)]
    ;; #x2f -io RIA abs
    [(#x30) (interpret-branch-rel state negative-flag?)]
    [(#x31) (interpret-logic-op-mem state bitwise-and peek-izy 2)]
    ;; #x32 -io KIL
    ;; #x33 -io RIA izy
    ;; #x34 -io NOP zpx 
    [(#x35) (interpret-logic-op-mem state bitwise-and peek-zpx 2)]
    [(#x36) (interpret-rol-mem state peek-zpx poke-abs 2)]
    ;; #x37 -io RLA zpx
    [(#x38) (interpret-sec state)]
    [(#x39) (interpret-logic-op-mem state bitwise-and peek-absy 3)]
    ;; #x3a -io NOP
    ;; #x3b -io RLA aby
    ;; #x3c -io NOP abx
    [(#x3d) (interpret-logic-op-mem state bitwise-and peek-absx 3)]
    [(#x3e) (interpret-rol-mem state peek-zpx poke-absx 3)]
    ;; #x3f -io RLA abx
    [(#x40) (interpret-rti state)]
    [(#x41) (interpret-logic-op-mem state bitwise-xor peek-izx 2)]
    ;; #x42 -io KIL
    ;; #x43 -io SRE izx
    ;; #x44 -io NOP zp
    [(#x45) (interpret-logic-op-mem state bitwise-xor peek-zp 2)]
    [(#x46) (interpret-lsr-mem state peek-zp poke-zp 2)]
    ;; #x47 -io SRE zp
    [(#x48) (interpret-pha state)]
    [(#x49) (interpret-logic-op-mem state bitwise-xor peek-pc+1 2)]
    [(#x4a) (interpret-lsr state peek-zp)]
    ;; #x4b -io ALR imm
    [(#x4c) (interpret-jmp-abs (peek-pc+2 state) (peek-pc+1 state) state)]
    [(#x4d) (interpret-logic-op-mem state bitwise-xor peek-abs 3)]
    [(#x4e) (interpret-lsr-mem state peek-abs poke-abs 3)]
    ;; #x4f -io SRE abs
    [(#x50) (interpret-branch-rel state not-overflow-flag?)]
    [(#x51) (interpret-logic-op-mem state bitwise-xor peek-izy 2)]
    ;; #x52 -io KIL
    ;; #x53 -io SRE izy
    ;; #x54 -io NOP zpx
    [(#x55) (interpret-logic-op-mem state bitwise-xor peek-zpx 2)]
    [(#x56) (interpret-lsr-mem state peek-zpx poke-zpx 2)]
    ;; #x57 -io SRE zpx
    [(#x58) (interpret-cli state)]
    [(#x59) (interpret-logic-op-mem state bitwise-xor peek-absy 3)]
    ;; #x5a -io NOP
    ;; #x5b -io SRE aby
    ;; #x5c -io NOP abx
    [(#x5d) (interpret-logic-op-mem state bitwise-xor peek-absx 3)]
    [(#x5e) (interpret-lsr-mem state peek-absx poke-absx 3)]
    ;; #x5f -io SRE abx
    [(#x60) (interpret-rts state)]
    [(#x61) (interpret-calc-op state fx+ (if (carry-flag? state) 1 0) peek-izx derive-carry-after-addition 2)]
    ;; #x62 -io KIL
    ;; #x63 -io RRA izx
    ;; #x64 -io NOP zp
    [(#x65) (interpret-calc-op state fx+ (if (carry-flag? state) 1 0) peek-zp derive-carry-after-addition 2)]
    [(#x66) (interpret-ror-mem state peek-zp poke-zp 2)]
    ;; #x67 -io RRA zp
    [(#x68) (interpret-pla state)]
    [(#x69) (interpret-calc-op state fx+ (if (carry-flag? state) 1 0) peek-pc+1 derive-carry-after-addition 2)]
    [(#x6a) (interpret-ror state)]
    ;; #x6b -io ARR imm
    [(#x6c) (interpret-jmp-ind state)]
    [(#x6d) (interpret-calc-op state fx+ (if (carry-flag? state) 1 0) peek-abs derive-carry-after-addition 3)]
    [(#x6e) (interpret-ror-mem state peek-abs poke-abs 3)]
    ;; #x6f -io RRA abs
    [(#x70) (interpret-branch-rel state overflow-flag?)]
    [(#x71) (interpret-calc-op state fx+ (if (carry-flag? state) 1 0) peek-izy derive-carry-after-addition 2)]
    ;; #x72 -io KIL
    ;; #x73 -io RRA izy
    ;; #x74 -io NOP zpx
    [(#x75) (interpret-calc-op state fx+ (if (carry-flag? state) 1 0) peek-zpx derive-carry-after-addition 2)]
    [(#x76) (interpret-ror-mem state peek-zpx poke-zpx 2)]
    ;; #x77 -io RRA zpx
    [(#x78) (interpret-sei state)]
    [(#x79) (interpret-calc-op state fx+ (if (carry-flag? state) 1 0) peek-absy derive-carry-after-addition 3)]
    ;; #x7a -io NOP
    ;; #x7b -io RRA aby
    ;; #x7c -io NOP abx
    [(#x7d) (interpret-calc-op state fx+ (if (carry-flag? state) 1 0) peek-absx derive-carry-after-addition 3)]
    [(#x7e) (interpret-ror-mem state peek-absx poke-absx 2)]
    ;; #x7f -io RRA abx
    ;; #x80 -io NOP imm
    [(#x81) (interpret-sta-mem state poke-izx 2)]
    ;; #x82 -io NOP imm
    ;; #x83 -io SAX izx
    [(#x84) (interpret-sty-mem state poke-zp 2)]
    [(#x85) (interpret-sta-mem state poke-zp 2)]
    [(#x86) (interpret-stx-mem state poke-zp 2)]
    ;; #x87 -io SAX zp
    [(#x88) (interpret-modify-y-index state -1)]
    ;; #x89 -io NOP imm
    [(#x8a) (interpret-t_a state cpu-state-x-index)]
    ;; #x8b -io XAA imm
    [(#x8c) (interpret-sty-mem state poke-abs 3)]
    [(#x8d) (interpret-sta-mem state poke-abs 3)]
    [(#x8e) (interpret-stx-mem state poke-abs 3)]
    ;; #x8f -io SAX abs
    [(#x90) (interpret-branch-rel state not-carry-flag?)]
    [(#x91) (interpret-sta-mem state peek-izy 2)]
    ;; #x92 -io KIL
    ;; #x93 -io AHX izy
    [(#x94) (interpret-sty-mem state poke-zpx 2)]
    [(#x95) (interpret-sta-mem state poke-zpx 2)]
    [(#x96) (interpret-stx-mem state poke-zpy 2)]
    ;; #x97 -io SAX zpy
    [(#x98) (interpret-t_a state cpu-state-y-index)]
    [(#x99) (interpret-sta-mem state poke-absy 3)]
    [(#x9a) (interpret-t_s state cpu-state-x-index)]
    ;; #x9b -io TAS avt
    ;; #x9c -io SHY abx
    [(#x9d) (interpret-sta-mem state poke-absx 3)]
    ;; #x9e -io SHX aby
    ;; #x9f -io AHX aby
    [(#xa0) (interpret-ldy-mem state peek-pc+1 2)]
    [(#xa1) (interpret-lda-mem state peek-izx 2)]
    [(#xa2) (interpret-ldx-mem state peek-pc+1 2)]
    ;; #xa3 -io LAX izx
    [(#xa4) (interpret-ldy-mem state peek-zp 2)]
    [(#xa5) (interpret-lda-mem state peek-zp 2)]
    [(#xa6) (interpret-ldx-mem state peek-zp 2)]
    ;; #xa7 -io LAX zp
    [(#xa8) (interpret-t_y state cpu-state-accumulator)]
    [(#xa9) (interpret-lda-mem state peek-pc+1 2)]
    [(#xaa) (interpret-t_x state cpu-state-accumulator)]
    ;; #xab -io LAX imm
    [(#xac) (interpret-ldy-mem state peek-abs 3)]
    [(#xad) (interpret-lda-mem state peek-abs 3)]
    [(#xae) (interpret-ldx-mem state peek-abs 3)]
    ;; #xaf -io LAX abs
    [(#xb0) (interpret-branch-rel state carry-flag?)]
    [(#xb1) (interpret-lda-mem state peek-izy 2)]
    ;; #xb2 -io KIL
    ;; #xb3 -io LAX izy
    [(#xb4) (interpret-ldy-mem state peek-zpx 2)]
    [(#xb5) (interpret-lda-mem state peek-zpx 2)]
    [(#xb6) (interpret-ldx-mem state peek-zpy 2)]
    ;; #xb7 -io LAX zpy
    [(#xb8) (interpret-clv state)]
    [(#xb9) (interpret-lda-mem state peek-absy 3)]
    [(#xba) (interpret-t_x state cpu-state-stack-pointer)]
    ;; #xbb -io LAS aby
    [(#xbc) (interpret-ldy-mem state peek-absx 3)]
    [(#xbd) (interpret-lda-mem state peek-absx 3)]
    [(#xbe) (interpret-ldx-mem state peek-absy 3)]
    ;; #xbf -io LAX aby
    [(#xc0) (interpret-compare state cpu-state-y-index peek-pc+1 2)]
    [(#xc1) (interpret-compare state cpu-state-accumulator peek-izx 2)]
    ;; #xc2 -io NOP imm
    ;; #xc3 -io DCP izx
    [(#xc4) (interpret-compare state cpu-state-y-index peek-zp 2)]
    [(#xc5) (interpret-compare state cpu-state-accumulator peek-zp 2)]
    [(#xc6) (interpret-crement-mem state fx- peek-zp poke-zp 2)]
    ;; #xc7 -io DCP zp
    [(#xc8) (interpret-modify-y-index state 1)]
    [(#xc9) (interpret-compare state cpu-state-accumulator peek-pc+1 2)]
    [(#xca) (interpret-modify-x-index state -1)]
    ;; #xcb -io AXS imm
    [(#xcc) (interpret-compare state cpu-state-y-index peek-abs 3)]
    [(#xcd) (interpret-compare state cpu-state-accumulator peek-abs 3)]
    [(#xce) (interpret-crement-mem state fx- peek-abs poke-abs 3)]
    ;; #xcf -io DCP abs
    [(#xd0) (interpret-branch-rel state not-zero-flag?)]
    [(#xd1) (interpret-compare state cpu-state-accumulator peek-izy 3)]
    ;; #xd2 -io KIL
    ;; #xd3 -io DCP izy
    ;; #xd4 -io NOP zpx
    [(#xd5) (interpret-compare state cpu-state-accumulator peek-zpx 3)]
    [(#xd6) (interpret-crement-mem state fx- peek-zpx poke-zpx 2)]
    ;; #xd7 -io DCP zpx
    [(#xd8) (interpret-cld state)]
    [(#xd9) (interpret-compare state cpu-state-accumulator peek-absy 3)]
    ;; #xda -io NOP
    ;; #xdb -io DCP aby
    ;; #xdc -io NOP abx
    [(#xdd) (interpret-compare state cpu-state-accumulator peek-absx 3)]
    [(#xde) (interpret-crement-mem state fx- peek-absx poke-absx 3)]
    ;; #xdf -io DCP abx
    [(#xe0) (interpret-compare state cpu-state-x-index peek-pc+1 2)]
    [(#xe1) (interpret-calc-op state fx- 0 peek-izx derive-carry-after-subtraction 2)]
    ;; #xe2 -io NOP imm
    ;; #xe3 -io ISC izx
    [(#xe4) (interpret-compare state cpu-state-x-index peek-zp 2)]
    [(#xe5) (interpret-calc-op state fx- 0 peek-zp derive-carry-after-subtraction 2)]
    [(#xe6) (interpret-crement-mem state fx+ peek-zp poke-zp 2)]
    ;; #xe7 -io ISC zp
    [(#xe8) (interpret-modify-x-index state 1)]
    [(#xe9) (interpret-calc-op state fx- 0 peek-pc+1 derive-carry-after-subtraction 2)]
    [(#xea) (interpret-nop state)]
    ;; #xeb -io SBC imm
    [(#xec) (interpret-compare state cpu-state-x-index peek-abs 3)]
    [(#xed) (interpret-calc-op state fx- 0 peek-abs derive-carry-after-subtraction 3)]
    [(#xee) (interpret-crement-mem state fx+ peek-abs poke-abs 3)]
    ;; #xef -io ISC abs
    [(#xf0) (interpret-branch-rel state zero-flag?)]
    [(#xf1) (interpret-calc-op state fx- 0 peek-izy derive-carry-after-subtraction 2)]
    ;; #xf2 -io KIL
    ;; #xf3 -io ISC izy
    ;; #xf4 -io NOP zpx
    [(#xf5) (interpret-calc-op state fx- 0 peek-zpx derive-carry-after-subtraction 2)]
    [(#xf6) (interpret-crement-mem state fx+ peek-zpx poke-zpx 2)]
    ;; #xf7 -io ISC zpx
    [(#xf8) (interpret-sed state)]
    [(#xf9) (interpret-calc-op state fx- 0 peek-absy derive-carry-after-subtraction 3)]
    ;; #xfa -io NOP
    ;; #xfb -io ISC aby
    ;; #xfc -io NOP abx
    [(#xfd) (interpret-calc-op state fx- 0 peek-absx derive-carry-after-subtraction 3)]
    [(#xfe) (interpret-crement-mem state fx+ peek-absx poke-absx 3)]
    ;; #xff -io ISC abx
    [else (error "unknown opcode")]))

(module+ test #| fd SBC abs,x |#
  (define (at-2000_sbc-2000-x_with-x-3 accumulator at-2003-value)
    (~>> (initialize-cpu)
        (with-program-counter _ #x2000)              ;; *=$2000
        (with-flags _ #xff)                          ;; carry set
        (with-accumulator     _ accumulator)         ;; LDA #,accumulator
        (poke _ #x2000 #xfd #x00 #x20 at-2003-value) ;; SBC $2000,X
                                                     ;; .byte ,at-2003-value
        (with-x-index _ #x03)))

  (let ((result (~>> (at-2000_sbc-2000-x_with-x-3 #x21 #x10)
                    (execute-cpu-step _))))
    (check-eq? (cpu-state-accumulator result) #x11 "#x21 - #x10 = #x11")
    (check-eq? (cpu-state-program-counter result) #x2003 "this was a 3 byte command")
    (check-true (carry-flag? result) "carry flag is still set, no borrow took place")
    (check-false (zero-flag? result) "zero flag is false since != $00")
    (check-false (negative-flag? result) "negative flag false, bit7, sign flag not set"))

  (let ((result (~>> (at-2000_sbc-2000-x_with-x-3 #x10 #x21)
                    (execute-cpu-step _))))
    (check-eq? (cpu-state-accumulator result) #xef "#x10 - #x21 = #xef")
    (check-false (carry-flag? result) "carry flag is false, borrow took place!")
    (check-false (zero-flag? result) "zero flag is false since != $00")
    (check-true (negative-flag? result) "negative flag true, bit7, sign flag set")))

(module+ test #| fe INC abs,x |#
  (define (at-2000_inc-2000-x_with-x-3 at-2003-value)
    (~>> (initialize-cpu)
        (with-flags _ #xff)
        (with-program-counter _ #x2000)              ;; *=$2000
        (poke _ #x2000 #xfe #x00 #x20 at-2003-value) ;; INC $2000,X
                                                     ;; .byte ,at-2003-value
        (with-x-index _ #x03)))                      ;; LDX #x03
  
  (let ((result (~>> (at-2000_inc-2000-x_with-x-3 #x10)
                    (execute-cpu-step _))))
    (check-eq? (peek-word-at-address result #x2003) #x11 "incremented byte at location $2000+x")
    (check-eq? (cpu-state-program-counter result) #x2003 "this was a 3 byte command")
    (check-false (zero-flag? result) "zero flag is false since ($10 + 1) != $00")
    (check-false (negative-flag? result) "negative flag false, bit7, sign flag not set"))

    (let ((result (~>> (at-2000_inc-2000-x_with-x-3 #xff)
                    (execute-cpu-step _))))
    (check-true (zero-flag? result) "zero flag is true since ($FF + 1) == $00")
    (check-false (negative-flag? result) "negative flag false, bit7, sign flag not set"))

    (let ((result (~>> (at-2000_inc-2000-x_with-x-3 #x7f)
                    (execute-cpu-step _))))
    (check-false (zero-flag? result) "zero flag is false since ($7f + 1) != $00")
    (check-true (negative-flag? result) "negative flag true, bit7, sign flag set")))

(module+ test #| disassemble |#
  (check-equal?
   (let-values (((str len) (disassemble (~>> (initialize-cpu)
                                             (with-flags _ #xff)
                                             (with-program-counter _ #x2000)              ;; *=$2000
                                             (poke _ #x2000 #x01 #xcd)))))
     str)
   "ORA ($cd,x)"))

(define (disassemble state)
;;  (-> cpu-state? (values string bytes))
  (case (peek-pc state)
    [(#x00) (values "BRK" 1)]
    [(#x01) (values (format "ORA ($~a,x)" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #x02 -io KIL
    ;; #x03 -io SLO izx
    ;; #x04 -io NOP zp
    [(#x05) (values (format "ORA $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#x06) (values (format "ASL $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #x07 -io SLO zp
    [(#x08) (values "PHP" 1)]
    [(#x09) (values (format "ORA #$~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#x0a) (values "ASL" 1)]
    ;; #x0b -io ANC imm
    ;; #x0c -io NOP abs
    [(#x0d) (values (format "ORA $~a" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    [(#x0e) (values (format "ASL $~a" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    ;; #x0f -io SLO abs
    [(#x10) (values (format "BPL $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#x11) (values (format "ORA ($~a),y" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #x12 -io KIL
    ;; #x13 -io SLO izy
    ;; #x14 -io NOP zpx
    [(#x15) (values (format "ORA $~a,x" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#x16) (values (format "ASL $~a,x" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #x17 -io SLO zpx
    [(#x18) (values "CLC" 1)]
    [(#x19) (values (format "ORA $~a,y" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    ;; #x1a -io NOP
    ;; #x1b -io SLO abt
    ;; #x1c -io NOP abx
    [(#x1d) (values (format "ORA $~a,x" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    [(#x1e) (values (format "ASL $~a,x" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    ;; #x1f -io SLO abx
    [(#x20) (values (format "JSR $~a" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    [(#x21) (values (format "AND ($~a,x)" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #x22 -io KIL
    ;; #x23 -io RLA izx
    [(#x24) (values (format "BIT $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#x25) (values (format "AND $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#x26) (values (format "ROL $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #x27 -io RLA zp
    [(#x28) (values "PLP" 1)]
    [(#x29) (values (format "AND #$~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#x2a) (values "ROL" 1)]
    ;; #x2b -io ANC imm
    [(#x2c) (values (format "BIT $~a" (word->hex-string (peek-word-at-pc+1))) 3)]
    [(#x2d) (values (format "AND $~a" (word->hex-string (peek-word-at-pc+1))) 3)]
    [(#x2e) (values (format "ROL $~a" (word->hex-string (peek-word-at-pc+1))) 3)]
    ;; #x2f -io RIA abs
    [(#x30) (values (format "BMI $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#x31) (values (format "AND ($~a),y" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #x32 -io KIL
    ;; #x33 -io RIA izy
    ;; #x34 -io NOP zpx 
    [(#x35) (values (format "AND $~a,x" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#x36) (values (format "ROL $~a,x" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #x37 -io RLA zpx
    [(#x38) (values "SEC" 1)]
    [(#x39) (values (format "AND $~a,y" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    ;; #x3a -io NOP
    ;; #x3b -io RLA aby
    ;; #x3c -io NOP abx
    [(#x3d) (values (format "AND $~a,x" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    [(#x3e) (values (format "ROL $~a,x" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    ;; #x3f -io RLA abx
    [(#x40) (values "RTI" 1)]
    [(#x41) (values (format "EOR ($~a,x)" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #x42 -io KIL
    ;; #x43 -io SRE izx
    ;; #x44 -io NOP zp
    [(#x45) (values (format "EOR $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#x46) (values (format "LSR $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #x47 -io SRE zp
    [(#x48) (values "PHA" 1)]
    [(#x49) (values (format "EOR #$~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#x4a) (values (format "LSR $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #x4b -io ALR imm
    [(#x4c) (values (format "JMP $~a" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    [(#x4d) (values (format "EOR $~a" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    [(#x4e) (values (format "LSR $~a" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    ;; #x4f -io SRE abs
    [(#x50) (values (format "BVC $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#x51) (values (format "EOR ($~a),y" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #x52 -io KIL
    ;; #x53 -io SRE izy
    ;; #x54 -io NOP zpx
    [(#x55) (values (format "EOR $~a,x" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#x56) (values (format "LSR $~a,x" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #x57 -io SRE zpx
    [(#x58) (values "CLI" 1)]
    [(#x59) (values (format "EOR $~a,y" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    ;; #x5a -io NOP
    ;; #x5b -io SRE aby
    ;; #x5c -io NOP abx
    [(#x5d) (values (format "EOR $~a,x" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    [(#x5e) (values (format "LSR $~a,x" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    ;; #x5f -io SRE abx
    [(#x60) (values "RTS" 1) (interpret-rts state)]
    [(#x61) (values (format "ADC ($~a,x)" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #x62 -io KIL
    ;; #x63 -io RRA izx
    ;; #x64 -io NOP zp
    [(#x65) (values (format "ADC $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#x66) (values (format "ROR $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #x67 -io RRA zp
    [(#x68) (values "PLA" 1) (interpret-pla state)]
    [(#x69) (values (format "ADC #$~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#x6a) (values "ROR" 1)]
    ;; #x6b -io ARR imm
    [(#x6c) (values (format "JMP $~a" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    [(#x6d) (values (format "ADC $~a" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    [(#x6e) (values (format "ROR $~a" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    ;; #x6f -io RRA abs
    [(#x70) (values "BVS" 1)]
    [(#x71) (values (format "ADC ($~a),y" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #x72 -io KIL
    ;; #x73 -io RRA izy
    ;; #x74 -io NOP zpx
    [(#x75) (values (format "ADC $~a,x" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#x76) (values (format "ROR $~a,x" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #x77 -io RRA zpx
    [(#x78) (values "SEI" 1)]
    [(#x79) (values (format "ADC $~a,y" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    ;; #x7a -io NOP
    ;; #x7b -io RRA aby
    ;; #x7c -io NOP abx
    [(#x7d) (values (format "ADC $~a,x" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    [(#x7e) (values (format "ROR $~a,x" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    ;; #x7f -io RRA abx
    ;; #x80 -io NOP imm
    [(#x81) (values (format "STA ($~a,x)" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #x82 -io NOP imm
    ;; #x83 -io SAX izx
    [(#x84) (values (format "STY $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#x85) (values (format "STA $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#x86) (values (format "STX $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #x87 -io SAX zp
    [(#x88) (values "DEY" 1)]
    ;; #x89 -io NOP imm
    [(#x8a) (values "TXA" 1)]
    ;; #x8b -io XAA imm
    [(#x8c) (values (format "STY $~a" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    [(#x8d) (values (format "STA $~a" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    [(#x8e) (values (format "STX $~a" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    ;; #x8f -io SAX abs
    [(#x90) (values (format "BCC $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#x91) (values (format "STA ($~a),y" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #x92 -io KIL
    ;; #x93 -io AHX izy
    [(#x94) (values (format "STY $~a,x" ) 2) (interpret-sty-mem state poke-zpx 2)]
    [(#x95) (values (format "STA $~a,x" ) 2) (interpret-sty-mem state poke-zpx 2)]
    [(#x96) (values (format "STX $~a,y" ) 2) (interpret-sty-mem state poke-zpx 2)]
    ;; #x97 -io SAX zpy
    [(#x98) (values "TYA" 1)]
    [(#x99) (values (format "STA $~a,y" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    [(#x9a) (values "TXS" 1)]
    ;; #x9b -io TAS avt
    ;; #x9c -io SHY abx
    [(#x9d) (values (format "STA $~a,x" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    ;; #x9e -io SHX aby
    ;; #x9f -io AHX aby
    [(#xa0) (values (format "LDY #$~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#xa1) (values (format "LDA ($~a,x)" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#xa2) (values (format "LDX #$~a" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #xa3 -io LAX izx
    [(#xa4) (values (format "LDY $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#xa5) (values (format "LDA $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#xa6) (values (format "LDX $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #xa7 -io LAX zp
    [(#xa8) (values "TAY" 1)]
    [(#xa9) (values (format "LDA #$~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#xaa) (values "TAX" 1)]
    ;; #xab -io LAX imm
    [(#xac) (values (format "LDY $~a" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    [(#xad) (values (format "LDA $~a" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    [(#xae) (values (format "LDX $~a" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    ;; #xaf -io LAX abs
    [(#xb0) (values (format "BCS $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#xb1) (values (format "LDA ($~a),y" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #xb2 -io KIL
    ;; #xb3 -io LAX izy
    [(#xb4) (values (format "LDY $~a,x" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#xb5) (values (format "LDA $~a,x" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#xb6) (values (format "LDX $~a,y" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #xb7 -io LAX zpy
    [(#xb8) (values "CLV" 1)]
    [(#xb9) (values (format "LDA $~a,y" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    [(#xba) (values "TSX" 1)]
    ;; #xbb -io LAS aby
    [(#xbc) (values (format "LDY $~a,x" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    [(#xbd) (values (format "LDA $~a,x" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    [(#xbe) (values (format "LDX $~a,y" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    ;; #xbf -io LAX aby
    [(#xc0) (values (format "CPY #$~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#xc1) (values (format "CMP ($~a,x" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #xc2 -io NOP imm
    ;; #xc3 -io DCP izx
    [(#xc4) (values (format "CPY $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#xc5) (values (format "CMP $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#xc6) (values (format "DEC $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #xc7 -io DCP zp
    [(#xc8) (values "INY" 1)]
    [(#xc9) (values (format "CMP #$~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#xca) (values "DEX" 1)]
    ;; #xcb -io AXS imm
    [(#xcc) (values (format "CPY $~a" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    [(#xcd) (values (format "CMP $~a" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    [(#xce) (values (format "DEC $~a" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    ;; #xcf -io DCP abs
    [(#xd0) (values "BNE" 1)]
    [(#xd1) (values (format "CMP ($~a),y" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #xd2 -io KIL
    ;; #xd3 -io DCP izy
    ;; #xd4 -io NOP zpx
    [(#xd5) (values (format "CMP $~a,x" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#xd6) (values (format "DEC $~a,x" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #xd7 -io DCP zpx
    [(#xd8) (values "CLD" 1)]
    [(#xd9) (values (format "CMP $~a,y" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    ;; #xda -io NOP
    ;; #xdb -io DCP aby
    ;; #xdc -io NOP abx
    [(#xdd) (values (format "CMP $~a,x" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    [(#xde) (values (format "DEC $~a,x" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    ;; #xdf -io DCP abx
    [(#xe0) (values (format "CPX #$~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#xe1) (values (format "SBC ($~a,x)" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #xe2 -io NOP imm
    ;; #xe3 -io ISC izx
    [(#xe4) (values (format "CPX $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#xe5) (values (format "SBC $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#xe6) (values (format "INC $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #xe7 -io ISC zp
    [(#xe8) (values "INX" 1)]
    [(#xe9) (values (format "SBC #$~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#xea) (values "NOP" 1)]
    ;; #xeb -io SBC imm
    [(#xec) (values (format "CPX $~a" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    [(#xed) (values (format "SBC $~a" (word->hex-string (peek-word-at-pc+1 state))) 3) ]
    [(#xee) (values (format "INC $~a" (word->hex-string (peek-word-at-pc+1 state))) 2)]
    ;; #xef -io ISC abs
    [(#xf0) (values (format "BEQ $~a" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#xf1) (values (format "SBC ($~a),y" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #xf2 -io KIL
    ;; #xf3 -io ISC izy
    ;; #xf4 -io NOP zpx
    [(#xf5) (values (format "SBC $~a,x" (byte->hex-string (peek-pc+1 state))) 2)]
    [(#xf6) (values (format "INC $~a,x" (byte->hex-string (peek-pc+1 state))) 2)]
    ;; #xf7 -io ISC zpx
    [(#xf8) (values "SED" 1)]
    [(#xf9) (values (format "SBC $~a,y" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    ;; #xfa -io NOP
    ;; #xfb -io ISC aby
    ;; #xfc -io NOP abx
    [(#xfd) (values (format "SBC $~a,x" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    [(#xfe) (values (format "INC $~a,x" (word->hex-string (peek-word-at-pc+1 state))) 3)]
    ;; #xff -io ISC abx
    [else (values "unknown" 0)]))

(require readline/readline)

;; run an read eval print loop debugger on the passed program
(define/c (run-interpreter-single-step-loop org raw-bytes)
  (-> word/c (listof byte/c) any/c)
  (displayln (format "loading program into debugger at ~a" org))
  (define states (list (6510-load (initialize-cpu) org raw-bytes)))
  (readline ">")
  (for ([i (in-naturals)])
    (displayln "")
    (display "Step-Debugger> ") 
    (let ((input (begin (readline ">"))))
      (define pm-regex #px"^pm *\\[([[:xdigit:]]{1,4}), *([[:xdigit:]]{1,2})\\]$")
      (define sm-regex #px"^sm *\\[([[:xdigit:]]{1,4})\\] *= *([[:xdigit:]]{1,2})$")
      (define sa-regex #px"^sa *= *([[:xdigit:]]{1,2})$")
      (define spc-regex #px"^spc *= *([[:xdigit:]]{1,4})$")
      (cond ((string=? input "q") (exit))
            ((string=? input "b") (set! states (cdr states)))
            ((string=? input "s") (set! states (cons (execute-cpu-step (car states)) states)))
            ((string=? input "p") (displayln "") (print-state (car states)))
            ((regexp-match? pm-regex input)             
             (match-let (((list _ addr len) (regexp-match pm-regex input)))
               (displayln (memory->string (string->number addr 16)
                                         (+ -1 (string->number addr 16) (string->number len 16))
                                         (car states)))))
            ((regexp-match? sm-regex input)             
             (match-let (((list _ addr value) (regexp-match sm-regex input)))
               (set! states (cons (poke (car states) (string->number addr 16) (string->number value 16)) states))))
            ((regexp-match? sa-regex input)             
             (match-let (((list _ value) (regexp-match sa-regex input)))
               (set! states (cons (with-accumulator (car states) (string->number value 16)) states))))
            ((regexp-match? spc-regex input)             
             (match-let (((list _ value) (regexp-match spc-regex input)))
               (set! states (cons (with-program-counter (car states) (string->number value 16)) states))))
            ((string=? input "sfc") (set! states (cons (set-carry-flag  (car states)) states)))
            ((string=? input "cfc") (set! states (cons (clear-carry-flag  (car states)) states)))
            ((string=? input "sfb") (set! states (cons (set-brk-flag  (car states)) states)))
            ((string=? input "cfb") (set! states (cons (clear-brk-flag  (car states)) states)))
            ((string=? input "sfn") (set! states (cons (set-negative-flag  (car states)) states)))
            ((string=? input "cfn") (set! states (cons (clear-negative-flag  (car states)) states)))
            ((string=? input "sfv") (set! states (cons (set-overflow-flag  (car states)) states)))
            ((string=? input "cfv") (set! states (cons (clear-overflow-flag  (car states)) states)))
            ((string=? input "sfd") (set! states (cons (set-decimal-flag  (car states)) states)))
            ((string=? input "cfd") (set! states (cons (clear-decimal-flag  (car states)) states)))
            ((string=? input "sfz") (set! states (cons (set-zero-flag  (car states)) states)))
            ((string=? input "cfz") (set! states (cons (clear-zero-flag  (car states)) states)))
            ((string=? input "sfi") (set! states (cons (set-interrupt-flag  (car states)) states)))
            ((string=? input "cfi") (set! states (cons (clear-interrupt-flag  (car states)) states)))
            ;; stop pc=c000 :: stop at pc = c000
            ;; stop a=ff :: stop at accumulator = ff
            ;; stop sp=ff :: stop at stack pointer = fff
            ;; r :: run until stopping
            ;; so :: step over (jsr)
            ((string=? input "pp") (displayln (let-values (((str _) (disassemble (car states)))) str)))
            ((or (string=? input "h")
                (string=? input "?")) (displayln " q = quit,\n s = single step forward,\n p = print cpu state,\n b = backward step,\n pp = pretty print current command"))
            (#t (display "? not understood ?"))))))

;; (run-interpreter-single-step-loop #xc000 (list #xa9 #x41 #x20 #xd2 #xff #x60))
;; put the raw bytes into memory (at org) and start running at org
(define/c (run-interpreter org raw-bytes)
  (-> word/c (listof byte/c) any/c)  
  (displayln (format "loading program into interpreter at ~a" org))
  (displayln "program execution starting:")
  (collect-garbage)
  (displayln (format "memory: ~a" (current-memory-use)))
  (define state (6510-load (initialize-cpu) org raw-bytes))
  (run (with-program-counter state org))
  (collect-garbage)
  (displayln (format "\nmemory: ~a" (current-memory-use)))
  (displayln "program execution done."))
