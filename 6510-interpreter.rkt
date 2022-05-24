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
(define (-poke state address value)
  (struct-copy cpu-state state 
               [memory (set-nth (cpu-state-memory state)
                                (word address)
                                (byte value))]))

(define (-pokem state address values)
  (if (empty? values)
      state
      (-pokem (-poke state address (car values))
              (+ 1 address)
              (cdr values))))

(define (poke state address . values)
  (-pokem state address values))

(define (byte->hex-string num)
  (~a (number->string num 16)
      #:width 2 #:left-pad-string "0" #:align 'right))

(define (word->hex-string num)
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
(define (memory->string from to state)
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

(define (print-memory from to state)
  (printf "~a\n" (memory->string from to state))
  state)

(module+ test #| dump-memory |#
  (check-equal? (memory->string 266 286 (poke (initialize-cpu) #x10C #xFE))
                "010a 00 00 fe 00 00 00 00 00 00 00 00 00 00 00 00 00\n011a 00 00 00 00 00")
  (check-equal? (memory->string 268 268 (poke (initialize-cpu) #x10C #xFE))
                "010c fe"))

(define (state->string state)
  (string-join (list
                (format "A  = x~a,   " (byte->hex-string (cpu-state-accumulator state)))
                (format " X = x~a, " (byte->hex-string (cpu-state-x-index state)))
                (format "Y = x~a~n" (byte->hex-string (cpu-state-y-index state)))
                (format "PC = x~a, " (word->hex-string (cpu-state-program-counter state)))
                (format "SP = x~a~n" (byte->hex-string (cpu-state-stack-pointer state)))
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
                "A  = x00,    X = x00, Y = x00\nPC = x0000, SP = xff\nN=_, O=_, B=_, D=_, I=_, Z=_, C=_"))
(define (print-state state)
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
(define (6510-load state memory-address program)
  (foldl (lambda (state pair) 
           (poke state (first pair) (last pair)))
         state 
         (map list
              (sequence->list (in-range memory-address (+ memory-address (length program))))
              program)))

(module+ test #| 6510-load |#
  (check-equal? (memory->string 10 13 (6510-load (initialize-cpu) 10 (list #x00 #x10 #x00 #x11)))
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
;; pop low-byte, then high-byte form stack, inc by one and write this into the pc
(define (interpret-rts state)
  (let* ([sp (cpu-state-stack-pointer state)]
         [low-ret  (peek state (+ #x100 (byte (+ 1 sp))))]
         [high-ret (peek state (+ #x100 (byte (+ 2 sp))))])
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
                 (poke _ (+ #x100 sp) (high-byte return-address))
                 (poke _ (+ #x100 (byte (- sp 1))) (low-byte return-address))))]))

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
                #x20)
  (check-equal? (peek
                 (interpret-jsr-abs #x40 #x08 (set-pc-in-state (initialize-cpu) #x2001))
                 #x1FE)
                #x03))

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

;; return a state with accumulator set
(define (-set-accumulator new-accumulator state)
  (struct-copy cpu-state state
               [accumulator (byte new-accumulator)]))

(define (-set-x-index new-x-index state)
  (struct-copy cpu-state state
               [x-index (byte new-x-index)]))

(define (-set-y-index new-y-index state)
  (struct-copy cpu-state state
               [y-index (byte new-y-index)]))

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


(define (set-flags-cznv state carry? zero? negative? overflow?)
  (~>> (cpu-state-flags state)
      (-adjust-zero-flag zero?)
      (-adjust-negative-flag negative?)
      (-adjust-carry-flag carry?)
      (-adjust-overflow-flag overflow?)))

(define (set-flags-czn state carry? zero? negative?)
  (~>> (cpu-state-flags state)
      (-adjust-zero-flag zero?)
      (-adjust-negative-flag negative?)
      (-adjust-carry-flag carry?)))

(define (set-flags-zn state zero? negative?)
  (~>> (cpu-state-flags state)
      (-adjust-zero-flag zero?)
      (-adjust-negative-flag negative?)))

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
  (check-true (overflow-flag? (interpret-adc-i (poke  (interpret-adc-i (poke  (initialize-cpu) 1 #x7f)) 3 1))))
  (check-false (overflow-flag? (interpret-clv (interpret-adc-i (poke  (interpret-adc-i (poke  (initialize-cpu) 1 #x7f)) 3 1))))))

(define (indirect-address state address)
    (let* [(low-byte  (peek state address))
           (high-byte (peek state (+ address 1)))]
    (absolute high-byte low-byte)))

;; get the byte that is stored at the memory address
;; that is stored low, high byte ordered at the given address
;;
;; address -> [ low ][ high ]
;; @high,low-> [ value ]
(define (peek-indirect state address)
  (peek state (indirect-address state address)))

(define (peek-indirect-woffset state address offset)
  (peek state (word (+ offset (indirect-address state address)))))

;; put the value at the address constructed from reading
;; low, high byte order from the address provided
;;
;; address -> [ low ][ high ]
;; @high, low <- value
(define (poke-indirect state address value)
  (poke state (indirect-address state address) value))

(define (poke-indirect-woffset state address offset value)  
  (poke state (word (+ offset (indirect-address state address))) value))

;; (zp,x) ->
(define (peek-izx state)
  (let* [(zero-page-idx (peek-pc+1 state))
         (x             (cpu-state-x-index state))]
    (peek-indirect state (+ x zero-page-idx))))

;; (zp,x) <-
(define (poke-izx state value)
  (let* [(zero-page-idx (peek-pc+1 state))
         (x             (cpu-state-x-index state))]
    (poke-indirect state (+ x zero-page-idx) value)))

;; (zp),y ->
(define (peek-izy state)
  (let* [(zero-page-idx (peek-pc+1 state))
         (y             (cpu-state-y-index state))]
    (peek-indirect-woffset state zero-page-idx y)))

(define (poke-izy state value)
  (let* [(zero-page-idx (peek-pc+1 state))
         (idy           (cpu-state-y-index state))]
    (poke-indirect-woffset state zero-page-idx idy value)))


(define (interpret-logic-op-mem state operation peeker pc-inc)
  (let* [(raw-accumulator     (operation (peeker state) (cpu-state-accumulator state)))
         (new-accumulator     (byte raw-accumulator))
         (zero?               (zero? new-accumulator))
         (negative?           (derive-negative raw-accumulator))
         (new-flags           (set-flags-zn state zero? negative?))
         (new-program-counter (+ pc-inc (cpu-state-program-counter state)))]
    (struct-copy cpu-state state
                 [accumulator     new-accumulator]
                 [flags           new-flags]
                 [program-counter new-program-counter])))

(define (peek-zp state)
  (peek state (peek-pc+1 state)))

(define (poke-zp state value)
  (peek state (peek-pc+1 state) value))

(define (peek-zpx state)
  (peek state (+ (cpu-state-x-index state) (peek-pc+1 state))))

(define (poke-zpx state value)
  (poke state (+ (cpu-state-x-index state) (peek-pc+1 state)) value))

(define (peek-absx state)
  (peek state (+ (cpu-state-x-index state) (absolute (peek-pc+2 state) (peek-pc+1 state)))))

(define (poke-absx state value)
  (poke state (+ (cpu-state-x-index state) (absolute (peek-pc+2 state) (peek-pc+1 state))) value))

(define (peek-abs state)
  (peek state (absolute (peek-pc+2 state) (peek-pc+1 state))))

(define (poke-abs state value)
  (poke state (absolute (peek-pc+2 state) (peek-pc+1 state)) value))

(define (peek-zpy state)
  (peek state (+ (cpu-state-y-index state) (peek-pc+1 state))))

(define (derive-carry-after-addition raw-accumulator)
  (< 255 raw-accumulator))

(define (derive-carry-after-subtraction raw-accumulator)
  (> 0 raw-accumulator))

(define (interpret-calc-op state operation add-calc-op peeker carry-deriver pc-inc)
  (let* [(accumulator         (cpu-state-accumulator state))
         (op                  (peeker state))
         (raw-accumulator     (operation accumulator op add-calc-op))
         (new-accumulator     (byte raw-accumulator))
         (carry?              (carry-deriver raw-accumulator))
         (zero?               (zero? new-accumulator))
         (negative?           (derive-negative raw-accumulator))
         [overflow?           (derive-overflow accumulator op raw-accumulator)]
         (new-flags           (set-flags-cznv state carry? zero? negative? overflow?))
         (new-program-counter (+ pc-inc (cpu-state-program-counter state)))]
    (struct-copy cpu-state state
                 [accumulator     new-accumulator]
                 [flags           new-flags]
                 [program-counter new-program-counter])))

;; interpret ADC immediate (add with carry)
(define (interpret-adc-i state)
  (let* [(cf-addon (if (carry-flag? state) 1 0))]
    (interpret-calc-op state + cf-addon peek-pc+1 derive-carry-after-addition 2)))

(module+ test #| interpret-adc-i - checking carry flag related|#
  (check-equal? (cpu-state-accumulator (interpret-adc-i (poke (initialize-cpu) 1 10)))
                10)

  (check-false (carry-flag? (interpret-adc-i (poke  (set-carry-flag (initialize-cpu)) 1 10))))
  (check-equal? (cpu-state-accumulator (interpret-adc-i (poke  (set-carry-flag (initialize-cpu)) 1 10)))
                11)
  (check-true (carry-flag? (interpret-adc-i (poke  (-set-accumulator 246 (initialize-cpu)) 1 10))))
  (check-false (carry-flag? (interpret-adc-i (poke  (-set-accumulator 245 (initialize-cpu)) 1 10)))))

(module+ test #| interpret-adc-i - checking zero flag related|#
  (check-true (zero-flag? (interpret-adc-i (poke  (-set-accumulator 246 (initialize-cpu)) 1 10))))
  (check-false (zero-flag? (interpret-adc-i (poke  (-set-accumulator 245 (initialize-cpu)) 1 10))))
  (check-true (zero-flag? (interpret-adc-i (poke  (set-carry-flag (-set-accumulator 245 (initialize-cpu))) 1 10)))))

(module+ test #| interpret-adc-i - checking overflow flag related|#
  (check-true (overflow-flag? (interpret-adc-i (poke  (-set-accumulator 118 (initialize-cpu)) 1 10))))
  (check-true (overflow-flag? (interpret-adc-i (poke  (-set-accumulator 120 (initialize-cpu)) 1 10))))
  (check-false (overflow-flag? (interpret-adc-i (poke  (-set-accumulator 117 (initialize-cpu)) 1 10))))
  (check-false (overflow-flag? (interpret-adc-i (poke  (-set-accumulator 20 (initialize-cpu)) 1 10))))

  (check-true (overflow-flag? (interpret-adc-i (poke  (-set-accumulator 127 (initialize-cpu)) 1 1))))
  (check-false (overflow-flag? (interpret-adc-i (poke  (-set-accumulator 126 (initialize-cpu)) 1 1)))))

(module+ test #| interpret-adc-i - checking negative flag related|#
  (check-true (negative-flag?  (interpret-adc-i (poke  (-set-accumulator (two-complement-of -2) (initialize-cpu)) 1 1))))
  (check-false (negative-flag? (interpret-adc-i (poke  (-set-accumulator (two-complement-of -1) (initialize-cpu)) 1 1))))
  (check-false (negative-flag? (interpret-adc-i (poke  (-set-accumulator (two-complement-of 1) (initialize-cpu)) 1 1))))
  (check-false (negative-flag? (interpret-adc-i (poke  (-set-accumulator (two-complement-of 0) (initialize-cpu)) 1 1)))))

(module+ test #| ora indirect zero page x - ora ($I,X) ) |#
  (define (interpret-ora-izx state)
    (interpret-logic-op-mem state bitwise-ior peek-izx 2))

  (define (-prepare-op-izx acc operand)
    (~>> (initialize-cpu)
        (-set-accumulator acc _)
        (-set-x-index #x02 _)
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
        (-set-accumulator acc _)
        (-set-y-index #x02 _)
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
      (interpret-calc-op state + cf-addon peek-izx derive-carry-after-addition 2)))

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

(define (interpret-lda-izx state)
  (struct-copy cpu-state state
               [accumulator     (peek-izx state)]
               [program-counter (+ 2 (cpu-state-program-counter state))]))

(define (interpret-sta-izx state)
  (poke-izx state (cpu-state-accumulator state))
  (struct-copy cpu-state state
               [program-counter (+ 2 (cpu-state-program-counter state))]))

(define (interpret-lda-izy state)
  (struct-copy cpu-state state
               [accumulator     (peek-izy state)]
               [program-counter (+ 2 (cpu-state-program-counter state))]))

(define (interpret-sta-izy state)
  (poke-izy state (cpu-state-accumulator state))
  (struct-copy cpu-state state
               [program-counter (+ 2 (cpu-state-program-counter state))]))

(module+ test #| sbc izx |#
  (define (interpret-sbc-izx state)
    (interpret-calc-op state - 0 peek-izx derive-carry-after-subtraction 2))

  (check-eq? (~>> (-prepare-op-izx #x1f #x22)
                 (interpret-sbc-izx _)
                 (cpu-state-accumulator _))
             (two-complement-of (- #x1f #x22)))
  (check-eq? (~>> (-prepare-op-izx #x1f #x22)
                 (set-carry-flag _)
                 (interpret-sbc-izx _)
                 (cpu-state-accumulator _))
             (two-complement-of (- #x1f #x22))
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

(define (compute-asl-result-n-flags state peeker)
    (let* ((operand (peeker state))
         (raw-result (* 2 operand))
         (result (byte raw-result))
         (carry? (< 255 raw-result))
         (zero? (= 0 result))
         (negative? (< 127 result)))
      (list result (set-flags-czn state carry? zero? negative?))))

(define (interpret-asl state)
  (match-let (((list result new-flags) (compute-asl-result-n-flags state cpu-state-accumulator)))
    (struct-copy cpu-state state
                 [accumulator result]
                 [flags new-flags]
                 [program-counter (+ 1 (cpu-state-program-counter state))])))

(module+ test #| asl |#
  (check-eq? (~>> (initialize-cpu)
                 (-set-accumulator #x11 _)
                 (interpret-asl _)
                 (cpu-state-accumulator _))
             #x22))

(define (interpret-asl-mem state peeker poker opcode-len)
  (match-let (((list result new-flags) (compute-asl-result-n-flags state peeker)))
    (struct-copy cpu-state (poker state result)
                 [flags new-flags]
                 [program-counter (+ opcode-len (cpu-state-program-counter state))])))


(module+ test #| interpret asl abs |#
  (define opcode-asl-abs #x0e)

  (check-eq? (~>> (initialize-cpu)
                 (poke _ #x0000 opcode-asl-abs #x0f #xf0)
                 (poke _ #xf00f #x11)
                 (execute-cpu-step _)
                 (peek _ #xf00f))
             #x22))

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
(define (execute-cpu-step state)
  (case (peek-pc state)
    [(#x00) (interpret-brk state)]
    [(#x01) (interpret-logic-op-mem state bitwise-ior peek-izx 2)]
    ;; #x02 -io KIL
    ;; #x03 -io SLO izx
    ;; #x04 -io NOP zp
    [(#x05) (interpret-logic-op-mem state bitwise-ior peek-zp 2)]
    [(#x06) (interpret-asl-mem state peek-zp poke-zp 2)]
    ;; #x07 -io SLO zp
    ;; #x08 PHP
    [(#x09) (interpret-logic-op-mem state bitwise-ior peek-pc+1 2)]
    [(#x0a) (interpret-asl state)]
    ;; #x0b -io ANC imm
    ;; #x0c -io NOP abs
    [(#x0d) (interpret-logic-op-mem state bitwise-ior peek-abs 3)]
    [(#x0e) (interpret-asl-mem state peek-abs poke-abs 3)]
    ;; #x0f -io SLO abs
    ;; #x10 BPL rel
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
    ;; #x24 BIT zp
    ;; #x26 ROL zp
    [(#x25) (interpret-logic-op-mem state bitwise-and peek-zp 2)]
    ;; #x27 -io RLA zp
    ;; #x28 PLP zp
    [(#x29) (interpret-logic-op-mem state bitwise-and peek-pc+1 2)]
    ;; #x2a ROL
    ;; #x2b -io ANC imm
    ;; #x2c BIT abs
    ;; #x2d AND abs
    ;; #x2e ROL bas
    ;; #x2f -io RIA abs
    ;; #x30 BMI rel
    [(#x31) (interpret-logic-op-mem state bitwise-and peek-izy 2)]
    ;; #x32 -io KIL
    ;; #x33 -io RIA izy
    ;; #x34 -io NOP zpx 
    ;; #x36 ROL zpx
    [(#x35) (interpret-logic-op-mem state bitwise-and peek-zpx 2)]
    ;; #x37 -io RLA zpx
    [(#x38) (interpret-sec state)]
    [(#x39) (interpret-logic-op-mem state bitwise-and peek-absy 3)]
    ;; #x3a -io NOP
    ;; #x3b -io RLA aby
    ;; #x3c -io NOP abx
    ;; #x3e ROL abx
    [(#x3d) (interpret-logic-op-mem state bitwise-and peek-absx 3)]
    ;; #x3f -io RLA abx
    [(#x40) (interpret-rti state)]
    [(#x41) (interpret-logic-op-mem state bitwise-xor peek-izx 2)]
    ;; #x42 -io KIL
    ;; #x43 -io SRE izx
    ;; #x44 -io NOP zp
    [(#x45) (interpret-logic-op-mem state bitwise-xor peek-zp 2)]
    ;; #x46 LSR zp
    ;; #x47 -io SRE zp
    ;; #x48 PHA
    [(#x49) (interpret-logic-op-mem state bitwise-xor peek-pc+1 2)]
    ;; #x4a LSR
    ;; #x4b -io ALR imm
    [(#x4C) (interpret-jmp-abs (peek-pc+2 state) (peek-pc+1 state) state)]
    [(#x4d) (interpret-logic-op-mem state bitwise-xor peek-abs 3)]
    ;; #x4e LSR abs
    ;; #x4f -io SRE abs
    ;; #x50 BVC rel
    [(#x51) (interpret-logic-op-mem state bitwise-xor peek-izy 2)]
    ;; #x52 -io KIL
    ;; #x53 -io SRE izy
    ;; #x54 -io NOP zpx
    [(#x55) (interpret-logic-op-mem state bitwise-xor peek-zpx 2)]
    ;; #x56 LSR zpx
    ;; #x57 -io SRE zpx
    [(#x58) (interpret-cli state)]
    [(#x59) (interpret-logic-op-mem state bitwise-xor peek-absy 3)]
    ;; #x5a -io NOP
    ;; #x5b -io SRE aby
    ;; #x5c -io NOP abx
    [(#x5d) (interpret-logic-op-mem state bitwise-xor peek-absx 3)]
    ;; #x5e LSR abx
    ;; #x5f -io SRE abx
    [(#x60) (interpret-rts state)]
    [(#x61) (interpret-calc-op state + (if (carry-flag? state) 1 0) peek-izx derive-carry-after-addition 2)]
    ;; #x62 -io KIL
    ;; #x63 -io RRA izx
    ;; #x64 -io NOP zp
    ;; #x66 ROR zp
    [(#x65) (interpret-calc-op state + (if (carry-flag? state) 1 0) peek-zp derive-carry-after-addition 2)]
    ;; #x67 -io RRA zp
    ;; #x68 PLA
    [(#x69) (interpret-calc-op state + (if (carry-flag? state) 1 0) peek-pc+1 derive-carry-after-addition 2)]
    ;; #x6a ROR
    ;; #x6b -io ARR imm
    ;; #x6c JMP ind
    ;; #x6e ROR abs
    [(#x6d) (interpret-calc-op state + (if (carry-flag? state) 1 0) peek-abs derive-carry-after-addition 3)]
    ;; #x6f -io RRA abs
    ;; #x70 BVS rel
    [(#x71) (interpret-calc-op state + (if (carry-flag? state) 1 0) peek-izy derive-carry-after-addition 2)]
    ;; #x72 -io KIL
    ;; #x73 -io RRA izy
    ;; #x74 -io NOP zpx
    ;; #x76 ROR zpx
    [(#x75) (interpret-calc-op state + (if (carry-flag? state) 1 0) peek-zpx derive-carry-after-addition 2)]
    ;; #x77 -io RRA zpx
    [(#x78) (interpret-sei state)]
    [(#x79) (interpret-calc-op state + (if (carry-flag? state) 1 0) peek-absy derive-carry-after-addition 3)]
    ;; #x7a -io NOP
    ;; #x7b -io RRA aby
    ;; #x7c -io NOP abx
    ;; #x7e ROR abx
    [(#x7d) (interpret-calc-op state + (if (carry-flag? state) 1 0) peek-absx derive-carry-after-addition 3)]
    ;; #x7f -io RRA abx
    ;; #x80 -io NOP imm
    [(#x81) (interpret-sta-izx state)]
    ;; #x82 -io NOP imm
    ;; #x83 -io SAX izx
    ;; #x84 STY zp
    ;; #x85 STA zp
    ;; #x86 STX zp
    ;; #x87 -io SAX zp
    ;; #x88 DEY
    ;; #x89 -io NOP imm
    ;; #x8a TXA
    ;; #x8b -io XAA imm
    ;; #x8c STY abs
    ;; #x8d STA abs
    ;; #x8e STX abs
    ;; #x8f -io SAX abs
    ;; #x90 BCC rel
    [(#x91) (interpret-sta-izy state)]
    ;; #x92 -io KIL
    ;; #x93 0io AHX izy
    ;; #x94 STY zpx
    ;; #x95 STA zpx
    ;; #x96 STX zpx
    ;; #x97 -io SAX zpy
    ;; #x98 TYA
    ;; #x99 STA aby
    ;; #x9a TXS
    ;; #x9b -io TAS avt
    ;; #x9c -io SHY abx
    ;; #x9d STA abx
    ;; #x9e -io SHX aby
    ;; #x9f -io AHX aby
    ;; #xa0 LDY imm
    [(#xa1) (interpret-lda-izx state)]
    ;; #xa2 LDX imm
    ;; #xa3 -io LAX izx
    ;; #xa4 LDY zp
    ;; #xa5 LDA zp
    ;; #xa6 LDX zp
    ;; #xa7 -io LAX zp
    ;; #xa8 TAY
    [(#xA9) (interpret-lda-i (peek-pc+1 state) state)]
    ;; #xaa TAX
    ;; #xab -io LAX imm
    ;; #xac LDY abs
    ;; #xad LDA abs
    ;; #xae LDX abs
    ;; #xaf -io LAX abs
    ;; #xb0 BCS rel
    [(#xb1) (interpret-lda-izy state)]
    ;; #xb2 -io KIL
    ;; #xb3 -io LAX izy
    ;; #xb4 LDY zpx
    ;; #xb5 LDA zpx
    ;; #xb6 LDX zpy
    ;; #xb7 -io LAX zpy
    [(#xB8) (interpret-clv state)]
    ;; #xb9 LDA aby
    ;; #xba TSX
    ;; #xbb -io LAS aby
    ;; #xbc LDY abx
    ;; #xbd LDA abx
    ;; #xbe LDX aby
    ;; #xbf -io LAX aby
    ;; #xc0 CPY imm
    ;; #xc1 CMP izx
    ;; #xc2 -io NOP imm
    ;; #xc3 -io DCP izx
    ;; #xc4 CPY zp
    ;; #xc5 CMP zp
    ;; #xc6 DEC zp
    ;; #xc7 -io DCP zp
    ;; #xc8 INY
    ;; #xc9 CMP imm
    ;; #xca DEX
    ;; #xcb -io AXS imm
    ;; #xcc CPY abs
    ;; #xcd CMP abs
    ;; #xce DEC abs
    ;; #xcf -io DCP abs
    [(#xD0) (interpret-bne-rel (peek-pc+1 state) state)]
    ;; #xd1 CMP izy
    ;; #xd2 -io KIL
    ;; #xd3 -io DCP izy
    ;; #xd4 -io NOP zpx
    ;; #xd5 CMP zpx
    ;; #xd6 DEC zpx
    ;; #xd7 -io DCP zpx
    [(#xD8) (interpret-cld state)]
    ;; #xd9 CMP aby
    ;; #xda -io NOP 
    ;; #xdb -io DCP aby
    ;; #xdc -io NOP abx
    ;; #xdd CMP abx
    ;; #xde DEC abx
    ;; #xdf -io DCP abx
    ;; #xe0 CPX imm
    [(#xe1) (interpret-calc-op state - 0 peek-izx derive-carry-after-subtraction 2)]
    ;; #xe2 -io NOP imm
    ;; #xe3 -io ISC izx
    ;; #xe4 CPX zp
    [(#xe5) (interpret-calc-op state - 0 peek-zp derive-carry-after-subtraction 2)]
    ;; #xe6 INC zp
    ;; #xe7 -io ISC zp
    ;; #xe8 INX
    [(#xe9) (interpret-calc-op state - 0 peek-pc+1 derive-carry-after-subtraction 2)]
    ;; #xea NOP
    ;; #xeb -io SBC imm
    ;; #xec CPX abs
    [(#xed) (interpret-calc-op state - 0 peek-abs derive-carry-after-subtraction 3)]
    ;; #xee INC abs
    ;; #xef -io ISC abs
    ;; #xf0 BEQ rel
    [(#xf1) (interpret-calc-op state - 0 peek-izy derive-carry-after-subtraction 2)]
    ;; #xf2 -io KIL
    ;; #xf3 -io ISC izy
    ;; #xf4 -io NOP zpx
    [(#xf5) (interpret-calc-op state - 0 peek-zpx derive-carry-after-subtraction 2)]
    ;; #xf6 INC zpx
    ;; #xf7 -io ISC zpx
    [(#xF8) (interpret-sed state)]
    [(#xf9) (interpret-calc-op state - 0 peek-absy derive-carry-after-subtraction 3)]
    ;; #xfa -io NOP
    ;; #xfb -io ISC aby
    ;; #xfc -io NOP abx
    [(#xfd) (interpret-calc-op state - 0 peek-absx derive-carry-after-subtraction 3)]
    ;; #xfe INC abx
    ;; #xff -io ISC abx
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
