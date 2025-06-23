#lang at-exp racket


#|

 basic 6510 interpreter

 e.g. (run-interpreter org raw-bytes)

 |#

;; todo: check whether lense implementation is better (more efficient) than struct-copy (see https://docs.racket-lang.org/lens/struct-guide.html)
;; reference: see c64os.com/post/6502instructions
;; or: https://www.middle-engine.com/blog/posts/2020/06/23/programming-the-nes-the-6502-in-detail
;; control characters: https://www.c64-wiki.com/wiki/control_character



;; set cursor speed = poke 56341,x (where x = 0 to 255)
;; switch to uppercase/graphics =poke 53272,21
;; switch to lowercase/uppercase = poke 53272,23
;; set border colour = poke 53280,x (where x = 0 to 15)
;; set background colour = poke 53281,x (where x = 0 to 15)
;; set cursor color = poke 646,x (where x = 0 to 15)
;; disable list = poke 775,200
;; enable list = poke 775,167
;; hide line numbers = poke 22,35
;; show line number = poke 22,25
;; turn cursor on during get = poke 204,0
;; turn cursor back off = poke 204,255
;; turn off question mark during input = poke 19,65
;; turn question mark back on = poke 19,0
;; make a click sound = poke 54296,15:POKE 54296,0
;; disable keyboard = poke 649,0
;; enable keyboard = poke 649,10
;; disable keyboard buffer = poke 649,1
;; increase keyboard buffer = poke 649,15
;; clear keyboard buffer = poke 198,0
;; no keys repeat = poke 650,127
;; all keys repeat = poke 650,128
;; normal repeat = poke 650,0
;; disable SHIFT-Commodore = poke 657,128
;; enable SHIFT-Commodore = poke 657,0
;; disable RUN/STOP = poke 808,239
;; disable RESTORE = poke 792,193
;; disable RUN-STOP/RESTORE = poke 808,239:poke 792,193
;; disable RUN-STOP/RESTORE and list = poke 808,234
;; enable RUN-STOP/RESTORE and list = poke 808,237:poke 792,71
;; poke 1,0 = disable OS (default value 1,1)

;; (require (only-in racket/fixnum make-fxvector fxvector-ref fxvector-set!))
(require (only-in threading ~>>))
(require "../6510-utils.rkt")
(require "../ast/6510-command.rkt")
(require "../ast/6510-assembler.rkt")
(require scribble/srcdoc)
(require (for-doc scribble/base scribble/manual))
(require data/pvector)
(require data/collection)
(require racket/fixnum)
(require (rename-in  racket/contract [define/contract define/c]))
(require (only-in ansi-color color-displayln))

(module+ test #| include rackunit |#
  (require rackunit))

(provide 6510-load
         6510-load-multiple
         execute-cpu-step
         (struct-out exn:fail:cpu-interpreter)
         initialize-cpu
         memory->string
         memory-list
         peek
         peek-pc
         peek-pc+1
         peek-word-at-pc+1
         peek-word-at-address
         -pokem
         poke
         print-state
         state->string
         reset-cpu
         run
         run-interpreter
         run-interpreter-on
         set-brk-flag clear-brk-flag
         set-carry-flag clear-carry-flag
         set-decimal-flag clear-decimal-flag
         set-interrupt-flag clear-interrupt-flag
         set-negative-flag clear-negative-flag
         set-overflow-flag clear-overflow-flag
         set-zero-flag clear-zero-flag
         with-accumulator
         with-program-counter
         reset-cycles
         with-flags)

(struct cpu-state (program-counter ;; pointer to current program execution (16 bit)
                   flags           ;; flag register (8 bit)
                   memory          ;; 64kB memory byte vector
                   accumulator     ;; accumulator register (8 bit)
                   x-index         ;; x index register (8 bit)
                   y-index         ;; y index register (8 bit)
                   stack-pointer   ;; current stack pointer (8+1 bit) 1xx
                   clock-cycles    ;; used clock cycles of the current interpretation
                   )
  #:guard (struct-guard/c
           word/c
           byte/c
           pvector?
           byte/c
           byte/c
           byte/c
           byte/c
           nonnegative-integer?))

(provide (struct-doc cpu-state ([program-counter word/c]
                                [flags byte/c]
                                [memory byte/c]
                                [accumulator byte/c]
                                [x-index byte/c]
                                [y-index byte/c]
                                [stack-pointer byte/c]
                                [clock-cycles nonnegative-integer?]) @{Doc test}))

;; flags all negative, program counter at 0, registers all 0, sp = 0xFF
(define/c (initialize-cpu)
  (-> cpu-state?)
  (cpu-state 0 0 (make-pvector 65536 0) 0 0 0 #xff 0))

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

;; set the byte at the given memory address
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
  (->* [cpu-state? word/c] [] #:rest (listof byte/c) cpu-state?)
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

(define/c (byte->dump-char dump-byte)
  (-> byte/c string?)
  (define dump-char (integer->char dump-byte))
  (cond
    [(char-graphic? dump-char) (string dump-char)]
    [else "."]))

(define/c (memory-list state from to)
  (-> cpu-state? word/c word/c (listof byte/c))
  (stream->list
   (map (lambda (idx) (peek state idx))
        (range from (fx+ 1 to)))))

;; create a string formated with 'address byte+0 byte+1 ... byte+15' per line
(define/c (indexed-memory-list state from to)
  (-> cpu-state? word/c word/c (stream/c pair?))
  (indexed
   (map (lambda (idx) (peek state idx))
        (range from (fx+ 1 to)))))

;; adress for dump
(define/c (chunk-address--indexed-memory-list idx-mem-list from)
  (-> (stream/c pair?) word/c string?)
  (word->hex-string (fx+ from (caar (stream->list idx-mem-list)))))

;; address + hex bytes for dump
(define/c (memory->addr-bytes-string--indexed-memory-list idx-mem-list from)
  (-> (stream/c pair?) word/c string?)
  (string-join
   (stream->list
    (append (list (chunk-address--indexed-memory-list idx-mem-list from))
            (map (lambda (pair) (byte->hex-string (cdr pair))) idx-mem-list)))
   " "))

;; string of bytes converted to visible chars or '.'
(define/c (memory->vis-bytes-string--indexed-memory-list state idx-mem-list from)
  (-> cpu-state? (stream/c pair?) word/c string?)
  (string-join
   (stream->list
    (append
     (map (lambda (pair) (byte->dump-char (peek state (fx+ from (car pair))))) idx-mem-list)))
   ""))

(define/c (memory->string from to state)
  (-> word/c word/c cpu-state? string?) 
  (string-join
   (stream->list
    (map (lambda (it)
           (string-append
            (memory->addr-bytes-string--indexed-memory-list it from)
            "  "
            (memory->vis-bytes-string--indexed-memory-list state it from)))
         (chunk 16 (indexed-memory-list state from to))))
   "\n"))

(module+ test #| indexed-memory-list |#
  (check-equal? (chunk-address--indexed-memory-list (stream '(10 . 0) '(11 . 65)) #xa000)
             "a00a")
  (check-equal? (stream->list (indexed-memory-list (poke (initialize-cpu) #x0100 #x41) #x00ff #x0101))
                '((0 . 0) (1 . #x41) (2 . 0))))

;; print memory starting at address FROM to address TO of STATE
(define/c (print-memory from to state)
  (-> word/c word/c cpu-state? cpu-state?)
  (printf "~a\n" (memory->string from to state))
  state)

(module+ test #| dump-memory |#
  (check-equal? (memory->string 266 286 (poke (poke (initialize-cpu) #x10C #x41) #x011b #x42))
                "010a 00 00 41 00 00 00 00 00 00 00 00 00 00 00 00 00  ..A.............\n011a 00 42 00 00 00  .B...")
  (check-equal? (memory->string 268 268 (poke (initialize-cpu) #x10C #x41))
                "010c 41  A"))

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

(define/c (state->compact-string state)
  (-> cpu-state? string?)
  (string-join (list
                (format "A: $~a, " (byte->hex-string (cpu-state-accumulator state)))
                (format "X: $~a, " (byte->hex-string (cpu-state-x-index state)))
                (format "Y: $~a (" (byte->hex-string (cpu-state-y-index state)))
                (format "SP: $~a, " (byte->hex-string (cpu-state-stack-pointer state)))
                (format "N~a O~a B~a D~a I~a Z~a C~a)"
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
(define/c (print-state state (compact #f))
  (->* [cpu-state?] [boolean?] cpu-state?)
  (printf "~a~a" (if compact (state->compact-string state) (state->string state)) (if compact "" "\n"))
  state)

(module+ test #| set-nth |#
  (check-equal? (nth (set-nth (cpu-state-memory (initialize-cpu)) 65535 1)
                     65535)
                1))

(module+ test #| peek and poke |#
  (check-match (peek (poke (initialize-cpu) #xc000 17) #xc000)
               17))

;; code-list is a list of pairs
;; each pair being the memory location to write to
;;                 and the list of raw bytes to write
;; last pair sets the pc
(define/c (6510-load-multiple state code-list)
  (-> cpu-state? (listof pair?) cpu-state?)
  (with-program-counter
    (foldl (lambda (lstate code)
             (6510-load lstate (car code) (cdr code)))
           state
           code-list)
    (caar code-list)))

(module+ test #| 6510-load-multiple |#
  (check-equal? (memory->string 10 13 (6510-load-multiple (initialize-cpu) '((10 . (#x00 #x10)) (12 . (#x41 #x11)))))
                "000a 00 10 41 11  ..A.")
  (check-equal? (cpu-state-program-counter (6510-load-multiple (initialize-cpu) '((10 . (#x00 #x10)) (12 . (#x41 #x11)))))
                10))

;; load program into memory using the 6510 state
(define/c (6510-load state memory-address program)
  (-> cpu-state? word/c (listof byte/c) cpu-state?)
  (with-program-counter  
    (foldl (lambda (lstate pair)
             (poke lstate (first pair) (last pair)))
           state 
           (map list
                (sequence->list (in-range memory-address (fx+ memory-address (length program))))
                program))
    memory-address))

(module+ test #| 6510-load |#
  (check-equal? (memory->string 10 13 (6510-load (initialize-cpu) 10 (list #x00 #x10 #x41 #x11)))
                "000a 00 10 41 11  ..A."
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
(define/c (run state (verbose #t) (string-output-function interpreter-output-function))
  (->* [cpu-state?] [boolean? (-> string? any/c)] cpu-state?)
  (cond [(zero? (peek-pc state))
         state]
        [else
         (let ([next-state (execute-cpu-step state verbose string-output-function)])
           (run next-state verbose string-output-function))]))

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
(define/c (display-c64charcode byte state (verbose #t) (string-output-function interpreter-output-function))
  (->* [byte/c cpu-state?] [boolean? (-> string? any/c)] cpu-state?)
  (case byte
    [(#x0d) (when verbose (string-output-function "\n")) state] ;; linefeed
    [(#x0e) (when verbose (string-output-function "")) (poke state 53272 23)] ;; switch to lower letter mode
    [(#x12) (when verbose (string-output-function "")) (poke state 199 #x12)] ;; reverse on
    [(#x92) (when verbose (string-output-function "")) (poke state 199 #x00)] ;; reverse off
    [else (when verbose (string-output-function (string (integer->char (c64-byte->unicode byte state)))))
          state]))

(define/c (display-c64zerotermstring word state (verbose #t) (string-output-function interpreter-output-function))
  (->* [word/c cpu-state?] [boolean? (-> string? any/c)] cpu-state?)
  (let ([cbyte (peek state word)])
    (unless (eq? 0 cbyte)
      (display-c64charcode cbyte state verbose string-output-function)
      (display-c64zerotermstring (fx+ 1 word) state verbose string-output-function)))
  state)

;; unmodified mapping
(define unicode->c64-high-map
  (hash
   ;;#x05	#xF100	;#WHITE COLOR SWITCH (CUS)
   ;; #x08	#xF118	;#DISABLE CHARACTER SET SWITCHING (CUS)
   ;; #x09	#xF119	;#ENABLE CHARACTER SET SWITCHING (CUS)
   #x0D	#x000D	;#CARRIAGE RETURN
   ;; #x0E	#x000E	;#SHIFT OUT
   ;; #x11	#xF11C	;#CURSOR DOWN (CUS)
   ;; #x12	#xF11A	;#REVERSE VIDEO ON (CUS)
   ;; #x13	#xF120	;#HOME (CUS)
   ;; #x14	#x007F	;#DELETE
   ;; #x1C	#xF101	;#RED COLOR SWITCH (CUS)
   ;; #x1D	#xF11D	;#CURSOR RIGHT (CUS)
   ;; #x1E	#xF102	;#GREEN COLOR SWITCH (CUS)
   ;; #x1F	#xF103	;#BLUE COLOR SWITCH (CUS)
   #x20	#x0020	;#SPACE
   #x21	#x0021	;#EXCLAMATION MARK
   #x22	#x0022	;#QUOTATION MARK
   #x23	#x0023	;#NUMBER SIGN
   #x24	#x0024	;#DOLLAR SIGN
   #x25	#x0025	;#PERCENT SIGN
   #x26	#x0026	;#AMPERSAND
   #x27	#x0027	;#APOSTROPHE
   #x28	#x0028	;#LEFT PARENTHESIS
   #x29	#x0029	;#RIGHT PARENTHESIS
   #x2A	#x002A	;#ASTERISK
   #x2B	#x002B	;#PLUS SIGN
   #x2C	#x002C	;#COMMA
   #x2D	#x002D	;#HYPHEN-MINUS
   #x2E	#x002E	;#FULL STOP
   #x2F	#x002F	;#SOLIDUS
   #x30	#x0030	;#DIGIT ZERO
   #x31	#x0031	;#DIGIT ONE
   #x32	#x0032	;#DIGIT TWO
   #x33	#x0033	;#DIGIT THREE
   #x34	#x0034	;#DIGIT FOUR
   #x35	#x0035	;#DIGIT FIVE
   #x36	#x0036	;#DIGIT SIX
   #x37	#x0037	;#DIGIT SEVEN
   #x38	#x0038	;#DIGIT EIGHT
   #x39	#x0039	;#DIGIT NINE
   #x3A	#x003A	;#COLON
   #x3B	#x003B	;#SEMICOLON
   #x3C	#x003C	;#LESS-THAN SIGN
   #x3D	#x003D	;#EQUALS SIGN
   #x3E	#x003E	;#GREATER-THAN SIGN
   #x3F	#x003F	;#QUESTION MARK
   #x40	#x0040	;#COMMERCIAL AT
   #x41	#x0041	;#LATIN CAPITAL LETTER A
   #x42	#x0042	;#LATIN CAPITAL LETTER B
   #x43	#x0043	;#LATIN CAPITAL LETTER C
   #x44	#x0044	;#LATIN CAPITAL LETTER D
   #x45	#x0045	;#LATIN CAPITAL LETTER E
   #x46	#x0046	;#LATIN CAPITAL LETTER F
   #x47	#x0047	;#LATIN CAPITAL LETTER G
   #x48	#x0048	;#LATIN CAPITAL LETTER H
   #x49	#x0049	;#LATIN CAPITAL LETTER I
   #x4A	#x004A	;#LATIN CAPITAL LETTER J
   #x4B	#x004B	;#LATIN CAPITAL LETTER K
   #x4C	#x004C	;#LATIN CAPITAL LETTER L
   #x4D	#x004D	;#LATIN CAPITAL LETTER M
   #x4E	#x004E	;#LATIN CAPITAL LETTER N
   #x4F	#x004F	;#LATIN CAPITAL LETTER O
   #x50	#x0050	;#LATIN CAPITAL LETTER P
   #x51	#x0051	;#LATIN CAPITAL LETTER Q
   #x52	#x0052	;#LATIN CAPITAL LETTER R
   #x53	#x0053	;#LATIN CAPITAL LETTER S
   #x54	#x0054	;#LATIN CAPITAL LETTER T
   #x55	#x0055	;#LATIN CAPITAL LETTER U
   #x56	#x0056	;#LATIN CAPITAL LETTER V
   #x57	#x0057	;#LATIN CAPITAL LETTER W
   #x58	#x0058	;#LATIN CAPITAL LETTER X
   #x59	#x0059	;#LATIN CAPITAL LETTER Y
   #x5A	#x005A	;#LATIN CAPITAL LETTER Z
   #x5B	#x005B	;#LEFT SQUARE BRACKET
   #x5C	#x00A3	;#POUND SIGN
   #x5D	#x005D	;#RIGHT SQUARE BRACKET
   #x5E	#x2191	;#UPWARDS ARROW
   #x5F	#x2190	;#LEFTWARDS ARROW
   #x60	#x2501	;#BOX DRAWINGS LIGHT HORIZONTAL
   #x61	#x2660	;#BLACK SPADE SUIT
   #x62	#x2502	;#BOX DRAWINGS LIGHT VERTICAL
   #x63	#x2501	;#BOX DRAWINGS LIGHT HORIZONTAL
   #x64	#xF122	;#BOX DRAWINGS LIGHT HORIZONTAL ONE QUARTER UP (CUS)
   #x65	#xF123	;#BOX DRAWINGS LIGHT HORIZONTAL TWO QUARTERS UP (CUS)
   #x66	#xF124	;#BOX DRAWINGS LIGHT HORIZONTAL ONE QUARTER DOWN (CUS)
   #x67	#xF126	;#BOX DRAWINGS LIGHT VERTICAL ONE QUARTER LEFT (CUS)
   #x68	#xF128	;#BOX DRAWINGS LIGHT VERTICAL ONE QUARTER RIGHT (CUS)
   #x69	#x256E	;#BOX DRAWINGS LIGHT ARC DOWN AND LEFT
   #x6A	#x2570	;#BOX DRAWINGS LIGHT ARC UP AND RIGHT
   #x6B	#x256F	;#BOX DRAWINGS LIGHT ARC UP AND LEFT
   #x6C	#xF12A	;#ONE EIGHTH BLOCK UP AND RIGHT (CUS)
   #x6D	#x2572	;#BOX DRAWINGS LIGHT DIAGONAL UPPER LEFT TO LOWER RIGHT
   #x6E	#x2571	;#BOX DRAWINGS LIGHT DIAGONAL UPPER RIGHT TO LOWER LEFT
   #x6F	#xF12B	;#ONE EIGHTH BLOCK DOWN AND RIGHT (CUS)
   #x70	#xF12C	;#ONE EIGHTH BLOCK DOWN AND LEFT (CUS)
   #x71	#x25CF	;#BLACK CIRCLE
   #x72	#xF125	;#BOX DRAWINGS LIGHT HORIZONTAL TWO QUARTERS DOWN (CUS)
   #x73	#x2665	;#BLACK HEART SUIT
   #x74	#xF127	;#BOX DRAWINGS LIGHT VERTICAL TWO QUARTERS LEFT (CUS)
   #x75	#x256D	;#BOX DRAWINGS LIGHT ARC DOWN AND RIGHT
   #x76	#x2573	;#BOX DRAWINGS LIGHT DIAGONAL CROSS
   #x77	#x25CB	;#WHITE CIRCLE
   #x78	#x2663	;#BLACK CLUB SUIT
   #x79	#xF129	;#BOX DRAWINGS LIGHT VERTICAL TWO QUARTERS RIGHT (CUS)
   #x7A	#x2666	;#BLACK DIAMOND SUIT
   #x7B	#x253C	;#BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
   #x7C	#xF12E	;#LEFT HALF BLOCK MEDIUM SHADE (CUS)
   #x7D	#x2502	;#BOX DRAWINGS LIGHT VERTICAL
   #x7E	#x03C0	;#GREEK SMALL LETTER PI
   #x7F	#x25E5	;#BLACK UPPER RIGHT TRIANGLE
   #x81	#xF104	;#ORANGE COLOR SWITCH (CUS)
   #x85	#xF110	;#FUNCTION KEY 1 (CUS)
   #x86	#xF112	;#FUNCTION KEY 3 (CUS)
   #x87	#xF114	;#FUNCTION KEY 5 (CUS)
   #x88	#xF116	;#FUNCTION KEY 7 (CUS)
   #x89	#xF111	;#FUNCTION KEY 2 (CUS)
   #x8A	#xF113	;#FUNCTION KEY 4 (CUS)
   #x8B	#xF115	;#FUNCTION KEY 6 (CUS)
   #x8C	#xF117	;#FUNCTION KEY 8 (CUS)
   ;; #x8D	#x000A	;#LINE FEED
   ;; #x8E	#x000F	;#SHIFT IN
   #x90	#xF105	;#BLACK COLOR SWITCH (CUS)
   ;; #x91	#xF11E	;#CURSOR UP (CUS)
   ;; #x92	#xF11B	;#REVERSE VIDEO OFF (CUS)
   ;; #x93	#x000C	;#FORM FEED
   ;; #x94	#xF121	;#INSERT (CUS)
   ;; #x95	#xF106	;#BROWN COLOR SWITCH (CUS)
   ;; #x96	#xF107	;#LIGHT RED COLOR SWITCH (CUS)
   ;; #x97	#xF108	;#GRAY 1 COLOR SWITCH (CUS)
   ;; #x98	#xF109	;#GRAY 2 COLOR SWITCH (CUS)
   ;; #x99	#xF10A	;#LIGHT GREEN COLOR SWITCH (CUS)
   ;; #x9A	#xF10B	;#LIGHT BLUE COLOR SWITCH (CUS)
   ;; #x9B	#xF10C	;#GRAY 3 COLOR SWITCH (CUS)
   ;; #x9C	#xF10D	;#PURPLE COLOR SWITCH (CUS)
   ;; #x9D	#xF11D	;#CURSOR LEFT (CUS)
   ;; #x9E	#xF10E	;#YELLOW COLOR SWITCH (CUS)
   ;; #x9F	#xF10F	;#CYAN COLOR SWITCH (CUS)
   #xA0	#x00A0	;#NO-BREAK SPACE
   #xA1	#x258C	;#LEFT HALF BLOCK
   #xA2	#x2584	;#LOWER HALF BLOCK
   #xA3	#x2594	;#UPPER ONE EIGHTH BLOCK
   #xA4	#x2581	;#LOWER ONE EIGHTH BLOCK
   #xA5	#x258F	;#LEFT ONE EIGHTH BLOCK
   #xA6	#x2592	;#MEDIUM SHADE
   #xA7	#x2595	;#RIGHT ONE EIGHTH BLOCK  
   #xA8	#xF12F	;#LOWER HALF BLOCK MEDIUM SHADE (CUS)
   #xA9	#x25E4	;#BLACK UPPER LEFT TRIANGLE
   #xAA	#xF130	;#RIGHT ONE QUARTER BLOCK (CUS)
   #xAB	#x251C	;#BOX DRAWINGS LIGHT VERTICAL AND RIGHT
   #xAC	#xF134	;#BLACK SMALL SQUARE LOWER RIGHT (CUS)
   #xAD	#x2514	;#BOX DRAWINGS LIGHT UP AND RIGHT
   #xAE	#x2510	;#BOX DRAWINGS LIGHT DOWN AND LEFT
   #xAF	#x2582	;#LOWER ONE QUARTER BLOCK
   #xB0	#x250C	;#BOX DRAWINGS LIGHT DOWN AND RIGHT
   #xB1	#x2534	;#BOX DRAWINGS LIGHT UP AND HORIZONTAL
   #xB2	#x252C	;#BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
   #xB3	#x2524	;#BOX DRAWINGS LIGHT VERTICAL AND LEFT
   #xB4	#x258E	;#LEFT ONE QUARTER BLOCK
   #xB5	#x258D	;#LEFT THREE EIGTHS BLOCK
   #xB6	#xF131	;#RIGHT THREE EIGHTHS BLOCK (CUS) 
   #xB7	#xF132	;#UPPER ONE QUARTER BLOCK (CUS)
   #xB8	#xF133	;#UPPER THREE EIGHTS BLOCK (CUS)
   #xB9	#x2583	;#LOWER THREE EIGHTHS BLOCK
   #xBA	#xF12D	;#ONE EIGHTH BLOCK UP AND LEFT (CUS)
   #xBB	#xF135	;#BLACK SMALL SQUARE LOWER LEFT (CUS)
   #xBC	#xF136	;#BLACK SMALL SQUARE UPPER RIGHT (CUS)
   #xBD	#x2518	;#BOX DRAWINGS LIGHT UP AND LEFT
   #xBE	#xF137	;#BLACK SMALL SQUARE UPPER LEFT (CUS)
   #xBF	#xF138	;#TWO SMALL BLACK SQUARES DIAGONAL LEFT TO RIGHT (CUS)
   #xC0	#x2501	;#BOX DRAWINGS LIGHT HORIZONTAL
   #xC1	#x2660	;#BLACK SPADE SUIT
   #xC2	#x2502	;#BOX DRAWINGS LIGHT VERTICAL
   #xC3	#x2501	;#BOX DRAWINGS LIGHT HORIZONTAL
   #xC4	#xF122	;#BOX DRAWINGS LIGHT HORIZONTAL ONE QUARTER UP (CUS)
   #xC5	#xF123	;#BOX DRAWINGS LIGHT HORIZONTAL TWO QUARTERS UP (CUS)
   #xC6	#xF124	;#BOX DRAWINGS LIGHT HORIZONTAL ONE QUARTER DOWN (CUS)
   #xC7	#xF126	;#BOX DRAWINGS LIGHT VERTICAL ONE QUARTER LEFT (CUS)
   #xC8	#xF128	;#BOX DRAWINGS LIGHT VERTICAL ONE QUARTER RIGHT (CUS)
   #xC9	#x256E	;#BOX DRAWINGS LIGHT ARC DOWN AND LEFT
   #xCA	#x2570	;#BOX DRAWINGS LIGHT ARC UP AND RIGHT
   #xCB	#x256F	;#BOX DRAWINGS LIGHT ARC UP AND LEFT
   #xCC	#xF12A	;#ONE EIGHTH BLOCK UP AND RIGHT (CUS)
   #xCD	#x2572	;#BOX DRAWINGS LIGHT DIAGONAL UPPER LEFT TO LOWER RIGHT
   #xCE	#x2571	;#BOX DRAWINGS LIGHT DIAGONAL UPPER RIGHT TO LOWER LEFT
   #xCF	#xF12B	;#ONE EIGHTH BLOCK DOWN AND RIGHT (CUS)
   #xD0	#xF12C	;#ONE EIGHTH BLOCK DOWN AND LEFT (CUS)
   #xD1	#x25CF	;#BLACK CIRCLE
   #xD2	#xF125	;#BOX DRAWINGS LIGHT HORIZONTAL TWO QUARTERS DOWN (CUS)
   #xD3	#x2665	;#BLACK HEART SUIT
   #xD4	#xF127	;#BOX DRAWINGS LIGHT VERTICAL TWO QUARTERS LEFT (CUS)
   #xD5	#x256D	;#BOX DRAWINGS LIGHT ARC DOWN AND LEFT
   #xD6	#x2573	;#BOX DRAWINGS LIGHT DIAGONAL CROSS
   #xD7	#x25CB	;#WHITE CIRCLE
   #xD8	#x2663	;#BLACK CLUB SUIT
   #xD9	#xF129	;#BOX DRAWINGS LIGHT VERTICAL TWO QUARTERS RIGHT (CUS)
   #xDA	#x2666	;#BLACK DIAMOND SUIT
   #xDB	#x253C	;#BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
   #xDC	#xF12E	;#LEFT HALF BLOCK MEDIUM SHADE (CUS)
   #xDD	#x2502	;#BOX DRAWINGS LIGHT VERTICAL
   #xDE	#x03C0	;#GREEK SMALL LETTER PI
   #xDF	#x25E5	;#BLACK UPPER RIGHT TRIANGLE
   #xE0	#x00A0	;#NO-BREAK SPACE
   #xE1	#x258C	;#LEFT HALF BLOCK
   #xE2	#x2584	;#LOWER HALF BLOCK
   #xE3	#x2594	;#UPPER ONE EIGHTH BLOCK
   #xE4	#x2581	;#LOWER ONE EIGHTH BLOCK
   #xE5	#x258F	;#LEFT ONE EIGHTH BLOCK
   #xE6	#x2592	;#MEDIUM SHADE
   #xE7	#x2595	;#RIGHT ONE EIGHTH BLOCK
   #xE8	#xF12F	;#LOWER HALF BLOCK MEDIUM SHADE (CUS)
   #xE9	#x25E4	;#BLACK UPPER LEFT TRIANGLE
   #xEA	#xF130	;#RIGHT ONE QUARTER BLOCK (CUS)
   #xEB	#x251C	;#BOX DRAWINGS LIGHT VERTICAL AND RIGHT
   #xEC	#xF134	;#BLACK SMALL SQUARE LOWER RIGHT (CUS)
   #xED	#x2514	;#BOX DRAWINGS LIGHT UP AND RIGHT
   #xEE	#x2510	;#BOX DRAWINGS LIGHT DOWN AND LEFT
   #xEF	#x2582	;#LOWER ONE QUARTER BLOCK
   #xF0	#x250C	;#BOX DRAWINGS LIGHT DOWN AND RIGHT
   #xF1	#x2534	;#BOX DRAWINGS LIGHT UP AND HORIZONTAL
   #xF2	#x252C	;#BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
   #xF3	#x2524	;#BOX DRAWINGS LIGHT VERTICAL AND LEFT
   #xF4	#x258E	;#LEFT ONE QUARTER BLOCK
   #xF5	#x258D	;#LEFT THREE EIGTHS BLOCK
   #xF6	#xF131	;#RIGHT THREE EIGHTHS BLOCK (CUS)
   #xF7	#xF132	;#UPPER ONE QUARTER BLOCK (CUS)
   #xF8	#xF133	;#UPPER THREE EIGHTS BLOCK (CUS)
   #xF9	#x2583	;#LOWER THREE EIGHTHS BLOCK
   #xFA	#xF12D	;#ONE EIGHTH BLOCK UP AND LEFT (CUS)
   #xFB	#xF135	;#BLACK SMALL SQUARE LOWER LEFT (CUS)
   #xFC	#xF136	;#BLACK SMALL SQUARE UPPER RIGHT (CUS)
   #xFD	#x2518	;#BOX DRAWINGS LIGHT UP AND LEFT
   #xFE	#xF137	;#BLACK SMALL SQUARE UPPER LEFT (CUS)
   #xFF	#x03C0	;#GREEK SMALL LETTER PI
))

(define unicode->c64-low-map-reverse (hash #xa7 #x2589))

;; mapping when switching lowercase
(define unicode->c64-low-map
  (hash 
   ;;   #x05	#xF100	#WHITE COLOR SWITCH (CUS)
   ;;#x08	#xF118	#DISABLE CHARACTER SET SWITCHING (CUS)
   ;;#x09	#xF119	#ENABLE CHARACTER SET SWITCHING (CUS)
   ;; #x0A		#UNDEFINED
   ;; #x0B		#UNDEFINED
   ;; #x0C		#UNDEFINED
   #x0D	#x000D	;#CARRIAGE RETURN
   ;; #x0E	#x000E	;#SHIFT OUT
   ;; #x11	#xF11C	;#CURSOR DOWN (CUS)
   ;; #x12	#xF11A	;#REVERSE VIDEO ON (CUS)
   ;; #x13	#xF120	;#HOME (CUS)
   ;; #x14	#x007F	;#DELETE
   ;; #x1C	#xF101	;#RED COLOR SWITCH (CUS)
   ;; #x1D	#xF11D	;#CURSOR RIGHT (CUS)
   ;; #x1E	#xF102	;#GREEN COLOR SWITCH (CUS)
   ;; #x1F	#xF103	;#BLUE COLOR SWITCH (CUS)
   #x20	#x0020	;#SPACE
   #x21	#x0021	;#EXCLAMATION MARK
   #x22	#x0022	;#QUOTATION MARK
   #x23	#x0023	;#NUMBER SIGN
   #x24	#x0024	;#DOLLAR SIGN
   #x25	#x0025	;#PERCENT SIGN
   #x26	#x0026	;#AMPERSAND
   #x27	#x0027	;#APOSTROPHE
   #x28	#x0028	;#LEFT PARENTHESIS
   #x29	#x0029	;#RIGHT PARENTHESIS
   #x2A	#x002A	;#ASTERISK
   #x2B	#x002B	;#PLUS SIGN
   #x2C	#x002C	;#COMMA
   #x2D	#x002D	;#HYPHEN-MINUS
   #x2E	#x002E	;#FULL STOP
   #x2F	#x002F	;#SOLIDUS
   #x30	#x0030	;#DIGIT ZERO
   #x31	#x0031	;#DIGIT ONE
   #x32	#x0032	;#DIGIT TWO
   #x33	#x0033	;#DIGIT THREE
   #x34	#x0034	;#DIGIT FOUR
   #x35	#x0035	;#DIGIT FIVE
   #x36	#x0036	;#DIGIT SIX
   #x37	#x0037	;#DIGIT SEVEN
   #x38	#x0038	;#DIGIT EIGHT
   #x39	#x0039	;#DIGIT NINE
   #x3A	#x003A	;#COLON
   #x3B	#x003B	;#SEMICOLON
   #x3C	#x003C	;#LESS-THAN SIGN
   #x3D	#x003D	;#EQUALS SIGN
   #x3E	#x003E	;#GREATER-THAN SIGN
   #x3F	#x003F	;#QUESTION MARK
   #x40	#x0040	;#COMMERCIAL AT
   #x41	#x0061	;#LATIN SMALL LETTER A
   #x42	#x0062	;#LATIN SMALL LETTER B
   #x43	#x0063	;#LATIN SMALL LETTER C
   #x44	#x0064	;#LATIN SMALL LETTER D
   #x45	#x0065	;#LATIN SMALL LETTER E
   #x46	#x0066	;#LATIN SMALL LETTER F
   #x47	#x0067	;#LATIN SMALL LETTER G
   #x48	#x0068	;#LATIN SMALL LETTER H
   #x49	#x0069	;#LATIN SMALL LETTER I
   #x4A	#x006A	;#LATIN SMALL LETTER J
   #x4B	#x006B	;#LATIN SMALL LETTER K
   #x4C	#x006C	;#LATIN SMALL LETTER L
   #x4D	#x006D	;#LATIN SMALL LETTER M
   #x4E	#x006E	;#LATIN SMALL LETTER N
   #x4F	#x006F	;#LATIN SMALL LETTER O
   #x50	#x0070	;#LATIN SMALL LETTER P
   #x51	#x0071	;#LATIN SMALL LETTER Q
   #x52	#x0072	;#LATIN SMALL LETTER R
   #x53	#x0073	;#LATIN SMALL LETTER S
   #x54	#x0074	;#LATIN SMALL LETTER T
   #x55	#x0075	;#LATIN SMALL LETTER U
   #x56	#x0076	;#LATIN SMALL LETTER V
   #x57	#x0077	;#LATIN SMALL LETTER W
   #x58	#x0078	;#LATIN SMALL LETTER X
   #x59	#x0079	;#LATIN SMALL LETTER Y
   #x5A	#x007A	;#LATIN SMALL LETTER Z
   #x5B	#x005B	;#LEFT SQUARE BRACKET
   #x5C	#x00A3	;#POUND SIGN
   #x5D	#x005D	;#RIGHT SQUARE BRACKET
   #x5E	#x2191	;#UPWARDS ARROW
   #x5F	#x2190	;#LEFTWARDS ARROW
   #x60	#x2501	;#BOX DRAWINGS LIGHT HORIZONTAL
   #x61	#x0041	;#LATIN CAPITAL LETTER A
   #x62	#x0042	;#LATIN CAPITAL LETTER B
   #x63	#x0043	;#LATIN CAPITAL LETTER C
   #x64	#x0044	;#LATIN CAPITAL LETTER D
   #x65	#x0045	;#LATIN CAPITAL LETTER E
   #x66	#x0046	;#LATIN CAPITAL LETTER F
   #x67	#x0047	;#LATIN CAPITAL LETTER G
   #x68	#x0048	;#LATIN CAPITAL LETTER H
   #x69	#x0049	;#LATIN CAPITAL LETTER I
   #x6A	#x004A	;#LATIN CAPITAL LETTER J
   #x6B	#x004B	;#LATIN CAPITAL LETTER K
   #x6C	#x004C	;#LATIN CAPITAL LETTER L
   #x6D	#x004D	;#LATIN CAPITAL LETTER M
   #x6E	#x004E	;#LATIN CAPITAL LETTER N
   #x6F	#x004F	;#LATIN CAPITAL LETTER O
   #x70	#x0050	;#LATIN CAPITAL LETTER P
   #x71	#x0051	;#LATIN CAPITAL LETTER Q
   #x72	#x0052	;#LATIN CAPITAL LETTER R
   #x73	#x0053	;#LATIN CAPITAL LETTER S
   #x74	#x0054	;#LATIN CAPITAL LETTER T
   #x75	#x0055	;#LATIN CAPITAL LETTER U
   #x76	#x0056	;#LATIN CAPITAL LETTER V
   #x77	#x0057	;#LATIN CAPITAL LETTER W
   #x78	#x0058	;#LATIN CAPITAL LETTER X
   #x79	#x0059	;#LATIN CAPITAL LETTER Y
   #x7A	#x005A	;#LATIN CAPITAL LETTER Z
   #x7B	#x253C	;#BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
   #x7C	#xF12E	;#LEFT HALF BLOCK MEDIUM SHADE (CUS)
   #x7D	#x2502	;#BOX DRAWINGS LIGHT VERTICAL
   #x7E	#x2592	;#MEDIUM SHADE
   #x7F	#xF139	;#MEDIUM SHADE SLASHED LEFT (CUS)
   #x81	#xF104	;#ORANGE COLOR SWITCH (CUS)
   #x85	#xF110	;#FUNCTION KEY 1 (CUS)
   #x86	#xF112	;#FUNCTION KEY 3 (CUS)
   #x87	#xF114	;#FUNCTION KEY 5 (CUS)
   #x88	#xF116	;#FUNCTION KEY 7 (CUS)
   #x89	#xF111	;#FUNCTION KEY 2 (CUS)
   #x8A	#xF113	;#FUNCTION KEY 4 (CUS)
   #x8B	#xF115	;#FUNCTION KEY 6 (CUS)
   #x8C	#xF117	;#FUNCTION KEY 8 (CUS)
   #x8D	#x000A	;#LINE FEED
   ;; #x8E	#x000F	#SHIFT IN
   ;; #x90	#xF105	#BLACK COLOR SWITCH (CUS)
   ;; #x91	#xF11E	#CURSOR UP (CUS)
   ;; #x92	#xF11B	#REVERSE VIDEO OFF (CUS)
   ;; #x93	#x000C	#FORM FEED
   ;; #x94	#xF121	#INSERT (CUS)
   ;; #x95	#xF106	#BROWN COLOR SWITCH (CUS)
   ;; #x96	#xF107	#LIGHT RED COLOR SWITCH (CUS)
   ;; #x97	#xF108	#GRAY 1 COLOR SWITCH (CUS)
   ;; #x98	#xF109	#GRAY 2 COLOR SWITCH (CUS)
   ;; #x99	#xF10A	#LIGHT GREEN COLOR SWITCH (CUS)
   ;; #x9A	#xF10B	#LIGHT BLUE COLOR SWITCH (CUS)
   ;; #x9B	#xF10C	#GRAY 3 COLOR SWITCH (CUS)
   ;; #x9C	#xF10D	#PURPLE COLOR SWITCH (CUS)
   ;; #x9D	#xF11D	#CURSOR LEFT (CUS)
   ;; #x9E	#xF10E	#YELLOW COLOR SWITCH (CUS)
   ;; #x9F	#xF10F	#CYAN COLOR SWITCH (CUS)
   #xA0	#x00A0	;  #NO-BREAK SPACE
   #xA1	#x258C	; ▌#LEFT HALF BLOCK
   #xA2	#x2584	; #LOWER HALF BLOCK
   #xA3	#x2594	; #UPPER ONE EIGHTH BLOCK
   #xA4	#x2581	; #LOWER ONE EIGHTH BLOCK
   #xA5	#x258F	; #LEFT ONE EIGHTH BLOCK
   #xA6	#x2592	; #MEDIUM SHADE
   #xA7	#x2595	; #RIGHT ONE EIGHTH BLOCK 
   #xA8	#xF12F	; #LOWER HALF BLOCK MEDIUM SHADE (CUS)
   #xA9	#xF13A	; #MEDIUM SHADE SLASHED RIGHT (CUS)
   #xAA	#xF130	; #RIGHT ONE QUARTER BLOCK (CUS)
   #xAB	#x251C	; #BOX DRAWINGS LIGHT VERTICAL AND RIGHT
   #xAC	#xF134	; #BLACK SMALL SQUARE LOWER RIGHT (CUS)
   #xAD	#x2514	; #BOX DRAWINGS LIGHT UP AND RIGHT
   #xAE	#x2510	; #BOX DRAWINGS LIGHT DOWN AND LEFT
   #xAF	#x2582	; #LOWER ONE QUARTER BLOCK
   #xB0	#x250C	; #BOX DRAWINGS LIGHT DOWN AND RIGHT
   #xB1	#x2534	; #BOX DRAWINGS LIGHT UP AND HORIZONTAL
   #xB2	#x252C	; #BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
   #xB3	#x2524	; #BOX DRAWINGS LIGHT VERTICAL AND LEFT
   #xB4	#x258E	; #LEFT ONE QUARTER BLOCK
   #xB5	#x258D	; #LEFT THREE EIGTHS BLOCK
   #xB6	#xF131	; #RIGHT THREE EIGHTHS BLOCK (CUS)
   #xB7	#xF132	; #UPPER ONE QUARTER BLOCK (CUS)
   #xB8	#xF133	; #UPPER THREE EIGHTS BLOCK (CUS)
   #xB9	#x2583	; #LOWER THREE EIGHTHS BLOCK
   #xBA	#x2713	; #CHECK MARK
   #xBB	#xF135	; #BLACK SMALL SQUARE LOWER LEFT (CUS)
   #xBC	#xF136	; #BLACK SMALL SQUARE UPPER RIGHT (CUS)
   #xBD	#x2518	; #BOX DRAWINGS LIGHT UP AND LEFT
   #xBE	#xF137	; #BLACK SMALL SQUARE UPPER LEFT (CUS)
   #xBF	#xF138	; #TWO SMALL BLACK SQUARES DIAGONAL LEFT TO RIGHT (CUS)
   #xC0	#x2501	; #BOX DRAWINGS LIGHT HORIZONTAL
   #xC1	#x0041	; #LATIN CAPITAL LETTER A
   #xC2	#x0042	; #LATIN CAPITAL LETTER B
   #xC3	#x0043	; #LATIN CAPITAL LETTER C
   #xC4	#x0044	; #LATIN CAPITAL LETTER D
   #xC5	#x0045	; #LATIN CAPITAL LETTER E
   #xC6	#x0046	; #LATIN CAPITAL LETTER F
   #xC7	#x0047	; #LATIN CAPITAL LETTER G
   #xC8	#x0048	; #LATIN CAPITAL LETTER H
   #xC9	#x0049	; #LATIN CAPITAL LETTER I
   #xCA	#x004A	; #LATIN CAPITAL LETTER J
   #xCB	#x004B	; #LATIN CAPITAL LETTER K
   #xCC	#x004C	; #LATIN CAPITAL LETTER L
   #xCD	#x004D	; #LATIN CAPITAL LETTER M
   #xCE	#x004E	; #LATIN CAPITAL LETTER N
   #xCF	#x004F	; #LATIN CAPITAL LETTER O
   #xD0	#x0050	; #LATIN CAPITAL LETTER P
   #xD1	#x0051	; #LATIN CAPITAL LETTER Q
   #xD2	#x0052	; #LATIN CAPITAL LETTER R
   #xD3	#x0053	; #LATIN CAPITAL LETTER S
   #xD4	#x0054	; #LATIN CAPITAL LETTER T
   #xD5	#x0055	; #LATIN CAPITAL LETTER U
   #xD6	#x0056	; #LATIN CAPITAL LETTER V
   #xD7	#x0057	; #LATIN CAPITAL LETTER W
   #xD8	#x0058	; #LATIN CAPITAL LETTER X
   #xD9	#x0059	; #LATIN CAPITAL LETTER Y
   #xDA	#x005A	; #LATIN CAPITAL LETTER Z
   #xDB	#x253C	; #BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
   #xDC	#xF12E	; #LEFT HALF BLOCK MEDIUM SHADE (CUS)
   #xDD	#x2502	; #BOX DRAWINGS LIGHT VERTICAL
   #xDE	#x2592	; #MEDIUM SHADE
   #xDF	#xF139	; #MEDIUM SHADE SLASHED LEFT (CUS)
   #xE0	#x00A0	; #NO-BREAK SPACE
   #xE1	#x258C	; #LEFT HALF BLOCK
   #xE2	#x2584	; #LOWER HALF BLOCK
   #xE3	#x2594	; #UPPER ONE EIGHTH BLOCK
   #xE4	#x2581	; #LOWER ONE EIGHTH BLOCK
   #xE5	#x258F	; #LEFT ONE EIGHTH BLOCK
   #xE6	#x2592	; #MEDIUM SHADE
   #xE7	#x2595	; #RIGHT ONE EIGHTH BLOCK
   #xE8	#xF12F	; #LOWER HALF BLOCK MEDIUM SHADE (CUS)
   #xE9	#xF13A	; #MEDIUM SHADE SLASHED RIGHT (CUS)
   #xEA	#xF130	; #RIGHT ONE QUARTER BLOCK (CUS)
   #xEB	#x251C	; #BOX DRAWINGS LIGHT VERTICAL AND RIGHT
   #xEC	#xF134	; #BLACK SMALL SQUARE LOWER RIGHT (CUS)
   #xED	#x2514	; #BOX DRAWINGS LIGHT UP AND RIGHT
   #xEE	#x2510	; #BOX DRAWINGS LIGHT DOWN AND LEFT
   #xEF	#x2582	; #LOWER ONE QUARTER BLOCK
   #xF0	#x250C	; #BOX DRAWINGS LIGHT DOWN AND RIGHT
   #xF1	#x2534	; #BOX DRAWINGS LIGHT UP AND HORIZONTAL
   #xF2	#x252C	; #BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
   #xF3	#x2524	; #BOX DRAWINGS LIGHT VERTICAL AND LEFT
   #xF4	#x258E	; #LEFT ONE QUARTER BLOCK
   #xF5	#x258D	; #LEFT THREE EIGTHS BLOCK
   #xF6	#xF131	; #RIGHT THREE EIGHTHS BLOCK (CUS)
   #xF7	#xF132	; #UPPER ONE QUARTER BLOCK (CUS)
   #xF8	#xF133	; #UPPER THREE EIGHTS BLOCK (CUS)
   #xF9	#x2583	; #LOWER THREE EIGHTHS BLOCK
   #xFA	#x2713	; #CHECK MARK
   #xFB	#xF135	; #BLACK SMALL SQUARE LOWER LEFT (CUS)
   #xFC	#xF136	; #BLACK SMALL SQUARE UPPER RIGHT (CUS)
   #xFD	#x2518	; #BOX DRAWINGS LIGHT UP AND LEFT
   #xFE	#xF137	; #BLACK SMALL SQUARE UPPER LEFT (CUS)
   #xFF	#x2592	; #MEDIUM SHADE
   ))

(define (c64-byte->unicode byte state)
  (let ([low-high-charset (peek state 53272)]
        [reverse (peek state 199)])
    (cond
      [(and (eq? low-high-charset 23)
          (eq? reverse 0)
          (hash-has-key? unicode->c64-low-map byte)) 
       (hash-ref unicode->c64-low-map byte)]
      [(and (eq? low-high-charset 23)
          (eq? reverse #x12)
          (hash-has-key? unicode->c64-low-map-reverse byte)) 
       (hash-ref unicode->c64-low-map-reverse byte)]
      [(hash-has-key? unicode->c64-low-map byte)
       (hash-ref unicode->c64-high-map byte)]
      [else byte])))

(define/c (c64-rom-routine? high low)
  (-> byte? byte? boolean?)
  (or (= (absolute high low) #xFFD2) ;; kernel output character
     (= (absolute high low) #xAB1E) ;; basic output string
     (= (absolute high low) #xBDCD) ;; basic output integer
     ))

;; since the 6502/6510 has its cpu stack located 0100-01ff, it should be safe to use JSR to these locations
;; for some "magic" functionality 
(define/c (magic-interpreter-routine? high low)
  (-> byte? byte? boolean?)
  (or (= (absolute high low) #x0100) ;; reset cpu cycles
     ))

(define/c (interpret-c64-rom-routine high low state (verbose #t) (string-output-function interpreter-output-function))
  (->* [byte? byte? cpu-state?] [boolean? (-> string? any/c)] cpu-state?)
  (case (absolute high low)
    [(#xFFD2) ;; (display (string (integer->char (cpu-state-accumulator state))))
     (~>> (cpu-state-accumulator state)
         (display-c64charcode _ state verbose string-output-function))]
    [(#xAB1E)
     (display-c64zerotermstring
      (absolute (cpu-state-y-index state)
                (cpu-state-accumulator state))
      state
      verbose
      string-output-function)]
    [(#xBDCD)
     (when verbose
       (string-output-function
        (number->string
         (absolute (cpu-state-accumulator state) (cpu-state-x-index state)))))
     state]))

;; interpret JSR absolute (jump to subroutine) command
;; mock kernel function FFD2 to print a string
(define/c (interpret-jsr-abs high low state (verbose #t) (string-output-function interpreter-output-function))
  (->* [byte/c byte/c cpu-state?] [boolean? (-> string? any/c)] cpu-state?)
  (cond [(c64-rom-routine? high low)
         (let ([after-rom-state (interpret-c64-rom-routine high low state verbose string-output-function)])
           (struct-copy cpu-state after-rom-state [program-counter (next-program-counter after-rom-state 3)]))]
        [(magic-interpreter-routine? high low)
         (case (absolute high low)
           [(#x0100) (struct-copy cpu-state state
                                  [program-counter (word (fx+ 3 (cpu-state-program-counter state)))]
                                  [clock-cycles 0])])]
        [else
         (let* ([new-program-counter (absolute high low)]
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
(define/c (interpret-jmp-abs high low state (verbose #t) (string-output-function interpreter-output-function))
  (->* (byte/c byte/c cpu-state?) (boolean? (-> string? any/c)) cpu-state?)
  (if (c64-rom-routine? high low)
      (interpret-rts (interpret-c64-rom-routine high low state verbose string-output-function))
      (struct-copy cpu-state state
                   [program-counter (word (absolute high low))])))

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
(define/c (peek-word-at-address state address (rev-endian #f))
  (->* [cpu-state? word/c] [boolean?] word/c)
  (let* [(low-byte  (peek state address))
         (high-byte (peek state (word (fx+ address 1))))]
    (absolute (if rev-endian low-byte high-byte)
              (if rev-endian high-byte low-byte))))

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
  (let ([target-adr (peek-word-at-address state address)])
    (peek state
          (if (<= target-adr #xff)
              (bitwise-and #xff (fx+ offset target-adr))
              (word (fx+ offset target-adr))))))

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
  (let ([target-adr (peek-word-at-address state address)])
    (poke state
          (if (<= target-adr #xff)
              (bitwise-and #xff (fx+ offset target-adr))
              (word (fx+ offset target-adr)))
          value)))

;; (zp,x) ->
(define/c (peek-izx state)
  (-> cpu-state? byte/c)
  (let* [(zero-page-idx (peek-pc+1 state))
         (x             (cpu-state-x-index state))]
    (peek-indirect state (bitwise-and #xff (fx+ x zero-page-idx)))))

;; (zp,x) <-
(define/c (poke-izx state value)
  (-> cpu-state? byte/c cpu-state?)
  (let* [(zero-page-idx (peek-pc+1 state))
         (x             (cpu-state-x-index state))]
    (poke-indirect state (bitwise-and #xff (fx+ x zero-page-idx)) value)))

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
  (let ([value (peeker state)])
    (struct-copy cpu-state state
                 [accumulator     value]
                 [flags           (set-flags-zn state (zero? value) (bit7? value))]
                 [program-counter (next-program-counter state pc-inc)])))

;; load into x-index
;; setting zero, negative flag (ZN)
(define/c (interpret-ldx-mem state peeker pc-inc)
  (-> cpu-state? peeker/c exact-nonnegative-integer? cpu-state?)
  (let ([value (peeker state)])
    (struct-copy cpu-state state
                 [x-index         value]
                 [flags           (set-flags-zn state (zero? value) (bit7? value))]
                 [program-counter (next-program-counter state pc-inc)])))

;; load into y-index
;; setting zero, negative flag (ZN)
(define/c (interpret-ldy-mem state peeker pc-inc)
  (-> cpu-state? peeker/c exact-nonnegative-integer? cpu-state?)
  (let ([value (peeker state)])
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
         [new-pc         (if (test state) new-pc-jump new-pc-no-jump)]
         [new-clock-cycles (fx+ (if (test state) 1 0) (cpu-state-clock-cycles state))])
    (struct-copy cpu-state state
                 [program-counter new-pc]
                 [clock-cycles new-clock-cycles])))

;; interpret bit test on memory
;; sets carry, zero, negative, overflow flag (CZNV)
(define/c (interpret-bit-mem state peeker pc-inc)
  (-> cpu-state? peeker/c exact-nonnegative-integer? cpu-state?)
  (let* ((peeked  (peeker state))
         (is-zero (not (zero? (bitwise-and (cpu-state-accumulator state) peeked))))
         (bit6    (not (zero? (bitwise-and #x40 peeked))))
         (bit7    (bit7? peeked)))
    (struct-copy cpu-state state
                 [flags           (set-flags-cznv state (carry-flag? state) is-zero bit7 bit6)]
                 [program-counter (next-program-counter state pc-inc)])))

(module+ test #| interpret-bit-mem |#
  (define opcode-bit-abs #x2c)
  (check-eq? (~>> (initialize-cpu)
                 (with-accumulator _ #x00)
                 (poke _ #x0000 opcode-bit-abs #x0f #xf0)
                 (poke _ #xf00f #xff)
                 (execute-cpu-step _)
                 (cpu-state-flags _))
             #b11000000)
  (check-eq? (~>> (initialize-cpu)
                 (with-accumulator _ #x0f)
                 (poke _ #x0000 opcode-bit-abs #x0f #xf0)
                 (poke _ #xf00f #x0f)
                 (execute-cpu-step _)
                 (cpu-state-flags _))
             #b00000010)
  (check-eq? (~>> (initialize-cpu)
                 (with-accumulator _ #x01)
                 (poke _ #x0000 opcode-bit-abs #x0f #xf0)
                 (poke _ #xf00f #xcf)
                 (execute-cpu-step _)
                 (cpu-state-flags _))
             #b11000010))

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
               [flags           (peek-stack+1 state)]
               [stack-pointer   (byte (fx+ 1 (cpu-state-stack-pointer state)))]
               [program-counter (next-program-counter state 1)]))

(define/c (interpret-pla state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state
               [accumulator     (peek-stack+1 state)]
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
  (let ([value (source state)])
    (struct-copy cpu-state state
                 [accumulator     value]
                 [flags           (set-flags-zn state (zero? value) (< 127 value))]
                 [program-counter (next-program-counter state 1)])))

(define/c (interpret-t_s state source)
  (-> cpu-state? (-> cpu-state? byte/c) cpu-state?)
  (let ([value (source state)])
    (struct-copy cpu-state state
                 [stack-pointer   value]
                 [program-counter (next-program-counter state 1)])))

(define/c (interpret-t_x state source)
  (-> cpu-state? (-> cpu-state? byte/c) cpu-state?)
  (let ([value (source state)])
    (struct-copy cpu-state state
                 [x-index         value]
                 [flags           (set-flags-zn state (zero? value) (bit7? value))]
                 [program-counter (next-program-counter state 1)])))

(define/c (interpret-t_y state source)
  (-> cpu-state? (-> cpu-state? byte/c) cpu-state?)
  (let ([value (source state)])
    (struct-copy cpu-state state
                 [y-index         value]
                 [flags           (set-flags-zn state (zero? value) (bit7? value))]
                 [program-counter (next-program-counter state 1)])))

(define/c (interpret-modify-x-index state delta)
  (-> cpu-state? exact-integer? cpu-state?)
  (let ([value (byte (fx+ delta (cpu-state-x-index state)))])
    (struct-copy cpu-state state
                 [x-index         value]
                 [flags           (set-flags-zn state (zero? value) (bit7? value))]
                 [program-counter (next-program-counter state 1)])))

(define/c (interpret-modify-y-index state delta)
  (-> cpu-state? exact-integer? cpu-state?)
  (let ([value (byte (fx+ delta (cpu-state-y-index state)))])
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

(define/c (interpret-jmp-ind state (verbose #t) (string-output-function interpreter-output-function))
  (->* [cpu-state?] [boolean? (-> string? any/c)] cpu-state?)
  (let* ((new-abs-address (peek-word-at-address state (peek-word-at-pc+1 state)))
         (hi              (high-byte new-abs-address))
         (lo              (low-byte new-abs-address)))
    (if (c64-rom-routine? hi lo)
        (interpret-rts (interpret-c64-rom-routine hi lo state verbose string-output-function))
        (struct-copy cpu-state state
                     [program-counter new-abs-address]))))

(define/c (interpret-compare state peeker1 peeker2 pc-inc)
  (-> cpu-state? peeker/c peeker/c exact-nonnegative-integer? cpu-state?)
  (let* ((value1 (peeker1 state))
         (value2 (peeker2 state))
         (diff   (byte (fx- value1 value2))))
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
                                           (lambda (_s) 10)
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

(define/c (bcd-- state peeker pc-inc)
  (-> cpu-state? peeker/c byte? cpu-state?)
  (define carry (carry-flag? state))
  (define op-a (cpu-state-accumulator state))
  (define op-b (peeker state))
  (define high-nibble-a (fxrshift op-a 4))
  (define high-nibble-b (fxrshift op-b 4))
  (define low-nibble-a (bitwise-and #x0F op-a))
  (define low-nibble-b (bitwise-and #x0F op-b))
  (define new-low (fx- low-nibble-a low-nibble-b (if carry 1 0)))
  (define new-high (fx- high-nibble-a high-nibble-b (if (< 0 (bitwise-and #x10 new-low)) 1 0)))
  (define corrected-low (bitwise-and #x0f (if (< 0 (bitwise-and #x10 new-low)) (fx- (bitwise-and #x0f new-low) 6) new-low))) ;  (if (> 0  new-low) (fx+ new-low 10) new-low)
  (define corrected-high (bitwise-and #x0f (if (< 0 (bitwise-and #x10 new-high)) (fx- (bitwise-and #x0f new-high) 6) new-high))) ; (if (> 0 new-high) (fx+ new-high 10) new-high)
  (define new-accumulator (bitwise-ior (fxlshift corrected-high 4) (bitwise-and #x0f corrected-low)))
  (define raw (fx- op-a op-b (if carry 0 1)))
  (define zero-flag (= 0 (bitwise-and #xff raw)))
  (define carry-flag (< 0 (bitwise-and #x100 raw)))
  (define negative-flag (< 0 (bitwise-and #x80 raw)))
  (define overflow-flag (and (< 0 (bitwise-and #x80 (bitwise-xor raw op-b)))
                           (< 0 (bitwise-and #x80 (bitwise-xor op-a op-b)))))
  (struct-copy cpu-state state
               [accumulator new-accumulator]
               [flags
                (set-flags-cznv state carry-flag zero-flag negative-flag overflow-flag)]
               [program-counter (next-program-counter state pc-inc)]))

(module+ test
  (check-equal? (cpu-state-accumulator (bcd-- (with-accumulator (initialize-cpu) #x10) (lambda (_s) #x01) 2))
                #x09)
  (check-equal? (cpu-state-accumulator (bcd-- (with-accumulator (initialize-cpu) #x00) (lambda (_s) #x01) 2))
                #x99)
  (check-equal? (cpu-state-accumulator (bcd-- (with-accumulator (initialize-cpu) #x21) (lambda (_s) #x10) 2))
                #x11)
  (check-equal? (cpu-state-accumulator (bcd-- (with-accumulator (initialize-cpu) #x99) (lambda (_s) #x10) 2))
                #x89)
  (check-equal? (cpu-state-accumulator (bcd-- (with-accumulator (initialize-cpu) #x95) (lambda (_s) #x26) 2))
                #x69))

(define/c (bcd-+ state peeker pc-inc)  
  (-> cpu-state? peeker/c byte? cpu-state?)
  (define carry (carry-flag? state))
  (define op-a (cpu-state-accumulator state))
  (define op-b (peeker state))
  (define high-nibble-a (fxrshift op-a 4))
  (define high-nibble-b (fxrshift op-b 4))
  (define low-nibble-a (bitwise-and #x0F op-a))
  (define low-nibble-b (bitwise-and #x0F op-b))
  (define zero-flag (= 0 (bitwise-and #xFF (fx+ op-a op-b (if carry 1 0)))))
  (define new-low (fx+ low-nibble-a low-nibble-b (if carry 1 0)))
  (define low-corrected (if (> new-low 9) (fx+ new-low 6) new-low))
  (define new-high (fx+ high-nibble-a high-nibble-b (if (> low-corrected 15) 1 0)))
  (define negative-flag (< 0 (bitwise-and #x08 new-high)))
  (define overflow-flag (and (= 0 (bitwise-and #x80 (bitwise-xor (fxlshift new-high 4) op-a)))
                             (< 0 (bitwise-and #x80 (bitwise-xor op-a op-b)))))
  (define high-corrected (if (> new-high 9) (fx+ new-high 6) new-high))
  (define carry-flag  (> high-corrected 15))
  (define new-accumulator (bitwise-and #xff (bitwise-ior (fxlshift high-corrected 4) (bitwise-and #x0f low-corrected))))
  (struct-copy cpu-state state
               [accumulator new-accumulator]
               [flags 
                (set-flags-cznv state carry-flag zero-flag negative-flag overflow-flag)]
               [program-counter (next-program-counter state pc-inc)]))

(module+ test
  (check-equal? (cpu-state-accumulator (bcd-+ (with-accumulator (initialize-cpu) #x10) (lambda (_s) #x01) 2))
                #x11)
  (check-equal? (cpu-state-accumulator (bcd-+ (with-accumulator (initialize-cpu) #x19) (lambda (_s) #x01) 2))
                #x20)
  (check-equal? (cpu-state-accumulator (bcd-+ (with-accumulator (initialize-cpu) #x60) (lambda (_s) #x0a) 2))
                #x70)
  (check-equal? (cpu-state-accumulator (bcd-+ (with-accumulator (initialize-cpu) #x60) (lambda (_s) #x0f) 2))
                #x75)
  (check-equal? (cpu-state-accumulator (bcd-+ (with-accumulator (initialize-cpu) #x5a) (lambda (_s) #x01) 2))
                #x61)
  (check-equal? (cpu-state-accumulator (bcd-+ (with-accumulator (initialize-cpu) #x5f) (lambda (_s) #x01) 2))
                #x66)
  (check-equal? (cpu-state-accumulator (bcd-+ (with-accumulator (initialize-cpu) #x99) (lambda (_s) #x01) 2))
                #x00)
  (check-false (zero-flag? (bcd-+ (with-accumulator (initialize-cpu) #x99) (lambda (_s) #x01) 2)))
  (check-true (overflow-flag? (bcd-+ (with-accumulator (initialize-cpu) #x99) (lambda (_s) #x01) 2)))
  (check-true (carry-flag? (bcd-+ (with-accumulator (initialize-cpu) #x99) (lambda (_s) #x01) 2)))
  (check-true (negative-flag? (bcd-+ (with-accumulator (initialize-cpu) #x99) (lambda (_s) #x01) 2))))


(define (interpreter-output-function str)
  (display str))

(define/c (add-cycles state cycles)
  (-> cpu-state? nonnegative-integer? cpu-state?)
  (struct-copy cpu-state state
               [clock-cycles (+ cycles (cpu-state-clock-cycles state))]))

(define/c (reset-cycles state)
  (-> cpu-state? cpu-state?)
  (struct-copy cpu-state state
               [clock-cycles 0]))

(struct exn:fail:cpu-interpreter exn:fail (cpu-state)
  #:extra-constructor-name make-exn:fail:cpu-interpreter
  #:transparent)

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
;; rel = $00 (PC-relative)
;; io = illegal opcode
(define/c (execute-cpu-step state (verbose #t) (string-output-function interpreter-output-function))
  (->* [cpu-state?] [boolean? (-> string? any/c)] cpu-state?)
  (case (peek-pc state)
    [(#x00) (interpret-brk state)]
    [(#x01) (interpret-logic-op-mem (add-cycles state 6) bitwise-ior peek-izx 2)]
    ;; #x02 -io KIL
    ;; #x03 -io SLO izx
    ;; #x04 -io NOP zp
    [(#x05) (interpret-logic-op-mem (add-cycles state 3) bitwise-ior peek-zp 2)]
    [(#x06) (interpret-asl-mem (add-cycles state 5) peek-zp poke-zp 2)]
    ;; #x07 -io SLO zp
    [(#x08) (interpret-php (add-cycles state 3))]
    [(#x09) (interpret-logic-op-mem (add-cycles state 2) bitwise-ior peek-pc+1 2)]
    [(#x0a) (interpret-asl (add-cycles state 2))]
    ;; #x0b -io ANC imm
    ;; #x0c -io NOP abs
    [(#x0d) (interpret-logic-op-mem (add-cycles state 4) bitwise-ior peek-abs 3)]
    [(#x0e) (interpret-asl-mem (add-cycles state 6) peek-abs poke-abs 3)]
    ;; #x0f -io SLO abs
    [(#x10) (interpret-branch-rel (add-cycles state 2) not-negative-flag?)]
    [(#x11) (interpret-logic-op-mem (add-cycles state 5) bitwise-ior peek-izy 2)]
    ;; #x12 -io KIL
    ;; #x13 -io SLO izy
    ;; #x14 -io NOP zpx
    [(#x15) (interpret-logic-op-mem (add-cycles state 4) bitwise-ior peek-zpx 2)]
    [(#x16) (interpret-asl-mem (add-cycles state 6) peek-zpx poke-zpx 2)]
    ;; #x17 -io SLO zpx
    [(#x18) (interpret-clc (add-cycles state 2))]
    [(#x19) (interpret-logic-op-mem (add-cycles state 4) bitwise-ior peek-absy 3)]
    ;; #x1a -io NOP
    ;; #x1b -io SLO abt
    ;; #x1c -io NOP abx
    [(#x1d) (interpret-logic-op-mem (add-cycles state 4) bitwise-ior peek-absx 3)]
    [(#x1e) (interpret-asl-mem (add-cycles state 7) peek-absx poke-absx 3)]
    ;; #x1f -io SLO abx
    [(#x20) (interpret-jsr-abs (peek-pc+2 (add-cycles state 6)) (peek-pc+1 state) state verbose string-output-function)]
    [(#x21) (interpret-logic-op-mem (add-cycles state 6) bitwise-and peek-izx 2)]
    ;; #x22 -io KIL
    ;; #x23 -io RLA izx
    [(#x24) (interpret-bit-mem (add-cycles state 3) peek-zp 2)]
    [(#x25) (interpret-logic-op-mem (add-cycles state 3) bitwise-and peek-zp 2)]
    [(#x26) (interpret-rol-mem (add-cycles state 5) peek-zp poke-zp 2)]
    ;; #x27 -io RLA zp
    [(#x28) (interpret-plp (add-cycles state 4))]
    [(#x29) (interpret-logic-op-mem (add-cycles state 2) bitwise-and peek-pc+1 2)]
    [(#x2a) (interpret-rol (add-cycles state 2))]
    ;; #x2b -io ANC imm
    [(#x2c) (interpret-bit-mem (add-cycles state 4) peek-abs 3)]
    [(#x2d) (interpret-logic-op-mem (add-cycles state 4) bitwise-and peek-abs 3)]
    [(#x2e) (interpret-rol-mem (add-cycles state 6) peek-abs poke-abs 3)]
    ;; #x2f -io RIA abs
    [(#x30) (interpret-branch-rel (add-cycles state 2) negative-flag?)]
    [(#x31) (interpret-logic-op-mem (add-cycles state 5) bitwise-and peek-izy 2)]
    ;; #x32 -io KIL
    ;; #x33 -io RIA izy
    ;; #x34 -io NOP zpx 
    [(#x35) (interpret-logic-op-mem (add-cycles state 4) bitwise-and peek-zpx 2)]
    [(#x36) (interpret-rol-mem (add-cycles state 6) peek-zpx poke-abs 2)]
    ;; #x37 -io RLA zpx
    [(#x38) (interpret-sec (add-cycles state 2))]
    [(#x39) (interpret-logic-op-mem (add-cycles state 4) bitwise-and peek-absy 3)]
    ;; #x3a -io NOP
    ;; #x3b -io RLA aby
    ;; #x3c -io NOP abx
    [(#x3d) (interpret-logic-op-mem (add-cycles state 4) bitwise-and peek-absx 3)]
    [(#x3e) (interpret-rol-mem (add-cycles state 7) peek-zpx poke-absx 3)]
    ;; #x3f -io RLA abx
    [(#x40) (interpret-rti (add-cycles state 6))]
    [(#x41) (interpret-logic-op-mem (add-cycles state 6) bitwise-xor peek-izx 2)]
    ;; #x42 -io KIL
    ;; #x43 -io SRE izx
    ;; #x44 -io NOP zp
    [(#x45) (interpret-logic-op-mem (add-cycles state 3) bitwise-xor peek-zp 2)]
    [(#x46) (interpret-lsr-mem (add-cycles state 5) peek-zp poke-zp 2)]
    ;; #x47 -io SRE zp
    [(#x48) (interpret-pha (add-cycles state 3))]
    [(#x49) (interpret-logic-op-mem (add-cycles state 2) bitwise-xor peek-pc+1 2)]
    [(#x4a) (interpret-lsr (add-cycles state 2) cpu-state-accumulator)] ; peek-zp
    ;; #x4b -io ALR imm
    [(#x4c) (interpret-jmp-abs (peek-pc+2 state) (peek-pc+1 state) (add-cycles state 3) verbose string-output-function)]
    [(#x4d) (interpret-logic-op-mem (add-cycles state 4) bitwise-xor peek-abs 3)]
    [(#x4e) (interpret-lsr-mem (add-cycles state 6) peek-abs poke-abs 3)]
    ;; #x4f -io SRE abs
    [(#x50) (interpret-branch-rel (add-cycles state 2) not-overflow-flag?)]
    [(#x51) (interpret-logic-op-mem (add-cycles state 5) bitwise-xor peek-izy 2)]
    ;; #x52 -io KIL
    ;; #x53 -io SRE izy
    ;; #x54 -io NOP zpx
    [(#x55) (interpret-logic-op-mem (add-cycles state 4) bitwise-xor peek-zpx 2)]
    [(#x56) (interpret-lsr-mem (add-cycles state 6) peek-zpx poke-zpx 2)]
    ;; #x57 -io SRE zpx
    [(#x58) (interpret-cli (add-cycles state 2))]
    [(#x59) (interpret-logic-op-mem (add-cycles state 4) bitwise-xor peek-absy 3)]
    ;; #x5a -io NOP
    ;; #x5b -io SRE aby
    ;; #x5c -io NOP abx
    [(#x5d) (interpret-logic-op-mem (add-cycles state 4) bitwise-xor peek-absx 3)]
    [(#x5e) (interpret-lsr-mem (add-cycles state 7) peek-absx poke-absx 3)]
    ;; #x5f -io SRE abx
    [(#x60) (interpret-rts (add-cycles state 6))]
    [(#x61) (if (decimal-flag? state)
                (bcd-+ state peek-izx 2)
                (interpret-calc-op (add-cycles state 6) fx+ (if (carry-flag? state) 1 0) peek-izx derive-carry-after-addition 2))]
    ;; #x62 -io KIL
    ;; #x63 -io RRA izx
    ;; #x64 -io NOP zp
    [(#x65) (if (decimal-flag? state)
                (bcd-+ state peek-zp 2)
                (interpret-calc-op (add-cycles state 3) fx+ (if (carry-flag? state) 1 0) peek-zp derive-carry-after-addition 2))]
    [(#x66) (interpret-ror-mem (add-cycles state 5) peek-zp poke-zp 2)]
    ;; #x67 -io RRA zp
    [(#x68) (interpret-pla (add-cycles state 4))]
    [(#x69) (if (decimal-flag? state)
                (bcd-+ state peek-pc+1 2)
                (interpret-calc-op (add-cycles state 2) fx+ (if (carry-flag? state) 1 0) peek-pc+1 derive-carry-after-addition 2))]
    [(#x6a) (interpret-ror (add-cycles state 2))]
    ;; #x6b -io ARR imm
    [(#x6c) (interpret-jmp-ind (add-cycles state 5) verbose string-output-function)]
    [(#x6d) (if (decimal-flag? state)
                (bcd-+ state peek-abs 3)
                (interpret-calc-op (add-cycles state 4) fx+ (if (carry-flag? state) 1 0) peek-abs derive-carry-after-addition 3))]
    [(#x6e) (interpret-ror-mem (add-cycles state 6) peek-abs poke-abs 3)]
    ;; #x6f -io RRA abs
    [(#x70) (interpret-branch-rel (add-cycles state 2) overflow-flag?)]
    [(#x71) (if (decimal-flag? state)
                (bcd-+ state peek-izy 2)
                (interpret-calc-op (add-cycles state 5) fx+ (if (carry-flag? state) 1 0) peek-izy derive-carry-after-addition 2))]
    ;; #x72 -io KIL
    ;; #x73 -io RRA izy
    ;; #x74 -io NOP zpx
    [(#x75) (if (decimal-flag? state)
                (bcd-+ state peek-zpx 2)
                (interpret-calc-op (add-cycles state 4) fx+ (if (carry-flag? state) 1 0) peek-zpx derive-carry-after-addition 2))]
    [(#x76) (interpret-ror-mem (add-cycles state 6) peek-zpx poke-zpx 2)]
    ;; #x77 -io RRA zpx
    [(#x78) (interpret-sei (add-cycles state 2))]
    [(#x79) (if (decimal-flag? state)
                (bcd-+ state peek-absy 3)
                (interpret-calc-op (add-cycles state 4) fx+ (if (carry-flag? state) 1 0) peek-absy derive-carry-after-addition 3))]
    ;; #x7a -io NOP
    ;; #x7b -io RRA aby
    ;; #x7c -io NOP abx
    [(#x7d) (if (decimal-flag? state)
                (bcd-+ state peek-absx 3)
                (interpret-calc-op (add-cycles state 4) fx+ (if (carry-flag? state) 1 0) peek-absx derive-carry-after-addition 3))]
    [(#x7e) (interpret-ror-mem (add-cycles state 7) peek-absx poke-absx 2)]
    ;; #x7f -io RRA abx
    ;; #x80 -io NOP imm
    [(#x81) (interpret-sta-mem (add-cycles state 6) poke-izx 2)]
    ;; #x82 -io NOP imm
    ;; #x83 -io SAX izx
    [(#x84) (interpret-sty-mem (add-cycles state 3) poke-zp 2)]
    [(#x85) (interpret-sta-mem (add-cycles state 3) poke-zp 2)]
    [(#x86) (interpret-stx-mem (add-cycles state 3) poke-zp 2)]
    ;; #x87 -io SAX zp
    [(#x88) (interpret-modify-y-index (add-cycles state 2) -1)]
    ;; #x89 -io NOP imm
    [(#x8a) (interpret-t_a (add-cycles state 2) cpu-state-x-index)]
    ;; #x8b -io XAA imm
    [(#x8c) (interpret-sty-mem (add-cycles state 4) poke-abs 3)]
    [(#x8d) (interpret-sta-mem (add-cycles state 4) poke-abs 3)]
    [(#x8e) (interpret-stx-mem (add-cycles state 4) poke-abs 3)]
    ;; #x8f -io SAX abs
    [(#x90) (interpret-branch-rel (add-cycles state 2) not-carry-flag?)]
    [(#x91) (interpret-sta-mem (add-cycles state 6) poke-izy 2)]
    ;; #x92 -io KIL
    ;; #x93 -io AHX izy
    [(#x94) (interpret-sty-mem (add-cycles state 4) poke-zpx 2)]
    [(#x95) (interpret-sta-mem (add-cycles state 4) poke-zpx 2)]
    [(#x96) (interpret-stx-mem (add-cycles state 4) poke-zpy 2)]
    ;; #x97 -io SAX zpy
    [(#x98) (interpret-t_a (add-cycles state 2) cpu-state-y-index)]
    [(#x99) (interpret-sta-mem (add-cycles state 5) poke-absy 3)]
    [(#x9a) (interpret-t_s (add-cycles state 2) cpu-state-x-index)]
    ;; #x9b -io TAS avt
    ;; #x9c -io SHY abx
    [(#x9d) (interpret-sta-mem (add-cycles state 5) poke-absx 3)]
    ;; #x9e -io SHX aby
    ;; #x9f -io AHX aby
    [(#xa0) (interpret-ldy-mem (add-cycles state 2) peek-pc+1 2)]
    [(#xa1) (interpret-lda-mem (add-cycles state 6) peek-izx 2)]
    [(#xa2) (interpret-ldx-mem (add-cycles state 2) peek-pc+1 2)]
    ;; #xa3 -io LAX izx
    [(#xa4) (interpret-ldy-mem (add-cycles state 3) peek-zp 2)]
    [(#xa5) (interpret-lda-mem (add-cycles state 3) peek-zp 2)]
    [(#xa6) (interpret-ldx-mem (add-cycles state 3) peek-zp 2)]
    ;; #xa7 -io LAX zp
    [(#xa8) (interpret-t_y (add-cycles state 2) cpu-state-accumulator)]
    [(#xa9) (interpret-lda-mem (add-cycles state 2) peek-pc+1 2)]
    [(#xaa) (interpret-t_x (add-cycles state 2) cpu-state-accumulator)]
    ;; #xab -io LAX imm
    [(#xac) (interpret-ldy-mem (add-cycles state 4) peek-abs 3)]
    [(#xad) (interpret-lda-mem (add-cycles state 4) peek-abs 3)]
    [(#xae) (interpret-ldx-mem (add-cycles state 4) peek-abs 3)]
    ;; #xaf -io LAX abs
    [(#xb0) (interpret-branch-rel (add-cycles state 2) carry-flag?)]
    [(#xb1) (interpret-lda-mem (add-cycles state 5) peek-izy 2)]
    ;; #xb2 -io KIL
    ;; #xb3 -io LAX izy
    [(#xb4) (interpret-ldy-mem (add-cycles state 4) peek-zpx 2)]
    [(#xb5) (interpret-lda-mem (add-cycles state 4) peek-zpx 2)]
    [(#xb6) (interpret-ldx-mem (add-cycles state 4) peek-zpy 2)]
    ;; #xb7 -io LAX zpy
    [(#xb8) (interpret-clv (add-cycles state 2))]
    [(#xb9) (interpret-lda-mem (add-cycles state 4) peek-absy 3)]
    [(#xba) (interpret-t_x (add-cycles state 2) cpu-state-stack-pointer)]
    ;; #xbb -io LAS aby
    [(#xbc) (interpret-ldy-mem (add-cycles state 4) peek-absx 3)]
    [(#xbd) (interpret-lda-mem (add-cycles state 4) peek-absx 3)]
    [(#xbe) (interpret-ldx-mem (add-cycles state 4) peek-absy 3)]
    ;; #xbf -io LAX aby
    [(#xc0) (interpret-compare (add-cycles state 2) cpu-state-y-index peek-pc+1 2)]
    [(#xc1) (interpret-compare (add-cycles state 6) cpu-state-accumulator peek-izx 2)]
    ;; #xc2 -io NOP imm
    ;; #xc3 -io DCP izx
    [(#xc4) (interpret-compare (add-cycles state 3) cpu-state-y-index peek-zp 2)]
    [(#xc5) (interpret-compare (add-cycles state 3) cpu-state-accumulator peek-zp 2)]
    [(#xc6) (interpret-crement-mem (add-cycles state 5) fx- peek-zp poke-zp 2)]
    ;; #xc7 -io DCP zp
    [(#xc8) (interpret-modify-y-index (add-cycles state 2) 1)]
    [(#xc9) (interpret-compare (add-cycles state 2) cpu-state-accumulator peek-pc+1 2)]
    [(#xca) (interpret-modify-x-index (add-cycles state 2) -1)]
    ;; #xcb -io AXS imm
    [(#xcc) (interpret-compare (add-cycles state 4) cpu-state-y-index peek-abs 3)]
    [(#xcd) (interpret-compare (add-cycles state 4) cpu-state-accumulator peek-abs 3)]
    [(#xce) (interpret-crement-mem (add-cycles state 6) fx- peek-abs poke-abs 3)]
    ;; #xcf -io DCP abs
    [(#xd0) (interpret-branch-rel (add-cycles state 2) not-zero-flag?)]
    [(#xd1) (interpret-compare (add-cycles state 5) cpu-state-accumulator peek-izy 2)]
    ;; #xd2 -io KIL
    ;; #xd3 -io DCP izy
    ;; #xd4 -io NOP zpx
    [(#xd5) (interpret-compare (add-cycles state 4) cpu-state-accumulator peek-zpx 2)]
    [(#xd6) (interpret-crement-mem (add-cycles state 6) fx- peek-zpx poke-zpx 2)]
    ;; #xd7 -io DCP zpx
    [(#xd8) (interpret-cld (add-cycles state 2))]
    [(#xd9) (interpret-compare (add-cycles state 4) cpu-state-accumulator peek-absy 3)]
    ;; #xda -io NOP
    ;; #xdb -io DCP aby
    ;; #xdc -io NOP abx
    [(#xdd) (interpret-compare (add-cycles state 4) cpu-state-accumulator peek-absx 3)]
    [(#xde) (interpret-crement-mem (add-cycles state 7) fx- peek-absx poke-absx 3)]
    ;; #xdf -io DCP abx
    [(#xe0) (interpret-compare (add-cycles state 2) cpu-state-x-index peek-pc+1 2)]
    [(#xe1) (if (decimal-flag? state)
                (bcd-- state peek-izx 2)
                (interpret-calc-op (add-cycles state 6) fx- 0 peek-izx derive-carry-after-subtraction 2))]
    ;; #xe2 -io NOP imm
    ;; #xe3 -io ISC izx
    [(#xe4) (interpret-compare (add-cycles state 3) cpu-state-x-index peek-zp 2)]
    [(#xe5) (if (decimal-flag? state)
                (bcd-- state peek-zp 2)
                (interpret-calc-op (add-cycles state 3) fx- 0 peek-zp derive-carry-after-subtraction 2))]
    [(#xe6) (interpret-crement-mem (add-cycles state 5) fx+ peek-zp poke-zp 2)]
    ;; #xe7 -io ISC zp
    [(#xe8) (interpret-modify-x-index (add-cycles state 2) 1)]
    [(#xe9) (if (decimal-flag? state)
                (bcd-- state peek-pc+1 2)
                (interpret-calc-op (add-cycles state 2) fx- 0 peek-pc+1 derive-carry-after-subtraction 2))]
    [(#xea) (interpret-nop (add-cycles state 2))]
    ;; #xeb -io SBC imm
    [(#xec) (interpret-compare (add-cycles state 4) cpu-state-x-index peek-abs 3)]
    [(#xed) (if (decimal-flag? state)
                (bcd-- state peek-abs 3)
                (interpret-calc-op (add-cycles state 4) fx- 0 peek-abs derive-carry-after-subtraction 3))]
    [(#xee) (interpret-crement-mem (add-cycles state 6) fx+ peek-abs poke-abs 3)]
    ;; #xef -io ISC abs
    [(#xf0) (interpret-branch-rel (add-cycles state 2) zero-flag?)]
    [(#xf1) (if (decimal-flag? state)
                (bcd-- state peek-izy 2)
                (interpret-calc-op (add-cycles state 5) fx- 0 peek-izy derive-carry-after-subtraction 2))]
    ;; #xf2 -io KIL
    ;; #xf3 -io ISC izy
    ;; #xf4 -io NOP zpx
    [(#xf5) (if (decimal-flag? state)
                (bcd-- state peek-zpx 2)
                (interpret-calc-op (add-cycles state 4) fx- 0 peek-zpx derive-carry-after-subtraction 2))]
    [(#xf6) (interpret-crement-mem (add-cycles state 6) fx+ peek-zpx poke-zpx 2)]
    ;; #xf7 -io ISC zpx
    [(#xf8) (interpret-sed (add-cycles state 2))]
    [(#xf9) (if (decimal-flag? state)
                (bcd-- state peek-absy 3)
                (interpret-calc-op (add-cycles state 4) fx- 0 peek-absy derive-carry-after-subtraction 3))]
    ;; #xfa -io NOP
    ;; #xfb -io ISC aby
    ;; #xfc -io NOP abx
    [(#xfd) (if (decimal-flag? state)
                (bcd-- state peek-absx 3)
                (interpret-calc-op (add-cycles state 4) fx- 0 peek-absx derive-carry-after-subtraction 3))]
    [(#xfe) (interpret-crement-mem (add-cycles state 7) fx+ peek-absx poke-absx 3)]
    ;; #xff -io ISC abx
    [else (raise (make-exn:fail:cpu-interpreter "" (current-continuation-marks) state))]))

(module+ test #| fd SBC abs,x |#
  (define (at-2000_sbc-2000-x_with-x-3 accumulator at-2003-value)
    (~>> (initialize-cpu)
        (with-program-counter _ #x2000)              ;; *=$2000
        (with-flags _ #x01)                          ;; carry set
        (with-accumulator     _ accumulator)         ;; LDA #,accumulator
        (poke _ #x2000 #xfd #x00 #x20 at-2003-value) ;; SBC $2000,X
                                                     ;; .byte ,at-2003-value
        (with-x-index _ #x03)))

  (let ([result (~>> (at-2000_sbc-2000-x_with-x-3 #x21 #x10)
                    (execute-cpu-step _))])
    (check-eq? (cpu-state-accumulator result) #x11 "#x21 - #x10 = #x11")
    (check-eq? (cpu-state-program-counter result) #x2003 "this was a 3 byte command")
    (check-true (carry-flag? result) "carry flag is still set, no borrow took place")
    (check-false (zero-flag? result) "zero flag is false since != $00")
    (check-false (negative-flag? result) "negative flag false, bit7, sign flag not set"))

  (let ([result (~>> (at-2000_sbc-2000-x_with-x-3 #x10 #x21)
                    (execute-cpu-step _))])
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
  
  (let ([result (~>> (at-2000_inc-2000-x_with-x-3 #x10)
                    (execute-cpu-step _))])
    (check-eq? (peek-word-at-address result #x2003) #x11 "incremented byte at location $2000+x")
    (check-eq? (cpu-state-program-counter result) #x2003 "this was a 3 byte command")
    (check-false (zero-flag? result) "zero flag is false since ($10 + 1) != $00")
    (check-false (negative-flag? result) "negative flag false, bit7, sign flag not set"))

    (let ([result (~>> (at-2000_inc-2000-x_with-x-3 #xff)
                    (execute-cpu-step _))])
    (check-true (zero-flag? result) "zero flag is true since ($FF + 1) == $00")
    (check-false (negative-flag? result) "negative flag false, bit7, sign flag not set"))

    (let ([result (~>> (at-2000_inc-2000-x_with-x-3 #x7f)
                    (execute-cpu-step _))])
    (check-false (zero-flag? result) "zero flag is false since ($7f + 1) != $00")
    (check-true (negative-flag? result) "negative flag true, bit7, sign flag set")

    (check-equal? (cpu-state-clock-cycles result)
                  7
                  "inc absolute,x ($fe) takes 7 clocks")))

;; run the interpreter on the given cpu state
(define/c (run-interpreter-on state (verbose #t) (string-output-function interpreter-output-function))
  (->* [cpu-state?] [boolean? (-> string? any/c)] cpu-state?)
  ;; (collect-garbage)
  ;; (displayln (format "memory: ~a" (current-memory-use)))
  (begin0
      (run state verbose string-output-function)
    ;; (collect-garbage)
    ;; (displayln (format "\nmemory: ~a" (current-memory-use)))
    (when verbose (color-displayln "\nprogram execution done."))))

;; put the raw bytes into memory (at org) and start running at org
(define/c (run-interpreter org program (verbose #t) (string-output-function interpreter-output-function))
  (->* [word/c (listof (or/c byte/c ast-command?))] [boolean? (-> string? any/c)] cpu-state?)
  (when verbose (displayln (format "loading program into interpreter at ~a" org)))
  (when verbose (displayln "program execution starting:"))
  (define raw-bytes (if (ast-command? (car program))
                        (assemble org program)
                        program))
  (define state (6510-load (initialize-cpu) org raw-bytes))
  (run-interpreter-on state verbose string-output-function))
