#lang racket

(require (only-in threading ~>>))
(require (rename-in  racket/contract [define/contract define/c]))
(require "6510-utils.rkt")
(require (only-in "6510-interpreter.rkt"
                  cpu-state?
                  with-program-counter
                  cpu-state-program-counter
                  peek-pc
                  peek-pc+1
                  peek-word-at-pc+1))

(provide disassemble disassemble-single)

(module+ test #| rackunit |#
  (require (only-in "6510-interpreter.rkt" poke with-flags initialize-cpu))
  (require rackunit))

(module+ test #| disassemble |#
  (check-equal?
   (disassemble (~>> (initialize-cpu)
                    (with-flags _ #xff)
                    (with-program-counter _ #x2000)
                    (poke _ #x2000 #x01 #xcd)))
   "2000 01 cd   \tORA ($cd,x)"))

(define/c (code-bytes state address len)
  (-> cpu-state? word/c word/c string?)
  (define use-state (with-program-counter state address))
  (define byte (byte->hex-string (peek-pc use-state)))
  (if (> len 1)
      (string-join (list byte (code-bytes state (add1 address) (sub1 len))) " ")
      byte))

(define/c (disassemble state [address (cpu-state-program-counter state)] [lines 1])
  (->* (cpu-state?) (word/c word/c) string?)
  (let-values (((str len) (disassemble-single state address)))
    (define code-byte-str (code-bytes state address len))
    (define formatted-str (format "~a ~a\t~a" (word->hex-string address) (~a code-byte-str #:width 8) str))
    (if (> lines 1)
        (string-join (list formatted-str (disassemble state (+ address len) (sub1 lines))) "\n")
        formatted-str)))

(define/c (byte-at-pc state)
  (-> cpu-state? string?)
  (byte->hex-string (peek-pc state)))

(define/c (byte-at-pc+1 state)
  (-> cpu-state? string?)
  (byte->hex-string (peek-pc+1 state)))

(define/c (word-at-pc+1 state)
  (-> cpu-state? string?)
  (word->hex-string (peek-word-at-pc+1 state)))

;; format the disassembled relative branch 
(define/c (format-relative-branch state branch-mnemonic)
  (-> cpu-state? string? string?)
  (format "~a $~a (->$~a)" branch-mnemonic
          (byte-at-pc+1 state)
          (word->hex-string (+ 2 (cpu-state-program-counter state)
                              (decimal-from-two-complement
                               (peek-pc+1 state))))))

(define/c (disassemble-single state [address (cpu-state-program-counter state)])
  (->* (cpu-state?) (word/c) (values string? byte/c))
  (define use-state (with-program-counter state address))
  (case (peek-pc use-state)
    [(#x00) (values "BRK" 1)]
    [(#x01) (values (format "ORA ($~a,x)" (byte-at-pc+1 use-state)) 2)]
    ;; #x02 -io KIL
    ;; #x03 -io SLO izx
    ;; #x04 -io NOP zp
    [(#x05) (values (format "ORA $~a" (byte-at-pc+1 use-state)) 2)]
    [(#x06) (values (format "ASL $~a" (byte-at-pc+1 use-state)) 2)]
    ;; #x07 -io SLO zp
    [(#x08) (values "PHP" 1)]
    [(#x09) (values (format "ORA #$~a" (byte-at-pc+1 use-state)) 2)]
    [(#x0a) (values "ASL A" 1)]
    ;; #x0b -io ANC imm
    ;; #x0c -io NOP abs
    [(#x0d) (values (format "ORA $~a" (word-at-pc+1 use-state)) 3)]
    [(#x0e) (values (format "ASL $~a" (word-at-pc+1 use-state)) 3)]
    ;; #x0f -io SLO abs
    [(#x10) (values (format-relative-branch use-state "BPL") 2)]
    [(#x11) (values (format "ORA ($~a),y" (byte-at-pc+1 use-state)) 2)]
    ;; #x12 -io KIL
    ;; #x13 -io SLO izy
    ;; #x14 -io NOP zpx
    [(#x15) (values (format "ORA $~a,x" (byte-at-pc+1 use-state)) 2)]
    [(#x16) (values (format "ASL $~a,x" (byte-at-pc+1 use-state)) 2)]
    ;; #x17 -io SLO zpx
    [(#x18) (values "CLC" 1)]
    [(#x19) (values (format "ORA $~a,y" (word-at-pc+1 use-state)) 3)]
    ;; #x1a -io NOP
    ;; #x1b -io SLO abt
    ;; #x1c -io NOP abx
    [(#x1d) (values (format "ORA $~a,x" (word-at-pc+1 use-state)) 3)]
    [(#x1e) (values (format "ASL $~a,x" (word-at-pc+1 use-state)) 3)]
    ;; #x1f -io SLO abx
    [(#x20) (values (format "JSR $~a" (word-at-pc+1 use-state)) 3)]
    [(#x21) (values (format "AND ($~a,x)" (byte-at-pc+1 use-state)) 2)]
    ;; #x22 -io KIL
    ;; #x23 -io RLA izx
    [(#x24) (values (format "BIT $~a" (byte-at-pc+1 use-state)) 2)]
    [(#x25) (values (format "AND $~a" (byte-at-pc+1 use-state)) 2)]
    [(#x26) (values (format "ROL $~a" (byte-at-pc+1 use-state)) 2)]
    ;; #x27 -io RLA zp
    [(#x28) (values "PLP" 1)]
    [(#x29) (values (format "AND #$~a" (byte-at-pc+1 use-state)) 2)]
    [(#x2a) (values "ROL" 1)]
    ;; #x2b -io ANC imm
    [(#x2c) (values (format "BIT $~a" (word->hex-string (peek-word-at-pc+1 use-state))) 3)]
    [(#x2d) (values (format "AND $~a" (word->hex-string (peek-word-at-pc+1 use-state))) 3)]
    [(#x2e) (values (format "ROL $~a" (word->hex-string (peek-word-at-pc+1 use-state))) 3)]
    ;; #x2f -io RIA abs
    [(#x30) (values (format-relative-branch use-state "BMI") 2)]
    [(#x31) (values (format "AND ($~a),y" (byte-at-pc+1 use-state)) 2)]
    ;; #x32 -io KIL
    ;; #x33 -io RIA izy
    ;; #x34 -io NOP zpx
    [(#x35) (values (format "AND $~a,x" (byte-at-pc+1 use-state)) 2)]
    [(#x36) (values (format "ROL $~a,x" (byte-at-pc+1 use-state)) 2)]
    ;; #x37 -io RLA zpx
    [(#x38) (values "SEC" 1)]
    [(#x39) (values (format "AND $~a,y" (word-at-pc+1 use-state)) 3)]
    ;; #x3a -io NOP
    ;; #x3b -io RLA aby
    ;; #x3c -io NOP abx
    [(#x3d) (values (format "AND $~a,x" (word-at-pc+1 use-state)) 3)]
    [(#x3e) (values (format "ROL $~a,x" (word-at-pc+1 use-state)) 3)]
    ;; #x3f -io RLA abx
    [(#x40) (values "RTI" 1)]
    [(#x41) (values (format "EOR ($~a,x)" (byte-at-pc+1 use-state)) 2)]
    ;; #x42 -io KIL
    ;; #x43 -io SRE izx
    ;; #x44 -io NOP zp
    [(#x45) (values (format "EOR $~a" (byte-at-pc+1 use-state)) 2)]
    [(#x46) (values (format "LSR $~a" (byte-at-pc+1 use-state)) 2)]
    ;; #x47 -io SRE zp
    [(#x48) (values "PHA" 1)]
    [(#x49) (values (format "EOR #$~a" (byte-at-pc+1 use-state)) 2)]
    [(#x4a) (values "LSR" 1)]
    ;; #x4b -io ALR imm
    [(#x4c) (values (format "JMP $~a" (word-at-pc+1 use-state)) 3)]
    [(#x4d) (values (format "EOR $~a" (word-at-pc+1 use-state)) 3)]
    [(#x4e) (values (format "LSR $~a" (word-at-pc+1 use-state)) 3)]
    ;; #x4f -io SRE abs
    [(#x50) (values (format-relative-branch use-state "BVC") 2)]
    [(#x51) (values (format "EOR ($~a),y" (byte-at-pc+1 use-state)) 2)]
    ;; #x52 -io KIL
    ;; #x53 -io SRE izy
    ;; #x54 -io NOP zpx
    [(#x55) (values (format "EOR $~a,x" (byte-at-pc+1 use-state)) 2)]
    [(#x56) (values (format "LSR $~a,x" (byte-at-pc+1 use-state)) 2)]
    ;; #x57 -io SRE zpx
    [(#x58) (values "CLI" 1)]
    [(#x59) (values (format "EOR $~a,y" (word-at-pc+1 use-state)) 3)]
    ;; #x5a -io NOP
    ;; #x5b -io SRE aby
    ;; #x5c -io NOP abx
    [(#x5d) (values (format "EOR $~a,x" (word-at-pc+1 use-state)) 3)]
    [(#x5e) (values (format "LSR $~a,x" (word-at-pc+1 use-state)) 3)]
    ;; #x5f -io SRE abx
    [(#x60) (values "RTS" 1)]
    [(#x61) (values (format "ADC ($~a,x)" (byte-at-pc+1 use-state)) 2)]
    ;; #x62 -io KIL
    ;; #x63 -io RRA izx
    ;; #x64 -io NOP zp
    [(#x65) (values (format "ADC $~a" (byte-at-pc+1 use-state)) 2)]
    [(#x66) (values (format "ROR $~a" (byte-at-pc+1 use-state)) 2)]
    ;; #x67 -io RRA zp
    [(#x68) (values "PLA" 1) ]
    [(#x69) (values (format "ADC #$~a" (byte-at-pc+1 use-state)) 2)]
    [(#x6a) (values "ROR" 1)]
    ;; #x6b -io ARR imm
    [(#x6c) (values (format "JMP ($~a)" (word-at-pc+1 use-state)) 3)]
    [(#x6d) (values (format "ADC $~a" (word-at-pc+1 use-state)) 3)]
    [(#x6e) (values (format "ROR $~a" (word-at-pc+1 use-state)) 3)]
    ;; #x6f -io RRA abs
    [(#x70) (values (format-relative-branch use-state "BVS") 2)]
    [(#x71) (values (format "ADC ($~a),y" (byte-at-pc+1 use-state)) 2)]
    ;; #x72 -io KIL
    ;; #x73 -io RRA izy
    ;; #x74 -io NOP zpx
    [(#x75) (values (format "ADC $~a,x" (byte-at-pc+1 use-state)) 2)]
    [(#x76) (values (format "ROR $~a,x" (byte-at-pc+1 use-state)) 2)]
    ;; #x77 -io RRA zpx
    [(#x78) (values "SEI" 1)]
    [(#x79) (values (format "ADC $~a,y" (word-at-pc+1 use-state)) 3)]
    ;; #x7a -io NOP
    ;; #x7b -io RRA aby
    ;; #x7c -io NOP abx
    [(#x7d) (values (format "ADC $~a,x" (word-at-pc+1 use-state)) 3)]
    [(#x7e) (values (format "ROR $~a,x" (word-at-pc+1 use-state)) 3)]
    ;; #x7f -io RRA abx
    ;; #x80 -io NOP imm
    [(#x81) (values (format "STA ($~a,x)" (byte-at-pc+1 use-state)) 2)]
    ;; #x82 -io NOP imm
    ;; #x83 -io SAX izx
    [(#x84) (values (format "STY $~a" (byte-at-pc+1 use-state)) 2)]
    [(#x85) (values (format "STA $~a" (byte-at-pc+1 use-state)) 2)]
    [(#x86) (values (format "STX $~a" (byte-at-pc+1 use-state)) 2)]
    ;; #x87 -io SAX zp
    [(#x88) (values "DEY" 1)]
    ;; #x89 -io NOP imm
    [(#x8a) (values "TXA" 1)]
    ;; #x8b -io XAA imm
    [(#x8c) (values (format "STY $~a" (word-at-pc+1 use-state)) 3)]
    [(#x8d) (values (format "STA $~a" (word-at-pc+1 use-state)) 3)]
    [(#x8e) (values (format "STX $~a" (word-at-pc+1 use-state)) 3)]
    ;; #x8f -io SAX abs
    [(#x90) (values (format-relative-branch use-state "BCC") 2)]
    [(#x91) (values (format "STA ($~a),y" (byte-at-pc+1 use-state)) 2)]
    ;; #x92 -io KIL
    ;; #x93 -io AHX izy
    [(#x94) (values (format "STY $~a,x" (byte-at-pc+1 use-state)) 2)]
    [(#x95) (values (format "STA $~a,x" (byte-at-pc+1 use-state)) 2)]
    [(#x96) (values (format "STX $~a,y" (byte-at-pc+1 use-state)) 2)]
    ;; #x97 -io SAX zpy
    [(#x98) (values "TYA" 1)]
    [(#x99) (values (format "STA $~a,y" (word-at-pc+1 use-state)) 3)]
    [(#x9a) (values "TXS" 1)]
    ;; #x9b -io TAS avt
    ;; #x9c -io SHY abx
    [(#x9d) (values (format "STA $~a,x" (word-at-pc+1 use-state)) 3)]
    ;; #x9e -io SHX aby
    ;; #x9f -io AHX aby
    [(#xa0) (values (format "LDY #$~a" (byte-at-pc+1 use-state)) 2)]
    [(#xa1) (values (format "LDA ($~a,x)" (byte-at-pc+1 use-state)) 2)]
    [(#xa2) (values (format "LDX #$~a" (byte-at-pc+1 use-state)) 2)]
    ;; #xa3 -io LAX izx
    [(#xa4) (values (format "LDY $~a" (byte-at-pc+1 use-state)) 2)]
    [(#xa5) (values (format "LDA $~a" (byte-at-pc+1 use-state)) 2)]
    [(#xa6) (values (format "LDX $~a" (byte-at-pc+1 use-state)) 2)]
    ;; #xa7 -io LAX zp
    [(#xa8) (values "TAY" 1)]
    [(#xa9) (values (format "LDA #$~a" (byte-at-pc+1 use-state)) 2)]
    [(#xaa) (values "TAX" 1)]
    ;; #xab -io LAX imm
    [(#xac) (values (format "LDY $~a" (word-at-pc+1 use-state)) 3)]
    [(#xad) (values (format "LDA $~a" (word-at-pc+1 use-state)) 3)]
    [(#xae) (values (format "LDX $~a" (word-at-pc+1 use-state)) 3)]
    ;; #xaf -io LAX abs
    [(#xb0) (values (format-relative-branch use-state "BCS") 2)]
    [(#xb1) (values (format "LDA ($~a),y" (byte-at-pc+1 use-state)) 2)]
    ;; #xb2 -io KIL
    ;; #xb3 -io LAX izy
    [(#xb4) (values (format "LDY $~a,x" (byte-at-pc+1 use-state)) 2)]
    [(#xb5) (values (format "LDA $~a,x" (byte-at-pc+1 use-state)) 2)]
    [(#xb6) (values (format "LDX $~a,y" (byte-at-pc+1 use-state)) 2)]
    ;; #xb7 -io LAX zpy
    [(#xb8) (values "CLV" 1)]
    [(#xb9) (values (format "LDA $~a,y" (word-at-pc+1 use-state)) 3)]
    [(#xba) (values "TSX" 1)]
    ;; #xbb -io LAS aby
    [(#xbc) (values (format "LDY $~a,x" (word-at-pc+1 use-state)) 3)]
    [(#xbd) (values (format "LDA $~a,x" (word-at-pc+1 use-state)) 3)]
    [(#xbe) (values (format "LDX $~a,y" (word-at-pc+1 use-state)) 3)]
    ;; #xbf -io LAX aby
    [(#xc0) (values (format "CPY #$~a" (byte-at-pc+1 use-state)) 2)]
    [(#xc1) (values (format "CMP ($~a,x)" (byte-at-pc+1 use-state)) 2)]
    ;; #xc2 -io NOP imm
    ;; #xc3 -io DCP izx
    [(#xc4) (values (format "CPY $~a" (byte-at-pc+1 use-state)) 2)]
    [(#xc5) (values (format "CMP $~a" (byte-at-pc+1 use-state)) 2)]
    [(#xc6) (values (format "DEC $~a" (byte-at-pc+1 use-state)) 2)]
    ;; #xc7 -io DCP zp
    [(#xc8) (values "INY" 1)]
    [(#xc9) (values (format "CMP #$~a" (byte-at-pc+1 use-state)) 2)]
    [(#xca) (values "DEX" 1)]
    ;; #xcb -io AXS imm
    [(#xcc) (values (format "CPY $~a" (word-at-pc+1 use-state)) 3)]
    [(#xcd) (values (format "CMP $~a" (word-at-pc+1 use-state)) 3)]
    [(#xce) (values (format "DEC $~a" (word-at-pc+1 use-state)) 3)]
    ;; #xcf -io DCP abs
    [(#xd0) (values (format-relative-branch use-state "BNE") 2)]
    [(#xd1) (values (format "CMP ($~a),y" (byte-at-pc+1 use-state)) 2)]
    ;; #xd2 -io KIL
    ;; #xd3 -io DCP izy
    ;; #xd4 -io NOP zpx
    [(#xd5) (values (format "CMP $~a,x" (byte-at-pc+1 use-state)) 2)]
    [(#xd6) (values (format "DEC $~a,x" (byte-at-pc+1 use-state)) 2)]
    ;; #xd7 -io DCP zpx
    [(#xd8) (values "CLD" 1)]
    [(#xd9) (values (format "CMP $~a,y" (word-at-pc+1 use-state)) 3)]
    ;; #xda -io NOP
    ;; #xdb -io DCP aby
    ;; #xdc -io NOP abx
    [(#xdd) (values (format "CMP $~a,x" (word-at-pc+1 use-state)) 3)]
    [(#xde) (values (format "DEC $~a,x" (word-at-pc+1 use-state)) 3)]
    ;; #xdf -io DCP abx
    [(#xe0) (values (format "CPX #$~a" (byte-at-pc+1 use-state)) 2)]
    [(#xe1) (values (format "SBC ($~a,x)" (byte-at-pc+1 use-state)) 2)]
    ;; #xe2 -io NOP imm
    ;; #xe3 -io ISC izx
    [(#xe4) (values (format "CPX $~a" (byte-at-pc+1 use-state)) 2)]
    [(#xe5) (values (format "SBC $~a" (byte-at-pc+1 use-state)) 2)]
    [(#xe6) (values (format "INC $~a" (byte-at-pc+1 use-state)) 2)]
    ;; #xe7 -io ISC zp
    [(#xe8) (values "INX" 1)]
    [(#xe9) (values (format "SBC #$~a" (byte-at-pc+1 use-state)) 2)]
    [(#xea) (values "NOP" 1)]
    ;; #xeb -io SBC imm
    [(#xec) (values (format "CPX $~a" (word-at-pc+1 use-state)) 3)]
    [(#xed) (values (format "SBC $~a" (word-at-pc+1 use-state)) 3) ]
    [(#xee) (values (format "INC $~a" (word-at-pc+1 use-state)) 3)]
    ;; #xef -io ISC abs
    [(#xf0) (values (format-relative-branch use-state "BEQ") 2)]
    [(#xf1) (values (format "SBC ($~a),y" (byte-at-pc+1 use-state)) 2)]
    ;; #xf2 -io KIL
    ;; #xf3 -io ISC izy
    ;; #xf4 -io NOP zpx
    [(#xf5) (values (format "SBC $~a,x" (byte-at-pc+1 use-state)) 2)]
    [(#xf6) (values (format "INC $~a,x" (byte-at-pc+1 use-state)) 2)]
    ;; #xf7 -io ISC zpx
    [(#xf8) (values "SED" 1)]
    [(#xf9) (values (format "SBC $~a,y" (word-at-pc+1 use-state)) 3)]
    ;; #xfa -io NOP
    ;; #xfb -io ISC aby
    ;; #xfc -io NOP abx
    [(#xfd) (values (format "SBC $~a,x" (word-at-pc+1 use-state)) 3)]
    [(#xfe) (values (format "INC $~a,x" (word-at-pc+1 use-state)) 3)]
    ;; #xff -io ISC abx
    [else (values (format ".data $~a" (byte-at-pc use-state)) 1)]))
