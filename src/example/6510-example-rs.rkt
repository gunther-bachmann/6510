#! /usr/bin/env racket
#lang racket

;; file is intended to document dsl possibilities
;; new language features are tested here first
;
;; C-c C-c and inspect program-p1, program-p2 and raw-bytes

(require "../6510.rkt")
(require "../ast/6510-resolver.rkt")
(require "../ast/6510-relocator.rkt")

(require "../tools/6510-interpreter.rkt")
(require "../ast/6510-constants.rkt")


(module+ test
  (require rackunit)
  (require (only-in "../6510-test-utils.rkt" drop-meta-infos)))

(module+ test #| smoke test |#
  #| test that the first step in compilation works
   | even when used in a totally different file (importing the macros)
   |#
  (check-equal?
   (drop-meta-infos (list
                    (ASL $10)
                    (BEQ $F0)
                    (BRK)
                    (JMP $FFD2)
                    (JMP ($FFFE))
                    (JMP (some))
                    (SBC some)
                    (SBC >some)
                    (BEQ some)
                    (NOP)
                    (SBC $10,x)
                    (STX $3A,y)
                    (asc "some")))
   (drop-meta-infos (list
                    (ast-opcode-cmd '() '(#x06 #x10))
                    (ast-rel-opcode-cmd '() '(240 240))
                    (ast-opcode-cmd '() '(#x00))
                    (ast-opcode-cmd '() '(#x4c #xD2 #xFF))
                    (ast-opcode-cmd '() '(#x6c #xFE #xFF))
                    (ast-unresolved-opcode-cmd '() '(#x6c) (ast-resolve-word-scmd "some"))
                    (ast-decide-cmd
                     '()
                     (list (ast-unresolved-opcode-cmd '() '(#xe5) (ast-resolve-byte-scmd "some" 'low-byte))
                           (ast-unresolved-opcode-cmd '() '(#xed) (ast-resolve-word-scmd "some"))))
                    (ast-unresolved-opcode-cmd '() '(#xe5) (ast-resolve-byte-scmd "some" 'high-byte))
                    (ast-unresolved-rel-opcode-cmd '() '(#xf0) (ast-resolve-byte-scmd "some" 'relative))
                    (ast-opcode-cmd '() '(#xea))
                    (ast-opcode-cmd '() '(#xf5 #x10))
                    (ast-opcode-cmd '() '(#x96 #x3a))
                    (ast-bytes-cmd '() '(#x73 #x6f #x6d #x65))))))

(define org #xc000)

(define program
  (list
   (byte-const  "MYVAL" "$42")     ;; define label with byte constant: myval = $42

   (LDX !$05)
   (label "NEXT")
   (LDA "!$41" )
   (JSR "COUT")
   (ADC "!$01")
   (JSR "COUT")
   (LDA "!%00001010") ; line feed
   (JSR "COUT")
   (LDA "!MYVAL")
   (JSR "COUT")
   (DEX)
   (BNE "NEXT")
   (BRK)

   ;; (IMPORT-BYTE ":IMPORTEDBVAL-L")     ;; in order to not use -L qualifier, there needs to be a function that will return the type (e.g. through an environment
   ;; (IMPORT-WORD ":IMPORTEDWORD")
   ;; (EXPORT-WORD ":EXPORTED" ":COUT")
   ;; (EXPORT-BYTE ":EXPORTED" ":COUT-L") ;;
   (word-const  "M16BVAL" "$C842") ;; define label with word constant: m16bval = $c842
   ;; (EXPORT-BYTE ":EXPORTED-VAL" ":MYVAL") ;; export a defined byte constant
   ;; (EXPORT-WORD ":EXPORTED-16BVAL" ":MY16BVAL") ;; export a defined word constant
   (asc "HELLO WORLD")

   (label "COUT")
   (JSR "$FFD2")
   (RTS)
   (JMP "COUT")
   (JMP ("COUT"))
   (ADC "<COUT")
   (ADC "!<COUT")
   (STA ">COUT")
   (STA ">COUT",x)
   (LDX ">COUT" ,y)
   (STA (">COUT",x) )
   (STA (">COUT"),y)
   (STA "COUT")
   (STA "COUT",x)
   (STA "COUT",y)
))

(define addressing-program
  (list
   (LDA "!$40")           ;; immediate
   (LDA "$40")            ;; zero-page
   (LDA "$40",x)          ;; zero-page-x (add reg x to $40, read memory from there)
   (LDA "$4000")          ;; absolute
   (LDA "$4000",x)        ;; absolute x
   (LDA "$4000",y)        ;; absolute y
   (LDA ("$40",x))        ;; indirect x (add x to $40, read pointer (low+high) from memory, read pointed to byte)
   (LDA ("$40"),y)        ;; indirect y (read pointer from $40, add y to pointer, read pointed to byte)
   (label "NEXT")
   (ASL A)                ;; work on accumulator
   (BEQ "$10")            ;; relative to current program counter
   (BEQ "NEXT")))

(define program-p1 (->resolved-decisions (label-instructions program) program))
(define program-p2 (->resolve-labels org (label-string-offsets org program-p1) program-p1 '()))
(define program-p3 (resolve-constants (constant-definitions-hash program-p1) program-p2))
(define raw-bytes (resolved-program->bytes program-p3))

(define data (6510-load (initialize-cpu) org raw-bytes))
(define executable-program (with-program-counter data org))

(when (not (getenv "INSIDE_EMACS"))
  (displayln "examine variables e.g.\n  program\n  program-p1\n  program-p2\n  program-p3\n")
  (displayln "execute program\n  (run executable-program)")
  (displayln "debug program\n  (run-debugger org raw-bytes)"))

;; (print-state (run executable-program))
;; (let ([_ (run executable-program)])
;;   (void))

(module+ main
  (let ([_ (run executable-program)])
    (void)))
