#lang racket/base

#|

provide routines that are useful for testing memory management routines

|#

(require (only-in racket/list
                  empty?
                  drop
                  flatten
                  range)
         (only-in racket/port
                  open-output-nowhere)
         (only-in racket/string
                  string-replace)
         (only-in uuid
                  uuid-string)
         "../6510.rkt"
         (only-in "../ast/6510-assembler.rkt"
                  new-assemble-to-code-list
                  assembly-code-list-org-code-sequences
                  assembly-code-list-labels)
         (only-in "../ast/6510-command.rkt"
                  ast-label-def-cmd?
                  ast-label-def-cmd-label)
         (only-in "../ast/6510-resolver.rkt"
                  add-label-suffix)
         (only-in "../tools/6510-debugger-shared.rkt"
                  breakpoint)
         (only-in "../tools/6510-debugger.rkt"
                  run-debugger-on
                  debugger--assembler-interactor)
         (only-in "../tools/6510-interpreter.rkt"
                  cpu-state-program-counter
                  6510-load-multiple
                  initialize-cpu
                  run-interpreter-on
                  memory-list))

(provide run-code-in-test-on-code
         remove-labels-for
         wrap-code-for-test
         list-with-label-suffix
         calls-to-mock
         compact-run-code-in-test-
         TEST_COUNTERS)

(define (remove-labels-for code labels-to-remove (result (list)))
  (cond
    [(empty? code) (reverse result)]
    [else
     (define cmd (car code))
     (cond
       [(ast-label-def-cmd? cmd)
        (define label (ast-label-def-cmd-label cmd))
        (if (findf (lambda (label-cmd) (string=? label (ast-label-def-cmd-label label-cmd))) labels-to-remove)
            (remove-labels-for (cdr code) labels-to-remove result)
            (remove-labels-for (cdr code) labels-to-remove (cons cmd result)))]
       [else (remove-labels-for (cdr code) labels-to-remove (cons cmd result))])]))

(define (run-code-in-test-on-code wrapped-test-code (debug #f))
  (define assembly (new-assemble-to-code-list wrapped-test-code))
  (define state-before
    (6510-load-multiple (initialize-cpu)
                        (assembly-code-list-org-code-sequences assembly)))
  (if debug
      (run-debugger-on state-before "" #t
                       (list (breakpoint "Start of actual test code"
                                         (lambda (lc-state)
                                           (eq? (cpu-state-program-counter lc-state)
                                                (hash-ref (assembly-code-list-labels assembly) "TEST_ENTRY")))
                                         #t))
                       (list)
                       debugger--assembler-interactor
                       #t
                       #:labels (assembly-code-list-labels assembly))
      (parameterize ([current-output-port (open-output-nowhere)])
        (run-interpreter-on state-before))))

  ;; test counters are kept at this address
  ;; each time a mocked call is done (no relative branches to it allowed!), the byte is incremented
  ;; mocks are numbered (index) as they are passed (at a003 = counter for first mock, a004 = counter for second mock ...)
  (define TEST_COUNTERS #xa003)
  (define (calls-to-mock state (mock-idx 0))
    (car (memory-list state (+ TEST_COUNTERS mock-idx) (+ TEST_COUNTERS mock-idx))))
  (define (wrap-code-for-test bc complete-code (mocked-code-list (list)))
    (append (list
      (org #xa000)
             (JMP TEST_START__) ;; takes three bytes => test counters start at a003
      (label TEST_COUNTERS)
             (byte 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)     ;; total 32 mock counters (probably never need that much)
             (byte 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (label TEST_START__)
             (JSR VM_INITIALIZE_MEMORY_MANAGER)
             (JSR $0100)) ;; reset clock cycles
             bc           ;; paste in test code
             (list (BRK)) ;; stop
             (remove-labels-for complete-code (filter (lambda (ast-cmd) (ast-label-def-cmd? ast-cmd)) mocked-code-list))))

  ;; add unique random suffix to labels ending on "__"
  ;; mock away labels in the mock list
  ;; add subroutine that counts calls to mocks incresing TEST_COUNTERS+<idx of mock>
  ;; where idx is the list position number of the mock passed
(define (list-with-label-suffix #:provide-own-test-entry-label (provide-own-test-entry-label #f) #:org (ast-org-cmd #f) #:mock (mocked-code-list (list)) . list-elements )
    (add-label-suffix
     "__" (string-replace (uuid-string) "-" "_")
     (append
      (if ast-org-cmd
          (list ast-org-cmd)
          (list))
      (list (JMP TEST_ENTRY))
      (flatten (map (lambda (mocked-label idx)
                      (list mocked-label
                            ;; INC TEST_COUNTERS+<idx-mock>
                            (ast-unresolved-opcode-cmd
                             '()
                             (list (car (ast-opcode-cmd-bytes (INC $a000))))
                             (ast-resolve-word-scmd (format "TEST_COUNTERS+~a"  idx)))
                            (RTS)
                            ))
                    mocked-code-list
                    (range (length mocked-code-list))))
      (if provide-own-test-entry-label
          (list)
          (list (label TEST_ENTRY)))
      list-elements)))

  ;; run the given code in test, wrapping it with mocks and counters, entering interactive debugger, if requested
(define (run-code-in-test bc (debug #f) #:runtime-code (vm-memory-manager (list)) #:mock (mocked-code-list (list)))
    (run-code-in-test-on-code (wrap-code-for-test bc vm-memory-manager mocked-code-list) debug))

  ;; run the given code using mocks, calls being counted, and label suffixes for the test-code
(define (compact-run-code-in-test- #:runtime-code (vm-memory-manager (list)) #:debug (debug #f) #:mock (mocked-labels (list)) . cmds)
    (run-code-in-test
     (apply list-with-label-suffix
            cmds
            #:mock mocked-labels)
     debug
     #:runtime-code vm-memory-manager
     #:mock mocked-labels))
