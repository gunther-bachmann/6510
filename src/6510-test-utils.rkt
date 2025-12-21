#lang racket

;; use this instead of (require rackunit) to make use of skipping tests

(require ansi-color)
(require rackunit) ;; TODO: remove from here to be able to use typed variant instead
(require racket/path)

(provide skip inform-check-equal? skip-module drop-meta-info drop-meta-infos)
(provide (all-from-out rackunit))
(provide (all-from-out racket/path))
(provide (all-from-out ansi-color))

(define (drop-meta-infos ast-commands)
  (map drop-meta-info ast-commands))

;; transform ast-command into a list and drop the meta-info part (useful for comparison)
(define (drop-meta-info ast-command)
    (define ast-command-as-list (vector->list (struct->vector ast-command)))
    (append (take ast-command-as-list 1)
            (drop ast-command-as-list 2)))

(define-syntax (inform-check-equal? stx)
  (syntax-case stx ()
    ([_ val-a val-b]
     (with-syntax [(line (syntax-line stx))
                   (fname (syntax-source stx))
                   (col (syntax-column stx))
                   (a (gensym))
                   (b (gensym))
                   (location (gensym))]
       (datum->syntax
        stx
        (syntax->datum
         #'(begin
             (define a val-a)
             (define b val-b)
             (define location (format "~a:~a:~a" (file-name-from-path fname) line col))
             (cond [(not (eq? a b))
                    (with-colors 'yellow
                      (lambda ()
                        (displayln (format "--------------------\nINFORMATION"))
                        (displayln (format " ~a" location))
                        (displayln (format "name:       inform-check-equal?\nlocation:   ~a\nactual:     ~a\nexpected:   ~a" location a b))
                        (displayln "--------------------")))]))))))
    ([_ val-a val-b msg]
     (with-syntax [(line (syntax-line stx))
                   (fname (syntax-source stx))
                   (col (syntax-column stx))
                   (a (gensym))
                   (b (gensym))
                   (location (gensym))]
       (datum->syntax
        stx
        (syntax->datum
         #'(begin
             (define a val-a)
             (define b val-b)
             (define location (format "~a:~a:~a" (file-name-from-path fname) line col))
             (cond [(not (eq? a b))
                    (with-colors 'yellow
                      (lambda ()
                        (displayln (format "--------------------\nINFORMATION"))
                        (displayln (format " ~a" location))
                        (displayln (format "name:       inform-check-equal?\nlocation:   ~a\nactual:     ~a\nexpected:   ~a\nmessage:    ~a" location a b msg))
                        (displayln "--------------------")))]))))))))

;; skip a test case, reporting it
;; usage:
;;   (skip (check...   ))              ;; skips the given test
;;   (skip "skip message" (check... )) ;; skip the test and write "skip message"
(define-syntax (skip stx)
  (syntax-case stx ()
    ([_ body]
     (datum->syntax
      stx
      (syntax->datum
       #'(check-true
          (begin
            (with-colors 'red
              (lambda () (color-displayln "test skipped.")))
            #t)))))
    ([_ msg body]
     (datum->syntax
      stx
      (syntax->datum
       #'(check-true
          (begin
            (with-colors 'red
              (lambda () (color-displayln (format "test skipped ~a." msg))))
            #t)))))))


(define-syntax (skip-module stx)
  (syntax-case stx ()
    ([_ body] #'(list))))
