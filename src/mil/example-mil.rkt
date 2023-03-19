#lang racket

(require "./mil-structures.rkt")
(require "./mil-interpreter.rkt")

(define mil-definition-funx80 
  (mil-definition 
   'funx80 '()
   "a function that returns the value #x80"
   (mil-uint8 #x80)))

(define mil-definition-times2
  (mil-definition
   'times2 (list (mil-parameter 'a))
   "returns 2 * a"
   (mil-l (mil-symbol '+) (mil-symbol 'a) (mil-symbol 'a))))

(define mil-definition-age-comment
  (mil-definition
   'age-comment (list (mil-parameter 'age))
   "return a comment about your age"
   (mil-if (mil-l (mil-symbol '>) (mil-symbol 'age) (mil-uint8 17))
           (mil-string "erwachsen")
           (mil-string "kind"))))

(define mil-module-a
  (mil-module 'a
              (list)
              (list)
              (list mil-definition-funx80
                    mil-definition-times2
                    mil-definition-age-comment)
              (list)
              (list)))

(module+ main
  (interpret (mil-l (mil-symbol 'display)
                    (mil-string "some ~a\n")
                    (mil-uint8 #x80))
               (list))

  (interpret
   (mil-l (mil-symbol '+)
          (mil-uint8 2)
          (mil-l (mil-symbol 'funx80)))
   (list (module-ctx mil-module-a)))

  (interpret
   (mil-l (mil-symbol 'times2) (mil-uint8 #x10))
   (list (module-ctx mil-module-a)))

  (interpret
   (mil-l (mil-symbol 'age-comment) (mil-uint8 #x10))
   (list (module-ctx mil-module-a)))

  (interpret
   (mil-l (mil-symbol 'age-comment) (mil-uint8 #x20))
   (list (module-ctx mil-module-a))))
