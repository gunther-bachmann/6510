#lang racket
#|

 provide all structures needed to define mil-commands

|#

(provide (struct-out mil-expression)
         (struct-out mil-symbol)
         (struct-out mil-list)
         mil-l
         (struct-out mil-value)
         (struct-out mil-atomic-value)
         (struct-out mil-uint8)
         (struct-out mil-string)
         (struct-out mil-char)
         (struct-out mil-bool)
         (struct-out mil-definition)
         (struct-out mil-parameter)
         (struct-out mil-module)
         (struct-out mil-void)
         (struct-out mil-quote)
         (struct-out mil-cell)
         (struct-out mil-if)
         (struct-out mil-let)
         (struct-out mil-progn)
         (struct-out mil-and)
         (struct-out mil-or)
         (struct-out mil-let-binding)
         (struct-out mil-typed-expression))

(struct mil-expression
  ()
  #:transparent)

(struct mil-value mil-expression
  ()
  #:transparent)

(struct mil-atomic-value mil-value
  ()
  #:transparent)

(struct mil-void mil-atomic-value
  ()
  #:transparent)

(struct mil-bool mil-atomic-value
  (value)
  #:transparent
  #:guard (struct-guard/c boolean?))

(struct mil-quote mil-expression
  (quoted)
  #:transparent
  #:guard (struct-guard/c mil-expression?))

(struct mil-cell mil-expression
  (head
   tail)
  #:transparent
  #:guard (struct-guard/c mil-expression? mil-expression?))

(struct mil-list mil-expression
  (elements)
  #:transparent
  #:guard (struct-guard/c (listof mil-expression?)))

(define (mil-l . params)
  (mil-list params))

(struct mil-progn mil-list
  ()
  #:transparent
  #:guard (struct-guard/c (listof mil-expression?)))

(struct mil-and mil-list
  ()
  #:transparent
  #:guard (struct-guard/c (listof mil-expression?)))

(struct mil-or mil-list
  ()
  #:transparent
  #:guard (struct-guard/c (listof mil-expression?)))

(struct mil-symbol mil-value
  (value)
  #:transparent
  #:guard (struct-guard/c symbol?))

(struct mil-string mil-atomic-value
  (value)
  #:transparent
  #:guard (struct-guard/c string?))

(struct mil-uint8 mil-atomic-value
  (value)
  #:transparent
  #:guard (struct-guard/c byte?))

(struct mil-char mil-atomic-value
  (value)
  #:transparent
  #:guard (struct-guard/c char?))

(struct mil-typed-expression mil-expression
  (type)
  #:transparent
  #:guard (struct-guard/c mil-expression?))

(struct mil-let-binding
  (id
   expr)
  #:transparent
  #:guard (struct-guard/c symbol?
                          mil-expression?))

(struct mil-let mil-expression
  (bindings
   body)
  #:transparent
  #:guard (struct-guard/c (listof mil-let-binding?)
                          mil-expression?))

(struct mil-if mil-expression
  (predicate
   true-body
   false-body)
  #:transparent
  #:guard (struct-guard/c mil-expression?
                          mil-expression?
                          mil-expression?))

(struct mil-type mil-typed-expression
  (id)
  #:transparent
  #:guard (struct-guard/c mil-typed-expression? symbol?))

(struct mil-parameter
  (id)
  #:transparent
  #:guard (struct-guard/c symbol?))

(struct mil-definition mil-expression
  (id
   parameters
   documentation
   body)
  #:transparent
  #:guard (struct-guard/c symbol?
                          (listof mil-parameter?)
                          string?
                          mil-expression?))

(struct mil-require mil-expression
  ()
  #:transparent)

(struct mil-provide mil-expression
  ()
  #:transparent)

(struct mil-module mil-expression
  (id
   required 
   provided
   definitions
   sub-modules
   root-expressions)
  #:transparent
  #:guard (struct-guard/c symbol?
                          (listof mil-require?)
                          (listof mil-provide?)
                          (listof mil-definition?)
                          (listof mil-expression?) ;; mil-sub-module
                          (listof mil-expression?) ;; 
                          ))
