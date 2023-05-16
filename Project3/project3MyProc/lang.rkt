#lang eopl

;; grammar for the PROC language

(provide (all-defined-out))

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '((program (expression) a-program)
    
    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    
    (expression
     ("zero?" "(" expression ")")
     zero?-exp)
    
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)
    
    (expression (identifier) var-exp)
    
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

    (expression
     ("proc" "(" identifier ")" expression)
     proc-exp)
    
    (expression
     ("(" expression expression ")")
     call-exp)

    ;;---------------------------------------------------------
    ; INSERT YOUR CODE HERE
    ; Add the syntax for the new expressions here below:
    (expression
       ("newqueue(" ")")
       newqueue-exp)
    (expression
       ("queue-push(" expression ", " expression ")")
       queue-push-exp)
    (expression
       ("queue-pop(" expression ")")
       queue-pop-exp)
    (expression
       ("queue-peek(" expression ")")
       queue-peek-exp)
    (expression
       ("queue-push-multi(" expression ", " expression ")")
       queue-push-multi-exp)
    (expression
       ("queue-pop-multi(" expression ", " expression ")")
       queue-pop-multi-exp)
    (expression
       ("queue-merge(" expression ", " expression ")")
       queue-merge-exp)
    ;;---------------------------------------------------------
    
    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

