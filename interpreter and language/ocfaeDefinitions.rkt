#lang plai

; represents an expression
(define-type OCFAE
  [num (n number?)]
  [bool (b boolean?)]
  [str (s string?)]
  [unop (op symbol?) (arg OCFAE?)]
  [binop (op symbol?) (lhs OCFAE?) (rhs OCFAE?)]
  [varref (name symbol?)]
  [switch (val OCFAE?) (clauses (listof SClause?)) (elseval OCFAE?)]
  [fun (params (listof symbol?)) (body OCFAE?)]
  [withrec (name symbol?) (rhs OCFAE?) (body OCFAE?)]
  [app (f OCFAE?) (args (listof OCFAE?))]
  [newexp (fields (listof (list/c symbol? OCFAE?)))]
  [objref (obj OCFAE?) (field symbol?)]
  [seq (exps (listof OCFAE?))])

; represents a 'switch' clause
(define-type SClause
  [clause (patval OCFAE?) (result OCFAE?)])

; represents a possible result of evaluation
(define-type OCFAE-Value
  [numV (n number?)]
  [boolV (b boolean?)]
  [strV (s string?)]
  [closureV (params (listof symbol?))
            (body OCFAE?)
            (env hash?)]
  [objectV (fields (hash/c symbol? OCFAE-Value?))]
  [boxV (loc number?)])

; An environment is represented as a hash mapping symbols
;  to (racket) boxes containing OCFAE-Values

; represents a store containing a hash and the next unused address
(define-type Store
  [sto (memory (and/c immutable? (hash/c number? OCFAE-Value?)))
       (next-loc number?)])
 

;table : symbol? -> operator
;table of arithemic operations  
(define (table sym)
  (cond [(equal? sym '+) +]
        [(equal? sym '-) -]
        [(equal? sym '*) *]
        [(equal? sym '/) /]
        [(equal? sym 'and) (lambda (x y) (and x y))]
        [(equal? sym 'or) (lambda (x y) (or x y))]
        [(equal? sym 'equal?) equal?]
        [(equal? sym '<=) <=]
        [(equal? sym 'not) not]
        [(equal? sym 'number?) number?]
        [(equal? sym 'string?) string?]
        
        
        ))


; isBinop: symbol? -> boolean?
; determines if the argument is a symbol, if
; it is then it checks if it is a binary operator keyword
(define (isBinop sym)
  (cond [(list? sym) false]
        [(symbol=? sym '+) true]
        [(symbol=? sym '-) true]
        [(symbol=? sym '*) true]
        [(symbol=? sym '/) true]
        [(symbol=? sym '<=) true]
        [(symbol=? sym 'and) true]
        [(symbol=? sym 'or) true]
        [(symbol=? sym 'equal?) true]
        [(symbol=? sym 'setbox!) true]
        [else false]))

; isUnop: symbol? -> boolean?
; determines if the argument is a symbol, if
; it is then it checks if it is a unary operator keyword.
(define (isUnop sym)
  (cond [(list? sym) false]
        [(symbol=? sym 'not) true]
        [(symbol=? sym 'string?) true]
        [(symbol=? sym 'number?) true]
        [else (isUnBoxExp sym)]))

; isUnBoxExp: symbol? -> boolean?
; determines if the argument is a symbol, if
; it is then it checks if it is a unary box keyword.
(define (isUnBoxExp sym)
  (cond [(symbol=? sym 'makebox) true]
        [(symbol=? sym 'getbox) true]
        [else false]))

; isKeyword: symbol? -> boolean?
; determines if the argument is a symbol, if
; it is then it checks if it is a keyword.
(define (isKeyword sym)
  (cond [(list? sym)  false]
        [(symbol=? sym 'true) true]
        [(symbol=? sym 'false) true]
        [(symbol=? sym 'with) true]
        [(symbol=? sym 'switch) true]
        [(symbol=? sym 'else) true]
        [(symbol=? sym 'fun) true]
        [(symbol=? sym 'withrec) true]
        [(symbol=? sym 'ref) true]
        [(symbol=? sym 'new) true]
        [(symbol=? sym 'seq) true]
        [else (or (isUnop sym) (isBinop sym))]))

;illegalArgumentException : symbol? number? number -> error || number
;signals an error for binary operator if arguments (left || right) are 
;invalid
(define (illegalArgumentException sym left right)
  (cond [(and (equal? / sym) (= 0 right)) 
         (error "/: divide by zero " illegalArgumentException)]
        [else (sym left right)]))