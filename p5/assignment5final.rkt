#lang plai
(print-only-errors #t)
 
; represents a 'switch' clause
(define-type SClause
  [clause (patval OCFAE?) (result OCFAE?)])

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
  [objref (obj OCFAE?) (field symbol?)])

; represents a possible result of evaluation
(define-type OCFAE-Value
  [numV (n number?)]
  [boolV (b boolean?)]
  [strV (s string?)]
  [closureV (params (listof varref))
            (body OCFAE?)
            (env hash?)]
  [objectV (fields (listof (hash/c symbol? OCFAE-Value?)))])


;table : symbol? -> operator
;table of arithemic operations  
(define (table sym)
  (cond [(equal? sym '+) +]
        [(equal? sym '-) -]
        [(equal? sym '*) *]
        [(equal? sym '/) /]
        [(equal? sym 'not) not]
        [(equal? sym 'or) (lambda (x y) (or x y))]
        [(equal? sym 'and) (lambda (x y) (and x y))]
        [(equal? sym 'equal?) equal?]
        [(equal? sym '<=) <=]
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
        [else false]))

; isUnop: symbol? -> boolean?
; determines if the argument is a symbol, if
; it is then it checks if it is a unary operator keyword.
(define (isUnop sym)
  (cond [(list? sym) false]
        [(symbol=? sym 'not) true]
        [(symbol=? sym 'string?) true]
        [(symbol=? sym 'number?) true]
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
        [else (or (isUnop sym) (isBinop sym))]))

;illegalArgumentException : symbol? number? number -> error || number
;signals an error for binary operator if arguments (left || right) are 
;invalid
(define (illegalArgumentException sym left right)
  (cond [(and (equal? / sym) (= 0 right)) 
         (error "/: divide by zero " illegalArgumentException)]
        [else (sym left right)]))


; parse-exp : s-exp? -> CFAE?
; This procedure parses an s-expression into a CFAE
(define (parse-exp sexp)
  (cond [(number? sexp) (num sexp)]
        [(string? sexp) (str sexp)]
        [(equal? sexp 'true) (bool true)]
        [(equal? sexp 'false) (bool false)]
        [(list? sexp)
         (define firstItem (first sexp))
         (cond
           ;if the expression is app 
           [(not (isKeyword firstItem))
            (app (parse-exp firstItem) (parse-args (rest sexp)))]
           [(and (= (length sexp) 2) (isUnop firstItem))
            ;if there is unop
            (unop firstItem (parse-exp (second sexp)))]
           ;if it is new
           [(symbol=? firstItem 'new)
            (if (> (length sexp) 1)
                (newexp (parse-new (rest sexp)))
                (error 'parse-exp "illegal expression: invalid length of new ~e" sexp))]
           [(= (length sexp) 3) ;we have a list of length 3
            (cond [(symbol=? 'with firstItem) ;with expression 
                   ;parse-with 
                   (define args-argv (parse-with-lst(second sexp)))
                   (cond [(validWithSub (first args-argv) (list))
                          (app (fun (first args-argv) (parse-exp (third sexp))) (second args-argv))]
                         [else (error 'parse-exp "illegal expression: invalid agrs ~e" (first args-argv) )])]
                  ;if we have withrec
                  [(and (symbol=? 'withrec firstItem) (list? (second sexp)))
                       (let ()
                         (define id-rhs(second sexp))
                         (if (and (= (length id-rhs) 2) (not (isKeyword (first id-rhs)))) 
                             (withrec (first id-rhs) (parse-exp (second id-rhs)) (parse-exp(third sexp)))
                             (error 'parse-exp "illegal expression: invalid rhs ~e" sexp)))]
                  ;if we have a binop, then parse-exp it
                  [(isBinop firstItem) (binop firstItem (parse-exp (second sexp)) 
                                              ( parse-exp (third sexp)))]
                  ;if we have a switch with no clauses.
                  [(symbol=? 'switch firstItem)
                   (cond [(and (list? (last sexp)) (symbol? (first (last sexp))) (symbol=? (first (last sexp)) 'else))
                          (switch (parse-exp (second sexp))  
                                  (parse-clauses empty) 
                                  (parse-exp (last (last sexp))))]
                         [else (error 'parse-exp "illegal expression ~e" sexp)])]
                  ;if we have a function.
                  [(and (symbol=? 'fun firstItem) (list? (second sexp))) 
                   (if (validSym (second sexp) (list)) 
                       (fun (second sexp) (parse-exp (third sexp)))
                       (error ' parse-exp "illegal expression ~e: invalid symbol " 'sexp))]
                  ;if we have a ref
                  [(symbol=? 'ref firstItem)   
                   (define id-ref(third sexp))
                   (if (and (symbol? id-ref) (not (isKeyword id-ref))) 
                        (objref (parse-exp (second sexp)) id-ref)
                        (error ' parse-exp "illegal expression ~e: invalid id" 'sexp))]
                  ;else its a error
                  [else (error ' parse-exp "illegal expression ~e" sexp)])]
           ;if we have a switch with clauses.
           [(and (>= (length sexp) 4) (symbol=? 'switch firstItem))
            (cond [(and (symbol? (first (last sexp))) 
                        (symbol=? (first (last sexp)) 'else))
                   (switch (parse-exp (second sexp))  
                           (parse-clauses (rest (rest sexp))) 
                           (parse-exp (last (last sexp))))]
                  [else (error 'parse-exp "illegal expression ~e" sexp)])]
           [else (error ' parse-exp "illegal expression ~e" sexp)])]
        [(and (symbol? sexp) (not (isKeyword sexp))) (varref sexp)]
        [else (error ' parse-exp "illegal expression ~e" sexp)]))

(define (parse-new exp)
  (cond [(empty? exp) empty]
        [else (let()
                (define first-item(first exp))

                (cond [(and (= (length first-item) 2) (symbol? (first first-item)) 
                            (not (isKeyword (first first-item))))
                       (define firstList (list (first first-item) (parse-exp (second first-item)))) 
                             (cons firstList (parse-new (rest exp)))]
                      [else (error 'parse-with 
                                   "illegal expression: invalid symbol-CAFE pair ~e" first-item)]))]))


;parse-args : (listof args) -> (listof CFAE)
;parae's list of CFAE from sexp to produce and list of C1F1WAE
(define (parse-args args)
  (cond [(empty? args) empty]
        [else (cons (parse-exp (first args)) (parse-args (rest args)))]))

;parse-with : e-exp -> WAE
;parse's with sexp to produce and WAE
(define (parse-with-lst lstLob)
  (parse-args-val lstLob (list (list) (list))))

;parse-args-val : (list symbol?) -> (list (list symbol?) (list symbol?))
;parses args and values from with binding. Returns a list of 
;(list of arguments) and (list of values)
(define (parse-args-val lstLob lst-args-val)
  (cond [(empty? lstLob) lst-args-val]
        [else (define fstExp (first lstLob))
              (cond [(and (= (length fstExp)3) 
                          (and (symbol? (first fstExp)) (symbol=? '= (second fstExp))))
                     (let()
                       (define new-lst-args-val(list (append (first lst-args-val) (list (first fstExp)))
                                                     (append  (second lst-args-val) (list (parse-exp (third fstExp))) ))) 
                       (parse-args-val (rest lstLob) new-lst-args-val))]
                    [else (error 'parse-with-list "illegal expression, parsing 'with' failed.
 expected: {x = exp}*, given" fstExp lstLob)])]))

;validSym : (list symbol)? (list symbol)-> boolean?
;returns false if the list of symbols contains duplicates or keywords,
;otherwise returns true
(define (validSym lstSym seenLst) 
  (cond [(empty? lstSym) true]
        [(or (member (first lstSym) seenLst) (isKeyword (first lstSym))) false]
        [else (validSym (rest lstSym) (append (list (first lstSym)) seenLst))]))

;validWithSub : (list binding)? (list binding-name)-> boolean?
;returns false if the list of binding contains duplicates or keywords,
;otherwise returns true
(define (validWithSub lstLob seenLst)
  (cond [(empty? lstLob) true]
        [(or (member (first lstLob) seenLst) (isKeyword (first lstLob))) false]
        [else (validWithSub (rest lstLob) (append (list (first lstLob)) seenLst))]))

;parse-clauses: (listof sexp) -> (listof S-Clauses) 
;parses list of sexp into list of S-Clauses. Helper function for parsing switch expressions
(define (parse-clauses clauseLst)
  (cond [(empty? clauseLst) (list)]
        [(and (equal? (length (first clauseLst)) 3) 
              (symbol=? (second (first clauseLst)) '=>)) 
         (define firstElement (first clauseLst)) 
         (append (list (clause (parse-exp (first firstElement))
                               (parse-exp (third firstElement)))) 
                 (parse-clauses(rest clauseLst)))]
        [(and (and (equal? (length (first clauseLst) ) 2) 
                   (symbol=? (first (first clauseLst)) 'else)) 
              (empty? (rest clauseLst))) (list)]
        [else (error 'parse-clauses "illegal expression ~e" clauseLst)]))



; interp-unop: unary-operator? CFAE/L-Value? -> CFAE/L-Value?
; Performs the unary operation on the two CFAE/L-Values.
(define (interp-unop op arg)
  (type-case OCFAE-Value arg
    [numV (n) (cond 
                [(or (equal? op not)) (error 'interp-unop "illegal expression ~e" arg)]
                [else (boolV (op n))])]
    [strV (s) (cond
                [(or (equal? op not)) (error 'interp-unop "illegal expression ~e" arg)]
                [else (boolV (op s))])]
    [boolV (b) (boolV (op b))]
    [objectV (fields)  (error 'interp-binop "illegal expression: unnop on objectV ~e" arg)]
    [closureV (params body env) (error 'interp-unop "Closure found ~e" arg)]))

; interp-binop: binop-operator? CFAE/L-Value? CFAE/L-Value? -> CFAE/L-Value?
; Performs the binary operation on the two CFAE/L-Values.
(define (interp-binop op left right)
  (type-case OCFAE-Value left
    [numV (n) (type-case OCFAE-Value right
                [numV (n2) (cond 
                             [(or (equal? op 'or) (equal? op 'and)) (error 'interp-binop "illegal expression ~e" op)]
                             [(or (equal? op 'equal?) (equal? op '<=)) (boolV ((table op) n n2))]
                             [else (numV ( illegalArgumentException (table op) n n2)) ])]
                [objectV (fields)  (error 'interp-binop "illegal expression: binop on objectV ~e" left)]
                [else (cond
                        [(equal? op 'equal?) (boolV ((table op) left right))]
                        [else (error 'interp-binop "illegal expression ~e" right)])])]
    [strV (s) (type-case OCFAE-Value right
                [strV (s2) (cond
                             [(or (equal? op 'or) (equal? op 'and) (equal? op '<=)) (error 'interp-binop "illegal expression ~e" op)]
                             [(equal? op 'equal?) (boolV ((table op) s s2))]
                             [else (error 'interp-binop "illegal expression ~e" op)])]
                [objectV (fields)  (error 'interp-binop "illegal expression: binop on objectV ~e" left)]
                [else (cond
                        [(equal? op 'equal?) (boolV ((table op) left right))]
                        [else (error 'interp-binop "illegal expression ~e" right)])])]
    [boolV (b) (type-case OCFAE-Value right
                 [boolV (b2) (cond
                               [(or (equal? op '+) (equal? op '-) (equal? op '*) (equal? op '/) (equal? op '<=)) (error 'interp-binop "illegal expression ~e" op)]
                               [else (boolV ((table op) b b2))])]
                 [objectV (fields)  (error 'interp-binop "illegal expression: binop on objectV ~e" left)]
                 [else (cond
                         [(equal? op 'equal?) (boolV ((table op) left right))]
                         [else (error 'interp-binop "illegal expression ~e" right)])])]
    [objectV (fields)  (error 'interp-binop "illegal expression: binop on objectV ~e" left)]
    [closureV (params body env) (error 'interp-binop "Closure found ~e" left)])) 
      

; interp-switch: CFAE/L-Value? (listof SClauses?) CFAE? immutable-hash-table? -> CF1WAE?
; returns the resulting expression if the patval is satisfied, otherwise the else expression.
(define (interp-switch val clauses elseval env)
  (cond
    [(empty? clauses) elseval]
    [(equal? val (interp (clause-patval (first clauses)) env)) (clause-result (first clauses))]
    [else (interp-switch val (rest clauses) elseval env)]))


; env-lookup: symbol? immutable-hash-table? -> immutable-hash-table?
(define (env-lookup name table)
  (cond
    [(equal? (hash-ref table name #f) #f) (error 'env-lookup "free variable ~e" name)]
    [else (hash-ref table name)]))


; interp : CFAE/L? immutable-hash-table? -> CFAE/L-Value?
; This procedure interprets the given CF1WAE in the given
;  environment with the given function definitions and
;  produces a result in the form of a CFAE/L-Value
(define (interp exp env)
  (type-case OCFAE exp
    [num (n) (numV n)]
    [str (s) (strV s)]
    [bool (b) (boolV b)]
    [unop (op arg) (interp-unop (table op) (interp arg env))] 
    [binop (op l r) (interp-binop op (interp l env) (interp r env))]
    [varref (name) (let()
                     (define return (unbox (env-lookup name env))) 
                     (if return return 
                         (error 'interp "empty box ~e" name)))] 
    [fun (params body) (closureV params body env)]
    [switch (val clauses elseval) 
            (let() (define ans(interp-switch (interp val env) clauses elseval env))
              (interp ans env))]
    [app (fun args)
         (let()
           (define pre-interp(interp fun env))
           (type-case OCFAE-Value pre-interp
             [closureV (c-params c-body c-env)
                       (let ()
                         (cond [(= (length c-params) (length args))
                                (define new-env (map-params-args env c-env c-params args))
                                (interp c-body new-env)]
                               [else (error "illegal expression : to few or to many args ( ~e ) supplied to function ( ~e ) " args fun)]))]
             [else pre-interp]))]
    [withrec (name rhs body) 
             (let()
               (define new-box (box false))
               (define new-env(hash-set env name new-box))
               (define rhsV(interp rhs new-env))
               (set-box! new-box rhsV)
               (interp body new-env))]
    [newexp (fields) (objectV(newObjV fields (list) env))]
    [objref (obj field) (let()
                          (define objV(interp obj env))
                          (type-case OCFAE-Value objV
                            [objectV (objFields) (find objFields field)]
                            [else (error "illegal expression: illegal call to objref, doesn't contain object ~e" obj)]))]))

;newObjV: 
(define (newObjV field seen env)
  (cond [(empty? field) empty]
        [else (if (or (not (symbol? (first (first field)))) (member (first (first field)) seen)) (error "illegal expression : duplicate field name" field)
                  (let ()
                    (define firstElem (first field))
                    
                    (define hashV(interp (second firstElem) env))
                    (cons (hash (first firstElem) hashV)
                          (newObjV (rest field) (append seen (list(first firstElem)) ) env))))]))

;find : symbol? 
(define (find obj field)
  (cond [(empty? obj) (error 'find "obj is not in the field" field)]
        [else (if (equal? (hash-ref (first obj) field #f) #f) (find (rest obj) field)
                  (hash-ref (first obj) field))]))
      

; map-params-args : immutable-hash-table? immutable-hash-table? (listof symbols?) (listof CFAE?) -> immutable-hash-table?
; creates a new hash table of closure, mapping c-params to args closure with static-env and false cache box 
; by expanding existing hash table
(define (map-params-args static-env dynamic-env params args)
  (cond [(empty? params) dynamic-env]
        [else 
         (define newHash(hash-set dynamic-env (first params) (box (interp (first args) static-env))))
         (map-params-args static-env newHash (rest params) (rest args))]))

 
;----------------------TESTS----------------------------------------------------

; Coverage test
(test (isBinop '()) false)
(test (isUnop '()) false)
(test/exn (interp (binop '<= (num 5) (newexp (list (list 'a (num 5))))) (hash)) "illegal expression: binop on objectV")
(test/exn (interp (binop '<= (bool true) (newexp (list (list 'a (num 5))))) (hash)) "illegal expression: binop on objectV")
(test/exn (interp (binop '<= (str "5") (newexp (list (list 'a (num 5))))) (hash)) "illegal expression: binop on objectV")
(test/exn (interp (binop '<= (newexp (list (list 'a (num 5)))) (newexp (list (list 'a (num 5))))) (hash)) "illegal expression: binop on objectV")
(test/exn (interp (unop 'not (newexp (list (list 'a (num 5))))) (hash)) "illegal expression: unnop on objectV")
(test/exn (interp (objref (num 5) 'BAD) (hash)) "illegal expression: illegal call to objref, doesn't contain object")

(define tempexp1 (parse-exp '{new 
                             {author "test"}
                             {author
                              "The Sciences of the Artificial"}
                             {year 1994}}))

(define tempexp2 (parse-exp '{new 
                             {author1 "test"}
                             {author
                              "The Sciences of the Artificial"}
                             {year 1994}}))

(test/exn (interp (objref tempexp1 'bad) (hash)) "duplicate field name")
(test/exn (interp (objref tempexp2 'bad) (hash)) "obj is not in the field")
(define tempHash(hash 'x (box #f)))
(test/exn (interp (varref 'x) tempHash) "empty box")

;test withrec
(test (parse-exp '(withrec {a {fun (x) (x)}} {a 3})) (withrec 'a (fun '(x) (app (varref 'x) '())) (app (varref 'a) (list (num 3)))))
(test (parse-exp '{withrec {countDown {fun {x} {switch x [0 => 0] [else {countDown {- x 1}}]}}}
  {countDown 13}}) (withrec
                    'countDown
                    (fun
                     '(x)
                     (switch
                      (varref 'x)
                      (list (clause (num 0) (num 0)))
                      (app (varref 'countDown) (list (binop '- (varref 'x) (num 1))))))
                    (app (varref 'countDown) (list (num 13)))))

(test (parse-exp '{withrec {fac {fun (x) (switch x 
                                                 (0 => 1)
                                                 (1 => 1)
                                                 (else (* x (fac (- x 1)))))}} (fac 4)})
      (withrec 'fac (fun '(x) (switch (varref 'x)
                                      (list (clause (num 0) (num 1))
                                            (clause (num 1) (num 1)))
                                      (binop '* (varref 'x) (app (varref 'fac) (list (binop '- (varref 'x) (num 1)))))))
               (app (varref 'fac) (list (num 4)))))



(test/exn (parse-exp '(withrec {a b {fun (x) (x)}} {a 3})) "parse-exp: illegal expression:")
(test/exn (parse-exp '(withrec {a b {fun (x) (x)}} {a 2} {a 3})) "illegal expression")


;test new
(test (parse-exp '(new (hello (fun (s) (+ 1 s)))
                 (world 2))) (newexp (list (list 'hello (fun '(s) (binop '+ (num 1) (varref 's)))) 
                                           (list 'world (num 2)))))

 
(test (parse-exp '{new {author
      {new {firstname "Herbert"} {surname "Simon"}}}
     {title
      "The Sciences of the Artificial"}
     {year 1994}})
       
      (newexp (list (list 'author (newexp (list (list 'firstname (str "Herbert")) 
                                                (list 'surname (str "Simon")))))
                    (list 'title (str "The Sciences of the Artificial"))
                    (list 'year (num 1994)))))
                          
(test/exn (parse-exp '{new }) "illegal")
(test/exn (parse-exp '{new (author "str" d)}) "illegal")     
(test/exn (parse-exp '{new (author )}) "illegal")     
(test/exn (parse-exp '{new (author "hello")
                           (wrong)}) "illegal")

;test ref
(test (parse-exp '(ref (new (title "myBook")
                            (author "myAuthor")) title)) 
      (objref (newexp (list (list 'title (str "myBook")) (list 'author (str "myAuthor")))) 'title))

(test (parse-exp '(ref (new (apple 1) 
                            (orange 0)
                            (mix (new (appleOrange 3) 
                                      (orangeApple 4)))) orange))
      (objref (newexp (list (list 'apple (num 1))
                            (list 'orange (num 0))
                            (list 'mix (newexp (list (list 'appleOrange (num 3))
                                                     (list 'orangeApple (num 4)))))))
              'orange))

(test/exn (parse-exp '(ref 3 3)) "illegal")
(test/exn (parse-exp '(ref 3 a h)) "illegal")


(define newexp1 (parse-exp '{new {author
      {new {firstname "Herbert"} {surname "Simon"}}}
     {title
      "The Sciences of the Artificial"}
     {year 1994}}))
(interp (objref (objref newexp1 'author) 'firstname) (hash))

;----------------------TESTS - INTERP ----------------------------------------------------
(define factorial(withrec 'fac (fun '(x) (switch (varref 'x)
                                      (list (clause (num 0) (num 1))
                                            (clause (num 1) (num 1)))
                                      (binop '* (varref 'x) (app (varref 'fac) (list (binop '- (varref 'x) (num 1)))))))
               (app (varref 'fac) (list (num 4)))))
(test (interp factorial (hash)) (numV 24)) 
(test (interp (withrec 'fac (fun '(x) (switch (varref 'x)
                                      (list (clause (num 0) (num 1))
                                            (clause (num 1) (num 1)))
                                      (binop '* (varref 'x) (app (varref 'fac) (list (binop '- (varref 'x) (num 1)))))))
               (app (varref 'fac) (list (num 5)))) (hash)) (numV 120))

(test (interp (parse-exp '{withrec {countDown {fun {x} {switch x [0 => 0] [else {countDown {- x 1}}]}}}
                                   {countDown 13}}) (hash))(numV 0))





;----------------------TESTS - OLD ----------------------------------------------------
(define t10 (parse-exp '(with ((x = 9)) 
                              (with ((y = (fun () (+ g  x))) (g = 8) (x = 1)) 
                                    (with ((a = 1) (b = (fun (numm) (+ numm y)))) 
                                          (with ((c = x)) (+ b c)))))))
(test/exn (interp t10 (hash)) "Closure found")


;test var's
(define tt1 (parse-exp '(with ((x = 2)) (x))))
(define tt2 (parse-exp '(with ((x = 2) (y = 0)) (x))))
(define tt3 (parse-exp '(with ((x = 2) (y = 0)) (+ x y))))
(define tt4 (parse-exp '(with ((x = 2) (y = 3)) 
                              (with ((z = (+ x y))) 
                                    (+ x z)))))
(define tt5 (parse-exp '(with ((x = 2)) 
                              (with ((x = (+ 1 x))) (x)))))
(define tt6 (parse-exp '(with ((x = 2) (y = 1)) 
                              (with ((x = (+ 1 x))) 
                                    (with ((x = (* x x)) (y = 5))
                                          (+ x y))))))
;test cases for with
(test (interp tt1 (hash) ) (numV 2))
(test (interp tt2 (hash) ) (numV 2))
(test (interp tt3 (hash) ) (numV 2))
(test (interp tt4 (hash) ) (numV 7))
(test (interp tt5 (hash) ) (numV 3))
(test (interp tt6 (hash) ) (numV 14))

(test (interp (parse-exp '(with ((a = 4) (b = 5)) (with ((c = (* b 4)) (d = (+ 4 a))) (+ (- b c) (* a d))))) (hash)) (numV 17))
(test (interp (app (fun '(a b) (binop '+ (varref 'a) (varref 'b))) (list (num 3) (num 4))) (hash)) (numV 7))
(test/exn (interp (app (fun '(a b) (binop '+ (varref 'a) (varref 'b))) (list (num 3) (num 4) (num 5))) (hash)) "illegal expression : to few or to many args")
(test (interp (app (num 10) (list (num 3) (num 4))) (hash)) (numV 10))
(test (interp (app (bool true) (list (num 3))) (hash)) (boolV true))
(test (interp (app (str "blah") (list (num 3))) (hash)) (strV "blah"))
(test/exn (parse-exp '(fun (with) (list (num 3)))) "illegal expression")
(test/exn (interp-unop number? (closureV (list 'a) (num 3) (hash))) "Closure found")
(test (interp-unop number? (numV 3)) (boolV true))

; Adding test for coverage
(test/exn (interp (binop '<= (num 5) (bool true)) (hash) ) "illegal expression") 
(test/exn (interp (binop 'and (str "abc") (str "abcd")) (hash) ) "illegal expression")
(test (parse-exp '(string? "hi")) (unop 'string? (str "hi")))
(test (interp (binop 'equal? (str "abc") (str "abc")) (hash)) (boolV true))
(test/exn (interp (binop '+ (bool true) (bool false)) (hash)) "illegal expression")
(test (interp (binop 'equal? (bool true) (num 3)) (hash)) (boolV false))
(test/exn (interp (binop 'or (bool true) (str "abc")) (hash)) "illegal expression")
(test (parse-exp '(number? 3)) (unop 'number? (num 3)))
(test/exn (parse-exp 'else) "illegal expression")
(test/exn (interp (app (varref 'f) (list (varref 'g))) (hash)) "free variable")

;binop tests 
(test/exn (interp (binop '/ (binop '+ (num 5) (num 1)) (num 0)) (hash)) "/: divide by zero")
(test (interp (binop '+ (binop '+ (num 5) (num 3)) (num 1)) (hash)) (numV 9))
(test (interp (binop '+ (num 3) (num 1)) (hash)) (numV 4))
(test (interp (binop '/ (num 10) (num 5)) (hash)) (numV 2))
(test (interp (binop '* (num 3) (num 3)) (hash)) (numV 9))
(test (interp (binop '- (num 5) (num 1)) (hash)) (numV 4))
(test (interp (binop 'equal? (num 5) (num 5)) (hash)) (boolV true))
(test (interp (binop 'equal? (num 5) (str "5")) (hash)) (boolV false))
(test (interp (binop 'or (bool false) (bool true)) (hash)) (boolV true))
(test (interp (binop 'or (bool false) (binop 'or (bool true) (bool false))) (hash)) (boolV true))
(test (interp (binop 'and (bool false) (bool true)) (hash)) (boolV false))
(test (interp (binop 'and (binop 'or (bool false) (bool true)) (bool true)) (hash)) (boolV true))
(test (interp (binop '<= (num 5) (num 7)) (hash)) (boolV true))
(test (interp (binop 'equal? (str "53") (bool false)) (hash)) (boolV false))
(test (interp (binop '<= (num 55) (num 7)) (hash)) (boolV false))
(test/exn (interp (binop '<= (str "53") (num 7)) (hash)) "illegal expression")
(test/exn (interp (binop 'or (num 5) (num 7)) (hash)) "illegal expression")
(test/exn (interp (binop 'and (str "53") (num 7)) (hash)) "illegal expression")
(test/exn (interp (binop '- (str "53") (num 7)) (hash)) "illegal expression")
(test/exn (interp (binop '/ (str "53") (str "hi")) (hash)) "illegal expression")
(test/exn (interp (binop '* (str "53") (bool false)) (hash)) "illegal expression")

;switch tests
(test (interp (switch
               (str "banana")
               (list
                (clause (str "apple") (str "good choice!"))
                (clause (str "banana") (str "excellent choice!"))
                (clause (str "durian") (str "Hmm, I'm not sure your taxi driver is going to like that.")))
               (str "I don't recognize your so-called 'fruit'.")) (hash)) (strV "excellent choice!"))

(test (interp (switch
               (binop '+ (num 3) (num 4))
               (list
                (clause (num 2) (str "good choice!"))
                (clause (num 7) (str "excellent choice!"))
                (clause (str "huh?") (str "Hmm, I'm not sure your taxi driver is going to like that.")))
               (str "I don't recognize your so-called 'fruit'.")) (hash)) (strV "excellent choice!"))

(test (interp (switch
               (str "BAD")
               (list
                (clause (str "apple") (str "good choice!"))
                (clause (str "banana") (str "excellent choice!"))
                (clause (str "durian") (str "Hmm, I'm not sure your taxi driver is going to like that.")))
               (str "I don't recognize your so-called 'fruit'.")) (hash)) (strV "I don't recognize your so-called 'fruit'."))

(test (parse-exp 'x) (varref 'x))
(test (parse-exp '(+ 3 4)) (binop '+ (num 3) (num 4)))
(test (parse-exp '(/ 3 4)) (binop '/ (num 3) (num 4)))
(test (parse-exp '(* 3 4)) (binop '* (num 3) (num 4)))
(test (parse-exp '(- 3 4)) (binop '- (num 3) (num 4)))
(test ( parse-exp '(/ 3 0)) (binop '/ (num 3) (num 0)))
(test ( parse-exp '0) (num 0))
;(test ( parse-exp 'null) (mt))


;is this handle in the parses as well
(test/exn ( parse-exp '(/ 0)) "illegal expression")
(test/exn ( parse-exp '(/)) "illegal expression")
(test/exn ( parse-exp '(with ((x = )) x)) "illegal expression")
(test/exn ( parse-exp '(with ((x = 8) (x  8) (x = 8) ) x)) "illegal expression")
(test/exn ( parse-exp '(with ((x = 8) (= 8) (x = 8) ) x)) "illegal expression")
(test/exn ( parse-exp '(with ((x = 8) (d = 8) (8) ) x)) "illegal expression")
(test/exn ( parse-exp '(with ((x = 8) (x = 1)) x)) "illegal expression")
(test/exn ( parse-exp '(with ((x = 8) (x = 1)) (varref 'x))) "illegal expression")
(test/exn ( parse-exp '(with ((x = (+ 5 5))) (with ((y  = (- x 3)) (y = 2)) (+ x y))))  "illegal expression")

;test for booleans
(test/exn (parse-exp '(true)) "illegal expression")
(test/exn (parse-exp '(false)) "illegal expression")
(test/exn (parse-exp '(not)) "illegal expression")
(test/exn (parse-exp '(not (true))) "illegal expression")
(test/exn (parse-exp '(not true true)) "illegal expression")
(test/exn (parse-exp '(not true true false)) "illegal expression")
(test/exn (parse-exp '(not true true false false)) "illegal expression")
(test (parse-exp 'true) (bool true))
(test (parse-exp 'false) (bool false))

;test cases for unop
(test (parse-exp '(and true true)) (binop 'and (bool true) (bool true)))
(test (parse-exp '(or true true)) (binop 'or (bool true) (bool true)))
(test (parse-exp '(not true)) (unop 'not (bool true)))
(test (parse-exp '(not false)) (unop 'not (bool false)))
(test (parse-exp "string") (str "string"))
(test (parse-exp "5") (str "5"))
(test (parse-exp '(and 3 4)) (binop 'and (num 3) (num 4)))
(test/exn (parse-exp '(and true))"illegal expression")
(test (interp (unop 'not (bool true)) (hash)) (boolV false))  
(test (interp (unop 'number? (num 5)) (hash)) (boolV true))
(test (interp (unop 'string? (str "abc")) (hash)) (boolV true))
(test (interp (unop 'string? (bool true)) (hash)) (boolV false))
(test (interp (unop 'number? (bool true)) (hash)) (boolV false))
(test (interp (unop 'number? (num 4)) (hash)) (boolV true))
(test/exn (interp (unop 'not (str "abd")) (hash)) "illegal expression")
(test/exn (interp (unop 'not (num 5)) (hash)) "illegal expression")

;test for comparsions
(test (parse-exp '(equal? true true)) (binop 'equal? (bool true) (bool true)))
(test/exn (parse-exp '(equal? true)) "illegal expression")
(test/exn (parse-exp '(equal? )) "illegal expression")
(test/exn (parse-exp '(equal? true false 3)) "illegal expression")

(test (parse-exp '(<= true true)) (binop '<= (bool true) (bool true)))
(test/exn (parse-exp '(<= true)) "illegal expression")
(test/exn (parse-exp '(<= )) "illegal expression")
(test/exn (parse-exp '(<= true false 3)) "illegal expression")

;parse test
(test (parse-exp 'x) (varref 'x))
(test (parse-exp '(+ 3 4)) (binop '+ (num 3) (num 4)))
(test (parse-exp '(/ 3 4)) (binop '/ (num 3) (num 4)))
(test (parse-exp '(* 3 4)) (binop '* (num 3) (num 4)))
(test (parse-exp '(- 3 4)) (binop '- (num 3) (num 4)))
(test ( parse-exp '(/ 3 0)) (binop '/ (num 3) (num 0)))
(test ( parse-exp '0) (num 0))
;(test ( parse-exp 'null) (mt))


;illegal with expressions
(test/exn ( parse-exp '(with ((x = )) x)) "illegal expression")
(test/exn ( parse-exp '(with ((x = 8) (x  8) (x = 8) ) x)) "illegal expression")
(test/exn ( parse-exp '(with ((x = 8) (= 8) (x = 8) ) x)) "illegal expression")
(test/exn ( parse-exp '(with ((x = 8) (d = 8) (8) ) x)) "illegal expression")
(test/exn ( parse-exp '(with ((x = 8) (x = 1)) x)) "illegal expression")
(test/exn ( parse-exp '(with ((x = 8) (x = 1)) (varref 'x))) "illegal expression")
(test/exn ( parse-exp '(with ((x = (+ 5 5))) (with ((y  = (- x 3)) (y = 2)) (+ x y))))  "illegal expression")
(test/exn ( parse-exp '(with ((x = (+ 5 5))) (with ((y  = (- x 3)) (y = 2)) (+ x y))))  "illegal expression")
(test/exn (parse-exp 'else) "illegal expression")
;test cases for with
(test ( parse-exp '(with ((x = 2)) x)) (app (fun '(x) (varref 'x)) (list (num 2))))
(test ( parse-exp '(with ((x = 2) (y = 0)) x)) (app (fun '(x y) (varref 'x)) (list (num 2) (num 0))))
(test (parse-exp '(with ((x = 2)) x)) (app (fun '(x) (varref 'x)) (list (num 2))))
(test (parse-exp '(with ((x = 2) (y = 2)) (+ x y))) (app (fun '(x y) (binop '+ (varref 'x) (varref 'y))) (list (num 2) (num 2))))
(test ( parse-exp '(with ((x = 2)
                          (y = 3))
                         (with ((z = (+ x y)))
                               (+ x z)))) (app (fun '(x y) 
                                                    (app (fun '(z) 
                                                              (binop '+ (varref 'x) (varref 'z))) 
                                                         (list (binop '+ (varref 'x) (varref 'y))))) 
                                               (list (num 2) (num 3))))

(test ( parse-exp '(with ((x = (+ 5 5))) (with ((y  = (- x 3))) (+ y y)))) 
      (app (fun '(x) (app (fun '(y) (binop '+ (varref 'y) (varref 'y))) (list (binop '- (varref 'x) (num 3))))) (list (binop '+ (num 5) (num 5)))))
;test for booleans
(test/exn (parse-exp '(true)) "illegal expression")
(test/exn (parse-exp '(false)) "illegal expression")
(test/exn (parse-exp '(not)) "illegal expression")
(test/exn (parse-exp '(not (true))) "illegal expression")
(test/exn (parse-exp '(not true true)) "illegal expression")
(test/exn (parse-exp '(not true true false)) "illegal expression")
(test/exn (parse-exp '(not true true false false)) "illegal expression")
(test (parse-exp 'true) (bool true))
(test (parse-exp 'false) (bool false))

;test cases for unop
(test (parse-exp '(and true true)) (binop 'and (bool true) (bool true)))
(test (parse-exp '(or true true)) (binop 'or (bool true) (bool true)))
(test (parse-exp '(not true)) (unop 'not (bool true)))
(test (parse-exp '(not false)) (unop 'not (bool false)))
(test (parse-exp "string") (str "string"))
(test (parse-exp "5") (str "5"))
(test (parse-exp '(and 3 4)) (binop 'and (num 3) (num 4)))
(test/exn (parse-exp '(and true))"illegal expression")
(test (parse-exp '(string? "abc")) (unop 'string? (str "abc")))
(test (parse-exp '(number? 3)) (unop 'number? (num 3)))

;test for comparsions
(test (parse-exp '(equal? true true)) (binop 'equal? (bool true) (bool true)))
(test/exn (parse-exp '(equal? true)) "illegal expression")
(test/exn (parse-exp '(equal? )) "illegal expression")
(test/exn (parse-exp '(equal? true false 3)) "illegal expression")

(test (parse-exp '(<= true true)) (binop '<= (bool true) (bool true)))
(test/exn (parse-exp '(<= true)) "illegal expression")
(test/exn (parse-exp '(<= )) "illegal expression")
(test/exn (parse-exp '(<= true false 3)) "illegal expression")

;test cases for switch
(test(parse-exp '{switch "get-fruit"
                         [else "I don't recognize your so-called 'fruit'."]}) 
     (switch (str "get-fruit") '() (str "I don't recognize your so-called 'fruit'.")))

(test (parse-exp '{switch "get-fruit"
                          ["apple" => "good choice!"]
                          ["banana" => "excellent choice!"]
                          ["durian" => "Hmm, I'm not sure your taxi driver is going to like that."]
                          [else "I don't recognize your so-called 'fruit'."]})
      (switch
       (str "get-fruit")
       (list
        (clause (str "apple") (str "good choice!"))
        (clause (str "banana") (str "excellent choice!"))
        (clause (str "durian") (str "Hmm, I'm not sure your taxi driver is going to like that.")))
       (str "I don't recognize your so-called 'fruit'.")))

(test (parse-exp '{switch "get-fruit"
                          ["apple" => "good choice!"]
                          ["banana" => "excellent choice!"]
                          ["durian" => "Hmm, I'm not sure your taxi driver is going to like that."]
                          [else "I don't recognize your so-called 'fruit'."]})
      (switch
       (str "get-fruit")
       (list
        (clause (str "apple") (str "good choice!"))
        (clause (str "banana") (str "excellent choice!"))
        (clause (str "durian") (str "Hmm, I'm not sure your taxi driver is going to like that.")))
       (str "I don't recognize your so-called 'fruit'.")))

(test/exn (parse-exp '{switch "get-fruit"
                              ["apple" => "good choice!"]
                              ["banana" => "excellent choice!"]
                              [else "I don't recognize your so-called 'fruit'."]
                              ["durian" => "Hmm, I'm not sure your taxi driver is going to like that."]})
          "illegal expression")
(test/exn (parse-exp '{switch "get-fruit"
                              ["apple" ? "good choice!"]
                              [else "I don't recognize your so-called 'fruit'."]})
          "illegal expression")

(test/exn (parse-exp '{switch "get-fruit"
                              ["apple" => "good choice!"]})
          "illegal expression")


(test/exn (parse-exp '{switch "get-fruit"
                              ["apple" => "good choice!"]
                              ["banana" => "excellent choice!"]})
          "illegal expression")

;test fun
(test (parse-exp '(fun (cons a empty) cd)) (fun '(cons a empty) (varref 'cd)))
(test (parse-exp '(fun (a) cd)) (fun '(a) (varref 'cd)))
(test (parse-exp '(fun (x) x)) (fun '(x) (varref 'x)))


;test app
(test (parse-exp '(f g)) (app (varref 'f) (list (varref 'g))))
(test (parse-exp '((x y) 13)) (app (app (varref 'x) (list (varref 'y))) (list (num 13))))
(test/exn (parse-exp '(fun (with) x)) "illegal expression")
(test/exn (parse-exp '(fun (x <= y) 3)) "illegal expression")

(define t1(parse-exp '(with ((a = 4) (b = 5)) (with ((c = (* b 4)) (d = (+ 4 a))) (+ (- b c) (* a d))))))
(test (interp t1 (hash))(numV 17))


(define t2(parse-exp '(with ((a = "Hello") (b = "World")) (with ((c = (equal? b "World")) (d = (equal? a b))) (equal? c d)))))
(test (interp t2 (hash))(boolV #f))

(define t5(parse-exp '(with ((doTwice = (fun (f x) (f (f x)))) (sqr = (fun (x) (* x x)))) (doTwice sqr 4))))

(test (interp t5 (hash))(numV 256))

(define t6(parse-exp '(with ({x = {+ 4 5}})
                            {with ({y = {+ x x}})
                                  {with ({z = y})
                                        {with ({x = 4})
                                              z}}})))
 

(define t7 (parse-exp '(with ((fact-maker = (fun (self x) (switch x (0 => 1) (else (* x (self self (- x 1)))))))) (fact-maker fact-maker 5))))
(test (interp t7 (hash))(numV 120))

;(interp (parse-exp '(with ((cons = (fun (a b) (fun (f) (f a b false)))) (car = (fun (pr) (pr (fun (a b isMT) a)))) (cdr = (fun (pr) (pr (fun (a b isMT) b))))) cons)))

(define t8 (parse-exp '(with ((a = (fun (self x) (switch x (0 => 1) (else (* x (self self (- x 1)))))))) (a a 8))))
(test (interp t8 (hash)) (numV 40320))   
