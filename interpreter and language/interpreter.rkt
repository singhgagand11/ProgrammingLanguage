#lang plai
(require "monadDefinitions.rkt")
(require "ocfaeDefinitions.rkt")
(require "ocfaeParser.rkt")

; interp-unop: unary-operator? OCFAE-Value? -> OCFAE-Value?
; Performs the unary operation on the two OCFAE-Values.
(define (interp-unop op arg)
  (type-case OCFAE-Value arg
    [numV (n) (cond 
                [(or (equal? op not)) (error 'interp-unop "illegal expression ~e" arg)]
                [else (boolV (op n))])]
    [strV (s) (cond
                [(or (equal? op not)) (error 'interp-unop "illegal expression ~e" arg)]
                [else (boolV (op s))])]
    [boolV (b) (boolV (op b))]
    [boxV (loc) (error "internal error")]
    [objectV (fields)  (error 'interp-binop "illegal expression: unnop on objectV ~e" arg)]
    [closureV (params body env) (error 'interp-unop "Closure found ~e" arg)]))

; interp-binop: binop-operator? OCFAE-Value? OCFAE-Value? -> OCFAE-Value?
; Performs the binary operation on the two OCFAE-Values.
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
    [closureV (params body env) (error 'interp-binop "Closure found ~e" left)]
    [boxV (loc) (error 'interp-binop "internal error")])) 

;TODO:CHANGE CONTRACT
; interp-switch: OCFAE-Value? (listof SClauses?) OCFAE? immutable-hash-table? -> CF1WAE?
; returns the resulting expression if the patval is satisfied, otherwise the else expression.
(define (interp-switch val clauses elseval env)
  (if (empty? clauses) 
      (interp elseval env)
    (sdo
     (caluseVal <- (interp (clause-patval (first clauses)) env))
     (if (equal? val caluseVal)
         (interp (clause-result (first clauses)) env) 
         (interp-switch val (rest clauses) elseval env)))))


; env-lookup: symbol? immutable-hash-table? -> immutable-hash-table?
(define (env-lookup name table)
  (cond
    [(equal? (hash-ref table name #f) #f) (error 'env-lookup "free variable ~e" name)]
    [else (hash-ref table name)]))

(define (interp-Unbox op val)
  (sdo 
   (store <- getstore)
   (cond [(equal? op 'makebox)
          (sdo       
           (addr <- (lift (sto-next-loc store)))
           (newSto <- (lift (sto (hash-set (sto-memory store) addr val)
                            (+ 1 addr))))
           (setstore newSto)
           (lift (boxV addr)))]
         [(and (equal? op 'getbox) (boxV? val)) 
          (sdo
           (if (equal? (hash-ref (sto-memory store) (boxV-loc val) #f) #f) 
               (error "illegal expression: undefined location ~e" (sto-next-loc store))
               (lift (hash-ref (sto-memory store) (boxV-loc val)))))]
         [else (error "illegal expression: invalid box operation")])))

;  This procedure interprets the given OCFAE and produces a result
;  in the form of a function that accepts a Store and produces
;  a OCFAE-Value paired with a new store.
;  interp : OCFAE? hash? -> (OCFAE-Value? computation)
(define (interp exp env)
  (type-case OCFAE exp
    [num (n) (lift (numV n))]
    [str (s) (lift (strV s))]
    [bool (b) (lift (boolV b))]
    [unop (op arg) (sdo
                    (interpArgs <- (interp arg env))
                    (if (isUnBoxExp op)
                        (interp-Unbox op interpArgs)
                        (lift (interp-unop (table op) interpArgs))))]
    [binop (op l r) 
           (sdo 
            (lInterp <- (interp l env))
            (rInterp <- (interp r env))
            (lift (interp-binop op lInterp rInterp)))]
    [varref (name) (sdo
                     (return <- (lift (unbox (env-lookup name env)))) 
                     (if return (lift return) 
                         (error 'interp "empty box ~e" name)))] 
    [fun (params body) (lift (closureV params body env))]
    [switch (val clauses elseval)
            (sdo 
             (valInterp <- (interp val env))
              (lift (interp-switch valInterp clauses elseval env)))]
    [app (fun args)
         (sdo
           (pre-interp <- (interp fun env))
           (type-case OCFAE-Value pre-interp
             [closureV (c-params c-body c-env)
                       (sdo
                         (cond [(= (length c-params) (length args))
                                (sdo 
                                 (new-env <- (lift (map-params-args env c-env c-params args)))
                                (lift (interp c-body new-env)))]
                               [else (error "illegal expression : to few or to many args ( ~e ) supplied to function ( ~e ) " args fun)]))]
             [else (lift pre-interp)]))]
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
                            [else (error "illegal expression: illegal call to objref, doesn't contain object ~e" obj)]))]
    [seq (exps) "TODO:IMPLEMENT"]))

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


; map-params-args : immutable-hash-table? immutable-hash-table? (listof symbols?) (listof OCFAE?) -> immutable-hash-table?
; creates a new hash table of closure, mapping c-params to args closure with static-env and false cache box 
; by expanding existing hash table
(define (map-params-args static-env dynamic-env params args)
  (cond [(empty? params) dynamic-env]
        [else 
         (define newHash(hash-set dynamic-env (first params) (box (interp (first args) static-env))))
         (map-params-args static-env newHash (rest params) (rest args))]))


;test cases
;num
(test (run (interp (num 3) (hash)) (sto (hash) 1)) (a*s (numV 3) (sto '#hash() 1)))
;str
(test (run (interp (str "HELLO") (hash)) (sto (hash) 1)) (a*s (strV "HELLO") (sto '#hash() 1)))
;bool
(test (run (interp (bool #f) (hash)) (sto (hash) 1)) (a*s (boolV #f) (sto '#hash() 1)))
;unop's
  ;number?
(test (run (interp (parse-exp '(number? 3)) (hash)) (sto (hash) 1)) (a*s (boolV #t) (sto (hash) 1)))
(test (run (interp (parse-exp '(number? "ff")) (hash)) (sto (hash) 1)) (a*s (boolV #f) (sto (hash) 1)))
   ;string?
(test (run (interp (parse-exp '(string? 3)) (hash)) (sto (hash) 1)) (a*s (boolV #f) (sto (hash) 1)))
(test (run (interp (parse-exp '(string? "hello")) (hash)) (sto (hash) 1)) (a*s (boolV #t) (sto (hash) 1)))
  ;makebox
(define val0 (run (interp (parse-exp '(makebox 10)) (hash)) (sto (hash) 0)))
(define val1 (run (interp (parse-exp '(makebox "hello World")) (hash)) (a*s-store val0))) 
(define val2 (run (interp (parse-exp '(makebox "PIG")) (hash)) (a*s-store val1)))

(test val0 (a*s (boxV 0) (sto (hash 0 (numV 10)) 1)))
(test val1 (a*s (boxV 1) (sto (hash 0 (numV 10) 1 (strV "hello World")) 2)))
(test val2 (a*s (boxV 2) (sto (hash 0 (numV 10) 1 (strV "hello World") 2 (strV "PIG")) 3)))


  ;setbox
;(run (interp (parse-exp '(setbox! a "hello")) (hash 'a (boxV 0))) (sto (hash 0 (numV 3)) 1))

   ;not
(test (run (interp (parse-exp '(not true)) (hash)) (sto (hash) 0)) (a*s (boolV #f) (sto '#hash() 0)))
(test (run (interp (parse-exp '(not (<= 1 2))) (hash)) (sto (hash) 0)) (a*s (boolV #f) (sto (hash) 0)))
(test (run (interp (parse-exp '(not (<= 21 2))) (hash)) (sto (hash) 0)) (a*s (boolV #t) (sto (hash) 0)))
;binop's
(test (run (interp (parse-exp '(+ 1 2)) (hash)) (sto (hash) 0)) (a*s (numV 3) (sto (hash) 0)))
(test (run (interp (parse-exp '(- 1 2)) (hash)) (sto (hash) 0)) (a*s (numV -1) (sto (hash) 0)))
(test (run (interp (parse-exp '(* 1 2)) (hash)) (sto (hash) 0)) (a*s (numV 2) (sto (hash) 0)))
(test (run (interp (parse-exp '(/ 1 2)) (hash)) (sto (hash) 0)) (a*s (numV 1/2) (sto (hash) 0)))
(test/exn (run (interp (parse-exp '(/ 1 0)) (hash)) (sto (hash) 0)) "divide by zero")

(test (run (interp (parse-exp '(<= 1 2)) (hash)) (sto (hash) 0)) (a*s (boolV #t) (sto (hash) 0)))
(test (run (interp (parse-exp '(<= 1 1)) (hash)) (sto (hash) 0)) (a*s (boolV #t) (sto (hash) 0)))
(test (run (interp (parse-exp '(<= 2 1)) (hash)) (sto (hash) 0)) (a*s (boolV #f) (sto (hash) 0)))

(test (run (interp (parse-exp '(or (<= 1 2) (equal? 3 3))) (hash)) (sto (hash) 0)) (a*s (boolV #t) (sto (hash) 0)))
(test (run (interp (parse-exp '(or (<= 4 2) (equal? 3 3))) (hash)) (sto (hash) 0)) (a*s (boolV #t) (sto (hash) 0)))
(test (run (interp (parse-exp '(or (<= 23 2) (equal? "hello" 3))) (hash)) (sto (hash) 0)) (a*s (boolV #f) (sto (hash) 0)))

(test (run (interp (parse-exp '(and (number? 2) (string? "hello"))) (hash)) (sto (hash) 0)) (a*s (boolV #t) (sto (hash) 0)))
(test (run (interp (parse-exp '(and (number? 2) (number? "hello"))) (hash)) (sto (hash) 0)) (a*s (boolV #f) (sto (hash) 0)))
(test (run (interp (parse-exp '(and (string? "hello") (<= 12 1))) (hash)) (sto (hash) 0)) (a*s (boolV #f) (sto (hash) 0)))
(test (run (interp (parse-exp '(and (number? "hello") (<= 12 1))) (hash)) (sto (hash) 0)) (a*s (boolV #f) (sto (hash) 0)))

  ;setbox!

;varref
(test (run (interp (varref 'a) (hash 'a (box (num 5)))) (sto (hash) 0)) (a*s (num 5) (sto (hash) 0)))
(test/exn (run (interp (varref 'a) (hash)) (sto (hash) 0)) "free variable")


#;(run (interp (switch
               (str "banana")
               (list
                (clause (str "apple") (str "good choice!"))
                (clause (str "banana") (str "excellent choice!"))
                (clause (str "durian") (str "Hmm, I'm not sure your taxi driver is going to like that.")))
               (str "I don't recognize your so-called 'fruit'.")) (hash))
           (sto (hash) 0))

;switch
(test (run (interp (switch
               (str "banana")
               (list
                (clause (str "apple") (str "good choice!"))
                (clause (str "banana") (str "excellent choice!"))
                (clause (str "durian") (str "Hmm, I'm not sure your taxi driver is going to like that.")))
               (str "I don't recognize your so-called 'fruit'.")) (hash))
           (sto (hash) 0)) 
      (a*s (strV "excellent choice!") (sto (hash) 0)))

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