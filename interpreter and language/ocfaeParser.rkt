#lang plai
(print-only-errors #t)
(require "ocfaeDefinitions.rkt")

; parse-exp : s-exp? -> OCFAE?
; This procedure parses an s-expression into a OCFAE
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
            (app (parse-exp firstItem) (parse-exps (rest sexp)))]
           [(and (= (length sexp) 2) (isUnop firstItem))
            ;if there is unop
            (unop firstItem (parse-exp (second sexp)))]
           ;if it is a seq
           [(symbol=? firstItem 'seq) 
            (cond [ (> (length sexp) 1) (seq (parse-exps (rest sexp)))]
                  [else (error 'parse-exp "illegal expression: invalid length of seq ~e" sexp)])]
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

;parse-new: s-exp -> (listof (list/c symbol? OCFAE?))
;parse's list of CFAE to produce list of, list of symbol and OCFAE.
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


;parse-exps : (listof args) -> (listof OCFAE)
;parae's list of OCFAE from sexp to produce and list of C1F1WAE
(define (parse-exps exp)
  (cond [(empty? exp) empty]
        [else (cons (parse-exp (first exp)) (parse-exps (rest exp)))]))

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
        [(or (not (symbol? (first lstSym))) (member (first lstSym) seenLst) (isKeyword (first lstSym))) false]
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

;test cases
;illegal expressions
(test/exn (parse-exp '(with ((x = 10) ( x = x)) x)) "illegal expression")
(test/exn (parse-exp '(fun (list 'x 'x ) (+ x x))) "illegal expression")
(test/exn (parse-exp '(fun (weight (zebra name weight)) (equal? weight weight))) "illegal expression")


;test makebox
(test (parse-exp '(makebox 1000)) (unop 'makebox (num 1000)))
(test (parse-exp '(makebox h)) (unop 'makebox (varref 'h)))
(test (parse-exp '(makebox "hello")) (unop 'makebox (str "hello")))

(test/exn (parse-exp '(makebox )) "illegal expression")
(test/exn (parse-exp '(makebox q 2)) "illegal expression")


;test getbox
(test (parse-exp '(getbox a)) (unop 'getbox (varref 'a)))
(test (parse-exp '(getbox 2)) (unop 'getbox (num 2)))
(test (parse-exp '(getbox (makebox 4))) (unop 'getbox (unop 'makebox (num 4))))

(test/exn (parse-exp '(getbox)) "illegal expression")
(test/exn (parse-exp '(getbox q 2)) "illegal expression")




;test setbox!
(test (parse-exp '(setbox! a 30)) (binop 'setbox! (varref 'a) (num 30)))
(test/exn (parse-exp '(setbox!)) "illegal expression")
(test/exn (parse-exp '(setbox! q)) "illegal expression")
(test/exn (parse-exp '(setbox! q 2 2)) "illegal expression")

;test seq
;one seq
(test (parse-exp '(seq (makebox false))) (seq (list (unop 'makebox (bool #f)))))
;list of two in seq
(test (parse-exp '(seq (makebox false) (makebox 1))) 
                 (seq (list (unop 'makebox (bool #f)) (unop 'makebox (num 1)))))
;grouping of seq
(test (parse-exp '(seq (makebox false) (seq (makebox 1) (seq 1)))) 
                 (seq (list (unop 'makebox (bool #f)) 
                            (seq (list (unop 'makebox (num 1))
                                 (seq (list (num 1))))))))

(test/exn (parse-exp '(seq )) "illegal expression")

(test (parse-exp '(with ((b = (makebox false))) 
                        (seq (setbox! b (+ b 1))
                             (setbox! b 100))))
      (app (fun '(b) (seq (list (binop 'setbox! (varref 'b) (binop '+ (varref 'b) (num 1)))
                                (binop 'setbox! (varref 'b) (num 100)))))
           (list (unop 'makebox (bool #f)))))


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

(test (parse-exp 'x) (varref 'x))
(test (parse-exp '(+ 3 4)) (binop '+ (num 3) (num 4)))
(test (parse-exp '(/ 3 4)) (binop '/ (num 3) (num 4)))
(test (parse-exp '(* 3 4)) (binop '* (num 3) (num 4)))
(test (parse-exp '(- 3 4)) (binop '- (num 3) (num 4)))
(test ( parse-exp '(/ 3 0)) (binop '/ (num 3) (num 0)))
(test ( parse-exp '0) (num 0))

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
