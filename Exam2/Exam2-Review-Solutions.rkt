#lang racket

(require "../chez-init.rkt")

(provide slay-slime mine-for-diamonds and-eval grade-tests eval-one-exp)
;-----------------------------------------------
; Follow The Grammer
;-----------------------------------------------

; Problem 1: Slaying Slimes
;
; <Slime-Army> ::= ({Slime}*)
; <Slime>      ::= Mega_Slime | Big_Slime | Medium_Slime | Small_Slime
;
; In this system, slimes are repeatedly transformed using
; these rules:
;
; Mega_Slime   -> Big_Slime Big_Slime
; Big_Slime    -> Medium_Slime Medium_Slime Medium_Slime
; Medium_Slime -> Small_Slime Small_Slime Small_Slime Small_Slime
; Small_Slime  -> Slime_Ball

; Make a procedure called slay-slime that covers two run through the slaying slimes
; syntax. If you encounter a Slime_Ball nothing should happen to it.
; Ex. (slay-slime '(Medium_Slime Small_Slime)) -> '(Slime_Ball Slime_Ball Slime_Ball Slime_Ball Slime_Ball)

(define slay-slime
  (lambda (slime-army)
    (slay-slime-helper (slay-slime-helper slime-army))))

(define slay-slime-helper
  (lambda (slime-army)
    (cond [(null? slime-army) '()]
          [(eqv? (car slime-army) 'Mega_Slime) (cons 'Big_Slime (cons 'Big_Slime (slay-slime-helper (cdr slime-army))))]
          [(eqv? (car slime-army) 'Big_Slime)  (cons 'Medium_Slime (cons 'Medium_Slime (cons 'Medium_Slime (slay-slime-helper (cdr slime-army)))))]
          [(eqv? (car slime-army) 'Medium_Slime) (cons 'Small_Slime (cons 'Small_Slime (cons 'Small_Slime (cons 'Small_Slime (slay-slime-helper (cdr slime-army))))))]
          [(eqv? (car slime-army) 'Small_Slime) (cons 'Slime_Ball (slay-slime-helper (cdr slime-army)))]
          [else (cons 'Slime_Ball (slay-slime-helper (cdr slime-army)))])))

; Problem 2: Mining for Diamonds
;
; <Mine>  ::= ({<Block>}*)
; <Block> ::= Diamond_Ore | Iron_Ore | Lapis_Ore | Redstone_Ore | Lava
;
; In this system, mined ores are transformed using
; these rules:
;
; Diamond_Ore  -> Diamond
; Iron_Ore     -> (Raw_Iron Raw_Iron)
; Lapis_Ore    -> Lapis Lapis Lapis
; Redstone_Ore -> RED (stone) RED
;
; !! If there is lava you turn back with what you have. !!
; (A.K.A. Stop recursing)

; Make a procedure call mine-for-diamonds that covers one run through the mine
; Ex. (mine-for-diamonds '(Diamond_Ore)) -> '(Diamond)

(define mine-for-diamonds
  (lambda (mine)
    (cond [(null? mine) '()]
          [(eqv? (car mine) 'Lava) '()]
          [(eqv? (car mine) 'Diamond_Ore) (cons 'Diamond (mine-for-diamonds (cdr mine)))]
          [(eqv? (car mine) 'Iron_Ore) (cons '(Raw_Iron Raw_Iron) (mine-for-diamonds (cdr mine)))]
          [(eqv? (car mine) 'Lapis_Ore) (cons 'Lapis (cons 'Lapis (cons 'Lapis (mine-for-diamonds (cdr mine)))))]
          [(eqv? (car mine) 'Redstone_Ore) (cons 'RED (cons '(stone) (cons 'RED (mine-for-diamonds (cdr mine)))))])))



;-----------------------------------------------
; Macros
;-----------------------------------------------

; Problem 1: and-eval

; Define a new syntax and-eval. and-eval takes
; a list of expressions and evaluates them one
; after another until one expression evaluates
; to false. In that case, it returns the list
; of true values so far, and does not evaluate
; anything else in the list. If it never encounters
; a false value, it returns the whole list.
; For example:
; (and-eval #t 'a 4 #f 19 2) ; returns '(#t a 4)
; (and-eval (display "this displays") 'a ((lambda (x) (* x x)) 3) (even? 19) (display "this doesn't") 15) ; returns '(#<void> a 9)
;
; A few requirements:
; A:
; and-eval must use short circuiting. That is, if there is a false value,
; it doesn't evaluate anything beyond it. For example:
; (and-eval #f (display "hello")) ; does not display hello, returns '()
; (and-eval (display "hello") #f) ; displays hello, returns '(#<void>)

; B:
; Any of the expressions in and-eval can be complex and mutate each other
; (let
;   ([x (make-vector 1)])
;     (vector-set! x 0 3)
;     (and-eval
;       (vector-ref x 0)
;       (begin (vector-set! x 0 14) 'set)
;       (vector-ref x 0)
;       (< (vector-ref x 0) 7)
;       27)) ; returns '(3 set 14)

(define-syntax and-eval
  (syntax-rules ()
    [(_ b1 b2 ...) ; If there's one or more bodies
     (let ([b1-result b1]) ; evaluate the first
       (if b1-result ; if the result is true
           (cons b1-result (and-eval b2 ...)) ; then cons it to the recursive result (notice I haven't evaluated b2 yet, just "copy-pasted" it)
           '()))] ; else just return empty list, throwing away the rest of the bodies
    [(_) ; if there aren't any bodies
     '()])) ; then just return empty list

; Problem 2: grade-tests
; You will write a portion of the grading code that has been used in this
; class to grade the assignments and exams. The syntax for this problem is
;
; (grade-tests testcase+)
; where
; testcase :: (e1 e2 pts) | (e1 e2 eq-op? pts).
;
; A testcase (e1 e2 pts) passes when e1 is equal? to e2.
; A testcase (e1 e2 eq-op? pts) when e1 is eq-op? to e2.
;
; You should sum up all the points for the testcases that
; passed and return the score of the assignment between
; 0 and 1; divide the earned points by the total points.
; For example:
;
; (grade-tests
;    ('foo 'foo 3) ; passes
;    ('bar 'baz 2) ; fails
;    (4 6 (lambda (x y) (eq? (modulo x 2) (modulo y 2))) 4)) ; passes
;
; returns 7/9. Note that 7/9 is a number and not a string, it's just
; Racket's fun way of formatting rationals.

(define-syntax grade-tests
  (syntax-rules ()
    [(grade-tests testcases ...)
      (apply / (grade-helper testcases ...))]))

(define-syntax grade-helper
  (syntax-rules ()
    [(grade-helper) '(0 0)] ; no testcases? Earned 0 of 0 points
    [(grade-helper tcar tcdr ...) ; recursive step
        (map + (eval-testcase tcar) (grade-helper tcdr ...))]))

(define-syntax eval-testcase
  (syntax-rules ()
    [(eval-testcase (e1 e2 pts)) (eval-testcase (e1 e2 equal? pts))] ; less thinking
    [(eval-testcase (e1 e2 e? pts)) (list (if [e? e1 e2] pts 0) pts)]))




;-----------------------------------------------
; Syntax Expand
;-----------------------------------------------

; Problem 1: mapcond

; This is a problem that requires you to modify your interpreter.
;
; cut and paste your interpreter code at the bottom of this file
; Use syntax-expand to implement a command called mapcond that acts
; a little like cond, but a bit more convenient to use
;
; (mapcond negative? (1 'pos) (3 'alsopos) (-2 'neg) (-4 'toolate)) ; returns 'neg
;
; The first parameter of mapcond is a predicate "pred?" that takes one variable
; The remaining parameters are cases. Each case has two elements: a condition "con"
; and a body "bod". mapcond should find the first case where (pred? con) is true,
; and then return the result of executing that case's body. If no (pred? con) is
; true, then mapcond should return #void (like how cond does when none are true)
;
; mapcond should not evaluate (pred? con) more than once for each case, it should
; only evaluate one body at most, and it should short-circuit after finding
; a (pred? con) that is true.
; For example:
;
; (mapcond (lambda (x) (display "pred evaled") (negative? x)) (1 'pos) (3 'alsopos) (-2 'neg) (-4 'toolate)) ; returns 'neg, prints "pred evaled" 3 times
;
; (mapcond negative? (2 (display "not neg!")) (4 (display "also not neg!")) (-1 'neg) (-3 (display "too late!"))) ; returns 'neg, doesn't print anything
;
; You are required to implement this problem using syntax-expand (and parse-exp).
; You should not need to make any changes to eval-exp to get this feature to work.

; I've included this little function to make the test cases run initially.
; delete it once your paste in your interpreter.

(define eval-one-exp
  (lambda (x) 'nyi))

; -------------------------------------------------------------------------
; Solution Changes

;in expression datatype
;[mapcond-exp
;   (pred expression?)
;   (conditions (lambda (conds) 
;                 (andmap (lambda (cnd) ; every condition must ...
;                           (and
;                           (expression? (car cnd)) ; be a list of size 2 (or more, but we don't care about more)
;                           (expression? (cadr cnd)))) conds)))] ; where both elements are expressions
;
;in parse-exp	
;[(eqv? (car datum) 'mapcond) ; if you see a mapcond
;	(mapcond-exp ; make a mapcond
;		(parse-exp (cadr datum)) ; the predicate is the first thing inside of mapcond
;		(map (lambda (cnd) (map parse-exp cnd)) (cddr datum)))] ; and the remaining elements are all conds. Parse the elements of each cond.
;
;in syntax-expand
;[mapcond-exp (pred conds)
;		   (syntax-exp (cond-exp (map (lambda (cnd) (app-exp pred (list (car cnd)))) conds) (map (lambda (cnd) (list (cadr cnd))) conds) (list (lit-exp (void)))))] ; make a cond-exp out of the conds.
;                                                                       ; Note that I'm making an (app-exp pred ...) for each condition (this is why I called it mapcond)
;                   ; NOTE: the intepreter I was using had a fun quirk where I had to specify that (void) was outputted when no conditions were true. Your cond-exp will look different.



;-----------------------------------------------
; General Interpreter
;-----------------------------------------------

; Problem 1: multivar
;;
;; Usually when we have a variable, we look for its binding in the environment.
;; But what if I wanted to look for multiple variables, and return the first one I found?
;; For example:
;; (let ([x 'coolval]) (let ([y 'coolerval]) (multivar x y))) ; returns 'coolerval
;;
;; I want you to write a new expression type multivar that takes a list of variables.
;; When you evaluate multivar, find the most recent binding of ANY variable in that
;; list and return it. You don't have to worry about a "tie" in binding depth such
;; as (let ([x 'coolval] [y 'coolerval']) (multivar x y)); this can return anything and won't be tested.
;;
;; multivar should work even if some variables aren't defined yet:
;; (let ([y 'coolval]) (multivar x y)) ; returns 'coolval without complaint
;;
;; multivar should work with global bindings and can be used in complex expressions:
;; ((multivar y banana futon +) 2 3) ; returns 5
;;
;; Note that multivar does not quote the variables inside of it -- they are directly in there, not symbols

; -------------------------------------------------------------------------
; Solution Changes

;in expression datatype
;[multivar-exp
;	(vars (list-of symbol?))] ; I didn't bother parsing the symbols into var-exps
;
;in parse-exp
;[(eqv? (car datum) 'multivar)
;	(multivar-exp (cdr datum))] ; My datatype definition being lazy made parsing easy
;
;in eval-exp
;[multivar-exp (vars)
;	(unbox (apply-env-mult env vars))] ; I'm unboxing because my environments use boxes for mutable values
;
;(define apply-env-mult ; My helper for looking up multiple variables
;  (lambda (env vars) ; Environment and list of acceptable variables
;    (cases environment env
;      [empty-env-record () ; If I hit empty env...
;                        (apply-env-mult-global vars)] ; then try the global environment
;      [extended-env-record (syms vals env)
;                           (let ([good-pos (filter (lambda (index) (member (list-ref syms index) vars)) (range (length vals)))]) ; Find the matches if any
;                             (if [null? good-pos] ; if there aren't any matches...
;                                 (apply-env-mult env vars) ; then look in the next environment
;                                 (list-ref vals (car good-pos))))]))) ; else we found something so return one of them (I chose to do first, the problem description lets me do any)
;
;(define apply-env-mult-global ; The same helper but the special case of the global environment
;  (lambda (vars) ; I don't need to pass in the global environment because there is none
;    (cases environment global-env
;      [empty-env-record () ; I should never hit empty env because global-env isn't empty
;                        (error 'global-env "This should never happen")]
;      [extended-env-record (syms vals env)
;                           (let ([good-pos (filter (lambda (index) (member (list-ref syms index) vars)) (range (length vals)))]) ; Same code, find matches
;                             (if [null? good-pos] ; if there aren't any matches...
;                                 (error 'global-env ; then we're out of places to look. Throw an error
;                                        "Symbols ~s are not bound in global env"
;                                        vars)
;                                 (list-ref vals (car good-pos))))]))) ; else we found something so return one of them









