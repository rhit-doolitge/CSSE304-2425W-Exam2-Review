#lang racket

; To use these tests:
; Click "Run" in the upper right
; (r)

; If you find errors in your code, fix them, save your file, click the "Run" button again, and type (r)
; You can run a specific group of tests using (run-tests group-name)

(require "../testcode-base.rkt")
(require "Exam2-Review-Problems.rkt")

(provide get-weights get-names individual-test test)

(define same-parity?
  (lambda (x y)
    (eq? (modulo x 2) (modulo y 2))))

(define test (make-test ; (r)

   (slay-slime equal?
      [(slay-slime '(Small_Slime)) '(Slime_Ball) 1] ; Single small slime
      [(slay-slime '(Medium_Slime)) '(Slime_Ball Slime_Ball Slime_Ball Slime_Ball) 1] ; Single medium slime
      [(slay-slime '(Big_Slime)) '(Small_Slime Small_Slime Small_Slime Small_Slime
                                               Small_Slime Small_Slime Small_Slime
                                               Small_Slime Small_Slime Small_Slime
                                               Small_Slime Small_Slime) 1] ; Single big slime 
      [(slay-slime '(Mega_Slime)) '(Medium_Slime Medium_Slime Medium_Slime Medium_Slime Medium_Slime Medium_Slime) 1] ; Single mega slime
      
      ;; Mixed input transformations
      [(slay-slime '(Small_Slime Medium_Slime)) '(Slime_Ball Slime_Ball Slime_Ball Slime_Ball Slime_Ball) 1] ; Mixed small and medium
      [(slay-slime '(Big_Slime Mega_Slime)) '(Small_Slime Small_Slime Small_Slime Small_Slime Small_Slime
                                                          Small_Slime Small_Slime Small_Slime Small_Slime
                                                          Small_Slime Small_Slime Small_Slime Medium_Slime
                                                          Medium_Slime Medium_Slime Medium_Slime Medium_Slime Medium_Slime) 1] ; Big and mega

      ;; Long test case
      [(slay-slime '(Mega_Slime Big_Slime Medium_Slime Small_Slime)) 
       '(Medium_Slime Medium_Slime Medium_Slime Medium_Slime Medium_Slime Medium_Slime Small_Slime Small_Slime Small_Slime Small_Slime
                                               Small_Slime Small_Slime Small_Slime
                                               Small_Slime Small_Slime Small_Slime
                                               Small_Slime Small_Slime 
          Slime_Ball Slime_Ball Slime_Ball Slime_Ball Slime_Ball) 1] ; Complex scenario
    )

   (mine-for-diamonds equal?
              ;; Basic transformations
              [(mine-for-diamonds '(Diamond_Ore)) '(Diamond) 1] ; Single diamond ore
              [(mine-for-diamonds '(Iron_Ore)) '((Raw_Iron Raw_Iron)) 1] ; Single iron ore
              [(mine-for-diamonds '(Lapis_Ore)) '(Lapis Lapis Lapis) 1] ; Single lapis ore
              [(mine-for-diamonds '(Redstone_Ore)) '(RED (stone) RED) 1] ; Single redstone ore

              ;; Lava scenarios
              [(mine-for-diamonds '(Diamond_Ore Lava)) '(Diamond) 1] ; Stop after diamond ore
              [(mine-for-diamonds '(Lava)) '() 1] ; Start with lava

              ;; Mixed scenarios
              [(mine-for-diamonds '(Diamond_Ore Iron_Ore)) '(Diamond (Raw_Iron Raw_Iron)) 1] ; Diamond followed by iron
              [(mine-for-diamonds '(Iron_Ore Lapis_Ore Redstone_Ore)) 
               '((Raw_Iron Raw_Iron) Lapis Lapis Lapis RED (stone) RED) 1] ; All ores without lava
              [(mine-for-diamonds '(Diamond_Ore Lava Iron_Ore)) '(Diamond) 1] ; Lava stops mining

              ;; Long test case
              [(mine-for-diamonds '(Diamond_Ore Iron_Ore Lapis_Ore Redstone_Ore Iron_Ore Diamond_Ore Lava Iron_Ore)) 
               '(Diamond (Raw_Iron Raw_Iron) Lapis Lapis Lapis RED (stone) RED (Raw_Iron Raw_Iron) Diamond) 1] ; Complex scenario
              )

  (and-eval equal? ; (run-test and-eval)
    [(and-eval #t 'a 4 #f 19 2) '(#t a 4) 1] ; (run-test and-eval 1)
    [(let ([x (make-vector 1)])              ; x = [0]
       (vector-set! x 0 3)                   ; x = [3]
       (and-eval
        (vector-ref x 0)                     ; returns 3
        (begin (vector-set! x 0 14) 'set)    ; x = [14] returns 'set
        (vector-ref x 0)                     ; returns 14
        (< (vector-ref x 0) 7)               ; 14 not < 7 so false, stops execution, returns '(3 set 14)
        27)) '(3 set 14) 1] ; (run-test and-eval 2)
    [(and-eval) '() 1] ; (run-test and-eval 3)
    [(and-eval 1 2 3 4 5) '(1 2 3 4 5) 1] ; (run-test and-eval 4)
    [(and-eval #f) '() 1] ; (run-test and-eval 5)
	[(let ([x (make-vector 1)])
	    (and-eval
		  #t
		  (vector-set! x 0 (add1 (vector-ref x 0))))
		(vector-ref x 0)) 1 1]; (run-test and-eval 6)
	[(let ([x (make-vector 1)])
	    (and-eval
		  #f
		  (vector-set! x 0 (add1 (vector-ref x 0))))
		(vector-ref x 0)) 0 1]; (run-test and-eval 7)
  )

   (grade-tests equal? ; (run-test grade-tests)
      [(grade-tests ('foo 'foo 1)) 1 1] ; (run-test grade-tests 1)
      [(grade-tests ('foo 'bar 1)) 0 1] ; (run-test grade-tests 2)
      [(grade-tests ('foo 'foo 1) ('foo 'bar 1)) 1/2 1] ; (run-test grade-tests 3)
      [(grade-tests ('foo 'foo 1) ('foo 'bar 2)) 1/3 1] ; (run-test grade-tests 4)
      [(grade-tests ('foo 'foo 2) ('foo 'bar 1)) 2/3 1] ; (run-test grade-tests 5)
      [(let ([x 0]) (grade-tests ((set! x (add1 x)) (set! x (add1 x)) 1)) x) 2 1] ; (run-test grade-tests 6)
      [(grade-tests (1 3 same-parity? 1)) 1 1] ; (run-test grade-tests 7)
      [(grade-tests (1 1 same-parity? 1)) 1 1] ; (run-test grade-tests 8)
      [(grade-tests (1 4 same-parity? 1)) 0 1] ; (run-test grade-tests 9)
      [(grade-tests (1 1 1) (1 0 1) (1 1 1) (1 0 1) (1 1 1)) 3/5 1] ; (run-test grade-tests 10)
      [(grade-tests ('foo 'foo 3) ('bar 'baz 2) (4 6 same-parity? 4)) 7/9 1] ; (run-test grade-tests 11)
   )

  (mapcond equal? ; (run-test mapcond)
    [(eval-one-exp '(mapcond negative? (1 'pos) (3 'alsopos) (-2 'neg) (-4 'toolate))) 'neg 1] ; (run-test mapcond 1)
    [(eval-one-exp '(let ([x (make-vector 1)])
       (mapcond negative? (1 (vector-set! x 0 'bad)) (3 (vector-set! x 0 'alsobad)) (-2 (vector-set! x 0 'good)) (-4 (vector-set! x 0 'toolate)))
       (vector-ref x 0))) 'good 1] ; (run-test mapcond 2)
    [(eval-one-exp '(let ([x (make-vector 1)])
       (mapcond (lambda (n) (vector-set! x 0 (add1 (vector-ref x 0))) (negative? n)) (1 'bad) (3 'alsobad) (-2 'good) (-4 'toolate)) ; should evaluate the pred 3 times only
       (vector-ref x 0))) 3 1] ; (run-test mapcond 3)
    [(eval-one-exp '(mapcond positive? (-2 'nope) (-3 'stillnope) (-4 'noneOfTheseArePositive))) (void) 1] ; (run-test mapcond 4)
    [(eval-one-exp '(mapcond undefined-procedure)) (void) 1] ; (run-test mapcond 5)
    [(eval-one-exp '(mapcond negative? (-3 'good) (-4 undefined-variable))) 'good 1] ; (run-test mapcond 6)
  )

  (multivar equal? ; (run-test multivar)
    [(eval-one-exp '(let ([x 'coolval]) (let ([y 'coolerval]) (multivar x y)))) 'coolerval 1] ; (run-test multivar 1)
    [(eval-one-exp '(let ([x 'coolval]) (let ([y 'coolerval]) (multivar y x)))) 'coolerval 1] ; (run-test multivar 2)
    [(eval-one-exp '(let ([y 'coolval]) (multivar x y))) 'coolval 1] ; (run-test multivar 3)
    [(eval-one-exp '(let ([y 'coolval]) (multivar y x))) 'coolval 1] ; (run-test multivar 4)
    [(eval-one-exp '((multivar +) 2 3)) 5 1] ; (run-test multivar 5)
    [(eval-one-exp '((multivar y banana futon +) 2 3)) 5 1] ; (run-test multivar 6)
  )
))

(implicit-run test) ; run tests as soon as this file is loaded