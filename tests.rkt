#lang racket/base
(require rackunit rackunit/text-ui "mK.rkt" "test-programs.rkt")
(provide microKanren-tests run-tests)

;; tests

(define microKanren-tests
  (test-suite "microKanren-tests"

    (check-equal?                
      (let (($ ((call/fresh (lambda (q) (== q 5))) empty-state)))
        (car $))
      '(((#(0) . 5)) . 1) 
      "second-set t1")

    (check-equal?                
      (let (($ ((call/fresh (lambda (q) (== q 5))) empty-state)))
        (cdr $))
      '() 
      "second-set t2")

    (check-equal?                
      (let (($ (a-and-b empty-state))) (car $))
      '(((#(1) . 5) (#(0) . 7)) . 2) 
      "second-set t3")

    (check-equal?                
      (let (($ (a-and-b empty-state))) (take 1 $))
      '((((#(1) . 5) (#(0) . 7)) . 2)) 
      "second-set t3, take")

    (check-equal?               
      (let (($ (a-and-b empty-state)))
        (car (cdr $)))
      '(((#(1) . 6) (#(0) . 7)) . 2) 
      "second-set t4")

    (check-equal?                
      (let (($ (a-and-b empty-state)))
        (cdr (cdr $)))
      '() 
      "second-set t5")

    (check-equal?                
      (let (($ ((call/fresh (lambda (q) (fives q))) empty-state)))
        (take 1 $))
      '((((#(0) . 5)) . 1)) 
      "who cares")

    (check-equal?                
      (let (($ (a-and-b empty-state))) (take 2 $))
      '((((#(1) . 5) (#(0) . 7)) . 2)
        (((#(1) . 6) (#(0) . 7)) . 2)) 
      "take 2 a-and-b stream")

    (check-equal?                
      (let (($ (a-and-b empty-state))) (take-all $))
      '((((#(1) . 5) (#(0) . 7)) . 2)
        (((#(1) . 6) (#(0) . 7)) . 2)) 
      "take-all a-and-b stream")

#|
    (check-equal?                
      (car ((ground-appendo empty-state)))
      '(((#(2) b) (#(1)) (#(0) . a)) . 3) 
      "ground appendo")

    (check-equal?                
      (car ((ground-appendo2 empty-state)))
      '(((#(2) b) (#(1)) (#(0) . a)) . 3) 
      "ground appendo2")

    (check-equal?                
      (take 2 (call-appendo empty-state))
      '((((#(0) #(1) #(2) #(3)) 
          (#(2) . #(3)) (#(1))) . 4)
        (((#(0) #(1) #(2) #(3)) 
          (#(2) . #(6)) 
          (#(5)) 
          (#(3) #(4) . #(6)) 
          (#(1) #(4) . #(5))) . 7)) 
      "appendo")

    (check-equal?                
      (take 2 (call-appendo2 empty-state))
      '((((#(0) #(1) #(2) #(3)) (#(2) . #(3)) (#(1))) . 4) 
        (((#(0) #(1) #(2) #(3)) 
          (#(3) #(4) . #(6))
          (#(2) . #(6)) 
          (#(5)) 
          (#(1) #(4) . #(5))) . 7)) 
      "appendo2")

    (check-equal?                
      (map reify-1st (take 2 (call-appendo empty-state)))
      '((() _.0 _.0) ((_.0) _.1 (_.0 . _.1))) 
      "reify-1st across appendo")

    (check-equal? 
      (map reify-1st (take 2 (call-appendo2 empty-state)))
      '((() _.0 _.0) ((_.0) _.1 (_.0 . _.1))) 
      "reify-1st across appendo2")

    (check-equal? 
      (take 1 (many-non-ans empty-state))
      '((((#(0) . 3)) . 1)) 
      "many non-ans")
|#

              
              ))

(run-tests microKanren-tests)
