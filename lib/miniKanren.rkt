#lang racket
(require (only-in "microKanren-base.rkt" call/fresh disj conj empty-state)
         (only-in "unification.rkt" ==)
         (only-in "state.rkt" reify-var0))
(provide run call/fresh disj conj ==)

(define (call/empty-state g) (g (empty-state)))

(define (pull $) (if (procedure? $) (pull ($)) $))

(define (take n)
  (lambda ($)
    (cond
      ((zero? n) '())
      (else
       (let (($ (pull $)))
         (cond
           ((null? $) '())
           (else
            (cons (car $)
             ((take (- n 1)) (cdr $))))))))))

(define-syntax run
  (syntax-rules ()
    ((_ n (q) g)
     (map reify-var0 ((take n) (call/empty-state (call/fresh (lambda (q) g))))))))
