#lang racket
(require (only-in "../state/p-state.rkt" var empty-state reify-var0)
         (only-in "../unification/p-unification.rkt" ==)
         (only-in web-server/lang/serial-lambda serial-lambda))
(provide $-append $-append-map
         disj conj call/fresh  == run 
         inverse-eta-delay reify-var0 take call/empty-state empty-state)

(define ($-append $1 $2)
  (cond
    ((procedure? $1) (serial-lambda () ($-append $2 ($1)))) 
    ((null? $1) $2)
    (else (cons (car $1) ($-append (cdr $1) $2)))))

(define ($-append-map g $)
  (cond
    ((procedure? $) (serial-lambda () ($-append-map g ($))))
    ((null? $) `())
    (else ($-append (g (car $)) ($-append-map g (cdr $))))))

(define (disj g1 g2) 
  (serial-lambda (s/c)
    ($-append (g1 s/c) (g2 s/c))))

(define (conj g1 g2) 
  (serial-lambda (s/c)
    ($-append-map g2 (g1 s/c))))

(define (call/fresh f)
  (serial-lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) (cons (car s/c) (+ 1 c))))))

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

(define-syntax inverse-eta-delay
  (syntax-rules ()
    ((_ g) (serial-lambda (s/c) (serial-lambda () (g s/c))))))
