#lang racket
(require (only-in "../state/state.rkt" var empty-state reify-var0)
         (only-in "../unification/unification.rkt" ==))
(provide $-append $-append-map disj conj call/fresh empty-state == run inverse-eta-delay reify-var0 take call/empty-state)

(define ($-append $1 $2)
  (cond
    ((procedure? $1) (lambda () ($-append $2 ($1)))) ;;serial-lambda 
    ((null? $1) $2)
    (else (cons (car $1) ($-append (cdr $1) $2)))))

(define ($-append-map g $)
  (cond
    ((procedure? $) (lambda () ($-append-map g ($)))) ;;serial-lambda
    ((null? $) `())
    (else ($-append (g (car $)) ($-append-map g (cdr $))))))

(define (disj g1 g2) 
  (lambda (s/c) ;; serial-lambda (s/c) 
    ($-append (g1 s/c) (g2 s/c))))

(define (conj g1 g2) 
  (lambda (s/c) ;; serial-lambda (s/c) 
    ($-append-map g2 (g1 s/c))))

(define (call/fresh f)
  (lambda (s/c) ;; serial-lambda (s/c)
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
    ((_ g) (lambda (s/c) (lambda () (g s/c))))))
