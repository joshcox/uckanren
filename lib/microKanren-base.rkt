#lang racket
(require (only-in "state.rkt" var empty-state))
(provide $-append $-append-map disj conj call/fresh empty-state)

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


;;abstract out serial-lambda and get rid of it - but also allow it. lambdag macro
(define (call/fresh f)
  (lambda (s/c) ;; serial-lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) (cons (car s/c) (+ 1 c))))))
