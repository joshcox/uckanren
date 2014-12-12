#lang racket
(require (only-in "../state/state.rkt" var? veqv? walk))
(provide ==)

(define (== u v)
  (lambda (s/c) ;; serial-lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (list (cons s (cdr s/c))) `()))))

(define (unify x v s)
  (let ((x (walk x s)) (v (walk v s)))
    (cond
      ((veqv? x v) s)
      ((var? x) (ext-s x v s))
      ((var? v) (ext-s v x s))
      ((and (pair? x) (pair? v))
       (let ((s (unify (car x) (car v) s)))
         (and s (unify (cdr x) (cdr v) s))))
      (else #f))))

(define (ext-s x v s) (if (occurs? x v s) #f (cons (cons x v) s)))

(define (occurs? x v s)
  (let ((v (walk v s)))
    (cond
     ((var? v) (veqv? v x))
     ((pair? v) (or (occurs? x (car v) s) (occurs? x (cdr v) s)))
     (else #f))))
