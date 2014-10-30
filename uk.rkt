#lang racket
(require C311/trace)
(provide (all-defined-out))

(trace-define (var n) n)
(trace-define (var? n) (number? n))

(trace-define (ext-s x v s) (cons (cons x v) s))

(trace-define (walk u s)
  (let ((pr (and (var? u) (assv u s))))
    (if pr (walk (cdr pr) s) u)))

(trace-define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
     ((eqv? u v) s)
     ((var? u) (ext-s u v s))
     ((var? v) (ext-s v u s))
     ((and (pair? u) (pair? v))
      (let ((s (unify (car u) (car v) s)))
        (and s (unify (cdr u) (cdr v) s))))
     (else #f))))

(trace-define (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (list (cons s (cdr s/c))) `()))))

(trace-define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) (cons (car s/c) (+ 1 c))))))

(trace-define (disj g1 g2) (lambda (s/c) ($-append (g1 s/c) (g2 s/c))))

(trace-define (conj g1 g2) (lambda (s/c) ($-append-map g2 (g1 s/c))))

(trace-define ($-append $1 $2)
  (cond
   ((procedure? $1) (lambda () ($-append $2 ($1))))
   ((null? $1) $2)
   (else (cons (car $1) ($-append (cdr $1) $2)))))

(trace-define ($-append-map g $)
  (cond
   ((procedure? $) (lambda () ($-append-map g ($))))
   ((null? $) `())
   (else ($-append (g (car $)) ($-append-map g (cdr $))))))

(trace-define (call/empty-state g) (g (empty-state)))
(trace-define (empty-state) `(() . 0))
