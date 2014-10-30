#lang racket
(require C311/trace)
(provide (all-defined-out))

(define (var v) (box v))
(define (unvar v) (unbox v))
(define (var? v) (box? v))
(define (var=? v1 v2) (and (var? v1) (var? v2) (eqv? (unvar v1) (unvar v2))))

;;not nearly what we want or need
;;(define (ext-s x v s) (cons (cons x v) s))

(define (walk u s)
  (let ((pr (and (var? u) (assoc u s var=?))))
    (if pr (walk (cdr pr) s) u)))

(define ext-s
  (lambda (x v s)
    (cond
     ((var? v) (cons (cons x v) s)) ; update var x with var v (cons (cons x v) s)
     ((occurs? x v s) #f) ; fail out if x occurs in v (only matters if v is list)
     (else (cons (cons x v) s))
     )))

(define occurs?
  (lambda (x v s)
    (cond
     ((var? v) (var=? x v))
     ((pair? v) (or (occurs? x (car v) s) (occurs? x (cdr v) s)))
     (else #f))))

(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
     ((or (eqv? u v) (var=? u v)) s)
     ((var? u) (ext-s u v s))
     ((var? v) (ext-s v u s))
     ((and (pair? u) (pair? v))
      (let ((s (unify (car u) (car v) s)))
        (and s (unify (cdr u) (cdr v) s))))
     (else #f))))

(define (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (list (cons s (cdr s/c))) `()))))

(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) (cons (car s/c) (+ 1 c))))))

(define (disj g1 g2) (lambda (s/c) ($-append (g1 s/c) (g2 s/c))))

(define (conj g1 g2) (lambda (s/c) ($-append-map g2 (g1 s/c))))

(define ($-append $1 $2)
  (cond
   ((procedure? $1) (lambda () ($-append $2 ($1))))
   ((null? $1) $2)
   (else (cons (car $1) ($-append (cdr $1) $2)))))

(define ($-append-map g $)
  (cond
   ((procedure? $) (lambda () ($-append-map g ($))))
   ((null? $) `())
   (else ($-append (g (car $)) ($-append-map g (cdr $))))))

(define (call/empty-state g) (g (empty-state)))
(define (empty-state) `(() . 0))


