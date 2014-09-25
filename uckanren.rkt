#lang racket
(provide (all-defined-out))
(define (var n) (box n))
(define (var? n) (box? n))
(define (var=? n1 n2) (eqv? (unbox n1) (unbox n2)))

;;extend the state with mapping from x -> cell
(define (ext-s x v s) (cons (cons x `('fresh '() '())) s))

(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      (let ((v (var c)))
        ((f v) `(,(ext-s v (car s/c)) . ,(+ 1 c)))))))

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

(define (call/empty-state g) (g (cons '() 0)))

;; (define (== u v)
;;   (lambda (s/c)
;;     (let ((s (unify u v (car s/c))))
;;       (if s (list (cons s (cdr s/c))) `()))))

;; (define (walk u s)
;;   (let ((pr (and (var? u) (assv u s))))
;;     (if pr (walk (cdr pr) s) u)))

;; (define (unify u v s)
;;   (let ((u (walk u s)) (v (walk v s)))
;;     (cond
;;      ((eqv? u v) s)
;;      ((var? u) (ext-s u v s))
;;      ((var? v) (ext-s v u s))
;;      ((and (pair? u) (pair? v))
;;       (let ((s (unify (car u) (car v) s)))
;;         (and s (unify (cdr u) (cdr v) s))))
;;      (else #f))))
