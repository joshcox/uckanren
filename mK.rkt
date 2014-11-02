#lang racket
(require "uk.rkt")
(provide (all-from-out "uk.rkt") (all-defined-out))

(define (walk* v s)
  (let ((v (walk v s)))
    (cond
     ((var? v) v)
     ((pair? v) (cons (walk* (car v) s) (walk* (cdr v) s)))
     (else v))))

;;NOTE: This is annoying. Have to retain old versions of walk* and walk
;; to be able to reify things. This needs to go away. It's a badness.
(define (orig-walk u s)
  (let ((pr (and (var? u) (assoc u s var=?))))
    (if pr (if (var=? u (cdr pr)) u (walk (cdr pr) s)) u)))

(define (walk** v s)
  (let ((v (orig-walk v s)))
    (cond
     ((var? v) v)
     ((pair? v) (cons (walk** (car v) s) (walk** (cdr v) s)))
     (else v))))

(define-syntax inverse-eta-delay
  (syntax-rules ()
    ((_ g) (lambda (s/c) (lambda () (g s/c))))))

(define-syntax conj+
  (syntax-rules ()
    ((_ g) g)
    ((_ g0 g ...) (conj g0 (conj+ g ...)))))

(define-syntax disj+
  (syntax-rules ()
    ((_ g) g)
    ((_ g0 g ...) (disj g0 (disj+ g ...)))))

(define-syntax fresh
  (syntax-rules ()
    ((_ () g0 g ...)
     (inverse-eta-delay (conj+ g0 g ...)))
    ((_ (x0 x ...) g0 g ...)
     (call/fresh (lambda (x0) (fresh (x ...) g0 g ...))))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) (g0* g* ...) ...)
     (inverse-eta-delay
      (disj+ (conj+ g0 g ...) (conj+ g0* g* ...) ...)))))

(define (reify-var0 s)
  (let ((v (walk* (var 0) s)))
    (walk** v (reify-s v '()))))

(define (reify-s v s)
  (let ((v (orig-walk v s)))
    (cond
     ((var? v)
      (let ((name (reify-name (length s))))
        (cons (cons v name) s)))
     ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
     (else s))))

(define (reify-name n)
  (string->symbol
   (string-append "_." (number->string n))))

(define (pull $) (if (procedure? $) (pull ($)) $))

(define (take n)
  (lambda ($)
    (cond
     ((zero? n) '())
     (else
      (let (($ (pull $)))
        (cond
         ((null? $) '())
         (else (cons (car $) ((take (- n 1)) (cdr $))))))))))

(define (take-all $)
  (let (($ (pull $)))
    (if (null? $) '() (cons (car $) (take-all (cdr $))))))

(define-syntax run
  (syntax-rules ()
    ((_ n (q) g0 g ...)
     (map reify-var0
          ((take n) (call/empty-state (fresh (q) g0 g ...)))))))

(define-syntax run*
  (syntax-rules ()
    ((_ (q) g0 g ...)
     (map reify-var0 (take-all (call/empty-state (fresh (q) g0 g ...)))))))
