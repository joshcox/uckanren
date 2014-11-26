#lang racket
(require C311/trace C311/pmatch)
(require racket/place)
(require racket/async-channel)
(provide (all-defined-out))

(define (var n) n)
(define (var? n) (number? n))

(define (walk u s)
  (let ((pr (and (var? u) (assv u s))))
    (if pr (walk (cdr pr) s) u)))

(define (occurs? x v s)
  (let ((v (walk v s)))
    (cond
     ((var? v) (eq? v x))
     ((pair? v) (or (occurs? x (car v) s) (occurs? x (cdr v) s)))
     (else #f))))

(define (ext-s x v s) (if (occurs? x v s) #f (cons (cons x v) s)))

(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((eqv? u v) s)
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

;; (define (pdisj g1 g2)
;;   (lambda (s/c)
;;     (let-values (((pch1 pch2) (place-channel)))
;;       (let ((p (place pch1 (let ((g (g2 s/c))) (place-channel-put pch1 g)))))
;;         (((curry $-append) (g1 s/c)) (place-channel-get pch2))))))

(define (pdisj g1 g2)
  (lambda (s/c)
    (let ((ch (make-async-channel)))
      (let ((t (thread (lambda () (async-channel-put ch (g2 s/c))))))
        (((curry $-append) (g1 s/c)) (sync ch))))))

;; (define (pdisj g1 g2)
;;   (lambda (s/c)
;;     (let ((g2 (thread (lambda () (g2 s/c)))))
;;       ($-append (g1 s/c) (touch g2)))))

(define (pconj g1 g2) (lambda (s/c) (p$-append-map g2 (g1 s/c))))

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

(define (p$-append-map g $)
  (let (($ ($-map g $)))
    (let l (($ $))
      (cond
       ((procedure? $) $)
       ((null? $) `())
       (else (let ((s (sync (car (car $)))))
               ($-append s (l (cdr $)))))))))

(define ($-map g $)
  (cond
   ((procedure? $) (lambda () (p$-append-map g ($))))
   ((null? $) `())
   (else (let ((ch (make-channel)))
           (let ((t (thread (lambda () (channel-put ch (g (car $)))))))
             (cons `(,ch . ,t) ($-map g (cdr $))))))))

(define (call/empty-state g) (g (cons '() 0)))

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

(define mk
  (lambda (exp env)
    (pmatch exp
      (`,x (guard (symbol? x)) (env x))
      (`,x (guard (or (number? x) (boolean? x))) x)
      (`(quote ,x) x)
      (`(conj ,g1 ,g2) (conj (mk g1 env) (mk g2 env))) 
      (`(disj ,g1 ,g2) (disj (mk g1 env) (mk g2 env)))
      (`(call/fresh ,f) (call/fresh (mk f env)))
      (`(call/empty-state ,g) (call/empty-state (mk g env)))
      (`(run ,num (,x) ,e) (guard (number? num))
       ((take num) (call/empty-state (mk `(call/fresh (lambda (,x) ,e)) env))))
      (`(== ,x ,y) (== (mk x env) (mk y env)))
      (`(lambda (,x) ,e) (lambda (a) (mk e (lambda (y) (if (eqv? x y) a (env y)))))))))
