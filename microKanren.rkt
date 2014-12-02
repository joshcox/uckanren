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

(define (disj g1 g2) (lambda (s/c) ($-append (g1 s/c) (g2 s/c))))
(define (conj g1 g2) (lambda (s/c) ($-append-map g2 (g1 s/c))))

(define (pdisj g1 g2)
  (lambda (s/c)
    (let ((ch (make-async-channel)))
      (let ((t (thread (lambda () (async-channel-put ch (g2 s/c))))))
        (((curry $-append) (g1 s/c)) (let ((v (sync ch))) (kill-thread t) v))))))

(define pconj conj)

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

(define (walk* v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) v)
      ((pair? v) (cons (walk* (car v) s) (walk* (cdr v) s)))
      (else v))))

(define (reify-var0 s/c)
  (let ((v (walk* (var 0) (car s/c))))
    (walk* v (reify-s v '()))))

(define (reify-s v s)
  (let ((v (walk v s)))
    (cond
      ((var? v)
       (let ((name (reify-name (length s))))
         (cons (cons v name) s)))
      ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
      (else s))))

(define (reify-name n)
  (string->symbol
    (string-append "_." (number->string n))))

(define (ext-env tag x e env)
  `((,tag ,x ,e) . ,env))

(define (apply-env x env)
  (pmatch env
    (`() (error 'unbound-variable))
    (`((,tag ,y ,e) . ,env^) (if (eqv? y x) `(,tag ,y ,e) (apply-env x env^)))))

(define vof
  (lambda (exp env)
    (pmatch exp
      ;; microKanren
      (`(conj ,g1 ,g2) (conj (vof g1 env) (vof g2 env))) 
      (`(disj ,g1 ,g2) (disj (vof g1 env) (vof g2 env)))
      (`(call/fresh ,f) (call/fresh (vof f env)))
      (`(call/empty-state ,g) (call/empty-state (vof g env)))
      (`(run ,num (,x) ,e) (guard (number? num))
       (map reify-var0 ((take num) (call/empty-state (vof `(call/fresh (lambda (,x) ,e)) env)))))
      (`(== ,x ,y) (== (vof x env) (vof y env)))

      ;;parallel microKanren
      (`(pdisj ,g1 ,g2) (pdisj (vof g1 env) (vof g2 env)))
      
      ;;typical interpreter stuff
      (`,x (guard (symbol? x))
           (pmatch (apply-env x env)
                   (`(var ,x ,e) e)
                   (`(rec ,x ,e) (vof e env))))
      (`,x (guard (or (number? x) (boolean? x))) x)
      (`(add1 ,n) (add1 (vof n env)))
      (`(plus ,n1 ,n2) (+ (vof n1 env) (vof n2 env)))
      (`(zero? ,n) (zero? (vof n env)))
      (`(* ,n1 ,n2) (* (vof n1 env) (vof n2 env)))
      (`(sub1 ,n) (sub1 (vof n env)))
      (`(if ,t ,c ,a) (if (vof t env) (vof c env) (vof a env)))
      (`(quote ,x) x)
      (`(quasiquote ,x) (vof x env))
      (`((quote unquote) ,x) (vof x env))
      (`(cons ,x ,y) (cons (vof x env) (vof y env)))
      (`(car ,l) (car (vof l env)))
      (`(cdr ,l) (cdr (vof l env)))
      (`(letrec ((,x ,e)) ,b) (vof b (ext-env 'rec x e env)))
      (`(lambda ,xs ,e)
       (let ((vs (for/list ((x xs)) (gensym))))
         (lambda vs (vof e (foldr (lambda (x v env) (ext-env 'var x v env)) env xs vs)))))
      (`(,rat . ,rands) (apply (vof rat env) (map (lambda (x) (vof x env)) rands))))))
