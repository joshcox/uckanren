#lang racket
(require "mk.rkt")
(require C311/trace C311/pmatch)
(require racket/place)
(provide (all-defined-out)
         (all-from-out "mk.rkt"))

(define mk
  (lambda (ls)
    (vof ls '())))

(define (ext-env tag x e env)
  `((,tag ,x ,e) . ,env))

(define (apply-env x env)
  (pmatch env
    (`() (error "Unbound Variable: " x))
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
      (`(procedure? ,x) (procedure? (vof x env)))
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
      (`(null? ,l) (null? (vof l env)))
      (`(letrec ((,x ,e)) ,b) (vof b (ext-env 'rec x e env)))
      ;(`(lambda () ,e) (lambda () (vof e env)))
      (`(lambda ,xs ,e)
       (let ((vs (for/list ((x xs)) (gensym))))
         (lambda vs (vof e (foldl (lambda (x v env) (ext-env 'var x v env)) env xs vs)))))
      ;(`(,th) ((vof th env)))
      (`(,rat . ,rands) (apply (vof rat env) (map (lambda (x) (vof x env)) rands))))))
