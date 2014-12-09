#lang racket
(require "mk.rkt")
(require C311/trace C311/pmatch)
(require racket/place)
(provide (all-defined-out)
         (all-from-out "mk.rkt"))

(define mk
  (lambda (ls)
    (vof ls (mpt-env))))

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
      ;;(`(disj ,g1 ,g2) (disj (vof g1 env) (vof g2 env)))
      (`(call/fresh ,f) (call/fresh (vof f env)))
      (`(call/empty-state ,g) (call/empty-state (vof g env)))
      (`(run ,num (,x) ,e) (guard (number? num))
       (map reify-var0 ((take num) (call/empty-state (vof `(call/fresh (lambda (,x) ,e)) env)))))
      (`(== ,x ,y) (== (vof x env) (vof y env)))

      ;;parallel microKanren
      (`(disj ,g1 ,g2) (pdisj g1 g2))
            
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

(define (mpt-env)
  `((rec preverseo
         (lambda (ls o)
           (pdisj
            (conj (== ls '()) (== o ls))
            (call/fresh
             (lambda (a)
               (call/fresh
                (lambda (b)
                  (call/fresh
                   (lambda (res)
                     (conj (== (cons a b) ls) ;; `(,a . ,b)
                           (conj  (lambda (s/c)
                                    (lambda ()
                                      ((preverseo b res) s/c)))
                                  (lambda (s/c)
                                    (lambda ()
                                      ((pappendo res (cons a '()) o) s/c))))))))))))))
    (rec pappendo
         (lambda (l s o)
           (pdisj
            (conj (== l '()) (== o s))
            (call/fresh
             (lambda (a)
               (call/fresh
                (lambda (b)
                  (call/fresh
                   (lambda (res)
                     (conj (== l (cons a b)) ;;`(,a . ,b) 
                           (conj (== o (cons a res)) ;; `(,a . ,res)
                                 (lambda (s/c)
                                   (lambda ()
                                     ((pappendo b s res) s/c))))))))))))))
    (rec reverseo
         (lambda (ls o)
           (disj
            (conj (== ls '()) (== o ls))
            (call/fresh
             (lambda (a)
               (call/fresh
                (lambda (b)
                  (call/fresh
                   (lambda (res)
                     (conj (== (cons a b) ls) ;; `(,a . ,b)
                           (conj  (lambda (s/c)
                                    (lambda ()
                                      ((reverseo b res) s/c)))
                                  (lambda (s/c)
                                    (lambda ()
                                      ((appendo res (cons a '()) o) s/c))))))))))))))
    (rec appendo
         (lambda (l s o)
           (disj
            (conj (== l '()) (== o s))
            (call/fresh
             (lambda (a)
               (call/fresh
                (lambda (b)
                  (call/fresh
                   (lambda (res)
                     (conj (== l (cons a b)) ;;`(,a . ,b) 
                           (conj (== o (cons a res)) ;; `(,a . ,res)
                                 (lambda (s/c)
                                   (lambda ()
                                     ((appendo b s res) s/c))))))))))))))))
