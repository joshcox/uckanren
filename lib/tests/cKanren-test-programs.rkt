#lang racket
(require "../cKanren.rkt")
(provide (all-defined-out))

;; test programs

(define appendo
  (lambda (l s o)
    (conde
     [(== l '()) (== o s)]
     [(fresh (a b res)
             (== l `(,a . ,b))
             (== o `(,a . ,res))
             (appendo b s res))])))

(define reverseo
  (lambda (ls o)
    (conde
     [(== ls '()) (== o ls)]
     [(fresh (a b res)
             (== `(,a . ,b) ls)
             (reverseo b res)
             (appendo res `(,a) o))])))


(define appendo2
  (lambda (l s o)
    (disj
     (conj (== l '()) (== o s))
     (call/fresh
      (lambda (a)
        (call/fresh
         (lambda (b)
           (call/fresh
            (lambda (res)
              (conj (== l `(,a . ,b))
                    (conj (== o `(,a . ,res))
                          (lambda (s/c) (lambda () ((appendo2 b s res) s/c))))))))))))))

(define appendo3
  (lambda (l s out)
    (disj
     (conj (== '() l) (== s out))
     (call/fresh
      (lambda (a)
        (call/fresh
         (lambda (d)
           (conj
            (== `(,a . ,d) l)
            (call/fresh
             (lambda (res)
               (conj
                (== `(,a . ,res) out)
                (lambda (s/c)
                  (lambda ()
                    ((appendo3 d s res) s/c))))))))))))))

(define reverseo2
  (lambda (ls o)
    (disj
     (conj (== ls '()) (== o ls))
     (call/fresh
      (lambda (a)
        (call/fresh
         (lambda (b)
           (call/fresh
            (lambda (res)
              (conj (== `(,a . ,b) ls)
                    (conj  (lambda (s/c) (lambda () ((reverseo2 b res) s/c)))
                           (lambda (s/c) (lambda () ((appendo2 res `(,a) o) s/c))))))))))))))


(define listo
  (lambda (x)
    (conde
     [(== x '())]
     [(fresh (ca cd)
             (== `(,ca . ,cd) x)
             (listo cd))])))

(define facto
  (lambda (n o)
    (conde
     [(== n (build-num 1))
      (== o n)]
     [(fresh (a b num)
             (== num (build-num 1))
             (minuso n num a)
             (facto a b)
             (*o b n o))])))

(define lengtho
  (lambda (ls o)
    (conde
     [(== ls '()) (== o (build-num 0))]
     [(fresh (a d res)
             (== `(,a . ,d) ls)
             (lengtho d res)
             (pluso '(1) res o))])))

(define one-itemo
  (lambda (x s o)
    (conde
     [(== s '()) (== o s)]
     [(fresh (a b res)
             (== `(,a . ,b) s)
             (== o `((,x . ,a) . ,res))
             (one-itemo x b res))])))
