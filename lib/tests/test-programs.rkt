#lang racket
(require "../miniKanren.rkt")
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
