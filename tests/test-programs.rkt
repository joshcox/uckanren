#lang racket
(require "../uk.rkt" "../mK.rkt" "../numbers.rkt")
(provide (all-from-out "../uk.rkt" "../mK.rkt" "../numbers.rkt") (all-defined-out))

;; test programs

;; (define appendo
;;   (lambda (l s o)
;;     (conde
;;      [(== l '()) (== o s)]
;;      [(fresh (a b res)
;;              (== l `(,a . ,b))
;;              (== o `(,a . ,res))
;;              (appendo b s res))])))

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

(define reverseo
  (lambda (ls o)
    (conde
     [(== ls '()) (== o ls)]
     [(fresh (a b res)
             (== `(,a . ,b) ls)
             (reverseo b res)
             (appendo res `(,a) o))])))

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

;; (define assqo
;;   (lambda (x ls o)
;;     (conde
;;      [(== ls '()) fail]
;;      [(fresh (a d aa da)
;;              (== `(,a . ,d) ls)
;;              (== `(,aa . ,da) a)
;;              (== aa x)
;;              (== o a))]
;;      [(fresh (a d)
;;              (== `(,a . ,d) ls)
;;              (assqo x d o))])))
