#lang racket
(require "microKanren.rkt" "extras-mK.rkt")
(provide (all-defined-out)
         (all-from-out "microKanren.rkt"
                       "extras-mK.rkt"))

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

(define pappendo
  (lambda (l s o)
    (pconde
     [(== l '()) (== o s)]
     [(fresh (a b res)
             (== l `(,a . ,b))
             (== o `(,a . ,res))
             (pappendo b s res))])))

(define preverseo
  (lambda (ls o)
    (pconde
     [(== ls '()) (== o ls)]
     [(fresh (a b res)
             (== `(,a . ,b) ls)
             (preverseo b res)
             (pappendo res `(,a) o))])))
