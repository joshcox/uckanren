#lang racket
(require (only-in "../microKanren-base/cKanren-microKanren-base.rkt" 
                  call/fresh disj conj == inverse-eta-delay reify-var0 take call/empty-state))
(provide call/fresh disj conj == 
         run conde fresh)

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

(define-syntax run
  (syntax-rules ()
    ((_ n (q) g0 g ...)
     (map reify-var0
          ((take n) (call/empty-state (fresh (q) g0 g ...)))))))


