#lang racket
(require (only-in "../microKanren-base/p-microKanren-base.rkt" 
                  call/fresh pdisj disj conj == inverse-eta-delay reify-var0 take call/empty-state))
(provide call/fresh pdisj disj conj == 
         run conde fresh)

(define-syntax fresh
  (syntax-rules ()
    ((_ () g0 g ...)
     (inverse-eta-delay (conj g0 g ...)))
    ((_ (x0 x ...) g0 g ...)
     (call/fresh (lambda (x0) (fresh (x ...) g0 g ...))))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) (g0* g* ...) ...)
     (inverse-eta-delay
      (disj (conj g0 g ...) (conj g0* g* ...) ...)))))

(define-syntax pconde
  (syntax-rules ()
    ((_ (g0 g ...) (g0* g* ...) ...)
     (inverse-eta-delay
      (let ((pdisj disj) (disj pdisj))
        (disj (conj+ g0 g ...) (conj+ g0* g* ...) ...))))))

(define-syntax run
  (syntax-rules ()
    ((_ n (q) g0 g ...)
     (map reify-var0 ((take n) (call/empty-state (fresh (q) g0 g ...)))))))


