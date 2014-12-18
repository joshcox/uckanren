#lang racket/base
;; Modified from SamTH' open-place unstable racket collection
;; https://code.google.com/p/racket/source/browse/collects/unstable/open-place.rkt?r=b601f52d4fd7ce4700ac9b9729db07fb895c1e15
(require (for-syntax syntax/parse racket/base syntax/free-vars racket/syntax)
         racket/fasl racket/place)

(provide close-exp close-exp->fasl eval-fasl)

(define-syntax (close-exp stx)
  (syntax-parse stx
    [(_ body:expr ...)
     (define b #'(let () body ...))
     (define/with-syntax b* (local-expand b 'expression null))
     (define/with-syntax (fvs ...) (filter identifier-binding (free-vars #'b*)))
     #'(let ()
         (for ([e (in-list (list fvs ...))]
               [n (in-list (syntax->list (quote-syntax (fvs ...))))])
           (unless (place-message-allowed? e)
             (raise-arguments-error 'serialize-exp
                                    "free variable values must be allowable as place messages"
                                    (symbol->string (syntax-e n)) e)))
         
         (syntax->list
          #`((lambda #,(syntax->list (quote-syntax (fvs ...)))
               (let () body ...))
             (quote #,fvs) ...)))]))

(define-syntax-rule (close-exp->fasl exp) (s-exp->fasl (close-exp exp)))

(define-syntax-rule (eval-fasl fasl namespace) (eval (fasl->s-exp fasl) namespace))
