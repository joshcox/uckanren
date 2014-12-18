#lang racket
(require (only-in "../state/cKanren-state.rkt" var? veqv? walk make-a)
         (only-in "../constraint-framework/cKanren-framework.rkt" 
                  goal-construct lambdam@ lambdas/c prefix-s process-prefix))
(provide ==)

(define == (lambda (u v) (goal-construct (==-c u v))))

(define ==-c  ;;; returns an ma if ((process-prefix) p c) => seq.
  (lambda (u v)
    (lambdas/c (s/c : a cnt : s d c)
              (cond
               ((unify u v s)
                => (lambda (s^)
                     (cond
                      ((eq? s s^) s/c) ;; used to be just 'a'
                      (else
                       (let ((p (prefix-s s s^))
                             (a (make-a s^ d c)))
                         (cons (((process-prefix) p c) a) cnt)))))) ;; used to have no cons
               (else #f)))))

(define (unify x v s)
  (let ((x (walk x s)) (v (walk v s)))
    (cond
      ((veqv? x v) s)
      ((var? x) (ext-s x v s))
      ((var? v) (ext-s v x s))
      ((and (pair? x) (pair? v))
       (let ((s (unify (car x) (car v) s)))
         (and s (unify (cdr x) (cdr v) s))))
      (else #f))))

(define (ext-s x v s) (if (occurs? x v s) #f (cons (cons x v) s)))

(define (occurs? x v s)
  (let ((v (walk v s)))
    (cond
     ((var? v) (veqv? v x))
     ((pair? v) (or (occurs? x (car v) s) (occurs? x (cdr v) s)))
     (else #f))))

;; (require racket/trace)
;; (trace == ==-c unify ext-s occurs?)
