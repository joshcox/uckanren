#lang racket
(require (only-in "../state/cKanren-state.rkt" 
                  any/var? build-oc ext-c make-a oc->rands oc->rator var? walk*)
         (only-in "../constraint-framework/cKanren-framework.rkt" 
                  enforce-constraints goal-construct lambdas/c prefix-s process-prefix reify-constraints run-constraints unit)
         (only-in "../unification/cKanren-unification.rkt" == unify)
         (only-in "../miniKanren-base/cKanren-miniKanren-base.rkt" conde fresh))

(define lhs car)
(define rhs cdr)

(define recover/vars
  (lambda (p)
    (cond
     ((null? p) '())
     (else
      (let ((x (lhs (car p)))
            (v (rhs (car p)))
            (r (recover/vars (cdr p))))
        (cond
         ((var? v) (ext/vars v (ext/vars x r)))
         (else (ext/vars x r))))))))

(define ext/vars
  (lambda (x r)
    (cond
     ((memq x r) r)
     (else (cons x r)))))

;;; serious functions

(define process-prefixneq
  (lambda (p c)
    (run-constraints (recover/vars p) c)))

(define oc->prefix
  (lambda (oc)
    (car (oc->rands oc))))

(define enforce-constraintsneq (lambda (x) unit))

;; (define reify-constraintsneq
;;   (lambda (m r)
;;     (lambdag@ (a : s d c)
;;       (let* ((c (walk* c r))
;;              (p* (map oc->prefix c))
;;              (c (remp any/var? p*)))
;;         (cond
;;           ((null? c) m)
;;           (else `(,m : . ((=/= . ,c)))))))))

(define remp 
  (lambda (proc ls)
    (filter (lambda (x) (not (proc x))) ls)))

(define reify-constraintsneq
  (lambda (m r)
    (lambdas/c (s/c : a cnt : s d c)
              (let* ((c (walk* c r))
                     (p* (remp any/var? (map oc->prefix c))))
                (cond
                 ((null? p*) m)
                 (else `(,m : . ((=/= . ,p*)))))))))

(define =/=neq-c
  (lambda (p)
    (lambdas/c (s/c : a cnt : s d c)
      (cond
       ((unify p s)
        =>
        (lambda (s^)
          (let ((p (prefix-s s s^)))
            (cond
             ((null? p) #f)
             (else ((normalize-store p) a))))))
       (else a)))))

(define normalize-store
  (lambda (p)
    (lambdas/c (s/c : a cnt : s d c)
      (let loop ((c c) (c^ '()))
        (cond
         ((null? c)
          (let ((c^ (ext-c (build-oc =/=neq-c p) c^)))
            (make-a s d c^)))
         ((eq? (oc->rator (car c)) '=/=neq-c)
          (let* ((oc (car c))
                 (p^ (oc->prefix oc)))
            (cond
             ((subsumes? p^ p) a)
             ((subsumes? p p^) (loop (cdr c) c^))
             (else (loop (cdr c) (cons oc c^))))))
         (else (loop (cdr c) (cons (car c) c^))))))))

(define subsumes?
  (lambda (p s)
    (cond
     ((unify p s)
      => (lambda (s^) (eq? s s^)))
     (else #f))))

;;;-----------------------------------------------------------------

;;; goals

;; (define =/=
;;   (lambda (u v)
;;     (lambdag@ (a : s d c)
;;       (cond
;;         ((unify `((,u . ,v)) s)
;;          => (lambda (s^)
;;               ((=/=neq-c (prefix-s s s^)) a)))
;;         (else (unit a))))))

(define =/=
  (lambda (u v)
    (goal-construct (=/=-c u v))))

(define =/=-c
  (lambda (u v)
    (lambdas/c (s/c : a cnt : s d c)
      (cond
       ((unify `((,u . ,v)) s)
        => (lambda (s^)
             ((=/=neq-c (prefix-s s s^)) a)))
       (else a)))))

(define all-diffo
  (lambda (l)
    (conde
     ((== l '()))
     ((fresh (a) (== l `(,a))))
     ((fresh (a ad dd)
             (== l `(,a ,ad . ,dd))
             (=/= a ad)
             (all-diffo `(,a . ,dd))
             (all-diffo `(,ad . ,dd)))))))

;;; to use the =/= definitions, invoke (useneverequalo)

(define useneq
  (lambda ()
    (process-prefix process-prefixneq)
    (enforce-constraints enforce-constraintsneq)
    (reify-constraints reify-constraintsneq)))
