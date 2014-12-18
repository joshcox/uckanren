#lang racket
(require (only-in "../state/cKanren-state.rkt" make-a oc->proc oc->rands var var?))
(provide goal-construct lambdam@ lambdas/c prefix-s process-prefix run-constraints unit)

(define unit (lambda (x) (cons x mzero)))
(define mzero '())

(define-syntax lambdam@
  (syntax-rules (:)
    ((_ (a : s d c) body)
     (lambda (a)
       (let ((s (car a)) (d (cadr a)) (c (cddr a)))
         body)))
    ((_ (a) body) (lambda (a) body))))

(define-syntax lambdas/c
  (syntax-rules (:)
    ((_ (s/c : a cnt : s d c) body)
     (lambda (s/c)
       (let ((a (car s/c)) (cnt (cdr s/c)))
         ((lambdam@ (a : s d c) body) a))))))

; just going to use identity
;(define identitym (lambdam@ (a) a))

(define composem
  (lambda (fm f^m)
    (lambdam@ (a)
              (let ((a (fm a)))
                (and a (f^m a))))))

(define goal-construct
  (lambda (fm)
    (lambda (s/c)
      (cond
       ((fm s/c) => unit)
       (else mzero)))))

(define process-prefix (make-parameter (lambda (p c) identity)))

(define prefix-s
  (lambda (s s^)
    (cond
     ((null? s) s^)
     (else (let loop ((s^ s^))
             (cond
              ((eq? s^ s) '())
              (else (cons (car s^) (loop (cdr s^))))))))))

;;(define succeed (== #f #f))
;;(define fail (== #f #t))
;;(define prt (lambda (a) (pretty-print a) (succeed a)))

(define run-constraints ;;; unitm is a sequel
  (lambda (x* c)
    (cond
     ((null? c) identity)
     ((any-relevant/var? (oc->rands (car c)) x*)
      (composem (rem/run (car c))
                (run-constraints x* (cdr c))))
     (else (run-constraints x* (cdr c))))))

(define rem/run  ;;; returns a seq.
  (lambda (oc)
    (lambdam@ (a : s d c)
              (cond
               ((memq oc c)
                (let ((c^ (remq oc c)))
                  ((oc->proc oc) (make-a s d c^))))
               (else a)))))

(define any-relevant/var?
  (lambda (t x*)
    (cond
     ((var? t) (memq t x*))
     ((pair? t)
      (or (any-relevant/var? (car t) x*)
          (any-relevant/var? (cdr t) x*)))
     (else #f))))

;; (define-syntax run
;;   (syntax-rules ()
;;     ((_ n (x) g0 g ...)
;;      (take n
;;            (lambdaf@ ()
;;                      ((fresh (x) g0 g ... (reify x))
;;                       empty-a))))))

;; (define-syntax run*
;;   (syntax-rules ()
;;     ((_ (x) g ...) (run #f (x) g ...))))

;; (define reify
;;   (lambda (x)
;;     (fresh ()
;;            ((enforce-constraints) x)
;;            (lambdag@ (a : s d c)
;;                      (choiceg
;;                       (let* ((v (walk* x s))
;;                              (r (reify-s v empty-s)))
;;                         (cond
;;                          ((null? r) v)
;;                          (else
;;                           (let ((v (walk* v r)))
;;                             (cond
;;                              ((null? c) v)
;;                              (else (((reify-constraints) v r) a)))))))
;;                       empty-f)))))

;; (require racket/trace)
;; (trace goal-construct prefix-s process-prefix)
