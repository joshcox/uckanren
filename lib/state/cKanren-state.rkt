#lang racket
(provide any/var? build-oc enforce-constraints empty-a empty-state ext-c ext-d make-a oc->proc oc->rands oc->rator reify-constraints reify-var0 var var? veqv? walk walk*)

(define unit (lambda (x) (cons x '())))
(define enforce-constraints (make-parameter (lambda (x) unit)))
(define reify-constraints (make-parameter (lambda (m r) unit)))

(define empty-s '())
(define empty-d '())
(define empty-c '())

(define make-a (lambda (s d c) (cons s (cons d c))))

(define empty-a (make-a empty-s empty-d empty-c))
(define (empty-state) (cons empty-a 0))

(define (var n) (vector n))
(define (var? n) (vector? n))
(define (unvar n) (vector-ref n 0))
(define (var-eqv? u v) (and (var? u) (var? v) (eqv? (unvar u) (unvar v))))
(define veqv? (lambda (x y) (or (eqv? x y) (var-eqv? x y))))

(define-syntax build-oc
  (syntax-rules ()
    ((_ op-c arg ...)
     (build-oc-aux op-c (arg ...) () (arg ...)))))

(define-syntax build-oc-aux  ;;; (op-c z ...) evaluates to a seq.
  (syntax-rules ()
    ((_  op-c () (z ...) (arg ...))
     (let ((z arg) ...)
       `(,(op-c z ...) op-c ,z ...)))
    ((_ op-c (arg0 arg ...) (z ...) args)
     (build-oc-aux op-c (arg ...) (z ... q) args))))

(define any/var?
  (lambda (p)
    (cond
     ((var? p) #t)
     ((pair? p)
      (or (any/var? (car p)) (any/var? (cdr p))))
     (else #f))))

(define oc->proc car)
(define oc->rands cddr)
(define oc->rator cadr)

(define ext-d (lambda (x fd d) (cons `(,x . ,fd) d)))

(define ext-c
  (lambda (oc c)
    (cond
     ((any/var? (oc->rands oc)) (cons oc c))
     (else c))))

(define (walk u s)
  (let ((pr (and (var? u) (assoc u s var-eqv?))))
    (if pr (walk (cdr pr) s) u)))

(define (walk* v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) v)
      ((pair? v) (cons (walk* (car v) s) (walk* (cdr v) s)))
      (else v))))

;;might need to do something here since s/c is an a

(define reify-var0
  (lambda (s/c)
    (let* ((a (car s/c)) (cnt (cdr s/c))
           (s (car a)) (d (cdr a)) (c (cddr a)))
      (let ((a ((enforce-constraints) a)))
        (let ((v (walk* (var 0) s)))
          (let ((r (reify-s v '())))
            (let ((v (walk* v r)))
              (if (null? c) v (((reify-constraints) v r) a)))))))))

;; (define (reify-var0 s/c)
;;   (let ((s/c (car s/c)))
;;     (let ((s/c ((enforce-constraints) (car s/c))))
;;       (if s/c 
;;           (let ((v (walk* (var 0) s/c)))
;;             (walk* v (reify-s v '())))
;;           '()))))

;; (cond
;;  ((null? r) v)
;;  (else
;;   (let ((v (walk* v r)))
;;     (cond
;;      ((null? c) v)
;;      (else (((reify-constraints) v r) a))))))

(define (reify-s v s)
  (let ((v (walk v s)))
    (cond
      ((var? v)
       (let ((name (reify-name (length s))))
         (cons (cons v name) s)))
      ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
      (else s))))

(define (reify-name n)
  (string->symbol
    (string-append "_." (number->string n))))

;; (require racket/trace)
;; (trace empty-state make-a oc->proc oc->rands reify-var0 var var? veqv? walk walk* reify-s)
