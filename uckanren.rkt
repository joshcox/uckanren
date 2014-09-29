#lang racket
(provide (all-defined-out))
(define (var n) (box n))
(define (var? n) (box? n))
(define (bump n) (var (add1 (unvar n))))
(define (var=? n1 n2) (eqv? (unbox n1) (unbox n2)))

;;extend the state with mapping from x -> cell
;;auto-bump for cleaner code later
(define (ext-s s)
  (let ((v (if (null? s) (var 0) (bump v))))
    `((,v . `(v '() '())) . s)))
(define cs-cell caddr)
(define dom-cell cadr)
(define val-cell car)

(define (call/fresh f)
  (lambda (s)
    (let ((s (ext-s s)))
      ((f (car (car s))) s))))

(define (disj g1 g2) (lambda (s/c) ($-append (g1 s/c) (g2 s/c))))

(define (conj g1 g2) (lambda (s/c) ($-append-map g2 (g1 s/c))))

(define ($-append $1 $2)
  (cond
   ((procedure? $1) (lambda () ($-append $2 ($1))))
   ((null? $1) $2)
   (else (cons (car $1) ($-append (cdr $1) $2)))))

(define ($-append-map g $)
  (cond
   ((procedure? $) (lambda () ($-append-map g ($))))
   ((null? $) `())
   (else ($-append (g (car $)) ($-append-map g (cdr $))))))

(define (call/empty-state g) (g '()))

;; (define (== u v)
;;   (lambda (s/c)
;;     (let ((s (unify u v (car s/c))))
;;       (if s (list (cons s (cdr s/c))) `()))))

(define update/f
  (lambda (f)
    (lambda (cell)
      (lambda (s)
        (pmatch s
          (`() `())
          (`((,aa . ,da) . ,d) `((,aa . ,(f da)) . d))
          (`(,a . ,d) `(,a . ,(((update/f f) cell) d))))))))

(define (add-constraint c)
  (update/f (lambda (x) (pmatch x (`(,v ,d ,cs) `(,v ,d ,(cons c cs)))))))

(define (== u v)
  (lambda (s/c)
    (let ((c (unify u v)))
      ((propagate u) (((add-constraint c) v) (((add-constraint c) u) (car s/c)))))))

(define (unify u v)
  (lambda (s)
    (lambda (k)
      'wat)))
;; u is val, v is val   => same?
;; [ u is self, v is self => u is v
;;   u is self, v is val  => u is val
;;   u is val, v is self  => v is val ]
;; u is self => u is v
;; v is self => v is u
;; u&v are lists => unify car, unify cdr

;; how to make sure that mutiple vars down clash in this unification^^^ (== a b) (== a c) (== a d)

(define p
  (lambda (cell)
    (lambda (s)
      (let ((ls^ (cs-cell (walk cell s))))
        (letrec ((p (lambda (ls)
                      (lambda (s)
                        (lambda (k)
                          (cond
                           ((null? ls) (k s))                        
                           (((car ls) s) k) =>
                           (lambda (x)
                             (cond
                              ((null? x) '())
                              ((null? (cdr x)) (((p (cdr ls)) s) (car x)))
                              ((null? (cdr (cdr x))) (((p ls^) (car x)) (car (cdr x))))))))))))
          (((p ls^) s) (lambda (x) x)))))))
;; (define (walk u s)
;;   (let ((pr (and (var? u) (assv u s))))
;;     (if pr (walk (cdr pr) s) u)))

;; (define (unify u v s)
;;   (let ((u (walk u s)) (v (walk v s)))
;;     (cond
;;      ((eqv? u v) s)
;;      ((var? u) (ext-s u v s))
;;      ((var? v) (ext-s v u s))
;;      ((and (pair? u) (pair? v))
;;       (let ((s (unify (car u) (car v) s)))
;;         (and s (unify (cdr u) (cdr v) s))))
;;      (else #f))))
