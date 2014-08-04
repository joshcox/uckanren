#lang racket
(require C311/trace)
(require (only-in data/p-union-find
                  make-initial-u-f union find allocate-new-index))

(provide (all-defined-out))

;;original unify
(define (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit `(,s . ,(cdr s/c))) mzero))))

(define (unit s/c) (cons s/c mzero))
(define mzero '())

(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) `(,(allocate (car s/c)) . ,(add1 c))))))

;; microKanren's core - the above is the representation of states and must be 
;; changed to make way for our constraint system.

(define (disj g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))
(define (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))

(define (mplus $1 $2)
  (cond
    ((null? $1) $2)
    ((procedure? $1) (lambda () (mplus $2 ($1))))
    (else (cons (car $1) (mplus (cdr $1) $2)))))

(define (bind $ g)
  (cond
    ((null? $) mzero)
    ((procedure? $) (lambda () (bind ($) g)))
    (else (mplus (g (car $)) (bind (cdr $) g)))))
;;end microKanren

(define (make-state u-f cells)
  (hasheqv 'u-f u-f
           'cells cells))
(define (empty-cell) (hasheqv 'value 'dummy 'constraints '()))
(define (empty-cells) (hasheqv))
(define (empty-state)
  `(,(make-state (make-initial-u-f 256) (empty-cells)) . 0))

;;microKanren variables
(define (var v) (box v))
(define (var-name v) (unbox v))
(define (var? v) (box? v))
(define (var=? v1 v2) (eqv? (var-name v1) (var-name v2)))

(define (allocate s)
  (make-state (cons (allocate-new-index (car (hash-ref s 'u-f))) (cdr (hash-ref s 'u-f))) (hash-ref s 'cells)))

(define (walk v s)
  (cond
   ((not (var? v)) v)
   ((let ((root (find (hash-ref s 'u-f) (var-name v))))
      (let ((cell (hash-ref (hash-ref s 'cells) root #f)))
        (if cell (hash-ref cell 'value) (var root))))))) ;;return cell or root

;;unification
(define (unify u^ v^ s)
  (let ((u (walk u^ s))
        (v (walk v^ s)))
    (cond
     ((eqv? u v) s) ;;possible *not* vars
     ((var? u) (ext-s u v s))
     ((var? v) (ext-s v u s))
     ((and (pair? u) (pair? v))
      (let ((s (unify (car u) (car v) s)))
        (and s (unify (cdr u) (cdr v) s))))
     (else (and (eqv? u v) s)))))

(define (ext-s x v s)
  (cond
   ((var? v) (make-state (union (hash-ref s 'u-f) (var-name x) (var-name v))))
   ((occurs? x v s) #f)
   (else
    (let ((cell (hash-set (empty-cell) 'value v)))
      (make-state (hash-ref s 'u-f) (hash-set (hash-ref s 'cells) (var-name x) cell))))))

(define (occurs? x v s)
  (let ((v (walk v s)))
    (cond
     ((var? v) (var=? x v)) ;;use var=? since we are testing that variables are the same. var = box(number)
     ((pair? v) (or (occurs? x (car v) s) (occurs? x (cdr v) s)))
     (else #f))))
;;end unification

#|
(define (make-goal f)
  (lambda (s/c)
    (cond
     ((f (car s/c)) => unit)
     (else mzero))))

(define add-content
  (lambda (var info state)
    (let ((cell (walk var state)))
      (cond
       ((dummy? info) state)
       ((dummy? (hash-ref cell 'value))
        (hash-set cell 'value info)
        ;;propagate here
        )
       (else (if (eqv? (hash-ref cell 'value) info)
                 state
                 #f))))))

(define new-neighbor
  (lambda (cell neighbor state)
    (let )))

|#
