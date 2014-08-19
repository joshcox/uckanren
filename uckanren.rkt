#lang racket
(require C311/trace)
(require (only-in data/p-union-find
                  make-initial-u-f union find allocate-new-index))

(provide (all-defined-out))

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

(define (make-state u-f cells)
  (hasheqv 'u-f u-f
           'cells cells))
(define (empty-cell) (hasheqv 'type 'cell 'value 'dummy 'constraints '()))
(define (empty-cells) (hasheqv))
(define (empty-state)
  `(,(make-state (make-initial-u-f 256) (empty-cells)) . 0))
(define (cell? c) (and (hash? c) (eqv? (hash-ref c 'type #f) 'cell)))

;;accessors
(define (get-u-f s)
  (hash-ref s 'u-f))
(define (get-cells s)
  (hash-ref s 'cells))
(define (get-cell x s)
  (hash-ref (get-cells s) x #f))
(define (get-cell-value c)
  (hash-ref c 'value))
(define (get-cell-constraints c)
  (hash-ref c 'constraints))

;;microKanren variables
(define (var v) (box v))
(define (var-name v) (unbox v))
(define (var? v) (box? v))
(define (var=? v1 v2) (eqv? (var-name v1) (var-name v2)))

(define (allocate s)
  (let ((u-f (get-u-f s)))
    (make-state (cons (allocate-new-index (car u-f)) (cdr u-f)) (get-cells s))))

(define (walk v s)
  (cond
   ((not (var? v)) v)
   ((let ((root (find (get-u-f s) (var-name v))))
      (let ((cell (get-cell root s)))
        (if cell cell (var root))))))) ;;return cell or root

;;unification
(define (unify u^ v^ s)
  (let ((u (walk u^ s))
        (v (walk v^ s)))
    (let ((u (if (cell? u) (get-cell-value u) u))
          (v (if (cell? v) (get-cell-value v) v)))
      (cond
       ((eqv? u v) s) ;;possible *not* vars
       ((var? u) (ext-s u v s))
       ((var? v) (ext-s v u s))
       ((and (pair? u) (pair? v))
        (let ((s (unify (car u) (car v) s)))
          (and s (unify (cdr u) (cdr v) s))))
       (else (and (eqv? u v) s)))))) 

(define (ext-s x v s)
  (cond
   ((var? v) (make-state (union (get-u-f s) (var-name x) (var-name v))))
   ((occurs? x v s) #f)
   (else
    (let ((cell (hash-set (empty-cell) 'value v)))
      (make-state (get-u-f s) (hash-set (get-cells s) (var-name x) cell))))))

(define (occurs? x v s)
  (let ((v (walk v s)))
    (cond
     ((var? v) (var=? x v)) ;;use var=? since we are testing that variables are the same. var = box(number)
     ((pair? v) (or (occurs? x (car v) s) (occurs? x (cdr v) s)))
     (else #f))))
;;end unification

#|
(define (propagate ls s/c k) 
  (cond
   ((null? cs) (k s/c)) ;;return state out
   (((car ls) s/c k) -> (lambda (pair) ((cdr pair) (car pair))))
   (else (p (cdr ls) s/c k))))


(define (pluso a b c)
  (lambda (s/c k)
    (let ((a (walk a s/c)) (b (walk b s/c)) (c (walk c s/c)))
      (cond
       ;;return #f with no change to state
       ((or (dummy? a-value) (dummy? b-value)) #f)
       ((and (not (dummy? a-value)) (not (dummy? b-value)) (not (dummy? c-value)))
        #f) ;; need to check consistency here
       (else `(,(hash-set c-value (+ a b)) .
               ,(lambda (s/c)
                  (propagate c-constraints s/c
                    (lambda (s/c)
                      (propagate b-constraints s/c
                        (lambda (s/c)
                          (propagate a-constraints s/c k))))))))))))

|#
#|

pluso a b c
pluso d e c
== a d
=/= d e ;; should fail here?
== a 5
== e 6  ;; should definitely at least fail here 

|#
