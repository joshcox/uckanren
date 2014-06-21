#lang racket
(require C311/trace)
(require (only-in "c-f-union-find.rkt" make-initial-u-f union find allocate-new-index))
(require data/hamt)
(provide unify empty-state allocate var? walk)

#|
  cells - module
  This module maintains our state/cells. It provides us functionality
  to:
  1. Create a state
  2. Manage equivalence classes through use of union-find
  3. Merge Cells
  4. Add Cells (representative node in u-f structure)
  5. Run constraint store
|#


(define-struct c (val dom cs))
(define-struct state (u-f c*))

(define (var? x) (box? x))

;;empty-state - first state created
(define (empty-state)
  (make-state (make-initial-u-f size) (hasheqv)))

;;allocate - create new index in u-f
(define (allocate state)
  (make-state (allocate-new-index (state-u-f state))
	      (state-c* state)))

;;walk - given a var, return the cell
(define (walk var s)
  (let ((var^ (unbox var)))
    (let ((u-f (state-u-f s)))
      (let ((rep (find u-f var^)))
	(let ((cells (state-c* s)))
	  (hash-ref cells rep #f))))))

;;if using *only* unify
(define unify
  (lambda (u^ v^ s)
    (let ((u (walk u^ s))
	  (v (walk v^ s)))
      (let ((u (c-val u))
	    (v (c-val v)))
	(cond
	 ((eq? u v) s)
	 ((var? u) (ext-s u v s))
	 ((var? v) (ext-s v u s))
	 ((and (pair? u) (pair? v))
	  (let ((s (unify (car u) (car v) s)))
	    (and s (unify (cdr u) (cdr v) s))))
	 (else (and (eqv? u v) s)))))))

(define ext-s
  (lambda (x v s)
    (cond
     ((var? v)
      (make-state (union (state-u-f s) x v)
		  (state-ht s)))
     ((occurs? x v s) #f)
     (else
      (make-state (state-u-f s)
		  (hash-set (state-ht s) x v))))))

(define occurs?
  (lambda (x v s)
    (let ((v (s-walk v s)))
      (cond
       ((var? v) (= x v))
       ((pair? v) (or (occurs? x (car v) s)
		      

