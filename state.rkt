#lang racket
(require C311/trace)
(require (only-in data/p-union-find
		  make-initial-u-f union find allocate-new-index))
(provide make-state
         var var-name var? var=?
	 empty-state/u-f
	 make-initial-cell
	 allocate
	 walk val-walk
	 set-cell-value
	 union
	 state-u-f state-cells)

(define-struct cell (val dom cs) #:mutable)
(define-struct state (u-f cells) #:mutable)

;; What's a var? It's a representation of the cell underneath.
;;  This time... a box.
(define (var v) (box v))
(define (var-name v) (unbox v))
(define (var? v) (box? v))
(define (var=? v1 v2) (eqv? (var-name v1) (var-name v2)))

#|
  Creators
|#

;; empty-state: create an empty state to be used with each run
(define (empty-state/u-f) (make-state (make-initial-u-f 256) (hasheqv)))

;; make-initial-cell: initialize a cell for the first time
(define dummy-val 'dne)
(define (dummy-val? val) (eqv? dummy-val val)) 
(define (make-initial-cell v s)
  (hash-ref (hash-set (state-cells s) v (make-cell 'fresh '() '())) v 'dne))

;; allocate-new-index-u-f: given an entire u-f structure `(,u-f . ,r),
;;   we want to keep the entire thing
(define (allocate-new-index-u-f u-f)
  (let ((pnl (car u-f)) (r (cdr u-f)))
    (let ((pnl^ (allocate-new-index pnl)))
      (cons pnl^ r))))

;;allocate new variable index when creating a new fresh variable
(define (allocate s)
  (make-state (allocate-new-index-u-f (state-u-f s)) (state-cells s)))

#|
  Accessors
|#

;; lookup-cell: looks up a cell in the state-cells
;; takes a var-name, not a var
(define (lookup-cell v cells)
  (hash-ref cells v 'dne))

;; walk: var, state -> given a var, returns the cell mapped to the rep
;; takes var, returns var || cell
(define (walk v s)
  (cond
   ((not (var? v)) v)
   (else (let ((u-f (state-u-f s)) (cells (state-cells s)))
	   (let ((rep (find u-f (var-name v)))) ;backwards
	     (lookup-cell rep cells))))))

;; val-walk: walks to cell, returns val out
;; takes var || cell, returns var || cell-val
(define (val-walk v s)
  (let ((c (walk v s)))
    (cond
     ; return root for unify's benefit
     ((dummy-val? c) (var (find (state-u-f s) (var-name v)))) ;constant-op
     ((cell? c) (cell-val c))
     (else v))))

#|
  Setters
|#

;; set-cell-value
(define (set-cell-value x v s)
  (let ((c (walk x s)))
    (if (cell? c)
	(set-cell-val! c v)
	(set-cell-val! (make-initial-cell x s) v))))
