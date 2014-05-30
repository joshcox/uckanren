#lang racket
(require C311/trace)
(require (only-in "c-f-union-find.rkt" make-initial-u-f union find allocate-new-index))
(require data/hamt)
(provide unify empty-state allocate var? s-walk)


#| ################# TODO ###############################

  1. Create a package (struct) containing
    a) value  b) domain  c) constraint-store
  2. When extending state:
    a) if (isAValue v) -> re-run constraint-store
  3. Simplify convenience procedures
  4. Generalize unification to a constraint that is 
     run in the constraint-store
    a) run the constraints - ext-constraint-store (ext-cs)
      i)  adds to constraint stores of neighbors
      ii) runs the unification from within constraint store,
          which is what performs the union of terms
    b) Dan mentioned removing the need to perform an occurs?
       - explore

################# END TODO ########################### |#

;;extend our struct with another hashtable
;;u-f - equivalence classes
;;ht1 - representative in u-f matches to value (or not)
;;ht2 - representative in u-f matches to constraint store

;;thinking about whether we need a domain variable?
;;how else can we restrict the domain of a variable?

;;maybe have one ht that contains a pair: (or p-array?)
;; (value . (constraint-store . domain))
;;I like this idea^ - will clean up below functions
(define-struct state (u-f ht1 ht2))

;;convenience procedures
(define (lookup-payload root state) 
  (let ((ht (state-ht1 state)))
    (let ((p (hash-ref ht root #f)))
      (if p p root))))

(define (lookup-constraints root state) 
  (let ((ht (state-ht2 state)))
    (hash-ref ht key #f))) ;;won't fail - 

(define (dummy-payload? p) (number? p))

;;state-walk
;;takes a var, a state, and a function to act upon the
;;state to return a value based on the representative
;;returned by find
(define (s-walk v state f)
  (cond
    ((not (var? v)) v)
    (else
      (let ((u-f (state-u-f state)))
        (let ((root (find u-f v)))
          (f root state))))))

(define (var? v) (integer? v))

;;this will need to change to become a propagator inside 
;;the constraint store. For now, let's keep it as-is to 
;;retain functionality.

;;need to merge cells when extending the state
(define unify
  (lambda (u^ v^ s)
    (let ((u (s-walk u^ s lookup-payload))
          (v (s-walk v^ s lookup-payload)))
      (cond
        ((eq? u v) s)
        ((var? u) (ext-s u v s))
        ((var? v) (ext-s v u s))
        ((and (pair? u) (pair? v))
         (let ((s (unify (car u) (car v) s)))
           (and s (unify (cdr u) (cdr v) s))))
        (else (and (eqv? u v) s))))))

;;looking to do away with occurs-check. How?
(define (ext-s x v s)
  (let ((u-f (state-uf s)) (ht1 (state-ht1 s)) (ht2 (state-ht2 s)))
    (cond
      ((var? v) (make-state (union u-f x v) ht1 ht2))
      ((occurs? x v s) #f)
      (else (make-state u-f (hash-set ht1 x v)))))) ;;re-rerun CS?

(define (occurs? x v s)
  (let ((v (s-walk v s)))
    (cond
      ((var? v) (= x v))
      ((pair? v) (or (occurs? x (car v) s)
                     (occurs? x (cdr v) s)))
      (else #f))))
