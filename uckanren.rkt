#lang racket
(require C311/trace)
(require (only-in data/p-union-find
                  make-initial-u-f union find allocate-new-index))

(provide (all-defined-out))

;; microKanren's core - the above is the representation of states and must be 
;; changed to make way for our constraint system.

;; (fold $-map $ g)

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

(define (unit s/c) (cons s/c mzero))
(define mzero '())

;;microKanren variables - box representation
(define (var v) (box v))
(define (var-name v) (unbox v))
(define (var? v) (box? v))
(define (var=? v1 v2) (eqv? (var-name v1) (var-name v2)))

(define (call/fresh f)
  (lambda (s/c)
   ((f (var (cdr s/c))) (allocate s/c)))) ;; keep state out of microKanren

(define-syntax e (syntax-rules () ((_ sym) (lambda () (error sym)))))

(define (empty-cell) (hasheqv 'type 'cell 'val (box 'fresh) 'dom '() 'cs '()))
(define (empty-state) (cons (hasheqv 'type 'state 'u-f (make-initial-u-f 256) 'cells (hasheqv 'type 'cells)) 0))

(define (state? s) (and (hash? s) (eqv? (hash-ref s 'type #f) 'state)))
(define (cells? c*) (and (hash? c*) (eqv? (hash-ref c* 'type #f) 'cells)))
(define (cell? c) (and (hash? c) (eqv? (hash-ref c 'type #f) 'cell)))
(define (fresh? c)
  (and (hash? c) (eqv? (hash-ref c 'type #f) 'cell)
       (box? (cell-ref c 'val)) (eqv? (unbox (cell-ref c 'val)) 'fresh)))

(define (state-init-cell i s)
  (let ((cells (hash-ref s 'cells (e 'malformed-state))))
    (hash-set s 'cells (hash-set cells i (empty-cell)))))

(define (cell-ref c sym) (hash-ref c sym (e 'undefined-cell-ref)))

(define (allocate s/c)
  (let ((s (car s/c)) (c (cdr s/c)))
    (let ((u-f (hash-ref s 'u-f (e 'missing-u-f-allocate))))
      `(,(state-init-cell c (hash-set s 'u-f (cons (allocate-new-index (car u-f)) (cdr u-f)))) . ,(add1 c)))))

;;walk takes <any> and attempts to return a cell, provided that <any> is of type var and is not fresh
(define (walk v s)
  (cond
   ((not (var? v)) v)
   (else (let ((rep (find (hash-ref s 'u-f (e 'miss-u-f-walk)) (var-name v))))
           (let ((c (hash-ref (hash-ref s 'cells (e 'miss-c*-walk)) rep (e 'rep-not-cell))))
             (if (fresh? c) (var rep) c)))))) ;; don't return a fresh cell - useless

(define (cell-update-val s var val)
  (let ((c* (hash-ref s 'cells (e 'miss-c*-update))))
    (let ((c (hash-ref c* (var-name var) (e 'miss-c-update))))
      (hash-set s 'cells (hash-set c* (var-name var) (hash-set c 'val val))))))

;;BELOW IS UNTESTED
(define (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit `(,s . ,(cdr s/c))) mzero))))

(define (unify u^ v^ s)
  (let ((u (walk u^ s)) (v (walk v^ s)))
    (let ((u (if (cell? u) (cell-ref 'val u) u))
          (v (if (cell? v) (cell-ref 'val v) v)))
      (cond
       ((eqv? u v) s)
       ;; ((var? u) (ext-s u v s))
       ;; ((var? v) (ext-s v u s))
       ((and (pair? u) (pair? v))
        (let ((s (unify (car u) (car v) s)))
          (and s (unify (cdr u) (cdr v) s))))
       (else #f)))))

(define (ext-s x v s) ;; if both vars, already walked from unify
  (cond
   ((occurs? x v s) #f)
   ((var? v)
    (let ((u-f (hash-ref s 'u-f (e 'miss-u-f-ext-s))) (cells (hash-ref s 'cells (e 'miss-cells-ext-s))))
      (let ((s^ (cells-merge x v (hash-set s 'u-f (union u-f (var-name x) (var-name v))))))
        ;; need to rewalk x to get the proper representative - union/cells-merge removes one rep/cell
        (propagate (hash-ref (hash-ref cells (walk x s^) (e 'merge-issue)) 'cs (e 'cell-struct-iss-ext-s))))))
   (else #f)
   ))

(define (cells-merge a b s) s)
;;set operations

;; (define (ext-s x v s)
;;   (cond
;;    ((var? v)
;;     (let ((s^ (make-state (union (get-u-f s) (var-name x) (var-name v))
;;                           (hash-set (get-cells s) x (merge-cells x v s)))));;ordering?
;;       (propagate (get-cell-constraints (get-cell x s^)) s^ (lambda (x) x)))) 
;;    ((occurs? x v s) #f)
;;    (else
;;     (let ((cell (hash-set (empty-cell) 'value v)))
;;       (make-state (get-u-f s) (hash-set (get-cells s) (var-name x) cell))))))

;; (define (occurs? x v s)
;;   (let ((v (walk v s)))
;;     (cond
;;      ((var? v) (var=? x v)) ;;use var=? since we are testing that variables are the same. var = box(number)
;;      ((pair? v) (or (occurs? x (car v) s) (occurs? x (cdr v) s)))
;;      (else #f))))
;; ;;end unification

(define (propagate constraints s empty-k) s) ;; identity

;; ;; begin propagation

;; (define propagate
;;   (lambda (ls s k)
;;     (cond
;;      ((null? ls) (k s))
;;      (else ((car ls) (cdr ls)  k))))) ;; microKanren failure


;; ;; (define (pluso a b c)
;; ;;   (lambda (s/c)
;; ;;     (let ((s^ (fold-right (lambda (x s) (add-constraint-to-cell (pluso-fd a b c) x s))
;; ;;                           (car s/c) (list a b c))))
;; ;;       (let ((p (propagate (get-cell-constraints (get-cell a (car s/c)))
;; ;;                           s/c
;; ;;                           (lambda (x) x))))
;; ;;         (if p p '())))))



;; ;; (define (pluso-fd n m q r)
;; ;;   (build-constraint `(,n ,m ,q ,r)
;; ;;     `((,(n m) ,(q r) ,(lambda (n m) (list (/ n m) (mod n m)))))))

;; (define-syntax build-constraint
;;   (syntax-rules ()
;;     ((_ (cell ...) (((ins ...) (outs ...) f) ...))
;;      (lambda (k)
;;        (lambda (s)
;;          (cond
;;           ((and (not (eqv? 'dummy (get-cell-value ins))) ... (eqv? 'dummy (get-cell-value outs)) ...)
;;            (let ((outs^ (apply f (ins ...))))
;;              (if (null? (outs ...))
;;                  `(some-k)
;;                  `(,(fold-right add-value-to-cells s (outs ...) outs^)
;;                    ,(build-k (cell ...) k)))))
;;           ...
;;           (else #f)))))))

;; (define-syntax build-k
;;   (syntax-rules ()
;;     ((_ () k) k)
;;     ((_ (cell cell1 ...) k)
;;      (build-k (cell1 ...) (lambda (s) (propagate (get-cell-constraints (get-cell cell s) s k)))))))

;; ;; (define (pluso-fd a b c)
;; ;;   (lambda (ls s k)
;; ;;     (cond
;; ;;      ((consistent) ;;easy - check if (eqv? (+ a-val b-val) c-val)
;; ;;       (propagate ls s
;; ;;           (lambda (s)
;; ;;             (propagate (get-cell-constraints (get-cell a s)) s
;; ;;               (lambda (s)
;; ;;                 (propagate (get-cell-constraints (get-cell b s)) s
;; ;;                   (lambda (s)
;; ;;                     (propagate (get-cell-constraints (get-cell a s)) s k))))))))
;; ;;      ((new-information) ;;how to work this backwards? (pluso 5 b 11)
;; ;;       (propagate (get-cell-constraints (get-cell a s)) s
;; ;;          (lambda (s)
;; ;;            (propagate (get-cell-constraints (get-cell b s)) s
;; ;;              (lambda (s)
;; ;;                (propagate (get-cell-constraints (get-cell a s)) s (lambda (x) x)))))))
;; ;;      (else #f))))
