#lang racket
(require C311/trace)
(require (only-in data/p-union-find
                  make-initial-u-f union find allocate-new-index))

(provide allocate unify var var? walk empty-state)


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
