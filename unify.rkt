#lang racket
(require C311/trace)
(require "state.rkt")
(provide var var? var=?
         unify
         walk val-walk
         set-cell-value
         allocate
         empty-state/u-f)

(trace-define (unify u^ v^ s)
  (let ((u (val-walk u^ s))
        (v (val-walk v^ s)))
    (cond
     ((eq? u v) s) ;;same box, same cell-val
     ((var? u) (ext-s u v s))
     ((var? v) (ext-s v u s))
     ((and (pair? u) (pair? v))
      (let ((s (unify (car u) (car v) s)))
        (and s (unify (cdr u) (cdr v) s))))
     (else (and (eqv? u v) s))))) ;;this line?

(trace-define ext-s
  (lambda (x v s)
    (cond
     ((var? v)
      (make-state (union (state-u-f s)
                         (var-name x)
                         (var-name v))
                  (state-cells s)))
     ((occurs? x v s) #f)
     (else
      (make-state (state-u-f s)
                  (set-cell-value x v s))))))

(trace-define occurs?
  (lambda (x v s)
    (let ((v (val-walk v s)))
      (cond
        ((var? v) (var=? x v))
        ((pair? v) (or (occurs? x (car v) s)
                       (occurs? x (cdr v) s)))
        (else #f)))))
