#lang racket
(provide empty-state make-a reify-var0 var var? veqv? walk walk*)

(define empty-s '())
(define empty-d '())
(define empty-c '())

(define make-a (lambda (s d c) (cons s (cons d c))))

(define empty-a (make-a empty-s empty-d empty-c))

(define (var n) (vector n))
(define (var? n) (vector? n))
(define (unvar n) (vector-ref n 0))
(define (var-eqv? u v) (and (var? u) (var? v) (eqv? (unvar u) (unvar v))))
(define veqv? (lambda (x y) (or (eqv? x y) (var-eqv? x y))))

(define (empty-state) (cons empty-a 0))

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
(define (reify-var0 s/c)
  (let ((v (walk* (var 0) (car s/c))))
    (walk* v (reify-s v '()))))

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
