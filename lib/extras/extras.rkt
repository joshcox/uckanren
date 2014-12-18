#lang racket

(define succeed (lambda (s/c) (list s/c)))

(define fail (lambda (s/c) `()))

(define (ifte g0 g1 g2)
  (lambda (s/c)
    (let loop (($ (g0 s/c)))
      (cond
        ((procedure? $) (lambda () (loop ($))))
        ((null? $) (g2 s/c))
        (else ($-append-map g1 $))))))

(define (once g)
  (lambda (s/c)
    (let loop (($ (g s/c)))
      (cond
        ((procedure? $) (lambda () (loop ($))))
        ((null? $) `())
        (else (list (car $)))))))

;; (define (call/project x f)
;;   (lambda (s/c)
;;     ((f (walk* x (car s/c))) s/c)))

(define-syntax inverse-eta-delay
  (syntax-rules ()
    ((_ g) (serial-lambda (s/c) (serial-lambda () (g s/c))))))

(define-syntax project
  (syntax-rules ()
    ((_ () g0 g ...) (conj+ g0 g ...))
    ((_ (x0 x ...) g0 g ...)
     (call/project x0 
       (lambda (x0) (project (x ...) g0 g ...))))))

(define-syntax ifte*
  (syntax-rules ()
    ((_ (g0 g ...)) (conj+ g0 g ...))
    ((_ (g0 g1 g ...) (h0 h ...) ...)
     (ifte g0 (conj+ g1 g ...) (ifte* (h0 h ...) ...)))))

(define-syntax conda
  (syntax-rules ()
    ((_ (g0 g ...) (h0 h ...) ...)
     (inverse-eta-delay
      (ifte* (g0 g ... succeed) (h0 h ... succeed) ... (fail))))))

(define-syntax condu
  (syntax-rules ()
    ((_ (g0 g ...) (h0 h ...) ...)
     (conda ((once g0) g ...) ((once h0) h ...) ...))))
