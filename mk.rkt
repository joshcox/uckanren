#lang racket
(require C311/trace C311/pmatch)
(require "vector-var-a-list-substitution.rkt")
(require data/queue)
(require web-server/lang/serial-lambda
         racket/serialize)
(provide (all-defined-out))

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define place* (make-parameter '()))
(define receiver (make-parameter #f))

(define make-places
  (lambda (num)
    (place* (for/list ((i (min num (sub1 (processor-count)))))
              (place receiver
                     (let ()
                       (define pdisj disj)
                       (define (main)
                         (let ((id (place-channel-get receiver))
                               (sender (place-channel-get receiver)))
                           (let f ()
                             (let ((job (place-channel-get receiver)))
                               (if (eqv? job 'done) (void)
                                   (let* ((job (deserialize job))
                                          (job-id (car job))
                                          (work (cdr job))
                                          (s/c (deserialize (place-channel-get receiver)))
                                          (package (cons id (cons job-id (work s/c)))))
                                     (place-channel-put sender (serialize package))
                                     (f)))))))
                       (main)))))
    (let-values (((rec sen) (place-channel)))
      (receiver rec)
      (for ((place (place*)) (i (in-range num))) (place-channel-put place i) (place-channel-put place sen)))))

(define pdisj
  (lambda jobs
    (serial-lambda (s/c)
      (let* ((jobs-num (length jobs))
           (q (make-queue))
           (jobs-queue (begin (for ((job jobs) (i (in-range jobs-num))) (enqueue! q (cons i job))) q))
           (jobs (make-hash))
           (s/c (serialize s/c)))
      (make-places (sub1 jobs-num))
      (let* ((ch (make-channel))
             (appender (thread (lambda ()
                                 (let f (($ '()) ($1 (channel-get ch)))
                                   (if (eqv? $1 'done)
                                       (begin (hash-set! jobs -1 $) (channel-put ch -1))
                                       (f ($-append (hash-ref jobs $1) $) (channel-get ch)))))))
             (sendj (lambda (place)
                      (if (queue-empty? jobs-queue) (void)
                          (let ((job (dequeue! jobs-queue)))
                            (hash-set! jobs (car job) #f)
                            (place-channel-put place (serialize job))
                            (place-channel-put place s/c))))))
        (for ((place (place*))) (sendj place))
        (let f ()
          (let ((msg (place-channel-get (receiver))))
            (let ((msg (deserialize msg)))
              (pmatch msg
                      (`(,place ,job-id . ,ans)
                       (begin (sendj (list-ref (place*) place))
                              (hash-set! jobs job-id ans)
                              (set! jobs-num (sub1 jobs-num))
                              (channel-put ch job-id))))))
          
          (if (zero? jobs-num)
              (begin (for ((place (place*))) (place-kill place))
                     (channel-put ch 'done)
                     (hash-ref jobs (channel-get ch)))
              (f))))))))

(define (call/empty-state g) (g (empty-state)))

(define (pull $) (if (procedure? $) (pull ($)) $))

(define (take n)
  (lambda ($)
    (cond
      ((zero? n) '())
      (else
       (let (($ (pull $)))
         (cond
           ((null? $) '())
           (else
            (cons (car $)
             ((take (- n 1)) (cdr $))))))))))



;;impure microKanren extensions
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


;; miniKanren
(define-syntax inverse-eta-delay
  (syntax-rules ()
    ((_ g) (serial-lambda (s/c) (serial-lambda () (g s/c))))))

;; (define-syntax conj+
;;   (syntax-rules ()
;;     ((_ g) g)
;;     ((_ g0 g ...) (conj g0 (conj+ g ...)))))

(define conj+ conj)

(define-syntax disj+
  (syntax-rules ()
    ((_ g) g)
    ((_ g0 g ...) (disj g0 (disj+ g ...)))))

(define-syntax pconj+
  (syntax-rules ()
    ((_ g) g)
    ((_ g0 g ...) (pconj g0 (conj+ g ...)))))

(define-syntax fresh
  (syntax-rules ()
    ((_ () g0 g ...)
     (inverse-eta-delay (conj+ g0 g ...)))
    ((_ (x0 x ...) g0 g ...)
     (call/fresh (serial-lambda (x0) (fresh (x ...) g0 g ...))))))

(define-syntax pconde
  (syntax-rules ()
    ((_ (g0 g ...) (g0* g* ...) ...)
     (inverse-eta-delay
      (let ((pdisj disj) (disj pdisj))
        (disj (conj+ g0 g ...) (conj+ g0* g* ...) ...))))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) (g0* g* ...) ...)
     (inverse-eta-delay
      (disj+ (conj+ g0 g ...) (conj+ g0* g* ...) ...)))))

(define-syntax run
  (syntax-rules ()
    ((_ n (q) g0 g ...)
     (map reify-var0
          ((take n)
           (call/empty-state (fresh (q) g0 g ...)))))))


;; impure minikanren extensions
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
(define conj-n-ary
  (lambda goals
    (serial-lambda (s/c)
      (let ((s/c ((car goals) s/c)))
        (foldl (lambda (f x) ($-append-map f x)) s/c (cdr goals))))))

(define c$a (curry $-append))
