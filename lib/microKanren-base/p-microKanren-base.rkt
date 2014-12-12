#lang racket
(require (only-in "../state/p-state.rkt" var empty-state reify-var0)
         (only-in "../unification/p-unification.rkt" ==)
         (only-in web-server/lang/serial-lambda serial-lambda)
         data/queue
         racket/serialize
         (only-in racket/match match))
(provide $-append $-append-map
         pdisj disj conj call/fresh  == run 
         inverse-eta-delay reify-var0 take call/empty-state empty-state)

(define ($-append $1 $2)
  (cond
    ((procedure? $1) (serial-lambda () ($-append $2 ($1)))) 
    ((null? $1) $2)
    (else (cons (car $1) ($-append (cdr $1) $2)))))

(define ($-append-map g $)
  (cond
    ((procedure? $) (serial-lambda () ($-append-map g ($))))
    ((null? $) `())
    (else ($-append (g (car $)) ($-append-map g (cdr $))))))

;; (define (disj g1 g2) 
;;   (serial-lambda (s/c)
;;     ($-append (g1 s/c) (g2 s/c))))

(define disj
  (lambda goals
    (serial-lambda (s/c)
      (foldr $-append '() (map (lambda (g) (g s/c)) goals)))))

;; (define (conj g1 g2) 
;;   (serial-lambda (s/c)
;;     ($-append-map g2 (g1 s/c))))

(define conj
  (lambda goals
    (serial-lambda (s/c)
      (let ((s/c ((car goals) s/c)))
        (foldl (lambda (f x) ($-append-map f x)) s/c (cdr goals))))))

(define (call/fresh f)
  (serial-lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) (cons (car s/c) (+ 1 c))))))

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

(define-syntax run
  (syntax-rules ()
    ((_ n (q) g)
     (map reify-var0 ((take n) (call/empty-state (call/fresh (lambda (q) g))))))))

(define-syntax inverse-eta-delay
  (syntax-rules ()
    ((_ g) (serial-lambda (s/c) (serial-lambda () (g s/c))))))

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
      (for ((place (place*)) (i (in-range num))) 
        (place-channel-put place i) 
        (place-channel-put place sen)))))

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
              (match msg
                ((cons place (cons job-id ans))
                 (begin (sendj (list-ref (place*) place))
                        (hash-set! jobs job-id ans)
                        (set! jobs-num (sub1 jobs-num))
                        (channel-put ch job-id))))))
          (if (zero? jobs-num)
              (begin (for ((place (place*))) (place-kill place))
                     (channel-put ch 'done)
                     (hash-ref jobs (channel-get ch)))
              (f))))))))
