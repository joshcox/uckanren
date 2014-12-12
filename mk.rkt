5#lang racket
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

;; miniKanren


(define-syntax pconj+
  (syntax-rules ()
    ((_ g) g)
    ((_ g0 g ...) (pconj g0 (conj+ g ...)))))



(define-syntax pconde
  (syntax-rules ()
    ((_ (g0 g ...) (g0* g* ...) ...)
     (inverse-eta-delay
      (let ((pdisj disj) (disj pdisj))
        (disj (conj+ g0 g ...) (conj+ g0* g* ...) ...))))))




;; impure minikanren extensions
(define conj-n-ary
  (lambda goals
    (serial-lambda (s/c)
      (let ((s/c ((car goals) s/c)))
        (foldl (lambda (f x) ($-append-map f x)) s/c (cdr goals))))))

(define c$a (curry $-append))
