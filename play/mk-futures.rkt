#lang racket
(require racket/place racket/fasl racket/match)
(provide (all-defined-out))

;; initializes n-1 new places where n = processor-count
(define init-work-stealing-rts
  (lambda (name-string main-symbol th) 
    (let ((place* (for/list ((i (sub1 (processor-count)))) 
                    (dynamic-place name-string main-symbol))))
      (let ((len (range 1 (length place*))))
          (let ((worker-list (for/list ((p place*) (i len)) (cons i p)))) 
         (for ((p place*) (i len)) 
           (place-channel-put p (remove i worker-list (lambda (x y) (eqv? x (car y)))))
           (place-channel-put p i))))
      (th))))

(define place-channel-put-s-exp
  (lambda (ch s-exp)
    (place-channel-put ch (s-exp->fasl s-exp))))

(define place-channel-get-s-exp
  (lambda (ch)
    (let ((s (fasl->s-exp (place-channel-get ch))))
      (begin (displayln s) s))))

(define place-main
  (lambda (ch)
    (let ((places (cons (cons 0 ch) (place-channel-get ch)))
          (place-id (place-channel-get ch))
          (work-queue (make-queue))
          (workers '())
          (idle-workers (make-queue)))
      (letrec ((add-to-work-queue (lambda (x) (enqueue! work-queue x)))
               (add-to-front-of-work-queue (lambda (x) (enqueue-front! work-queue x)))
               (message-handler 
                (thread (lambda ()
                          (let f ()
                            ((lambda (msg)
                               (match msg
                                 ((list place-id 'job job) (add-to-work-queue job))
                                 ((list place-id 'has-extra?) #f)))
                             (place-channel-get ch))))))
               (make-worker-thread (lambda () (thread (lambda () )))))
        (let loop ()
          )
        )
      )))

(define test-main
  (lambda (ch) 
    (let ((ans (place-channel-get ch)))
      (let ((f (place-channel-get-s-exp ch))) 
        (let loop ()
          (let ((work (place-channel-get ch)))
            (let ((work (parameterize ((current-namespace (make-base-empty-namespace)))
                          (namespace-require 'racket)
                          ((eval f) work)))) 
              (place-channel-put ans work)
              (loop))))))))

(define (go)
  (init-vms "mk-futures.rkt" 'test-main))
