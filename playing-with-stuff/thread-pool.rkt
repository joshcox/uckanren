#lang racket
;(require racket/threads)

(define (thread-pool n)
  (lambda (proc items)
    (define tasks (make-channel))
    (define results (make-semaphore))
    (define threads #f)
    (dynamic-wind
      (lambda () (set! threads 
                       (build-list n 
                         (lambda (i) 
                           (thread 
                            (lambda ()
                              (let f ()
                                (let ((v (channel-get tasks))) (proc v))
                                (semaphore-post results)
                                (f))))))))
      (lambda ()
        (for-each
         (lambda (item)
           (channel-put tasks item))
         items)
        (for-each
         (lambda (item)
           (semaphore-wait results))
         items)
        )
      (lambda ()
        (for-each
         (lambda (t)
           (kill-thread t)
           (thread-wait t))
         threads)))))

(define (example)
  (define pool-for-each (thread-pool 20))
  (pool-for-each
   (lambda (i) 
     ;(sleep 1)
     (display i) (display "-")
     (flush-output))
   (build-list 100 (lambda (i) (- 100 i)))))

(provide thread-pool example)
