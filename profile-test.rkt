#lang racket
;; (require racket/future future-visualizer)
(require "test-programs.rkt")
(require C311/pmatch)
(provide (all-defined-out) (all-from-out "test-programs.rkt"))

;; profiler section

;; (define profile
;;   (lambda (th)
;;     (visualize-futures th)))

(define-syntax run/time
  (syntax-rules ()
    ((_ th) (time (th)))))

(define-syntax run/stats*
  (syntax-rules ()
    ((_ exp ...)
     (begin
       (begin (run/time (lambda () exp))
              (display "Collecting Garbage ... ")
              (collect-garbage)
              (display "Garbage Collected \n"))
       ...
       (void)))))


(define als (build-list 100 (lambda (x) 'a)))

(define (run-tests)
  (run/stats*
   (run 3 (q) (call/fresh (lambda (a) (pconj (== q a) (pdisj (== a 'a) (== a 'b))))))
   (run 3 (q) (call/fresh (lambda (a) (conj (== q a) (disj (== a 'a) (== a 'b))))))

   (run 1 (q) (reverseo als q))
   (run 1 (q) (preverseo als q))
   )

  )


