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
    ((_ th) (time (displayln (th))))))

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
(define als2 (build-list 500 (lambda (x) 'a)))

(define (run-tests)
  (run/stats*
   (run 3 (q) (call/fresh (lambda (a) (pconj (== q a) (pdisj (== a 'a) (== a 'b))))))
   (run 3 (q) (call/fresh (lambda (a) (conj (== q a) (disj (== a 'a) (== a 'b))))))

   (run 1 (q) (reverseo als q))
   (run 1 (q) (preverseo als q))

   
   (vof `(letrec ((appendo
                  (lambda (l s o)
                    (disj
                     (conj (== l '()) (== o s))
                     (call/fresh
                      (lambda (a)
                        (call/fresh
                         (lambda (b)
                           (call/fresh
                            (lambda (res)
                              (conj (== l (cons a b))       ;;`(,a . ,b) 
                                    (conj (== o (cons a res)) ;; `(,a . ,res)
                                          (lambda (s/c)
                                            (lambda ()
                                              ((appendo b s res) s/c)))))))))))))))
          (letrec ((reverseo
                    (lambda (ls o)
                      (disj
                       (conj (== ls '()) (== o ls))
                       (call/fresh
                        (lambda (a)
                          (call/fresh
                           (lambda (b)
                             (call/fresh
                              (lambda (res)
                                (conj (== (cons a b) ls) ;; `(,a . ,b)
                                      (conj  (lambda (s/c)
                                               (lambda ()
                                                 ((reverseo b res) s/c)))
                                             (lambda (s/c)
                                               (lambda ()
                                                 ((appendo res (cons a '()) o) s/c)))))))))))))))
            (run 1 (q) (reverseo (quote ,als) q)))) '())

   (vof `(letrec ((appendo
                  (lambda (l s o)
                    (pdisj
                     (conj (== l '()) (== o s))
                     (call/fresh
                      (lambda (a)
                        (call/fresh
                         (lambda (b)
                           (call/fresh
                            (lambda (res)
                              (conj (== l (cons a b))       ;;`(,a . ,b) 
                                    (conj (== o (cons a res)) ;; `(,a . ,res)
                                          (lambda (s/c)
                                            (lambda ()
                                              ((appendo b s res) s/c)))))))))))))))
          (letrec ((reverseo
                    (lambda (ls o)
                      (pdisj
                       (conj (== ls '()) (== o ls))
                       (call/fresh
                        (lambda (a)
                          (call/fresh
                           (lambda (b)
                             (call/fresh
                              (lambda (res)
                                (conj (== (cons a b) ls) ;; `(,a . ,b)
                                      (conj  (lambda (s/c)
                                               (lambda ()
                                                 ((reverseo b res) s/c)))
                                             (lambda (s/c)
                                               (lambda ()
                                                 ((appendo res (cons a '()) o) s/c)))))))))))))))
            (run 1 (q) (reverseo (quote ,als) q)))) '())
   ))
