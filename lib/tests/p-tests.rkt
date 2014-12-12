#lang racket
(require "../miniKanren-base/p-miniKanren-base.rkt"
         "../numbers/p-numbers.rkt"
         "p-test-programs.rkt"
         "benchmarking.rkt")
(provide main)

;; MicroKanren Benchmarking
(define a '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
(define ls25 (build-list 25 (lambda (i) (list-ref a (modulo i 26)))))
(define ls50 (build-list 50 (lambda (i) (list-ref a (modulo i 26)))))
(define ls100 (build-list 100 (lambda (i) (list-ref a (modulo i 26)))))
(define ls2 (build-list 200 (lambda (i) (list-ref a (modulo i 26)))))
(define num1000 (build-num 1000))
(define num2000 (build-num 2000))
(define num10000 (build-num 10000))

(define microKanren-benchmarks
  (make-benchmark-suite
   "microKanren - nonInterpreter"

   (benchmark "Parellel 1000^2" (run 1 (q) (*o num1000 num1000 q)))

   (benchmark "Parellel 2000^2" (run 1 (q) (*o num2000 num2000 q)))

   (benchmark "Parellel 10000^2" (run 1 (q) (*o num10000 num10000 q)))
   
   (benchmark "two big items"
              (run 2 (q) (pdisj (reverseo ls50 q) (reverseo ls50 q))))
   
   
   ;; (benchmark "Reverseo ~~ 25" (run 1 (q) (reverseo ls25 q)))
   ;; (benchmark "Concurrent Reverseo ~~ 25" (run 1 (q) (preverseo ls25 q)))
   
   ;; (benchmark "Reverseo ~~ 50" (run 1 (q) (reverseo ls50 q)))
   ;; (benchmark "Concurrent Reverseo ~~ 50" (run 1 (q) (preverseo ls50 q)))
   
   ;(benchmark "Appendo" (run 1 (q) (pappendo2 '(a b) '(c d) q)))
   ;; (benchmark "Reverseo ~~ 100" (run 1 (q) (reverseo ls100 q)))
   ;; (benchmark "Concurrent Reverseo ~~ 100" (run 1 (q) (preverseo ls100 q)))
   ;; (benchmark "Reverseo ~~ 200"(run 1 (q) (reverseo ls2 q)))
   ;; (benchmark "Concurrent Reverseo ~~ 200" (run 1 (q) (preverseo ls2 q)))
   ))

(define (main)
  (run-benchmark-suite* mk-runner microKanren-benchmarks))

