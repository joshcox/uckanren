#lang racket
(require "mk.rkt" "extras-mk.rkt" "test-programs.rkt")
(require C311/pmatch)
;(require profile)
;(require profile/render-text)
(provide (all-defined-out) (all-from-out "test-programs.rkt"))

(define-syntax-rule (run/time th) (time-apply th '()))
(define-syntax-rule (benchmark str bench) (cons str (lambda () bench)))
(define-syntax-rule (run-benchmark-suite* runner b* ...) 
  (begin (begin (printf "~a:~n" (car b*)) (run-benchmark-suite b* runner) (newline)) ...))
(define make-benchmark-suite list)

(define run-benchmark-suite
  (lambda (benchmarks runner)
    (let ((suite-name (car benchmarks)))
      (for ((b (cdr benchmarks)))
        (begin
          (let-values (((ans cpu real gc) (run/time (cdr b))))
              (runner suite-name (car b) (car ans) cpu real gc))
          (collect-garbage))))))

;; Runners :: (suite-name, bench-name, ans, cpu-time, real-time, gc-time) -> something
(define-syntax-rule (runner (v ...) exp) (lambda (v ...) exp))
(define-syntax-rule (runners (v ...) r ...) (lambda (v ...) (begin (r v ...) ...)))

(define std-runner (runner (s n a c r g) (printf "\t~a ~n\t  Cpu: ~a Real: ~a GC: ~a~n" n c r g)))
(define print-answer (runner (s n a c r g) (printf "\t  ~a~n" a)))
(define mk-runner (runners (s n a c r g) std-runner print-answer))

;; MicroKanren Benchmarking
(define a '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
(define ls25 (build-list 25 (lambda (i) (list-ref a (modulo i 26)))))
(define ls50 (build-list 50 (lambda (i) (list-ref a (modulo i 26)))))
(define ls100 (build-list 100 (lambda (i) (list-ref a (modulo i 26)))))
(define ls2 (build-list 200 (lambda (i) (list-ref a (modulo i 26)))))

(time (init-place))

(define microKanren-benchmarks
  (make-benchmark-suite
   "microKanren - nonInterpreter"
   
   ;(benchmark "Appendo" (run 1 (q) (pappendo2 '(a b) '(c d) q)))
   (benchmark "Reverseo ~~ 25" (run 1 (q) (reverseo ls25 q)))
   (benchmark "Concurrent Reverseo ~~ 25" (run 1 (q) (preverseo ls25 q)))
   (benchmark "Reverseo ~~ 50" (run 1 (q) (reverseo ls50 q)))
   (benchmark "Concurrent Reverseo ~~ 50" (run 1 (q) (preverseo ls50 q)))
   (benchmark "Reverseo ~~ 100" (run 1 (q) (reverseo ls100 q)))
   (benchmark "Concurrent Reverseo ~~ 100" (run 1 (q) (preverseo ls100 q)))
   ;; (benchmark "Reverseo ~~ 200"(run 1 (q) (reverseo ls2 q)))
   ;; (benchmark "Concurrent Reverseo ~~ 200" (run 1 (q) (preverseo ls2 q)))
   ))

;; (define microKanren-interpreter-benchmarks
;;   (make-benchmark-suite
;;    "microKanren - Interpreter"
;;    (benchmark "Reverseo ~~ 100" (mk `(run 1 (q) (reverseo (quote ,ls1) q))))
;;    (benchmark "Concurrent Reverseo ~~ 100" (mk `(run 1 (q) (preverseo (quote ,ls1) q))))
;;    (benchmark "Reverseo ~~ 200" (mk `(run 1 (q) (reverseo (quote ,ls2) q))))
;;    (benchmark "Concurrent Reverseo ~~ 200" (mk `(run 1 (q) (preverseo (quote ,ls2) q))))))

;; (define (run-all-benchmarks)
;;   (run-benchmark-suite* mk-runner microKanren-benchmarks microKanren-interpreter-benchmarks))

(define (main)
                                        ;(run-all-benchmarks)
  (run-benchmark-suite* mk-runner microKanren-benchmarks))

;; (define (main)
;;   (profile-thunk run-all-benchmarks #:render render))
