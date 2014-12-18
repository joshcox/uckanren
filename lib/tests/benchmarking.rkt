#lang racket
(provide make-benchmark-suite benchmark run-benchmark-suite run-benchmark-suite* mk-runner)

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
(define mk-runner (runners (s n a c r g) write-answer std-runner print-answer))
(define write-answer 
  (runner (s n a c r g)
    (let ((f (open-output-file 
              (build-path (current-directory) "benchmark-results" (string-append n ".txt"))
              #:exists 'append)))
      (write r f)(newline f))))
