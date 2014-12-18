#lang racket
(require "../cKanren.rkt" "cKanren-test-programs.rkt")
(provide main)

(define main
  (lambda ()
    (run 5 (q) (appendo '(1 2 3) '(4 5) q))))
