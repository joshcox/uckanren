#lang racket
(require "../cKanren.rkt")
(provide main)

(define-syntax print-all
  (syntax-rules ()
    ((_ x ...) (begin (displayln x) ...))))

(define (main)
  (print-all
   (run 1 (q) (=/= q 5))
   (run 1 (q) (conj (=/= q 5) (== q 6)))
   (run 1 (q) (conj (=/= q 5) (== q 5))))
  )
