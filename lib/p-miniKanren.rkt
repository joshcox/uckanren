#lang racket
(require "miniKanren-base/p-miniKanren-base.rkt"
         "numbers/p-numbers.rkt")
(provide (all-from-out "miniKanren-base/p-miniKanren-base.rkt")
         (all-from-out "numbers/p-numbers.rkt")
         main)

(define (main)
  (run 1 (q) (== q 1)))

