#lang racket
(require "miniKanren-base/cKanren-miniKanren-base.rkt"
         "numbers/cKanren-numbers.rkt"
         "neq/cKanren-neq.rkt")
(provide (all-from-out "miniKanren-base/cKanren-miniKanren-base.rkt")
         (all-from-out "numbers/cKanren-numbers.rkt")
         (all-from-out "neq/cKanren-neq.rkt"))

(useneq)
