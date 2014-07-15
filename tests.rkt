#lang racket
(require "mK.rkt" "test-programs.rkt")

(define-syntax multi-test
  (syntax-rules ()
    ((_ title tested-expression expected-result* ...)
     (let* ((produced tested-expression))
       (cond
        [(equal? produced expected-result*) (printf "~s works!\n" title)]
        ...
        [else (error
               'test
               "Failed ~s: ~s~nComputed: ~s~nExpected one of: ~s~n"
               title 'tested-expression produced (list
                                                  expected-result* ...))])))))


(multi-test "appendo"
  (run* (q) (appendo '(1 2 3) '(4 5 6) q))
  '((1 2 3 4 5 6)))
