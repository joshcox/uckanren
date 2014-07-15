# ucKanren

Usage:

1. (require "miniKanren.rkt")
2. (define appendo
  (lambda (l s out)
    (disj
      (conj (== '() l) (== s out))
      (call/fresh
        (lambda (a)
          (call/fresh
            (lambda (d)
              (conj
                (== `(,a . ,d) l)
                (call/fresh
                  (lambda (res)
                    (conj
                      (== `(,a . ,res) out)
                      (lambda (s/c)
                        (lambda ()
                          ((appendo d s res) s/c))))))))))))))

3. (define s ((call/fresh (lambda (q) (appendo '(1 2 3) '(4 5 6) q))) '(() . 0)))
4. (((s)))
5. Trace var 0 to see the appended list



OR



1. (require "mK.rkt")
2. (define s (run 1 (q) (appendo '(1 2 3) '(4 5 6) q)))
3. (((s)))

to see the reified version.
