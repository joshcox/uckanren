#lang racket/base
(require rackunit rackunit/text-ui "../cKanren.rkt" "cKanren-test-programs.rkt")
(provide (all-defined-out))

(define member?
  (lambda (item)
    (lambda (list)
      (if (member item list) #t #f))))

(define member*?
  (lambda (item*)
    (lambda (list)
      (cond
       ((null? item*) #t)
       (else (and ((member? (car item*)) list) ((member*? (cdr item*)) list)))))))

(define miniKanren-Tests
  (test-suite
   "mK Tests"
   (test-suite
    "Appendo"

    (test-pred
     "Appendo1"
     (member? '(1 2 3 4 5)) (run 5 (q) (appendo '(1 2 3) '(4 5) q)))

    (test-pred
     "Appendo can go backwards 1"
     (member? '(4 5))
     (run 5 (q) (appendo '(1 2 3) q '(1 2 3 4 5))))

    (test-pred
     "Appendo can go backward 2"
     (member? '(1 2 3))
     (run 5 (q) (appendo q '(4 5) '(1 2 3 4 5))))

    (let ((a (run 5 (q) (fresh (a b c) (== q `(,a ,b ,c)) (appendo a b c)))))
      (test-pred
       "Appendo can generate lists"
       
       (member*? '((() _.0 _.0)
          ((_.0) _.1 (_.0 . _.1))
          ((_.0 _.1) _.2 (_.0 _.1 . _.2))
          ((_.0 _.1 _.2) _.3 (_.0 _.1 _.2 . _.3))
          ((_.0 _.1 _.2 _.3) _.4 (_.0 _.1 _.2 _.3 . _.4))))
       a)))
   
   (test-suite
    "Reverseo"

    (test-pred
     "Find the reverse of a list"
     (member? '(5 4 3 2 1)) (run 1 (q) (reverseo '(1 2 3 4 5) q)))

    (test-pred
     "Find the list of a reverse"
     (member? '(5 4 3 2 1)) (run 1 (q) (reverseo q '(1 2 3 4 5))))

    (test-pred
     "Generate reversed lists"
     (member*? '(() (_.0) (_.0 _.1) (_.0 _.1 _.2) (_.0 _.1 _.2 _.3)))
     (run 5 (q) (fresh (a) (reverseo a q))))
    )

   (test-suite
    "Factorial"

    (test-pred
     "What is Fact 5?"
     (member? (build-num 120))
     (run 5 (q) (facto (build-num 5) q)))

    (test-pred
     "What yields 120 (and not everything else in between?)"
     (member? (build-num 5))
     (run 1 (q) (facto q (build-num 120))))

    ;; (test-pred
    ;;  "What yields 120 (and everything else in between?)"
    ;;  (member? (build-num 5))
    ;;  (run* (q) (facto q (build-num 120))))
    )
   ))

(run-tests miniKanren-Tests)
(define (r) (run-tests miniKanren-Tests))

;; (define state-tests
;;   (test-suite
;;    "state-tests"
;;    (let ((s (car (empty-state))))
;;      (test-suite
;;       "Tests for the creation of the state"
;;       (test-case
;;        "We create a state of the correct form"
;;        (check-pred pair? s) ;; state is a pair
;;        (check-pred number? (cdr s)) ;; check that the cdr is a number (count)
;;        (check-pred zero? (cdr s)) ;; count starts at zero and is a pair
;;        (check-pred hash-eqv? (car s)) ;; ensure it's a hash-table
;;        (check-pred state? (car s)) ;; self-defined state? pred to test the type
;;        (check-pred cells? (hash-ref (car s) 'cells #f)))  ;; test we have cells
      
;;       (let* ((s^ (state-init-cell 0 s))
;;              (cells (hash-ref s^ 'cells #f))
;;              (new-cell (hash-ref cells 0 #f)))
;;         (test-pred
;;          "Empty State creates a State"
;;          state? s)

;;         (test-pred
;;          "state-init-cell returns a state"
;;          state? s^)

;;         (test-not-false
;;          "Confirm that cells(ht) exists in new-state"
;;          cells)

;;         (test-not-false
;;          "Confirm that state-init-cell creates a key for i/0"
;;          new-cell)

;;         (test-pred
;;          "state-init-cell creates a new cell"
;;          cell? new-cell)

;;         (test-case
;;          "Ensure that the cell was constructed properly"
;;          (check-pred box? (hash-ref new-cell 'val #f))
;;          (check-eqv? (unbox (hash-ref new-cell 'val #f)) 'fresh)
;;          (check-pred null? (hash-ref new-cell 'dom))
;;          (check-pred null? (hash-ref new-cell 'cs)))))

;;      (test-suite
;;       "cell-ref"
;;       (let* ((s (state-init-cell 1 (state-init-cell 0 s)))
;;              (c0 (hash-ref (hash-ref s 'cells #f) 0 #f))
;;              (c1 (hash-ref (hash-ref s 'cells #f) 1 #f)))
;;         (test-eqv?
;;          "initial cell-val with cell-ref is 'fresh"
;;          (unbox (cell-ref c0 'val)) 'fresh)

;;         (test-pred
;;          "initial cell-dom with cell-ref is '()"
;;          null? (cell-ref c0 'dom))

;;         (test-pred
;;          "initial cell-cs with cell-ref is '()"
;;          null? (cell-ref c0 'cs))
;;         )))
   
;;    (test-suite
;;     "allocate"
;;     (let* ((s/c (allocate (empty-state)))
;;            (s/c^ (allocate s/c)))

;;       (test-case
;;        "First allocation on empty-state successful"
;;        (check-pred zero? (sub1 (cdr s/c)))           ; count = 1
;;        (check-pred cell? (hash-ref (hash-ref (car s/c) 'cells #f) 0 #f)))
      
;;       (test-case
;;        "Second allocation on empty-state successful"
;;        (check-pred zero? (sub1 (sub1 (cdr s/c^)))); count = 1
;;        (check-pred cell? (hash-ref (hash-ref (car s/c^) 'cells #f) 1 #f)))))

;;    (test-suite
;;     "walk"
;;     (let* ((s (car (allocate (allocate (empty-state))))))
;;       (test-case
;;        "Constants"
;;        (check-eqv? (walk 'foo s) 'foo)
;;        (check-equal? (walk '(a b c) s) '(a b c)))

;;       (let ((c0 (walk (var 0) s))
;;             (c1 (walk (var 1) s)))
;;         (test-pred
;;          "We can't walk to a newly allocated cell - should defer to rep in u-f"
;;          (lambda (x) (and (not (cell? x)) (var? x))) c0)

;;         (let ((s^ (cell-update-val s (walk (var 0) s) 'wat)))
;;           (let ((c0^ (walk (var 0) s^)))
;;             (test-pred
;;              "We can walk to an updated cell"
;;              cell? c0^)

;;             (test-eqv?
;;              "We can get a value out of an updated cell"
;;              (cell-ref c0^ 'val) 'wat)))
;;         )))))

;; (run-tests state-tests)
;; (define (r) (run-tests state-tests))
