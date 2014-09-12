#lang racket/base
(require rackunit rackunit/text-ui "uckanren.rkt")

(define state-tests
  (test-suite
   "state-tests"
   (let ((s (car (empty-state))))
     (test-suite
      "Tests for the creation of the state"
      (test-case
       "We create a state of the correct form"
       (check-pred pair? s) ;; state is a pair
       (check-pred number? (cdr s)) ;; check that the cdr is a number (count)
       (check-pred zero? (cdr s)) ;; count starts at zero and is a pair
       (check-pred hash-eqv? (car s)) ;; ensure it's a hash-table
       (check-pred state? (car s)) ;; self-defined state? pred to test the type
       (check-pred cells? (hash-ref (car s) 'cells #f)))  ;; test we have cells
      
      (let* ((s^ (state-init-cell 0 s))
             (cells (hash-ref s^ 'cells #f))
             (new-cell (hash-ref cells 0 #f)))
        (test-pred
         "Empty State creates a State"
         state? s)

        (test-pred
         "state-init-cell returns a state"
         state? s^)

        (test-not-false
         "Confirm that cells(ht) exists in new-state"
         cells)

        (test-not-false
         "Confirm that state-init-cell creates a key for i/0"
         new-cell)

        (test-pred
         "state-init-cell creates a new cell"
         cell? new-cell)

        (test-case
         "Ensure that the cell was constructed properly"
         (check-pred box? (hash-ref new-cell 'val #f))
         (check-eqv? (unbox (hash-ref new-cell 'val #f)) 'fresh)
         (check-pred null? (hash-ref new-cell 'dom))
         (check-pred null? (hash-ref new-cell 'cs)))))

     (test-suite
      "cell-ref"
      (let* ((s (state-init-cell 1 (state-init-cell 0 s)))
             (c0 (hash-ref (hash-ref s 'cells #f) 0 #f))
             (c1 (hash-ref (hash-ref s 'cells #f) 1 #f)))
        (test-eqv?
         "initial cell-val with cell-ref is 'fresh"
         (unbox (cell-ref c0 'val)) 'fresh)

        (test-pred
         "initial cell-dom with cell-ref is '()"
         null? (cell-ref c0 'dom))

        (test-pred
         "initial cell-cs with cell-ref is '()"
         null? (cell-ref c0 'cs))
        )))
   
   (test-suite
    "allocate"
    (let* ((s/c (allocate (empty-state)))
           (s/c^ (allocate s/c)))

      (test-case
       "First allocation on empty-state successful"
       (check-pred zero? (sub1 (cdr s/c)))           ; count = 1
       (check-pred cell? (hash-ref (hash-ref (car s/c) 'cells #f) 0 #f)))
      
      (test-case
       "Second allocation on empty-state successful"
       (check-pred zero? (sub1 (sub1 (cdr s/c^)))); count = 1
       (check-pred cell? (hash-ref (hash-ref (car s/c^) 'cells #f) 1 #f)))))

   (test-suite
    "walk"
    (let* ((s (car (allocate (allocate (empty-state))))))
      (test-case
       "Constants"
       (check-eqv? (walk 'foo s) 'foo)
       (check-equal? (walk '(a b c) s) '(a b c)))

      (let ((c0 (walk (var 0) s))
            (c1 (walk (var 1) s)))
        (test-pred
         "We can't walk to a newly allocated cell - should defer to rep in u-f"
         (lambda (x) (and (not (cell? x)) (var? x))) c0)

        (let ((s^ (cell-update-val s (walk (var 0) s) 'wat)))
          (let ((c0^ (walk (var 0) s^)))
            (test-pred
             "We can walk to an updated cell"
             cell? c0^)

            (test-eqv?
             "We can get a value out of an updated cell"
             (cell-ref c0^ 'val) 'wat)))
        )))))

(run-tests state-tests)
(define (r) (run-tests state-tests))
