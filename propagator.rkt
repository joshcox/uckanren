#lang racket
(require C311/trace)
(provide (all-defined-out))

(define nothing (list '*the-nothing*))

(define listify (lambda (x) (if (list? x) x (list x))))

(define (nothing? thing) (eq? thing nothing))

(define default-equal? eqv?)

(define (alert-propagators props)
  (for-each (lambda (x) (x)) (listify props)))

(define (any pred ls)
  (cond
    ((null? ls) #f)
    ((pred (car ls)) #t)
    (else (any pred (cdr ls)))))

(define (every pred ls)
  (cond
    ((null? ls) #t)
    ((not (pred (car ls))) #f)
    (else (every pred (cdr ls)))))

(define (except-last-pair ls)
  (if (null? (cdr ls))
    '()
    (cons (car ls) (except-last-pair (cdr ls)))))

(define MY-GLOBAL-CT 0)

(define cntr
  (lambda ()
    (let ((c MY-GLOBAL-CT))
      (set! MY-GLOBAL-CT (add1 MY-GLOBAL-CT))
      c)))

(define (make-cell name)
  (let ((neighbors '()) (content nothing) (name name) (count (cntr)))
    (define (new-neighbor! new-neighbor)
      (unless (memq new-neighbor neighbors)
        (begin
          (set! neighbors (cons new-neighbor neighbors))
          (alert-propagators new-neighbor))))
    (define (add-content increment)
      (cond ((nothing? increment) 'ok)
            ((nothing? content)
             (set! content increment)
             (alert-propagators neighbors))
            (else
              (unless (default-equal? content increment)
                (error "Ack! Inconsistency!")))))
    (define (me message)
      (cond ((eq? message 'new-neighbor!) new-neighbor!)
            ((eq? message 'add-content) add-content)
            ((eq? message 'content)
             ;;(display (me 'id))
             ;;(newline)
             content)
            ((eq? message 'id) (cons name count))
            (else (error "Unknown message" message))))
    me))

(define (new-neighbor! cell neighbor) ((cell 'new-neighbor!) neighbor))
(define (add-content cell increment) ((cell 'add-content) increment))
(define (content cell) (cell 'content))

(define compound-propagator 
  (lambda (neighbors to-build)
    (let ((done? #f) (neighbors (listify neighbors)))
      (define (test)
        (if done? 'ok
          (if (every nothing? (map content neighbors))
            'ok1
            (begin (set! done? #t)
                   (to-build)))))
      (propagator neighbors test))))

(define (propagator neighbors to-do)
  (for-each (lambda (cell) (new-neighbor! cell to-do)) (listify neighbors))
  (alert-propagators to-do));;run to-do, schedule to-do to be rerun if
;;any /neighbors/ content changes 

(define (conditional p if-true if-false output)
  (propagator 
    (list p if-true if-false)
    (lambda ()
      (let ((predicate (content p)))
        (if (nothing? predicate)
          'done ;;unnecessary? replace with one-armed if/unless
          (add-content output
                       (if predicate
                         (content if-true)
                         (content if-false))))))))

;;takes a function (+, - , etc) and if any of the args,
;; which are input args in fun->prop-constructor,
;; have 'nothing', does nothing, else applys f to args
;; in order. (* arg1 arg2), etc.
(define (lift-to-cell-contents f)
  (lambda args
    (if (any nothing? args) nothing (apply f args))))

;;constraints here
;;takes a scheme function and returns a function that takes
;; a list of cells 1...n. Inputs are 1...(n-1) and ouput
;; is n. It then creates a propagator that gets added to 
;; all inputs, which will then re-run if *any* input's content
;; changes. When all inputs have content, then the propagator
;; will write using lift-to-cell-contents to the output.
(define (function->propagator-constructor f)
  (lambda cells
    (let ((output (car (last-pair cells)))
          (inputs (except-last-pair cells))
          (lifted-f (lift-to-cell-contents f)))
      (propagator inputs ; The output isn't a neighbor!
                  (lambda () (add-content output
                                          (apply lifted-f (map content inputs))))))))

;;primitives
(define (constant value) (function->propagator-constructor (lambda () value)))
(define adder (function->propagator-constructor +))
(define subtractor (function->propagator-constructor -))
(define multiplier (function->propagator-constructor *))
(define divider (function->propagator-constructor /))
(define absolute-value (function->propagator-constructor abs))
(define sqrter (function->propagator-constructor sqrt))
(define =? (function->propagator-constructor =))
(define <? (function->propagator-constructor <))
(define >? (function->propagator-constructor >))
(define <=? (function->propagator-constructor <=))
(define >=? (function->propagator-constructor >=))
(define inverter (function->propagator-constructor not))

(define (switch predicate if-true output)
  (conditional predicate if-true (make-cell 'conditional) output))

#|
(define conjoiner (function->propagator-constructor boolean/and))
(define disjoiner (function->propagator-constructor boolean/or))
(define squarer (function->propagator-constructor square))
|#

(define (heron-step x g h)
  (compound-propagator (list x g) ;inputs
                       (lambda () ;how to build
                         (let ((x/g (make-cell 'x/g))
                               (g+x/g (make-cell 'g+x/g))
                               (two (make-cell 'two)))
                           (divider x g x/g)
                           (adder g x/g g+x/g)
                           ((constant 2) two)
                           (divider g+x/g two h)))))

(define (sqrt-network x answer)
  (compound-propagator x ;;
                       (lambda ()
                         (let ((one (make-cell 'one)))
                           ((constant 1.) one)
                           (sqrt-iter x one answer)))))

(define (sqrt-iter x g answer)
  (compound-propagator (list x g)
                       (lambda ()
                         (let ((done (make-cell 'done))
                               (not-done (make-cell 'not-done))
                               (x-if-not-done (make-cell 'xifnotdone))
                               (g-if-not-done (make-cell 'gifnotdone))
                               (new-g (make-cell 'newg)))
                           (good-enuf? g x done)
                           (switch done g answer)
                           (inverter done not-done)
                           (switch not-done x x-if-not-done)
                           (switch not-done g g-if-not-done)
                           (heron-step x-if-not-done g-if-not-done new-g)
                           (sqrt-iter x-if-not-done new-g answer)))))

(define (good-enuf? g x done)
  (compound-propagator (list g x)
                       (lambda ()
                         (let ((g^2 (make-cell 'g^2))
                               (eps (make-cell 'eps))
                               (x-g^2 (make-cell 'x-g^2))
                               (ax-g^2 (make-cell 'axg^2)))
                           ((constant .00000001) eps)
                           (multiplier g g g^2)
                           (subtractor x g^2 x-g^2)
                           (absolute-value x-g^2 ax-g^2)
                           (<? ax-g^2 eps done)))))

(define (inf? w x y z)
  (compound-propagator (list w x y z)
                       (lambda ()
                         (let ((one (make-cell 'one)))
                           ((constant 1) one)
                           (adder w one x)
                           (adder x one y)
                           (adder y one z)
                           (adder z one w)
                           ))))

#|
(define x (make-cell)) ;;no neighbors or content
(define answer (make-cell)) ;;no neighbors or content

(sqrt-network x answer) 
(add-content x 2)

(content answer)
1.4142135623746899
|#

