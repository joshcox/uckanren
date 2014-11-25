#lang racket
(require racket/future future-visualizer)
(provide (all-defined-out))

(define (mandelbrot iterations x y n)
  (let ([ci (- (/ (* 2.0 y) n) 1.0)]
        [cr (- (/ (* 2.0 x) n) 1.5)])
    (let loop ([i 0] [zr 0.0] [zi 0.0])
      (if (> i iterations)
          i
          (let ([zrq (* zr zr)]
                [ziq (* zi zi)])
            (cond
             [(> (+ zrq ziq) 4) i]
             [else (loop (add1 i)
                         (+ (- zrq ziq) cr)
                         (+ (* 2 zr zi) ci))]))))))

;; (visualize-futures
;;  (let ([f (future (lambda () (mandelbrot 10000000 62 501 1000)))])
;;    (list (mandelbrot 10000000 62 500 1000)
;;          (touch f))))

(define (lvf)
  (visualize-futures
   (let ((f (future (lambda () (length '(1 2 3 4 5))))))
     (+ (length '(1 2 3 4 5)) (touch f)))))
