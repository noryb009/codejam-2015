#lang racket

(define rounding 1e-12)
(define 2pi (* 2 pi))

(define (find-angle a b)
  (define ax (first a))
  (define ay (second a))
  (define bx (first b))
  (define by (second b))
  (define angle (atan (- by ay) (- bx ax))))

(define (run N t trees)
  (define n (sub1 N))
  (define angles
    (list->vector (sort (map (lambda (t2) (find-angle t t2))
                             trees) <)))
  ;; index of first invalid angle, after i
  (define (smallest-j i cur j)
    (cond
      [(< (- (+ (vector-ref angles (modulo j n))
                (* 2pi (floor (/ j n))))
             cur)
          (- pi rounding))
       (smallest-j i cur (add1 j))]
      [else j]))
  (define (smallest-after-i start i j)
    (cond
      [(= (- i start) n) n]
      [else
       (define new-j
         (smallest-j i (vector-ref angles i) (max j (add1 i))))
       (min (max (- new-j i 1) 0)
            (smallest-after-i start (add1 i) new-j))]))
  (smallest-after-i 0 0 1))
      

(define T (read))

(for ([i T])
  (define N (read))
  (define trees (build-list N (lambda (x) (list (read) (read)))))
  (printf "Case #~a:\n" (add1 i))
  (map (lambda (t)
         (printf "~a\n" (run N t (remove t trees))))
       trees))
