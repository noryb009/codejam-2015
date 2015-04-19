#lang racket

(define rounding 1e-5)
(define 2pi (* 2 pi))

(define (find-angle a b)
  (define ax (first a))
  (define ay (second a))
  (define bx (first b))
  (define by (second b))
  (define angle (atan (- by ay) (- bx ax)))
  (if (< angle 0)
      (+ angle 2pi)
      angle))

(define (angle-diff a b)
  (cond
    [(>= b a) (- b a)]
    [else (- (+ b 2pi) a)]))

(define (num-in-180 baseline angles)
  (length
   (filter
    (lambda (angle)
      (< rounding (angle-diff baseline angle) (- pi rounding)))
    angles)))

(define (run N t trees)
  (define angles (map (lambda (t2) (find-angle t t2)) trees))
  (foldr
   (lambda (angle acc)
     (min acc (num-in-180 angle angles)))
   (sub1 N)
   angles))

(define T (read))

(for ([i T])
  (define N (read))
  (define trees (build-list N (lambda (x) (list (read) (read)))))
  (printf "Case #~a:\n" (add1 i))
  (map (lambda (t)
         (printf "~a\n" (run N t (remove t trees))))
       trees))
