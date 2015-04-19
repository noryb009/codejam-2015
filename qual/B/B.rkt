#lang racket

;; it is never optimal to wait before moving pancakes to a empty plate
;; it is never optimal to move pancakes to a non-empty plate

;; m is max number of pancakes allowed on a plate
;; count how many moves are required to have a maximum of m pancakes on all plates
(define (moves-for-min P m)
  (cond
    [(empty? P) 0]
    [else (+ (floor (/ (sub1 (first P)) m))
             (moves-for-min (rest P) m))]))

(define (run P)
  (apply min (build-list (apply max P)
                         (lambda (m) (+ (add1 m) (moves-for-min P (add1 m)))))))
  
(define T (read))

(for ([i T])
  (define D (read))
  (define P (build-list D (lambda (x) (read))))
  (printf "Case #~a: ~a\n"
          (add1 i)
          (run P)))
