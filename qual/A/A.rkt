#lang racket

(define (run k)
  ;; list of all inputs; needed to stand; currently standing
  (define (run/inner in n S)
    (cond
      [(empty? in) 0]
      [(> n S)
       (+ (- n S)
          (run/inner (rest in) (add1 n) (+ n (first in))))]
      [else
       (run/inner (rest in) (add1 n) (+ S (first in)))]))
  (run/inner
   (map (lambda (x) (- (char->integer x) (char->integer #\0)))
        (string->list k))
   0 0))

(define T (read))

(for ([i T])
  (define Smax (read))
  (define in (string-trim (read-line)))
  (printf "Case #~a: ~a\n"
          (add1 i)
          (run in)))
