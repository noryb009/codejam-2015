#lang racket

(define (run r c w)
  ;; remove first (sub1 r) rows
  (define skeleton
    (floor (/ c w)))
  (define ez
    (* (max 0 (sub1 r))
       skeleton))
  (define extra
    (cond [(= (modulo c w) 0) (sub1 w)]
          [else (+ (sub1 w)
                   (if (= w 1) 0 1))]))
  (+ ez skeleton extra))
    
  
(define T (read))

(for ([i T])
  (define R (read))
  (define C (read))
  (define W (read))
  (printf "Case #~a: ~a\n"
          (add1 i)
          (run R C W)))
