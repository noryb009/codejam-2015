#lang racket

(define (first-method m)
  (define (first/inner m current)
    (cond
      [(empty? m) 0]
      [(>= (first m) current)
       (first/inner (rest m) (first m))]
      [else
       (+ (- current (first m))
          (first/inner (rest m) (first m)))]))
  (first/inner m 0))

(define (second-method m)
  (define (find-max-diff m)
    (cond
      [(empty? m) 0]
      [(empty? (rest m)) 0]
      [else
       (max (- (first m) (second m))
            (find-max-diff (rest m)))]))
  (define max-diff (find-max-diff m))
  (define (second/inner m current)
    (cond
      [(empty? m) 0]
      [(< current max-diff)
       (+ current
          (second/inner (rest m) (first m)))]
      [else
       (+ (max (- current (first m)) max-diff)
          (second/inner (rest m) (first m)))]))
  (second/inner m 0))

(define T (read))

(for ([i T])
  (define N (read))
  (define m (build-list N (lambda (x) (read))))
  (printf "Case #~a: ~a ~a\n"
          (add1 i)
          (first-method m)
          (second-method m)))
