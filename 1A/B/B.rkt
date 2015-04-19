#lang racket

(define (cust-after-time M t)
  (foldr
   (lambda (m acc)
     (+ (max (ceiling (/ t m)) 0) acc))
   0
   M))

(define (binary-search get-val valid)
  (define (binary/bounded l u)
    (cond
      [(< l (sub1 u))
       (define mid (floor (/ (+ l u) 2)))
       (cond
         [(valid mid)
          (binary/bounded mid u)]
         [else
          (binary/bounded l mid)])]
      [(valid l) l]
      [else u]))
  (define (binary/inner c g)
    (define val (get-val c))
    (cond
      [(valid val)
       (binary/inner (+ c g) (* g 2))]
      [else
       (binary/bounded 0 c)]))
  (binary/inner 0 1))

(define (find-time M N)
  (binary-search
   (lambda (t) (cust-after-time M t))
   (lambda (c) (>= c N))))

(define (free-on-nth-min M n line count)
    (cond
      [(empty? M) 1]
      [(zero? (modulo n (first M)))
       (if (= line 1)
           count
           (free-on-nth-min (rest M) n (sub1 line) (add1 count)))]
      [else (free-on-nth-min (rest M) n line (add1 count))]))

(define (run B N M)
  (define time
    (find-time M N 0 1 false))
  (cond
    [(<= N B) N]
    [else
     (free-on-nth-min M time (- N (cust-after-time M time)) 1)]))

(define T (read))

(for ([i T])
  (define B (read))
  (define N (read))
  (define M (build-list B (lambda (x) (read))))
  (printf "Case #~a: ~a\n"
          (add1 i)
          (run B N M)))
