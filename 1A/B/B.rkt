#lang racket

(define (cust-after-time M t)
  (foldr
   (lambda (m acc)
     (+ (max (ceiling (/ t m)) 0) acc))
   0
   M))

(define (find-time M N)
  (define step (expt 2 30))
  (define current 0)
  
  (let loop ()
    (set! current (+ current step))
    (define c (cust-after-time M current))
    (unless (>= c N)
      (loop)))
  
  (set! current (- current step))
  (set! step (/ step 2))
  
  (let loop ()
    (unless (< step 1)
      (define c (cust-after-time M (+ current step)))
      (if (< c N)
          (set! current (+ current step))
          (void))
      (set! step (/ step 2))
      (loop)))
  
  current)

(define (run B N M)
  #|(define (find-time current growth backtracking)
    (define c (cust-after-time M current))
    (cond
      [backtracking
       (cond
         [(<= growth 1)
          (if (> c N)
              (sub1 current)
              current)]
         [(>= c N)
          (find-time (- current (/ growth 2)) (/ growth 2) true)]
         [else
          (find-time (+ current (/ growth 2)) (/ growth 2) true)])]
      [(>= c N)
       (if (<= growth 1)
           (if (> c N)
              (sub1 current)
              current)
           (find-time (- current (/ growth 2)) (/ growth 2) true))]
      [else
       (find-time (+ current growth) (* growth 2) false)]))|#
  (define (free-on-nth-min M n line count)
    (cond
      [(empty? M) 1]
      [(zero? (modulo n (first M)))
       (if (= line 1)
           count
           (free-on-nth-min (rest M) n (sub1 line) (add1 count)))]
      [else (free-on-nth-min (rest M) n line (add1 count))]))
  (define time
    (find-time M N))
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
