#lang racket

;; 10^16 is big
;; like, really big
;; each block boils down to either +-: 1, i, j, k
;; each 2 blocks boils down to 1 or -1
;; each 4 blocks boils down to 1

;; we can multiply from the left until we reach an i;
;;   an (i * j) = k, then an (i * j * k) = -1. Then
;;   check if the remaining digits boil down to 1.

;; multiplying

(define (q-to-index q)
  (match (second q)
    [1 0]
    ['i 1]
    ['j 2]
    ['k 3]))

(define mult-table
  '(((+ 1) (+ i) (+ j) (+ k))
    ((+ i) (- 1) (+ k) (- j))
    ((+ j) (- k) (- 1) (+ i))
    ((+ k) (+ j) (- i) (- 1))))

(define (signs* a b)
  (cond [(symbol=? a b) '+]
        [else '-]))

(define (q* a b)
  (define absolute (list-ref (list-ref mult-table (q-to-index a))
                             (q-to-index b)))
  (define sign (signs* (first absolute) (signs* (first a) (first b))))
  (list sign (second absolute)))

;; actual code

(define one '(+ 1))

(define (run S L X)
  (define len (* L X))
  (define loop-len (min (* L 4) len))
  
  (define (run/inner needed orig-i N)
    (cond
      [(empty? needed)
       (define (mult-until-loop N i rem)
         (cond
           [(= rem 0)
            (equal? N one)]
           [else
            (mult-until-loop (q* N (vector-ref S (modulo i L))) (add1 i) (sub1 rem))]))
       (mult-until-loop one orig-i (modulo (- len orig-i) loop-len))]
      [else
       (define (run/inner/inner i N)
         (cond
           [(equal? N (first needed))
            (run/inner (rest needed) i N)]
           [(or (= i len)
                (and (= 0 (modulo (- i orig-i) loop-len)) (not (= i orig-i))))
            false]
           [else
            (run/inner/inner (add1 i) (q* N (vector-ref S (modulo i L))))]))
       (run/inner/inner orig-i N)]))
  (run/inner needed 0 one))

(define needed `((+ i) ,(q* '(+ i) '(+ j)) ,(q* (q* '(+ i) '(+ j)) '(+ k))))
(define (bool-to-result b)
  (cond [b "YES"]
        [else "NO"]))

(define T (read))

(for ([i T])
  (define L (read))
  (define X (read))
  (read-char) ; newline
  (define S (build-vector L (lambda (x) (list '+ (string->symbol (list->string (list (read-char))))))))
  (printf "Case #~a: ~a\n"
          (add1 i)
          (bool-to-result (run S L X))))
