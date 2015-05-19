#lang racket

(define (overlap w1 w2)
  (define (overlap/inner w1 w2)
    (cond
      [(or (empty? w1) (empty? w2)) 0]
      [(equal? (first w1) (first w2))
       (add1 (overlap/inner (rest w1) (rest w2)))]
      [else 0]))
  (cond
    [(empty? w1) 0]
    [else
     (+ (overlap/inner w1 w2)
        (overlap (rest w1) w2))]))

(define (repeat w)
  (define len (length w))
  (define (repeat/in n)
    (cond
      [(> n len) 0]
      [(= (modulo len n) 0)
       (define part (take n w))
       (define (check rst l)
         (cond
           [(empty? rst) true]
           [(equal? (list-ref w l) (first rst))
            (check (rest rst) (modulo (add1 l) n))]
           [else false]))
       (if (check w)
           n
           (repeat/in (add1 n)))]
      [else
       (repeat/in (add1 n))]))
  (define rep (repeat/in 1))
  (cond
    [(= rep 0) 0]
    [else (/ len rep)]))

(define (unique-len w)
  (define (unique-len/in2 a b acc)
    (cond
      [(or (empty? a) (empty? b)) acc]
      [(equal? (first a) (first b))
       (unique-len/in2 (rest a) (rest b) (add1 acc))]
      [else 0]))
  (define (unique-len/in part)
    (cond
      [(empty? part) 0]
      [else
       (max (unique-len/in2 w part 0)
            (unique-len/in (rest part)))]))
  (- (length w) (unique-len/in (rest w))))

(define (possible? k w)
  (cond
    [(empty? w) true]
    [(member? (first w) k)
     (possible k (rest w))]
    [else false]))

(define (max-num k w S)
  (define len (length w))
  (define rep (repeat w))
  (define uni (unique-len w))
  (cond
    [(not (possible? k w))
     0]
    [(= rep 0)
     (add1 (floor (/ (- S len) uni)))]
    [else
     (define k (floor (/ (- S len) rep)))
     (add1 (floor (/ (- S len) rep)))]))

(define (k-short k)
  (define unique (remove-duplicates k equal?))
  (map 
   (lambda (l)
     (list l (length (filter (lambda (x) (equal? x l)) k)))) unique))

(define (not-empty? n)
  (not (empty? n)))

(define (avg-num k w S)
  (define w-len (length w))
  (define k-len (length k))
  (define k-short (k-short k))
  #|(define (chance w)
    (cond
      [(empty? w) 1]
      [else
       (define keys (assoc k-short (first w)))
       (* (/ keys k-len)
          (chance (rest w)))]))|#
  (define chances (map w (lambda (l) (filter k (lambda (x) (equal? x l))))))
  ;(define w-chance (chance w))
  
  (define (avg-num/in S strs)
    (cond
      [(= S 0) 0]
      [else
       (foldr
        (lambda (k-key acc)
          
          
          )
        0
        k-short)]))
  
  
  
  #|(define (avg-num/in S strs)
    (cond
      [(= 0 S) 0]
      [else
       (define done (length (filter empty? strs)))
       (define new-strs (append (list w)
                                (map (filter not-empty? strs) rest)))
       (define rst (avg-num/in (sub1 S)
                               new-strs))
       (define (chance-next strs)
         )
      ))|#

(define (run k w S)
  (define bring (max-num k w S))
  (cond
    [(= bring 0) 0]
    [else
     (define given (avg-num (give k w S)))
     (/ bring given)]))

(define T (read))

(for ([i T])
  (define K (read))
  (define L (read))
  (define S (read))
  (define keyboard (string->list (read)))
  (define word (string->list (read)))
  (printf "Case #~a: ~a\n"
          (add1 i)
          (run keyboard word S)))
