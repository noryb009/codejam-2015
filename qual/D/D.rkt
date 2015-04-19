#lang racket

;; simple no:
;; if X >= 7, since there can be a space surrounded by blocks
;; if (R * C) % X != 0, since can't fit an even amount

;; after above:
;; if R >= X and C >= X, always works
;; if X == 3, x2
;; if X == 4, x3
;; if X == 5, x3
;; if X == 6, x4

(define (run X R C)
  (define smaller (min R C))
  (define larger (max R C))
  (and (not (or (>= X 7)
                (not (= (modulo (* R C) X) 0))
                (< larger X)
                (and (= X 5) (= larger 5) (= smaller 3))))
       (>= smaller (match X
                     [1 1]
                     [2 1]
                     [3 2]
                     [4 3]
                     [5 3]
                     [6 4]))))

(define (bool-to-result b)
  (cond [b "GABRIEL"]
        [else "RICHARD"]))

(define T (read))

(for ([i T])
  (define X (read))
  (define R (read))
  (define C (read))
  (printf "Case #~a: ~a\n"
          (add1 i)
          (bool-to-result (run X R C))))
