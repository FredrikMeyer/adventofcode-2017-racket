#lang racket

(define vv
   (list->vector (map string->number (map string (string->list (string-trim (file->string "input1.txt"))))))
  )

;; vector (a,b,c). True if v[i]=v[i+1] (mod 3)
(define (matches-next v i)
  (let ((this-pos (vector-ref v i))
        (next-pos (vector-ref v (remainder (+ i 1) (vector-length v))))
        )
    (equal? this-pos next-pos)
    )
  )

(define v (vector 1 1 2 3 1))

(define (sum-matching v)
  (let ((matching-indices
         (filter (lambda (i) (matches-next v i)) (range 0 (vector-length v)))
         )
        )
    (foldl (lambda (x y) (+ x y)) 0 (map (lambda (i) (vector-ref v i)) matching-indices))
  )
  )