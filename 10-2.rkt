#lang racket
(require racket/block)


(define (reverse-vec vec from length)
  (let (
        (vec-copy (vector-copy vec))
        (len (vector-length vec))
        )
    (define (iter k)
      (if (= k length)
          vec-copy
          (block
           (vector-set! vec-copy (modulo (+ k from) len) (vector-ref vec (modulo (- (- (+ from length) 1) k) len)))
           (iter (+ k 1))
           )
          )
      )
    (iter 0)
    )
  )
                              
(define (tie-circle circle current-pos skip-size)
  (lambda (length)
    (let*
        (
         (len (vector-length circle))
         (c-copy (vector-copy circle))
         (reversed (reverse-vec c-copy current-pos length))
         )
      (block
       ;(display (list reversed (modulo (+ current-pos length skip-size) len) (+ skip-size 1)))
       (list reversed (modulo (+ current-pos length skip-size) len) (+ skip-size 1))
       )
      )
    )
  )

(define (process-lengths start lengths)
  (if (empty? lengths) start
      (process-lengths ((apply tie-circle start) (car lengths)) (cdr lengths))
      )
  )


(define (product-of-first-two-terms w)
  (* (vector-ref w 0) (vector-ref w 1))
  )

(define v (list->vector (range 0 256)))
(define lengths (list 206 63 255 131 65 80 238 157 254 24 133 2 16 0 1 3))
(define vv (list->vector (range 0 5)))
(define tlengths (list 3 4 1 5))

(define result (process-lengths (list v 0 0) lengths))
(define answer (product-of-first-two-terms (car result)))