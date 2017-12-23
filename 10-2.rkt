#lang racket
(require racket/block)

(define inp "206,63,255,131,65,80,238,157,254,24,133,2,16,0,1,3")

(define (str->lengths str)
  (append (map char->integer (string->list str)) (list 17 31 73 47 23))
  )

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
       (list reversed (modulo (+ current-pos length skip-size) len) (+ skip-size 1))
      )
    )
  )

(define (process-lengths start lengths)
  (if (empty? lengths) start
      (process-lengths ((apply tie-circle start) (car lengths)) (cdr lengths))
      )
  )

(define (process-n-times start lengths n)
  (if (= n 0)
      start
      (let* (
             (res (process-lengths start lengths))
             (circle (first res))
             (current-pos (second res))
             (skip-size (third res))
             )
        (process-n-times res lengths (- n 1))
        )
      )
  )

(define (xor-vec vec)
  (apply bitwise-xor (vector->list vec))
  )
  

(define (sparse->dense-hash w)
  (if (= (vector-length w) 0) '()
      (cons (xor-vec (vector-copy w 0 16)) (sparse->dense-hash (vector-drop w 16)))
      )
  )

(define (dense-hash->hexstring seq)
  (define (number->string2 n)
    (if (< n 16) (string-append "0" (number->string n 16))
        (number->string n 16)
        )
    )
  (apply string-append (map number->string2 seq))
  )

(define (make-hash inp-str)
  (let (
        (sparse-hash (car (process-n-times (list (list->vector (range 0 256)) 0 0) (str->lengths inp-str) 64)))
        )
    (dense-hash->hexstring (sparse->dense-hash sparse-hash))
    )
  )